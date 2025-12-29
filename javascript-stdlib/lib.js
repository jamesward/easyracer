import * as net from "net";

function url(port, scenario) {
    return `http://localhost:${port}/${scenario}`
}

async function raceWithCancellation(racers) {
    const racersWithCleanup = racers.map((racer) => {
        const controller = new AbortController()

        const promise = racer(controller.signal).then(
            (response) => ({ response, status: 'completed', controller }),
            (error) => ({ error, status: 'failed', controller })
        )

        return { promise, controller }
    })

    const racerPromises = racersWithCleanup.map(r => r.promise)

    // Wait for first successful completion
    const results = await new Promise((resolve) => {
        const settled = []
        let resolved = false

        racerPromises.forEach((p, i) => {
            p.then((result) => {
                settled[i] = result
                if (!resolved && result.status === 'completed') {
                    resolved = true
                    resolve({ winner: result, all: racerPromises })
                }
            })
        })

        // Fallback if all fail
        Promise.allSettled(racerPromises).then((all) => {
            if (!resolved) resolve({ winner: null, all })
        })
    })

    // Abort all non-winners
    racersWithCleanup.forEach((racer, i) => {
        if (racer !== results.winner?.controller) {
            racer.controller.abort()
        }
    })

    // Wait for all connections to fully close by cancelling response bodies
    const cleanupPromises = racerPromises.map(async (p) => {
        const result = await p
        if (result !== results.winner && result.status === 'completed') {
            // Cancel the response body stream to close the connection
            await result.response.body?.cancel()
        }
    })

    await Promise.all(cleanupPromises)

    if (!results.winner) {
        throw new AggregateError(racersWithCleanup.map(r => r.error), 'All racers failed')
    }

    return results.winner.response
}

export async function scenario1(port) {
    const req = async (signal) => {
        const resp = await fetch(url(port, 1), { signal })
        return resp.text()
    }

    return raceWithCancellation([req, req])
}

export async function scenario2(port) {
    const req = async (signal) => {
        const resp = await fetch(url(port, 2), { signal })
        return resp.text()
    }

    return raceWithCancellation([req, req])
}

export async function scenario3(port) {
    const req = async (signal) => {
        const resp = await fetch(url(port, 3), { signal })
        return resp.text()
    }
    const reqs = Array.from(new Array(10000), () => req)
    return raceWithCancellation(reqs)
}

export async function scenario4(port) {
    const req = async () => {
        const resp = await fetch(url(port, 4))
        return resp.text()
    }

    const reqWithTimeout = async () => {
        const controller = new AbortController()
        const timeoutId = setTimeout(() => controller.abort(), 1000)

        try {
            const resp = await fetch(url(port, 4), {signal: controller.signal})
            clearTimeout(timeoutId)
            return resp.text()
        } catch (error) {
            clearTimeout(timeoutId)
            throw error
        }
    }

    return Promise.any([req(), reqWithTimeout()])
}

export async function scenario5(port) {
    const req = async (signal) => {
        const resp = await fetch(url(port, 5), { signal })
        if (resp.status >= 300) {
            return Promise.reject(new Error("Not a successful response"))
        }
        else {
            return resp.text()
        }
    }

    return raceWithCancellation([req, req])
}

export async function scenario6(port) {
    const req = async (signal) => {
        const resp = await fetch(url(port, 6), { signal })
        if (resp.status >= 300) {
            return Promise.reject(new Error("Not a successful response"))
        }
        else {
            return resp.text()
        }
    }

    return raceWithCancellation([req, req, req])
}

export async function scenario7(port) {

    const req = async (signal) => {
        return fetch(url(port, 7) , { signal })
            .then((resp) => resp.text())
    }

    const hedge = async (signal) => {
        await new Promise((_, reject) => setTimeout(() => {
            // not sure why I need the catch & reject here
            req(signal).catch((error) => {
                reject(error)
            })
        }, 3000))
    }

    return raceWithCancellation([req, hedge])
}

export async function scenario8(port) {
    const req = async (params, signal) => {
        const resp = await fetch(url(port, 8) + `?${params}`, { signal })
        if (resp.status >= 300) {
            return Promise.reject(new Error("Not a successful response"))
        }
        else {
            return resp.text()
        }
    }

    const open = (signal) => req("open", signal)
    const use = (id, signal) => req(`use=${id}`, signal)
    const close = (id, signal) => req(`close=${id}`, signal)

    const reqRes = async (signal) => {
        const id = await open(signal)
        try {
            return await use(id, signal)
        }
        finally {
            await close(id, signal)
        }
    }

    return raceWithCancellation([reqRes, reqRes])
}

export async function scenario9(port) {
    const req = async () => {
        const resp = await fetch(url(port, 9))
        if (resp.status >= 300) {
            return Promise.reject(new Error("Not a successful response"))
        }
        else {
            return { now: performance.now(), text: await resp.text() }
        }
    }

    const reqs = Array.from(new Array(10), req)
    const responses = await Promise.allSettled(reqs)
    const successes = responses.filter((result) => result.status === "fulfilled")
    const ordered = successes.sort((a, b) => a.value.now - b.value.now)
    return ordered.reduce((acc, resp) => acc + resp.value.text, "")
}
