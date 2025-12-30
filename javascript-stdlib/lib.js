import http from "node:http";

export function url(port, scenario) {
    return `http://localhost:${port}/${scenario}`
}

// races promises
// losers are cancelled
// winner is returned
export async function raceWithCancellation(racers) {
    const controllers = racers.map(() => new AbortController())
    const promises = racers.map((racer, i) => racer(controllers[i].signal))

    return new Promise((resolve, reject) => {
        let settled = false
        promises.forEach((promise, winnerIndex) => {
            promise.then((value) => {
                if (!settled) {
                    settled = true
                    // Cancel all losers
                    controllers.forEach((controller, i) => {
                        if (i !== winnerIndex) {
                            controller.abort()
                        }
                    })
                    resolve(value)
                }
            }).catch((err) => {
                // Ignore errors from cancelled promises
            })
        })
    })
}

// make a cancelable http request
// use the node http module
// returned promise has statusCode and text (not a promise)
// cancelled promises are not rejected until the connection is closed
// request or response errors are rejected
export async function httpGet(url, signal) {
    return new Promise((resolve, reject) => {
        const req = http.get(url, (res) => {
            let data = ''
            res.on('data', (chunk) => {
                data += chunk
            })
            res.on('end', () => {
                resolve({statusCode: res.statusCode, text: data})
            })
            res.on('error', reject)
        })

        req.on('error', reject)

        if (signal) {
            const onAbort = () => {
                req.destroy(new Error('Request aborted'))
            }
            signal.addEventListener('abort', onAbort)

            // Clean up the listener when request completes
            req.on('close', () => {
                signal.removeEventListener('abort', onAbort)
            })
        }
    })
}

export async function scenario1(port) {
    const req = async (signal) => httpGet(url(port, 1), signal).then(resp => resp.text)
    return raceWithCancellation([req, req])
}

export async function scenario2(port) {
    const req = async (signal) => httpGet(url(port, 2), signal).then(resp => resp.text)
    return raceWithCancellation([req, req])
}

export async function scenario3(port) {
    const req = async (signal) => httpGet(url(port, 3), signal).then(resp => resp.text)
    const reqs = Array.from(new Array(10000), () => req)
    return raceWithCancellation(reqs)
}

export async function scenario4(port) {
    const req = async (signal) => httpGet(url(port, 4), signal).then(resp => resp.text)
    const reqWithTimeout = async (signal) => {
        const timeoutController = new AbortController()
        const timeout = setTimeout(() => timeoutController.abort(), 1000)

        signal?.addEventListener('abort', () => timeoutController.abort())

        try {
            return await httpGet(url(port, 4), timeoutController.signal).then(resp => resp.text)
        } finally {
            clearTimeout(timeout)
        }
    }
    return raceWithCancellation([req, reqWithTimeout])
}

export async function scenario5(port) {
    const req = async (signal) => {
        const resp = await httpGet(url(port, 5), signal)
        if (resp.statusCode !== 200) {
            throw new Error(`Non-200 response: ${resp.statusCode}`)
        }
        return resp.text
    }
    return raceWithCancellation([req, req])
}

export async function scenario6(port) {
    const req = async (signal) => {
        const resp = await httpGet(url(port, 6), signal)
        if (resp.statusCode !== 200) {
            throw new Error(`Non-200 response: ${resp.statusCode}`)
        }
        return resp.text
    }
    return raceWithCancellation([req, req, req])
}

export async function scenario7(port) {
    const req = async (signal) => httpGet(url(port, 7), signal).then(resp => resp.text)
    const delayedReq = async (signal) => {
        await new Promise(resolve => setTimeout(resolve, 3000))
        if (signal?.aborted) throw new Error('Aborted')
        return httpGet(url(port, 7), signal).then(resp => resp.text)
    }
    return raceWithCancellation([req, delayedReq])
}

export async function scenario8(port) {
    const req = async (params, signal) => {
        const resp = await httpGet(url(port, 8) + `?${params}`, signal)
        if (resp.statusCode >= 300) {
            throw new Error("Not a successful response")
        }
        return resp.text
    }

    const open = (signal) => req("open", signal)
    const use = (id, signal) => req(`use=${id}`, signal)
    const close = (id, signal) => req(`close=${id}`, signal)

    const reqRes = async (signal) => {
        const id = await open(signal)
        try {
            return await use(id, signal)
        } finally {
            await close(id, signal)
        }
    }

    return raceWithCancellation([reqRes, reqRes])
}

export async function scenario9(port) {
    const req = async () => {
        const resp = await httpGet(url(port, 9))
        if (resp.statusCode >= 300) {
            throw new Error("Not a successful response")
        }
        return {now: performance.now(), text: resp.text}
    }

    const reqs = Array.from(new Array(10), req)
    const responses = await Promise.allSettled(reqs)
    const successes = responses.filter((result) => result.status === "fulfilled")
    const ordered = successes.sort((a, b) => a.value.now - b.value.now)
    return ordered.reduce((acc, resp) => acc + resp.value.text, "")
}
