import * as net from "net";

function url(port, scenario) {
    return `http://localhost:${port}/${scenario}`
}

async function raceWithCancellation(racers) {
    const racersAndSignals = racers.map((racer) => {
        const controller = new AbortController()
        const signal = controller.signal

        return {
            promise: racer(signal).then((result) => {
                controller.isFinished = true
                return result
            }),
            controller: controller
        }
    })

    const racerPromises = racersAndSignals.map(racer => racer.promise)
    return Promise.any(racerPromises).then((winner) =>  {
        racersAndSignals.forEach((racer) => {
            if (racer.controller.isFinished === undefined) {
                racer.controller.abort()
            }
        })
        // wait for all the promises to settle before completing the winner
        // note that this doesn't actually wait for all the loser connections to close
        return Promise.allSettled(racerPromises).then(() => winner)
    })
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

/*
On slow machines this one can't use fetch for the timeout'd request because:
scenario3 finishes and the connections are still being closed (there doesn't seem to be a way to wait until they are all closed to continue).
Then when scenario4 runs, fetch doesn't immediately make a connection (because the libuv thread pool is exhausted).
This causes the timeout to stop making the second connection before it actually makes the connection.
So instead of using a fetch for the timeout'd request, use a Socket and do not start the timeout until the request connects.
 */
export async function scenario4(port) {
    const req = async () => {
        const resp = await fetch(url(port, 4))
        return resp.text()
    }

    const reqWithTimeout = () => {
        const client = net.createConnection({ port }, () => {
            client.write('GET /4 HTTP/1.1\n\n');
        })

        // don't worry about this request returning a result, cause it will always be the loser
        return new Promise((_, reject) => {
            client.on("connect", () => {
                setTimeout(() => {
                    client.end()
                    reject(new Error("Request timed out"))
                }, 1000)
            })
        })
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
