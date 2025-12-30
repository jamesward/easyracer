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
                resolve({ statusCode: res.statusCode, text: data })
            })
            res.on('error', reject)
        })

        req.on('error', reject)

        if (signal) {
            signal.addEventListener('abort', () => {
                req.destroy(new Error('Request aborted'))
            })
        }
    })
}
