import {Array, Effect, Queue, Random} from "effect"
import {HttpClient, HttpClientResponse} from "effect/http"
import {NodeHttpClient} from "@effect/platform-node"
import {Worker} from "node:worker_threads"


function scenarioUrl(port: number, scenario: number, query?: string): string {
    const url = `http://localhost:${port}/${scenario}`
    return query === undefined ? url : `${url}?${query}`
}

export type Scenario = (port: number) => Effect.Effect<string, unknown, HttpClient.HttpClient>

export function scenario1(port: number) {
    const req = Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 1))
        return yield* resp.text
    })

    return Effect.race(req, req)
}

export function scenario2(port: number) {
    const req = Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 2))
        return yield* resp.text
    })

    return Effect.race(req, req)
}

export function scenario3(port: number) {
    const req = Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 3))
        return yield* resp.text
    })

    return Effect.raceAll(Array.replicate(req, 10000))
}

export function scenario4(port: number) {
    const req = Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 4))
        return yield* resp.text
    })

    return Effect.race(req, req.pipe(Effect.timeout("1 second")))
}

export function scenario5(port: number) {
    const req = Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 5))
        const valid = yield* HttpClientResponse.filterStatusOk(resp)
        return yield* valid.text
    })

    return Effect.race(req, req)
}

export function scenario6(port: number) {
    const req = Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 6))
        const valid = yield* HttpClientResponse.filterStatusOk(resp)
        return yield* valid.text
    })

    return Effect.raceAll([req, req, req])
}

export function scenario7(port: number) {
    const req = Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 7))
        return yield* resp.text
    })

    return Effect.race(req, req.pipe(Effect.delay("3 seconds")))
}

export function scenario8(port: number) {
    const open = Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 8, "open"))
        const valid = yield* HttpClientResponse.filterStatusOk(resp)
        return yield* valid.text
    })
    const use = (id: string) => Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 8, `use=${id}`))
        const valid = yield* HttpClientResponse.filterStatusOk(resp)
        return yield* valid.text
    })
    const close = (id: string) => Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 8, `close=${id}`))
        const valid = yield* HttpClientResponse.filterStatusOk(resp)
        return yield* valid.text
    }).pipe(Effect.orDie)

    const reqRes = Effect.acquireUseRelease(open, use, close)

    return Effect.race(reqRes, reqRes)
}

export function scenario9(port: number) {
    return Effect.gen(function*() {
        const responses = yield* Queue.unbounded<string>()
        const req = Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 9))
            const valid = yield* HttpClientResponse.filterStatusOk(resp)
            yield* Queue.offer(responses, yield* valid.text)
        }).pipe(Effect.ignore)

        yield* Effect.all(Array.replicate(req, 10), {concurrency: "unbounded", discard: true})
        return (yield* Queue.clear(responses)).join("")
    })
}

export function scenario10(port: number) {
    const cpuLoad = (previousUsage: NodeJS.CpuUsage, previousTime: bigint): [number, NodeJS.CpuUsage, bigint] => {
        const usage = process.cpuUsage(previousUsage)
        const now = process.hrtime.bigint()
        const elapsedMicros = Number((now - previousTime) / BigInt(1000))
        const usedMicros = usage.user + usage.system
        const load = elapsedMicros === 0 ? 0 : Math.min(1, Math.max(0, usedMicros / elapsedMicros))
        return [load, process.cpuUsage(), now]
    }

    const busyWorker = Effect.acquireRelease(
        Effect.try(() => new Worker(`
            const crypto = require("node:crypto");
            let seed = crypto.randomBytes(512);
            while (true) {
                seed = crypto.createHash("sha512").update(seed).digest();
            }
        `, {eval: true})),
        (worker) => Effect.promise(() => worker.terminate())
    )

    const blocker = (id: string) =>
        Effect.scoped(Effect.gen(function*() {
            yield* busyWorker
            const resp = yield* HttpClient.get(scenarioUrl(port, 10, id))
            return yield* resp.text
        }))

    const report = (id: string, previousUsage: NodeJS.CpuUsage, previousTime: bigint): Effect.Effect<string, unknown, HttpClient.HttpClient> =>
        Effect.gen(function*() {
            const [load, usage, time] = cpuLoad(previousUsage, previousTime)
            const resp = yield* HttpClient.get(scenarioUrl(port, 10, `${id}=${load}`))
            if (resp.status >= 200 && resp.status < 300) {
                return yield* resp.text
            }
            if (resp.status >= 300 && resp.status < 400) {
                return yield* report(id, usage, time).pipe(Effect.delay("1 second"))
            }
            return yield* Effect.fail(new Error(yield* resp.text))
        })

    return Effect.scoped(Effect.gen(function*() {
        const id = (yield* Random.nextIntBetween(0, Number.MAX_SAFE_INTEGER)).toString(36)
        yield* Effect.forkScoped(blocker(id))
        return yield* report(id, process.cpuUsage(), process.hrtime.bigint())
    }))
}

export function scenario11(port: number) {
    const req = Effect.gen(function*() {
        const resp = yield* HttpClient.get(scenarioUrl(port, 11))
        return yield* resp.text
    })

    return Effect.race(Effect.race(req, req), req)
}

export const scenarios = [
    scenario1,
    scenario2,
    scenario3,
    scenario4,
    scenario5,
    scenario6,
    scenario7,
    scenario8,
    scenario9,
    scenario10,
    scenario11,
] satisfies ReadonlyArray<Scenario>

export function program(port: number) {
    return Effect.forEach(scenarios, (scenario) => scenario(port)).pipe(
        Effect.provide(NodeHttpClient.layerNodeHttp)
    )
}
