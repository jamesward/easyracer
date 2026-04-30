import {Array, Chunk, Effect, Queue, Random} from "effect"
import {seconds} from "effect/Duration"
import {HttpClient, HttpClientResponse} from "@effect/platform"
import {NodeHttpClient} from "@effect/platform-node"
import {Worker} from "node:worker_threads"


function scenarioUrl(port: number, scenario: number, query?: string): string {
    const url = `http://localhost:${port}/${scenario}`
    return query === undefined ? url : `${url}?${query}`
}

export type Scenario = (port: number) => Effect.Effect<string, unknown, HttpClient.HttpClient>

export function scenario1(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 1))
            return yield* resp.text
        })
    )

    return Effect.race(req, req)
}

export function scenario2(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 2))
            return yield* resp.text
        })
    )

    return Effect.race(req, req)
}

export function scenario3(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 3))
            return yield* resp.text
        })
    )

    return Effect.raceAll(Array.replicate(req, 10000))
}

export function scenario4(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 4))
            return yield* resp.text
        })
    )

    return Effect.race(req, req.pipe(Effect.timeout("1 second")))
}

export function scenario5(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 5))
            const valid = yield* HttpClientResponse.filterStatusOk(resp)
            return yield* valid.text
        })
    )

    return Effect.race(req, req)
}

export function scenario6(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 6))
            const valid = yield* HttpClientResponse.filterStatusOk(resp)
            return yield* valid.text
        })
    )

    return Effect.raceAll([req, req, req])
}

export function scenario7(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 7))
            return yield* resp.text
        })
    )

    return Effect.race(req, Effect.delay(req, seconds(3)))
}

export function scenario8(port: number) {
    const open = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 8, "open"))
            const valid = yield* HttpClientResponse.filterStatusOk(resp)
            return yield* valid.text
        })
    )
    const use = (id: string) => Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 8, `use=${id}`))
            const valid = yield* HttpClientResponse.filterStatusOk(resp)
            return yield* valid.text
        })
    )
    const close = (id: string) => Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 8, `close=${id}`))
            const valid = yield* HttpClientResponse.filterStatusOk(resp)
            return yield* valid.text
        })
    ).pipe(Effect.orDie)

    const reqRes = Effect.acquireUseRelease(open, use, close)

    return Effect.race(reqRes, reqRes)
}

export function scenario9(port: number) {
    return Effect.gen(function*() {
        const responses = yield* Queue.unbounded<string>()
        const req = Effect.scoped(
            Effect.gen(function*() {
                const resp = yield* HttpClient.get(scenarioUrl(port, 9))
                const valid = yield* HttpClientResponse.filterStatusOk(resp)
                const text = yield* valid.text
                yield* Queue.offer(responses, text)
            })
        )

        yield* Effect.all(Array.replicate(req, 10), {concurrency: "unbounded", discard: true})
        return Chunk.toArray(yield* Queue.takeAll(responses)).join("")
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

    const acquireBusyWorker = () =>
        Effect.acquireRelease(
            Effect.try({
                try: () => new Worker(`
                const crypto = require("node:crypto");
                let seed = crypto.randomBytes(512);
                while (true) {
                    seed = crypto.createHash("sha512").update(seed).digest();
                }
            `, {eval: true}),
                catch: (error) => error instanceof Error ? error : new Error(String(error))
            }),
            (worker) => Effect.promise(() => worker.terminate()).pipe(Effect.asVoid, Effect.orDie)
        )

    const poll = (id: string, previousUsage: NodeJS.CpuUsage, previousTime: bigint) =>
        Effect.scoped(
            Effect.gen(function*() {
                const [load, usage, time] = cpuLoad(previousUsage, previousTime)
                const resp = yield* HttpClient.get(scenarioUrl(port, 10, `${id}=${load}`))

                if (resp.status >= 200 && resp.status < 300) {
                    return {type: "done" as const, text: yield* resp.text}
                }

                if (resp.status >= 300 && resp.status < 400) {
                    return {type: "retry" as const, usage, time}
                }

                const body = yield* resp.text
                return yield* Effect.fail(new Error(body))
            })
        )

    const report = (id: string, previousUsage: NodeJS.CpuUsage, previousTime: bigint): Effect.Effect<string, unknown, HttpClient.HttpClient> =>
        Effect.gen(function*() {
            const result = yield* poll(id, previousUsage, previousTime)
            if (result.type === "done") {
                return result.text
            }
            return yield* report(id, result.usage, result.time).pipe(Effect.delay(seconds(1)))
        })

    const blocker = (id: string) =>
        Effect.scoped(
            Effect.gen(function*() {
                yield* acquireBusyWorker()
                const resp = yield* HttpClient.get(scenarioUrl(port, 10, id))
                yield* resp.text
            })
        ).pipe(Effect.asVoid)

    return Effect.scoped(Effect.gen(function*() {
        const id = (yield* Random.nextIntBetween(0, Number.MAX_SAFE_INTEGER)).toString(36)
        yield* Effect.forkScoped(blocker(id))
        return yield* report(id, process.cpuUsage(), process.hrtime.bigint())
    }))
}

export function scenario11(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 11))
            return yield* resp.text
        })
    )

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
    return Effect.gen(function* () {
        const results: Array<string> = []
        for (const scenario of scenarios) {
            results.push(yield* scenario(port))
        }
        return results
    }).pipe(
        Effect.provide(NodeHttpClient.layer)
    )
}
