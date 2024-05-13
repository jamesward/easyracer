import { Chunk, Effect } from "effect"
import * as Http from "@effect/platform/HttpClient"

function scenarioUrl(port: number, scenario: number): string {
    return `http://localhost:${port}/${scenario}`
}

function scenario1(port: number) {
    const req = Effect.gen(function* () {
        const res = yield* Http.request.get(scenarioUrl(port, 1))
        return yield* res.text
    }).pipe(Effect.scoped)

    return Effect.race(req, req)
}

function scenario2(port: number) {
    const req = Effect.gen(function* () {
        const res = yield* Http.request.get(scenarioUrl(port, 2))
        return yield* res.text
    }).pipe(Effect.scoped)

    return Effect.race(req, req)
}

function scenario3(port: number) {
    const req = Effect.gen(function* () {
        const res = yield* Http.request.get(scenarioUrl(port, 3))
        return yield* res.text
    }).pipe(Effect.scoped)

    const reqs = Chunk.range(1, 10000).pipe(
        Chunk.map(() => req)
    )

    return Effect.raceAll(reqs)
}

function scenario4(port: number) {
    const req = Effect.gen(function* () {
        const res = yield* Http.request.get(scenarioUrl(port, 4))
        return yield* res.text
    }).pipe(Effect.scoped)


    return Effect.race(req, req.pipe(Effect.timeout("3 seconds")))
}

export function program(port: number) {
    return Effect.gen(function* () {
        return [
            yield* scenario1(port),
            yield* scenario2(port),
            yield* scenario3(port),
            yield* scenario4(port),
        ]
    }).pipe(Effect.provide(Http.client.layer))
}
