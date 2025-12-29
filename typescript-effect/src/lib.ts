import {Array, Effect} from "effect"
import {seconds} from "effect/Duration"
import {HttpClient, HttpClientResponse} from "@effect/platform"
import {NodeHttpClient} from "@effect/platform-node"


function scenarioUrl(port: number, scenario: number): string {
    return `http://localhost:${port}/${scenario}`
}

function scenario1(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 1))
            return yield* resp.text
        })
    )

    return Effect.race(req, req)
}

function scenario2(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 2))
            return yield* resp.text
        })
    )

    return Effect.race(req, req)
}

function scenario3(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 3))
            return yield* resp.text
        })
    )

    return Effect.raceAll(Array.replicate(req, 10000))
}

function scenario4(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 4))
            return yield* resp.text
        })
    )

    return Effect.race(req, req.pipe(Effect.timeout("1 second")))
}

function scenario5(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 5))
            const valid = yield* HttpClientResponse.filterStatusOk(resp)
            return yield* valid.text
        })
    )

    return Effect.race(req, req)
}

function scenario6(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 6))
            const valid = yield* HttpClientResponse.filterStatusOk(resp)
            return yield* valid.text
        })
    )

    return Effect.raceAll([req, req, req])
}

function scenario7(port: number) {
    const req = Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(scenarioUrl(port, 7))
            return yield* resp.text
        })
    )

    return Effect.race(req, Effect.delay(req, seconds(3)))
}

function scenario8(port: number) {
    const req = (url: string) => Effect.scoped(
        Effect.gen(function*() {
            const resp = yield* HttpClient.get(url)
            const valid = yield* HttpClientResponse.filterStatusOk(resp)
            return yield* valid.text
        })
    )

    const open = req(scenarioUrl(port, 8) + "?open")
    const use = (id: string) => req(scenarioUrl(port, 8) + `?use=${id}`)
    const close = (id: string) => req(scenarioUrl(port, 8) + `?close=${id}`).pipe(Effect.orDie)

    const reqRes = Effect.acquireUseRelease(open, use, close)

    return Effect.race(reqRes, reqRes)
}

export function program(port: number) {
    return Effect.gen(function* () {
        return [
            yield* scenario1(port),
            yield* scenario2(port),
            yield* scenario3(port),
            yield* scenario4(port),
            yield* scenario5(port),
            yield* scenario6(port),
            yield* scenario7(port),
            yield* scenario8(port),
        ]
    }).pipe(
        Effect.provide(NodeHttpClient.layer)
    )
}
