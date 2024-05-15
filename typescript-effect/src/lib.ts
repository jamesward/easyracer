import {Array, Effect} from "effect"
import * as Http from "@effect/platform/HttpClient"
import {seconds} from "effect/Duration";

function scenarioUrl(port: number, scenario: number): string {
    return `http://localhost:${port}/${scenario}`
}

function scenario1(port: number) {
    const req = Http.request.get(scenarioUrl(port, 1)).pipe(Http.client.fetchOk, Http.response.text)
    return Effect.race(req, req)
}

function scenario2(port: number) {
    const req = Http.request.get(scenarioUrl(port, 2)).pipe(Http.client.fetchOk, Http.response.text)
    return Effect.race(req, req)
}

function scenario3(port: number) {
    const req = Http.request.get(scenarioUrl(port, 3)).pipe(Http.client.fetchOk, Http.response.text)
    return Effect.raceAll(Array.replicate(req, 10000))
}

function scenario4(port: number) {
    const req = Http.request.get(scenarioUrl(port, 4)).pipe(Http.client.fetchOk, Http.response.text)
    return Effect.race(req, req.pipe(Effect.timeout("3 seconds")))
}

function scenario5(port: number) {
    const req = Effect.gen(function* () {
        const res = yield* Http.request.get(scenarioUrl(port, 5))
        if (res.status >= 300) {
            throw new Error(`HTTP ${res.status}`)
        }
        else {
            return yield* res.text
        }
    }).pipe(Effect.scoped)

    return Effect.race(req, req)
}

function scenario6(port: number) {
    const req = Effect.gen(function* () {
        const res = yield* Http.request.get(scenarioUrl(port, 6))
        if (res.status >= 300) {
            throw new Error(`HTTP ${res.status}`)
        }
        else {
            return yield* res.text
        }
    }).pipe(Effect.scoped) // todo: maybe can pipe through a filter?

    return Effect.raceAll([req, req, req])
}

function scenario7(port: number) {
    const req = Effect.gen(function* () {
        const res = yield* Http.request.get(scenarioUrl(port, 7))
        return yield* res.text
    }).pipe(Effect.scoped)

    return Effect.race(req, Effect.delay(req, seconds(3)))
}

function scenario8(port: number) {
    const req = (url: string) => Effect.gen(function* () {
        const res = yield* Http.request.get(url)
        if (res.status >= 300) {
            throw new Error(`HTTP ${res.status}`)
        }
        else {
            return yield* res.text
        }
    }).pipe(Effect.scoped)

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
            // yield* scenario4(port).pipe(Effect.delay("10 seconds")), // wait for scenario3 connections to actually close
            // yield* scenario5(port),
            // yield* scenario6(port),
            // yield* scenario7(port),
            // yield* scenario8(port),
        ]
    })
}
