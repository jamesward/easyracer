import { Array, Effect } from "effect"
import * as Http from "@effect/platform/HttpClient"

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

export function program(port: number) {
    return Effect.gen(function* () {
        return [
            yield* scenario1(port),
            yield* scenario2(port),
            yield* scenario3(port),
            yield* scenario4(port).pipe(Effect.delay("10 seconds")),
        ]
    })
}
