import {GenericContainer, StartedTestContainer, Wait} from "testcontainers"
import {afterAll, beforeAll, expect, layer} from "@effect/vitest"
import {Effect} from "effect"
import {NodeHttpClient} from "@effect/platform-node"
import * as lib from "../src/lib.js"

let container: StartedTestContainer

beforeAll(async () => {
    container = await new GenericContainer("ghcr.io/jamesward/easyracer")
        .withExposedPorts(8080)
        .withWaitStrategy(Wait.forHttp("/", 8080))
        // .withCommand(["--debug"])
        .start()
}, 30_000)

afterAll(async () => {
    await container.stop()
})

// excludeTestServices: the scenarios rely on real time (delays, timeouts), so use the live Clock instead of TestClock
layer(NodeHttpClient.layerNodeHttp, {excludeTestServices: true})("all work", (it) => {
    lib.scenarios.forEach((scenario, i) => {
        it.effect(`scenario ${i + 1} works`, () =>
            Effect.gen(function*() {
                const result = yield* scenario(container.getFirstMappedPort())
                expect(result).toEqual("right")
            })
        )
    })
})
