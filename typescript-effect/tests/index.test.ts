import {GenericContainer, StartedTestContainer, Wait} from "testcontainers";
import {Effect} from "effect";
import {program} from "../src/lib";

describe("all work", () => {
    let container: StartedTestContainer

    beforeAll(async () => {
        container = await new GenericContainer("ghcr.io/jamesward/easyracer")
            .withExposedPorts(8080)
            .withWaitStrategy(Wait.forHttp("/", 8080))
            .start()
    })

    it("works", async () => {
        const httpPort = container.getFirstMappedPort()
        const results = await Effect.runPromise(program(httpPort))
        expect(results).toEqual(["right", "right", "right", "right"])
    }, 600_000)

    afterAll(async () => {
        await container.stop()
    })
})
