import {GenericContainer, StartedTestContainer, Wait} from "testcontainers";
import {Effect} from "effect";
import * as lib from "../src/lib";
import {NodeHttpClient} from "@effect/platform-node";

describe("all work", () => {
    let container: StartedTestContainer

    beforeAll(async () => {
        container = await new GenericContainer("ghcr.io/jamesward/easyracer")
            .withExposedPorts(8080)
            .withWaitStrategy(Wait.forHttp("/", 8080))
            // .withCommand(["--debug"])
            .start()
    }, 30_000)

    it("scenario 1 works", async () => {
        const httpPort = container.getFirstMappedPort()
        const result = await Effect.runPromise(
          lib.scenario1(httpPort).pipe(Effect.provide(NodeHttpClient.layer))
        )
        expect(result).toEqual("right")
    }, 600_000)

    it("scenario 2 works", async () => {
        const httpPort = container.getFirstMappedPort()
        const result = await Effect.runPromise(
          lib.scenario2(httpPort).pipe(Effect.provide(NodeHttpClient.layer))
        )
        expect(result).toEqual("right")
    }, 600_000)

    it("scenario 3 works", async () => {
        const httpPort = container.getFirstMappedPort()
        const result = await Effect.runPromise(
          lib.scenario3(httpPort).pipe(Effect.provide(NodeHttpClient.layer))
        )
        expect(result).toEqual("right")
    }, 600_000)

    it("scenario 4 works", async () => {
        const httpPort = container.getFirstMappedPort()
        const result = await Effect.runPromise(
          lib.scenario4(httpPort).pipe(Effect.provide(NodeHttpClient.layer))
        )
        expect(result).toEqual("right")
    }, 600_000)

    it("scenario 5 works", async () => {
        const httpPort = container.getFirstMappedPort()
        const result = await Effect.runPromise(
          lib.scenario5(httpPort).pipe(Effect.provide(NodeHttpClient.layer))
        )
        expect(result).toEqual("right")
    }, 600_000)

    it("scenario 6 works", async () => {
        const httpPort = container.getFirstMappedPort()
        const result = await Effect.runPromise(
          lib.scenario6(httpPort).pipe(Effect.provide(NodeHttpClient.layer))
        )
        expect(result).toEqual("right")
    }, 600_000)

    it("scenario 7 works", async () => {
        const httpPort = container.getFirstMappedPort()
        const result = await Effect.runPromise(
          lib.scenario7(httpPort).pipe(Effect.provide(NodeHttpClient.layer))
        )
        expect(result).toEqual("right")
    }, 600_000)

    it("scenario 8 works", async () => {
        const httpPort = container.getFirstMappedPort()
        const result = await Effect.runPromise(
          lib.scenario8(httpPort).pipe(Effect.provide(NodeHttpClient.layer))
        )
        expect(result).toEqual("right")
    }, 600_000)

    afterAll(async () => {
        await container.stop()
    })
})
