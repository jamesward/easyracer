import {GenericContainer, Wait} from "testcontainers"
import * as lib from "./lib.js"

describe("all work", () => {
    let container

    beforeAll(async () => {
        container = await new GenericContainer("ghcr.io/jamesward/easyracer")
            .withExposedPorts(8080)
            .withWaitStrategy(Wait.forHttp("/", 8080))
            .start()
    }, 15000);

    afterAll(async () => {
        await container.stop()
    })

    it("scenario1 works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await lib.scenario1(httpPort)).toBe("right")
    }, 600000)

    it("scenario2 works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await lib.scenario2(httpPort)).toBe("right")
    }, 600000)

    it("scenario3 works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await lib.scenario3(httpPort)).toBe("right")
    }, 600000)

    it("scenario4 works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await lib.scenario4(httpPort)).toBe("right")
    }, 600000)

    it("scenario5 works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await lib.scenario5(httpPort)).toBe("right")
    }, 600000)

    it("scenario6 works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await lib.scenario6(httpPort)).toBe("right")
    }, 600000)

    it("scenario7 works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await lib.scenario7(httpPort)).toBe("right")
    }, 600000)

    it("scenario8 works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await lib.scenario8(httpPort)).toBe("right")
    }, 600000)

    it("scenario9 works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await lib.scenario9(httpPort)).toBe("right")
    }, 600000)
})
