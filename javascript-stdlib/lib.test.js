import {GenericContainer, Wait} from "testcontainers"
import {
    scenario1,
    scenario2,
    scenario3,
    scenario4,
    scenario5,
    scenario6,
    scenario7,
    scenario8,
    scenario9
} from "./lib.js"

describe("all work", () => {
    let container

    beforeAll(async () => {
        container = await new GenericContainer("ghcr.io/jamesward/easyracer")
            .withExposedPorts(8080)
            .withWaitStrategy(Wait.forHttp("/", 8080))
            .withCommand(["--debug"])
            .start()
    }, 15000);

    afterAll(async () => {
        await container.stop()
    })

    it("scenario1", async () => {
        expect(await scenario1(container.getFirstMappedPort())).toBe("right")
    }, 5000)

    it("scenario2", async () => {
        expect(await scenario2(container.getFirstMappedPort())).toBe("right")
    }, 5000)

    it("scenario3", async () => {
        expect(await scenario3(container.getFirstMappedPort())).toBe("right")
    }, 20000)

    it("scenario4", async () => {
        expect(await scenario4(container.getFirstMappedPort())).toBe("right")
    }, 5000)

    it("scenario5", async () => {
        expect(await scenario5(container.getFirstMappedPort())).toBe("right")
    }, 5000)

    it("scenario6", async () => {
        expect(await scenario6(container.getFirstMappedPort())).toBe("right")
    }, 5000)

    it("scenario7", async () => {
        expect(await scenario7(container.getFirstMappedPort())).toBe("right")
    }, 20000)

    it("scenario8", async () => {
        expect(await scenario8(container.getFirstMappedPort())).toBe("right")
    }, 20000)

    it("scenario9", async () => {
        expect(await scenario9(container.getFirstMappedPort())).toBe("right")
    }, 20000)
})
