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
            .start()
    });

    afterAll(async () => {
        await container.stop()
    })

    it("works", async () => {
        const httpPort = container.getFirstMappedPort()
        expect(await scenario1(httpPort)).toBe("right")
        expect(await scenario2(httpPort)).toBe("right")
        expect(await scenario3(httpPort)).toBe("right")
        expect(await scenario4(httpPort)).toBe("right")
        expect(await scenario5(httpPort)).toBe("right")
        expect(await scenario6(httpPort)).toBe("right")
        expect(await scenario7(httpPort)).toBe("right")
        expect(await scenario8(httpPort)).toBe("right")
        expect(await scenario9(httpPort)).toBe("right")
    }, 30000)
})
