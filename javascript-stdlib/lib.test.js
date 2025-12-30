import {GenericContainer, Wait} from "testcontainers"
import * as lib from "./lib.js"

describe("all work", () => {
    let container

    beforeAll(async () => {
        container = await new GenericContainer("ghcr.io/jamesward/easyracer")
            .withExposedPorts(8080)
            .withWaitStrategy(Wait.forHttp("/", 8080))
            .start()
    }, 15000)

    afterAll(async () => {
        await container.stop()
    })

    it("get / works", async () => {
        const httpPort = container.getFirstMappedPort()
        const resp = await lib.httpGet(lib.url(httpPort, ""))
        expect(resp.statusCode).toBe(200)
    })

    it("get /1 can be canceled after 100ms", async () => {
        const httpPort = container.getFirstMappedPort()
        const controller = new AbortController()
        const promise = lib.httpGet(lib.url(httpPort, 1), controller.signal)
        setTimeout(() => controller.abort(), 100)
        await expect(promise).rejects.toThrow()
    })

    it("cancelled request does not reject until connection is closed", async () => {
        const httpPort = container.getFirstMappedPort()
        const controller = new AbortController()
        const promise = lib.httpGet(lib.url(httpPort, 1), controller.signal)

        // Abort immediately
        controller.abort()

        // Check that promise hasn't rejected yet (still pending)
        const pending = Symbol('pending')
        const raceResult = await Promise.race([
            promise.catch(() => 'rejected'),
            Promise.resolve(pending)
        ])
        expect(raceResult).toBe(pending)

        // Eventually it should reject after connection closes
        await expect(promise).rejects.toThrow()
    })

    it("race works", async () => {
        const racers = [
            (signal) => new Promise((resolve) => setTimeout(() => resolve("first"), 50)),
            (signal) => new Promise((resolve) => setTimeout(() => resolve("second"), 100)),
        ]
        const result = await lib.raceWithCancellation(racers)
        expect(result).toBe("first")
    })

    it("race cancels the losers", async () => {
        const aborted = []
        const racers = [
            (signal) => new Promise((resolve, reject) => {
                const timeout = setTimeout(() => resolve("first"), 50)
                signal.addEventListener('abort', () => {
                    clearTimeout(timeout)
                    aborted.push(1)
                    reject(new Error('aborted'))
                })
            }),
            (signal) => new Promise((resolve, reject) => {
                const timeout = setTimeout(() => resolve("second"), 100)
                signal.addEventListener('abort', () => {
                    clearTimeout(timeout)
                    aborted.push(2)
                    reject(new Error('aborted'))
                })
            }),
        ]
        const result = await lib.raceWithCancellation(racers)
        expect(result).toBe("first")
        expect(aborted).toContain(2)
        expect(aborted).not.toContain(1)
    })

    it("race works with 10000 promises", async () => {
        const racers = Array.from({ length: 10000 }, (_, i) =>
            (signal) => new Promise((resolve) => setTimeout(() => resolve(i), 50 + i))
        )
        const result = await lib.raceWithCancellation(racers)
        expect(result).toBe(0)
    })

})
