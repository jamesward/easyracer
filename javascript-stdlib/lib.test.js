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
        controller.abort()
        const pending = Symbol('pending')
        const raceResult = await Promise.race([
            promise.catch(() => 'rejected'),
            Promise.resolve(pending)
        ])
        expect(raceResult).toBe(pending)
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

    it("httpGet cleans up abort listener after completion", async () => {
        const httpPort = container.getFirstMappedPort()
        const controller = new AbortController()

        let listenerRemoved = false
        const originalRemoveEventListener = controller.signal.removeEventListener.bind(controller.signal)
        controller.signal.removeEventListener = (type, listener) => {
            if (type === 'abort') {
                listenerRemoved = true
            }
            return originalRemoveEventListener(type, listener)
        }

        await lib.httpGet(lib.url(httpPort, ""), controller.signal)

        expect(listenerRemoved).toBe(true)
    })

    it("scenario 1 works", async () => {
        const result = await lib.scenario1(container.getFirstMappedPort())
        expect(result).toBe("right")
    })

    it("scenario 2 works", async () => {
        const result = await lib.scenario2(container.getFirstMappedPort())
        expect(result).toBe("right")
    })

    it("scenario 3 works", async () => {
        const result = await lib.scenario3(container.getFirstMappedPort())
        expect(result).toBe("right")
    }, 20000)

    it("scenario 4 works", async () => {
        const result = await lib.scenario4(container.getFirstMappedPort())
        expect(result).toBe("right")
    })

    it("scenario 5 works", async () => {
        const result = await lib.scenario5(container.getFirstMappedPort())
        expect(result).toBe("right")
    })

    it("scenario 6 works", async () => {
        const result = await lib.scenario6(container.getFirstMappedPort())
        expect(result).toBe("right")
    })

    it("scenario 7 works", async () => {
        const result = await lib.scenario7(container.getFirstMappedPort())
        expect(result).toBe("right")
    })

    it("scenario 8 works", async () => {
        const result = await lib.scenario8(container.getFirstMappedPort())
        expect(result).toBe("right")
    })

    it("scenario 9 works", async () => {
        const result = await lib.scenario9(container.getFirstMappedPort())
        expect(result).toBe("right")
    })

})
