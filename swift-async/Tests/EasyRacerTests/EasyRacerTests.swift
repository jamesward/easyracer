import DockerClientSwift
import Logging
import XCTest
@testable import EasyRacer

final class EasyRacerTests: XCTestCase {
    func testAllScenarios() async throws {
        // Set up
        var logger = Logger(label: "docker-client")
        logger.logLevel = .error
        let client = DockerClient(logger: logger)
        let image = try await client.images
            .pullImage(byName: "ghcr.io/jamesward/easyracer", tag: "latest").get()
        let container = try await client.containers
            .createContainer(image: image, portBindings: [PortBinding(containerPort: 8080)]).get()
        let portBindings = try await container.start(on: client).get()
        let randomPort = portBindings[0].hostPort
        let baseURL = URL(string: "http://localhost:\(randomPort)")!
        // Wait for scenario server to start handling HTTP requests
        while true {
            do {
                _ = try await URLSession.shared.data(from: baseURL)
                break
            } catch {
                try await Task.sleep(nanoseconds: 10_000_000) // 10ms
                continue
            }
        }
        
        // Tear down
        defer {
            _ = try? client.containers.stop(container: container).wait()
            _ = try? client.containers.prune().wait()
            try? client.syncShutdown()
        }

        // Test
        let results = await EasyRacer(baseURL: baseURL).scenarios()
        XCTAssertEqual(results.count, 9, "Number of Scenarios")
        for (idx, result) in results.enumerated() {
            XCTAssertEqual(result, "right", "Scenario \(idx + 1)")
        }
    }
}
