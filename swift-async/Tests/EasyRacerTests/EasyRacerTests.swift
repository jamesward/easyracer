import DockerClientSwift
import EasyRacer
import Logging
import XCTest

final class EasyRacerTests: XCTestCase {
    func testAllScenarios() async throws {
        // Set up
        var logger = Logger(label: "docker-client")
        logger.logLevel = .info
        let client = DockerClient(logger: logger)
        let image = try await client.images
            .pullImage(byName: "ghcr.io/jamesward/easyracer", tag: "latest").get()
        let container = try await client.containers
            .createContainer(image: image, portBindings: [PortBinding(containerPort: 8080)]).get()
        let portBindings = try await container.start(on: client).get()
        let randomPort = portBindings[0].hostPort
        let baseURL = URL(string: "http://localhost:\(randomPort)")!
        logger.info("waiting for scenario service to start")
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
        logger.info("scenario service to started")

        // Test
        let easyRacer = EasyRacer(baseURL: baseURL)
        let results = try await easyRacer.scenarios()
        for (scenarioNum, result) in results {
            XCTAssertEqual(result, "right", "Scenario \(scenarioNum)")
        }
        
        // Teardown
        _ = try? await client.containers.stop(container: container).get()
        _ = try? await client.containers.prune().get()
        try? client.syncShutdown()
    }
}
