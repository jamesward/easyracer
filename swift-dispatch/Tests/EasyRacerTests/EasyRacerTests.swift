import DockerClientSwift
import Logging
import XCTest
#if canImport(FoundationNetworking)
import FoundationNetworking
#endif
@testable import EasyRacer

final class EasyRacerTests: XCTestCase {
    func testAllScenarios() throws {
        // Set up
        var logger = Logger(label: "docker-client")
        logger.logLevel = .error
        let client = DockerClient(logger: logger)
        let image = try client.images
            .pullImage(byName: "ghcr.io/jamesward/easyracer", tag: "latest").wait()
        let container = try client.containers
            .createContainer(image: image, portBindings: [PortBinding(containerPort: 8080)]).wait()
        let portBindings = try container.start(on: client).wait()
        let randomPort = portBindings[0].hostPort
        let baseURL = URL(string: "http://localhost:\(randomPort)")!
        // Wait for scenario server to start handling HTTP requests
        var serverStarted: Bool = false
        while true {
            let connectionAttempted: DispatchSemaphore = DispatchSemaphore(value: 0)
            URLSession.shared.dataTask(with: baseURL) { _, _, error in
                serverStarted = error == nil
                connectionAttempted.signal()
            }.resume()
            connectionAttempted.wait()
            if serverStarted {
                break
            } else {
                Thread.sleep(forTimeInterval: 0.01) // 10ms
            }
        }
        
        // Tear down
        defer {
            _ = try? client.containers.stop(container: container).wait()
            _ = try? client.containers.prune().wait()
            try? client.syncShutdown()
        }
        
        // Test
        let easyRacer = EasyRacer(baseURL: baseURL)
        easyRacer.scenarios { results in
            XCTAssertEqual(results.count, 9, "Number of Scenarios")
            for (idx, result) in results.enumerated() {
                XCTAssertEqual(result, "right", "Scenario \(idx + 1)")
            }
        }
    }
}
