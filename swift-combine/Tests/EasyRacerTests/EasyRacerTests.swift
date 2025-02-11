import Atomics
import Combine
import DockerClientSwift
import Logging
import XCTest
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
        let serverStarted = ManagedAtomic(false)
        while true {
            let connectionAttempted: DispatchSemaphore = DispatchSemaphore(value: 0)
            URLSession.shared.dataTask(with: baseURL) { _, _, error in
                serverStarted.store(error == nil, ordering: .relaxed)
                connectionAttempted.signal()
            }.resume()
            connectionAttempted.wait()
            if serverStarted.load(ordering: .relaxed) {
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
        let completed = DispatchSemaphore(value: 0)
        var subscriptions: Set<AnyCancellable> = Set()
        withExtendedLifetime(subscriptions) {
            EasyRacer(baseURL: baseURL).scenarios()
                .sink(
                    receiveCompletion: { _ in completed.signal() },
                    receiveValue: { results in
                        XCTAssertEqual(results.count, 11, "Number of Scenarios")
                        for (idx, result) in results.enumerated() {
                            XCTAssertEqual(result, "right", "Scenario \(idx + 1)")
                        }
                    }
                )
                .store(in: &subscriptions)
            completed.wait()
        }
    }
}
