import Combine
import DockerSwift
import Logging
import Synchronization
import XCTest
@testable import EasyRacer

final class EasyRacerTests: XCTestCase {
    func testAllScenarios() async {
        // Set up
        var logger = Logger(label: "docker-client")
        logger.logLevel = .error
        let docker = DockerClient(logger: logger)
        let containerSpec = ContainerSpec(
            config: .init(
                image: "ghcr.io/jamesward/easyracer:latest",
                command: ["--debug"],
                exposedPorts: [.tcp(8080)]
            ),
            hostConfig: .init(
                portBindings: [
                    .tcp(8080): [.publishTo(hostIp: "0.0.0.0", hostPort: 0)]
                ]
            )
        )
        do {
            _ = try await docker.images.pull(byName: "ghcr.io/jamesward/easyracer:latest")
            let container = try await docker.containers.create(spec: containerSpec)
            try? await docker.containers.start(container.id)
            guard
                let runningContainer = try? await docker.containers.get(container.id)
            else {
                XCTFail("Failed to start container")
                return
            }
            let randomPort = runningContainer.networkSettings.ports["8080/tcp"]!!.first!.hostPort
            let baseURL = URL(string: "http://localhost:\(randomPort)")!
            // Wait for scenario server to start handling HTTP requests
            while true {
                do {
                    _ = try await URLSession.shared.data(from: baseURL)
                    break
                } catch {
                    try? await Task.sleep(nanoseconds: 10_000_000) // 10ms
                    continue
                }
            }
            
            // Test
            let completed = DispatchSemaphore(value: 0)
            var subscriptions: Set<AnyCancellable> = Set()
            withExtendedLifetime(subscriptions) {
                EasyRacer(baseURL: baseURL).scenarios()
                    .collect()
                    .map { results in
                        results.sorted { $0.0 < $1.0 }
                    }
                    .sink(
                        receiveCompletion: { _ in completed.signal() },
                        receiveValue: { results in
                            XCTAssertEqual(results.count, 11, "Number of Scenarios")
                            for (scenario, result) in results {
                                XCTAssertEqual(result, "right", "Scenario \(scenario)")
                            }
                        }
                    )
                    .store(in: &subscriptions)
                completed.wait()
            }
            
            // Tear down
            try? await docker.containers.stop(container.id)
        } catch {
            XCTFail("Failed to create container: \(error)")
        }
        
        _ = try? await docker.containers.prune()
        try? await docker.shutdown()
    }
}
