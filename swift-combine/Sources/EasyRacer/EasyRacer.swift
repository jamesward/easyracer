import Combine
import Foundation

extension URLSession {
    func bodyTextTaskPublisher(for url: URL) -> some Publisher<String, any Error> {
        dataTaskPublisher(for: url).tryMap { data, response in
            guard
                let response = response as? HTTPURLResponse,
                200..<300 ~= response.statusCode
            else {
                throw URLError(.badServerResponse)
            }
            
            guard
                let text: String = String(data: data, encoding: .utf8)
            else {
                throw URLError(.cannotDecodeRawData)
            }
            
            return text
        }
    }
}

@main
public struct EasyRacer {
    let baseURL: URL
    let urlSession: some URLSession = ScalableURLSession(
        configuration: {
            let configuration = URLSessionConfiguration.ephemeral
            configuration.httpMaximumConnectionsPerHost = 1_000
            configuration.timeoutIntervalForRequest = 120
            return configuration
        }(),
        requestsPerSession: 100
    )
    
    func scenario1() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("1")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario2() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("2")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario3() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("3")
        
        return Publishers
            .MergeMany(
                (1...10_000).map { _ in
                    urlSession
                        .bodyTextTaskPublisher(for: url)
                        .map { $0 }.replaceError(with: nil)
                }
            )
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario4() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("4")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        let publisher1SecTimeout = Foundation.URLSession(
            configuration: {
                let configuration: URLSessionConfiguration = .ephemeral
                configuration.timeoutIntervalForRequest = 1
                return configuration
            }())
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher1SecTimeout)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario5() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("5")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario6() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("6")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher, publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario7() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("7")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        let delayedPublisher = Just(())
            .delay(for: .seconds(3), scheduler: DispatchQueue.global())
            .flatMap { _ in return publisher }
        
        return publisher.merge(with: delayedPublisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario8() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("8")
        
        guard
            let urlComps: URLComponents = URLComponents(
                url: url, resolvingAgainstBaseURL: false
            )
        else {
            return Empty().eraseToAnyPublisher()
        }
        
        // Build "open" URL
        var openURLComps = urlComps
        openURLComps.queryItems = [URLQueryItem(name: "open", value: nil)]
        
        guard
            let openURL: URL = openURLComps.url
        else {
            return Empty().eraseToAnyPublisher()
        }
        
        // Open
        let publisher = urlSession
            .bodyTextTaskPublisher(for: openURL)
            .flatMap { id in
                // Use
                var useURLComps = urlComps
                useURLComps.queryItems = [URLQueryItem(name: "use", value: id)]
                
                guard
                    let useURL: URL = useURLComps.url
                else {
                    return Fail<String?, any Error>(error: URLError(.badURL))
                        .eraseToAnyPublisher()
                }
                
                return urlSession
                    .bodyTextTaskPublisher(for: useURL)
                    .map { $0 }.replaceError(with: nil) // so that we close even if use call errored
                    .flatMap { text in
                        // Close
                        var closeURLComps = urlComps
                        closeURLComps.queryItems = [URLQueryItem(name: "close", value: id)]
                        
                        guard
                            let closeURL: URL = closeURLComps.url
                        else {
                            return Fail<String?, any Error>(error: URLError(.badURL))
                                .eraseToAnyPublisher()
                        }
                        
                        return urlSession
                            .bodyTextTaskPublisher(for: closeURL)
                            .map { _ in  text }
                            .eraseToAnyPublisher()
                    }
                    .eraseToAnyPublisher()
            }
            .replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario9() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("9")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return Publishers.MergeMany(Array(repeating: publisher, count: 10))
            .compactMap { $0 }
            .collect().map { $0.joined() }
            .eraseToAnyPublisher()
    }
    
    func scenario10() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("10")
        let id: String = UUID().uuidString
        
        guard
            let urlComps: URLComponents = URLComponents(
                url: url, resolvingAgainstBaseURL: false
            )
        else {
            return Empty().eraseToAnyPublisher()
        }
        
        var blockerURLComps = urlComps
        blockerURLComps.queryItems = [URLQueryItem(name: id, value: nil)]
        guard
            let blockerURL: URL = blockerURLComps.url
        else {
            return Empty().eraseToAnyPublisher()
        }
        let blocker: some Publisher<String?, Never> = urlSession
            .bodyTextTaskPublisher(for: blockerURL)
            .map { $0 }.replaceError(with: nil)
        
        // Busy-wait by publishing nils as quickly as possible
        let blocking: some Publisher<String?, Never> = Publishers.Repeating(nil)
        
        func currentWallTime() -> TimeInterval {
            var timeval: timeval = timeval()
            // Should never error as parameters are valid
            gettimeofday(&timeval, nil)
            
            return TimeInterval(timeval.tv_sec) + TimeInterval(timeval.tv_usec) / 1_000_000.0
        }
        func currentCPUTime() -> TimeInterval {
            var rusage: rusage = rusage()
            // Should never error as parameters are valid
            getrusage(RUSAGE_SELF, &rusage)
            let utime = rusage.ru_utime
            let stime = rusage.ru_stime
            let secs = utime.tv_sec + stime.tv_sec
            let usecs = utime.tv_usec + stime.tv_usec
            
            return TimeInterval(secs) + TimeInterval(usecs) / 1_000_000.0
        }
        func reportProcessLoad(
            startWallTime: TimeInterval, startCPUTime: TimeInterval
        ) -> AnyPublisher<String?, URLError> {
            let endWallTime: TimeInterval = currentWallTime()
            let endCPUTime: TimeInterval = currentCPUTime()
            let totalUsageOfCPU: Double = (endCPUTime - startCPUTime) / (endWallTime - startWallTime)
            
            var reporterURLComps = urlComps
            reporterURLComps.queryItems = [URLQueryItem(name: id, value: "\(totalUsageOfCPU)")]
            guard
                let reporterURL: URL = reporterURLComps.url
            else {
                return Fail(error: URLError(.badURL)).eraseToAnyPublisher()
            }
            
            return urlSession
                .dataTaskPublisher(for: reporterURL)
                .flatMap { data, response -> AnyPublisher<String?, URLError> in
                    guard
                        let response: HTTPURLResponse = response as? HTTPURLResponse,
                        200..<400 ~= response.statusCode
                    else {
                        return Fail(error: URLError(.badServerResponse))
                            .eraseToAnyPublisher()
                    }
                    
                    if 300..<400 ~= response.statusCode {
                        return Just(())
                            .delay(for: .seconds(1), scheduler: DispatchQueue.global(qos: .background))
                            .flatMap { _ in
                                reportProcessLoad(
                                    startWallTime: endWallTime, startCPUTime: endCPUTime
                                )
                            }
                            .eraseToAnyPublisher()
                    }
                    
                    guard
                        let text: String = String(data: data, encoding: .utf8)
                    else {
                        return Fail(error: URLError(.cannotDecodeRawData))
                            .eraseToAnyPublisher()
                    }
                    
                    return Just(text).setFailureType(to: URLError.self).eraseToAnyPublisher()
                }
                .eraseToAnyPublisher()
        }
        let reporter: some Publisher<String?, Never> = reportProcessLoad(
            startWallTime: currentWallTime(), startCPUTime: currentCPUTime()
        ).replaceError(with: nil)
        
        return blocker.merge(with: blocking).first { $0 != nil }
            .merge(with: reporter)
            .compactMap { $0 }.first { $0 != "" }
            .eraseToAnyPublisher()
    }
    
    public func scenarios() -> AnyPublisher<[String?], Never> {
        let scenarios = [
            (1, scenario1()),
            (2, scenario2()),
            (4, scenario4()),
            (5, scenario5()),
            (6, scenario6()),
            (7, scenario7()),
            (8, scenario8()),
            (9, scenario9()),
            (10, scenario10()),
            (3, scenario3()), // This has to come last, as it frequently causes other scenarios to fail
        ]
        return scenarios.publisher
            .flatMap(maxPublishers: .max(1)) { scenarioAndNumber in
                let (num, scenario) = scenarioAndNumber
                return scenario.map { $0 }.replaceEmpty(with: nil).map { (num, $0) }
            }
            .collect()
            .map { results in
                results.sorted { $0.0 < $1.0 }.map { $0.1 }
            }
            .eraseToAnyPublisher()
    }
    
    public static func main() {
        guard
            let baseURL = URL(string: "http://localhost:8080")
        else { return }
        
        let completed = DispatchSemaphore(value: 0)
        var subscriptions: Set<AnyCancellable> = Set()
        withExtendedLifetime(subscriptions) {
            EasyRacer(baseURL: baseURL).scenarios()
                .sink(
                    receiveCompletion: { _ in completed.signal() },
                    receiveValue: { results in
                        for (idx, result) in results.enumerated() {
                            print("Scenario \(idx + 1): \(result ?? "error")")
                        }
                        if !results.allSatisfy({ $0 != nil }) {
                            exit(EXIT_FAILURE)
                        }
                    }
                )
                .store(in: &subscriptions)
            completed.wait()
        }
    }
}
