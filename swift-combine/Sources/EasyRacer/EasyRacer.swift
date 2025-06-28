import Foundation
#if canImport(FoundationNetworking)
import FoundationNetworking
#endif
#if canImport(Combine)
import Combine
#else
import OpenCombine
import OpenCombineDispatch
#endif

extension URLSession {
    func bodyTextTaskPublisher(for url: URL) -> AnyPublisher<String, any Error> {
        dataTaskPublisher(for: url)
            .tryMap { data, response in
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
            .eraseToAnyPublisher()
    }
}

@main
public struct EasyRacer {
    let baseURL: URL
    let urlSession: some URLSession = FoundationURLSession.shared
    let dispatchQueue = DispatchQueue(label: "easyracer")

    func scenario1() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "1")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario2() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "2")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario3() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "3")
        func publisher() -> some Publisher<String?, Never> {
            urlSession
                .bodyTextTaskPublisher(for: url)
                .map { $0 }.replaceError(with: nil)
        }
        
        return Publishers
            .MergeMany((1...10_000).map { _ in publisher() })
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario4() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "4")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        let publisher1SecTimeout = publisher
            .timeout(.seconds(1), scheduler: dispatchQueue)
        
        return publisher.merge(with: publisher1SecTimeout)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario5() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "5")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario6() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "6")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher, publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario7() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "7")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        let delayedPublisher = Just(())
            .delay(for: .seconds(3), scheduler: dispatchQueue)
            .flatMap { _ in return publisher }
        
        return publisher.merge(with: delayedPublisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario8() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "8")
        
        let open = urlSession
            .bodyTextTaskPublisher(
                for: url.appending(queryItems: [.init(name: "open", value: nil)])
            )
        func use(id: String) -> some Publisher<String, any Error> {
            urlSession.bodyTextTaskPublisher(
                for: url.appending(queryItems: [.init(name: "use", value: id)])
            )
        }
        func close(id: String) -> some Publisher<String, any Error> {
            urlSession.bodyTextTaskPublisher(
                for: url.appending(queryItems: [.init(name: "close", value: id)])
            )
        }
        
        let publisher = open
            .flatMap { id in
                use(id: id)
                    .map { $0 }.replaceError(with: nil) // so that we close even if use call errored
                    .flatMap { text in
                        close(id: id).map { _ in  text }
                    }
            }
            .replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario9() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "9")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return Publishers.MergeMany(Array(repeating: publisher, count: 10))
            .compactMap { $0 }
            .collect().map { $0.joined() }
            .eraseToAnyPublisher()
    }
    
    func scenario10() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "10")
        let id: String = UUID().uuidString
        
        let blocker: some Publisher<String?, Never> = urlSession
            .bodyTextTaskPublisher(
                for: url.appending(queryItems: [.init(name: id, value: nil)])
            )
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
            _ = getrusage(RUSAGE_SELF, &rusage)
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
            
            return urlSession
                .dataTaskPublisher(
                    for: url.appending(queryItems: [.init(name: id, value: "\(totalUsageOfCPU)")])
                )
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
                            .delay(for: .seconds(1), scheduler: dispatchQueue)
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
    
    func scenario11() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appending(path: "11")
        let publisher = urlSession
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher).merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    public func scenarios() -> AnyPublisher<(Int, String?), Never> {
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
            (11, scenario11()),
            (3, scenario3()), // This has to come last, as it frequently causes other scenarios to fail
        ]
        return scenarios.publisher
            .flatMap(maxPublishers: .max(1)) { scenarioAndNumber in
                let (num, scenario) = scenarioAndNumber
                return scenario.map { $0 }.replaceEmpty(with: nil).map { (num, $0) }
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
                    receiveValue: { scenarioAndResult in
                        let (scenario, result) = scenarioAndResult
                        print("Scenario \(scenario): \(result ?? "error")")
                    }
                )
                .store(in: &subscriptions)
            completed.wait()
        }
    }
}
