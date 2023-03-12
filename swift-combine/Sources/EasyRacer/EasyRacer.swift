import Combine
import Foundation

extension URLSession {
    func bodyTextTaskPublisher(for url: URL) -> Publishers.TryMap<URLSession.DataTaskPublisher, String> {
        dataTaskPublisher(for: url).tryMap { data, response in
            guard
                let response = response as? HTTPURLResponse,
                (200..<300).contains(response.statusCode)
            else {
                throw URLError(.badServerResponse)
            }
            
            guard
                let text: String = String(data: data, encoding: .utf8)
            else {
                throw URLError(.cannotDecodeContentData)
            }
            
            return text
        }
    }
}

@main
public struct EasyRacer {
    let baseURL: URL
    
    func scenario1() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("1")
        let publisher = URLSession(configuration: .ephemeral)
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario2() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("2")
        let publisher = URLSession(configuration: .ephemeral)
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario3() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("3")
        // Ideally, we'd use a single URLSession configured to handle 10k connections.
        // This doesn't seem to work - observed from the scenario server, it'll create
        // ~110 connections, and then stall.
        // URLSession is close-sourced, so it's hard to tell what is going on.
//        let urlSession: URLSession = URLSession(
//            configuration: {
//                let urlSessionCfg = URLSessionConfiguration.ephemeral
//                urlSessionCfg.httpMaximumConnectionsPerHost = 10_000
//                return urlSessionCfg
//            }(),
//            delegate: nil,
//            delegateQueue: {
//                let opQueue: OperationQueue = OperationQueue()
//                opQueue.maxConcurrentOperationCount = 10_000
//                return opQueue
//            }()
//        )
        let urlSessionCfg = URLSessionConfiguration.ephemeral
        urlSessionCfg.timeoutIntervalForRequest = 900 // Seems to be required for GitHub Action environment
        func publisher() -> Publishers.ReplaceError<Publishers.TryMap<URLSession.DataTaskPublisher, String?>> {
            URLSession(configuration: urlSessionCfg)
                .bodyTextTaskPublisher(for: url)
                .map { $0 }.replaceError(with: nil)
        }
        
        return Publishers
            .MergeMany((1...10_000).map { _ in publisher() })
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario4() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("4")
        let publisher = URLSession(configuration: .ephemeral)
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        let publisher1SecTimeout = URLSession(
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
        let publisher = URLSession(configuration: .ephemeral)
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario6() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("6")
        let publisher = URLSession(configuration: .ephemeral)
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return publisher.merge(with: publisher, publisher)
            .compactMap { $0 }.first()
            .eraseToAnyPublisher()
    }
    
    func scenario7() -> AnyPublisher<String, Never> {
        let url: URL = baseURL.appendingPathComponent("7")
        let publisher = URLSession(configuration: .ephemeral)
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
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        
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
        let publisher = URLSession(
            configuration: {
                let configuration: URLSessionConfiguration = .ephemeral
                configuration.httpMaximumConnectionsPerHost = 10 // Default is 6
                return configuration
            }())
            .bodyTextTaskPublisher(for: url)
            .map { $0 }.replaceError(with: nil)
        
        return Publishers.MergeMany(Array(repeating: publisher, count: 10))
            .compactMap { $0 }
            .collect().map { $0.joined() }
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
        var disposeBag: Set<AnyCancellable> = Set()
        EasyRacer(baseURL: baseURL).scenarios()
            .sink(
                receiveCompletion: { _ in completed.signal() },
                receiveValue: { results in
                    for (idx, result) in results.enumerated() {
                        print("Scenario \(idx + 1): \(result ?? "error")")
                    }
                }
            )
            .store(in: &disposeBag)
        completed.wait()
    }
}
