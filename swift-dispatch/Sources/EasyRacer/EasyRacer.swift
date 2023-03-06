import Foundation
#if canImport(FoundationNetworking)
import FoundationNetworking
#endif

extension URLSession {
    func bodyTextTask(
        with url: URL, completionHandler: @escaping (Result<String, Error>) -> Void
    ) -> URLSessionDataTask {
        dataTask(with: url) { data, response, error in
            if let error = error {
                completionHandler(.failure(error))
                return
            }
            
            guard
                let response: HTTPURLResponse = response as? HTTPURLResponse,
                (200..<300).contains(response.statusCode)
            else {
                completionHandler(.failure(URLError(.badServerResponse)))
                return
            }
            
            guard
                let data: Data = data,
                let text: String = String(data: data, encoding: .utf8)
            else {
                completionHandler(.failure(URLError(.cannotDecodeContentData)))
                return
            }
            
            completionHandler(.success(text))
        }
    }
}

@main
public struct EasyRacer {
    let baseURL: URL
    
    func scenario1(scenarioHandler: @escaping @Sendable (String?) -> Void) {
        let url: URL = baseURL.appendingPathComponent("1")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        let expectedResponsesGroup: DispatchGroup = DispatchGroup()
        var result: String? = nil
        let resultLock: NSLock = NSLock()
        expectedResponsesGroup.enter() // Expecting one response
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...2)
            .map { _ in
                urlSession.bodyTextTask(with: url) { bodyText in
                    if case let .success(text) = bodyText {
                        resultLock.lock()
                        defer { resultLock.unlock() }
                        
                        if result == nil {
                            result = text
                            expectedResponsesGroup.leave()
                        }
                    }
                    allRequestsGroup.leave()
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Got what we wanted, cancel remaining requests
        expectedResponsesGroup.notify(queue: .global()) {
            for dataTask in dataTasks {
                dataTask.cancel()
            }
        }
        
        // Send result
        allRequestsGroup.notify(queue: .global()) {
            scenarioHandler(result)
        }
    }
    
    func scenario2(scenarioHandler: @escaping @Sendable (String?) -> Void) {
        let url: URL = baseURL.appendingPathComponent("2")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        let expectedResponsesGroup: DispatchGroup = DispatchGroup()
        var result: String? = nil
        let resultLock: NSLock = NSLock()
        expectedResponsesGroup.enter() // Expecting one response
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...2)
            .map { _ in
                urlSession.bodyTextTask(with: url) { bodyText in
                    if case let .success(text) = bodyText {
                        resultLock.lock()
                        defer { resultLock.unlock() }
                        
                        if result == nil {
                            result = text
                            expectedResponsesGroup.leave()
                        }
                    }
                    allRequestsGroup.leave()
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Got what we wanted, cancel remaining requests
        expectedResponsesGroup.notify(queue: .global()) {
            for dataTask in dataTasks {
                dataTask.cancel()
            }
        }
        
        // Send result
        allRequestsGroup.notify(queue: .global()) {
            scenarioHandler(result)
        }
    }
    
    func scenario3(scenarioHandler: @escaping @Sendable (String?) -> Void) {
        let url: URL = baseURL.appendingPathComponent("3")
        // Ideally, we'd use a single URLSession configured to handle 10k connections.
        // This doesn't seem to work - observed from the scenario server, it'll create
        // ~110 connections, and then stall.
        // URLSession is close-sourced, so it's hard to tell what is going on.
        //            let urlSession: URLSession = URLSession(
        //                configuration: {
        //                    let urlSessionCfg = URLSessionConfiguration.ephemeral
        //                    urlSessionCfg.httpMaximumConnectionsPerHost = 10_000
        //                    return urlSessionCfg
        //                }(),
        //                delegate: nil,
        //                delegateQueue: {
        //                    let opQueue: OperationQueue = OperationQueue()
        //                    opQueue.maxConcurrentOperationCount = 10_000
        //                    return opQueue
        //                }()
        //            )
        let urlSessionCfg = URLSessionConfiguration.ephemeral
        urlSessionCfg.timeoutIntervalForRequest = 900 // Seems to be required for GitHub Action environment
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        let expectedResponsesGroup: DispatchGroup = DispatchGroup()
        var result: String? = nil
        let resultLock: NSLock = NSLock()
        expectedResponsesGroup.enter() // Expecting one response
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...10_000)
            .map { _ in
                URLSession(configuration: urlSessionCfg).bodyTextTask(with: url) { bodyText in
                    if case let .success(text) = bodyText {
                        resultLock.lock()
                        defer { resultLock.unlock() }
                        
                        if result == nil {
                            result = text
                            expectedResponsesGroup.leave()
                        }
                    }
                    allRequestsGroup.leave()
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Got what we wanted, cancel remaining requests
        expectedResponsesGroup.notify(queue: .global()) {
            for dataTask in dataTasks {
                dataTask.cancel()
            }
        }
        
        // Send result
        allRequestsGroup.notify(queue: .global()) {
            scenarioHandler(result)
        }
    }
    
    func scenario4(scenarioHandler: @escaping @Sendable (String?) -> Void) {
        let url: URL = baseURL.appendingPathComponent("4")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let urlSession1SecTimeout: URLSession = URLSession(
            configuration: {
                let configuration: URLSessionConfiguration = .ephemeral
                configuration.timeoutIntervalForRequest = 1
                return configuration
            }()
        )
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        let expectedResponsesGroup: DispatchGroup = DispatchGroup()
        var result: String? = nil
        let resultLock: NSLock = NSLock()
        expectedResponsesGroup.enter() // Expecting one response
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = [urlSession, urlSession1SecTimeout]
            .map { urlSession in
                urlSession.bodyTextTask(with: url) { bodyText in
                    if case let .success(text) = bodyText {
                        resultLock.lock()
                        defer { resultLock.unlock() }
                        
                        if result == nil {
                            result = text
                            expectedResponsesGroup.leave()
                        }
                    }
                    allRequestsGroup.leave()
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Got what we wanted, cancel remaining requests
        expectedResponsesGroup.notify(queue: .global()) {
            for dataTask in dataTasks {
                dataTask.cancel()
            }
        }
        
        // Send result
        allRequestsGroup.notify(queue: .global()) {
            scenarioHandler(result)
        }
    }
    
    func scenario5(scenarioHandler: @escaping @Sendable (String?) -> Void) {
        let url: URL = baseURL.appendingPathComponent("5")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        let expectedResponsesGroup: DispatchGroup = DispatchGroup()
        var result: String? = nil
        let resultLock: NSLock = NSLock()
        expectedResponsesGroup.enter() // Expecting one response
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...2)
            .map { _ in
                urlSession.bodyTextTask(with: url) { bodyText in
                    if case let .success(text) = bodyText {
                        resultLock.lock()
                        defer { resultLock.unlock() }
                        
                        if result == nil {
                            result = text
                            expectedResponsesGroup.leave()
                        }
                    }
                    allRequestsGroup.leave()
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Got what we wanted, cancel remaining requests
        expectedResponsesGroup.notify(queue: .global()) {
            for dataTask in dataTasks {
                dataTask.cancel()
            }
        }
        
        // Send result
        allRequestsGroup.notify(queue: .global()) {
            scenarioHandler(result)
        }
    }
    
    func scenario6(scenarioHandler: @escaping @Sendable (String?) -> Void) {
        let url: URL = baseURL.appendingPathComponent("6")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        let expectedResponsesGroup: DispatchGroup = DispatchGroup()
        var result: String? = nil
        let resultLock: NSLock = NSLock()
        expectedResponsesGroup.enter() // Expecting one response
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...3)
            .map { _ in
                urlSession.bodyTextTask(with: url) { bodyText in
                    if case let .success(text) = bodyText {
                        resultLock.lock()
                        defer { resultLock.unlock() }
                        
                        if result == nil {
                            result = text
                            expectedResponsesGroup.leave()
                        }
                    }
                    allRequestsGroup.leave()
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Got what we wanted, cancel remaining requests
        expectedResponsesGroup.notify(queue: .global()) {
            for dataTask in dataTasks {
                dataTask.cancel()
            }
        }
        
        // Send result
        allRequestsGroup.notify(queue: .global()) {
            scenarioHandler(result)
        }
    }
    
    func scenario7(scenarioHandler: @escaping @Sendable (String?) -> Void) {
        let url: URL = baseURL.appendingPathComponent("7")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        allRequestsGroup.enter()
        var result: String? = nil
        
        let secondDataTask: URLSessionDataTask = urlSession.bodyTextTask(with: url) { _ in
            allRequestsGroup.leave()
        }
        
        // Execute first HTTP request
        urlSession
            .bodyTextTask(with: url) { bodyText in
                if case let .success(text) = bodyText {
                    result = text
                }
                secondDataTask.cancel()
            }
            .resume()
        
        // Execute second request after 3 seconds
        DispatchQueue.global().asyncAfter(deadline: .now() + 3.0) {
            secondDataTask.resume()
        }
        
        // Send result
        allRequestsGroup.notify(queue: .global()) {
            scenarioHandler(result)
        }
    }
    
    func scenario8(scenarioHandler: @escaping @Sendable (String?) -> Void) {
        let url: URL = baseURL.appendingPathComponent("8")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        let expectedResponsesGroup: DispatchGroup = DispatchGroup()
        var result: String? = nil
        let resultLock: NSLock = NSLock()
        expectedResponsesGroup.enter() // Expecting one response
        
        guard
            let urlComps: URLComponents = URLComponents(
                url: url, resolvingAgainstBaseURL: false
            )
        else {
            scenarioHandler(nil)
            return
        }
        
        // Build "open" URL
        var openURLComps = urlComps
        openURLComps.queryItems = [URLQueryItem(name: "open", value: nil)]
        
        guard
            let openURL: URL = openURLComps.url
        else {
            scenarioHandler(nil)
            return
        }
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...2)
            .map { _ in
                // Open
                urlSession.bodyTextTask(with: openURL) { openBodyText in
                    if case let .success(id) = openBodyText {
                        // Use
                        var useURLComps = urlComps
                        useURLComps.queryItems = [URLQueryItem(name: "use", value: id)]
                        
                        guard
                            let useURL: URL = useURLComps.url
                        else {
                            allRequestsGroup.leave()
                            return
                        }
                        urlSession.bodyTextTask(with: useURL) { useBodyText in
                            if case let .success(text) = useBodyText {
                                resultLock.lock()
                                defer { resultLock.unlock() }
                                
                                if result == nil {
                                    result = text
                                    expectedResponsesGroup.leave()
                                }
                            }
                            
                            // Close
                            var closeURLComps = urlComps
                            closeURLComps.queryItems = [URLQueryItem(name: "close", value: id)]
                            
                            guard
                                let closeURL: URL = closeURLComps.url
                            else {
                                allRequestsGroup.leave()
                                return
                            }
                            urlSession.bodyTextTask(with: closeURL) { _ in
                                allRequestsGroup.leave()
                            }.resume()
                        }.resume()
                    }
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Got what we wanted, cancel remaining requests
        expectedResponsesGroup.notify(queue: .global()) {
            for dataTask in dataTasks {
                dataTask.cancel()
            }
        }
        
        // Send result
        allRequestsGroup.notify(queue: .global()) {
            scenarioHandler(result)
        }
    }
    
    func scenario9(scenarioHandler: @escaping @Sendable (String?) -> Void) {
        let url: URL = baseURL.appendingPathComponent("9")
        let urlSession: URLSession = URLSession(
            configuration: {
                let configuration: URLSessionConfiguration = .ephemeral
                configuration.httpMaximumConnectionsPerHost = 10 // Default is 6
                return configuration
            }()
        )
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        var resultAccum: String = ""
        let resultLock: NSLock = NSLock()
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...10)
            .map { _ in
                urlSession.bodyTextTask(with: url) { bodyText in
                    if case let .success(text) = bodyText {
                        resultLock.lock()
                        defer { resultLock.unlock() }
                        
                        resultAccum += text
                    }
                    allRequestsGroup.leave()
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Notify failure if all requests completed before expected number of successful requests
        allRequestsGroup.notify(queue: .global()) {
            scenarioHandler(resultAccum)
        }
    }
    
    // Runs scenarios one by one, blocking until they are all complete
    public func scenarios(scenariosHandler: @escaping @Sendable ([String?]) -> Void) {
        let scenarios = [
            (1, scenario1),
            (2, scenario2),
            (3, scenario3),
            (4, scenario4),
            (5, scenario5),
            (6, scenario6),
            (7, scenario7),
            (8, scenario8),
            (9, scenario9),
        ]
        let completions: DispatchSemaphore = DispatchSemaphore(value: 0)
        func sortResultsAndNotify(results: [(Int, String?)]) {
            scenariosHandler(results.sorted { $0.0 < $1.0 }.map { $0.1 })
            completions.signal()
        }
        scenarios.reversed().reduce(sortResultsAndNotify) { nextScenarios, currentScenario in
            { resultsAccum in
                let (scenarioNumber, scenario) = currentScenario
                scenario { result in
                    nextScenarios(resultsAccum + [(scenarioNumber, result)])
                }
            }
        }([])
        completions.wait()
    }
    
    public static func main() {
        guard
            let baseURL = URL(string: "http://localhost:8080")
        else { return }
        
        EasyRacer(baseURL: baseURL).scenarios { results in
            for (idx, result) in results.enumerated() {
                print("Scenario \(idx + 1): \(result ?? "error")")
            }
        }
    }
}
