import Foundation
#if canImport(FoundationNetworking)
import FoundationNetworking
#endif

@main
public struct EasyRacer {
    let baseURL: URL
    
    func scenario1(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 1
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        var succeeded: Bool = false
        let succeededLock: NSLock = NSLock()
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...2)
            .map { _ in
                urlSession.dataTask(with: url) { data, response, error in
                    if
                        error == nil && (200..<300).contains((response as? HTTPURLResponse)?.statusCode ?? -1),
                        let data: Data = data,
                        let text: String = String(data: data, encoding: .utf8)
                    {
                        succeededLock.lock()
                        defer { succeededLock.unlock() }
                        
                        if !succeeded {
                            scenarioHandler(scenario, text)
                            succeeded = true
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
        
        // Notify failure if all requests completed without a single successful response
        allRequestsGroup.notify(queue: .global()) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario2(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 2
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        var succeeded: Bool = false
        let succeededLock: NSLock = NSLock()
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...2)
            .map { _ in
                urlSession.dataTask(with: url) { data, response, error in
                    if
                        error == nil && (200..<300).contains((response as? HTTPURLResponse)?.statusCode ?? -1),
                        let data: Data = data,
                        let text: String = String(data: data, encoding: .utf8)
                    {
                        succeededLock.lock()
                        defer { succeededLock.unlock() }
                        
                        if !succeeded {
                            scenarioHandler(scenario, text)
                            succeeded = true
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
        
        // Notify failure if all requests completed without a single successful response
        allRequestsGroup.notify(queue: .global()) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario3(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 3
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSessionCfg = URLSessionConfiguration.ephemeral
        urlSessionCfg.timeoutIntervalForRequest = 900 // Seems to be required for GitHub Action environment
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        var succeeded: Bool = false
        let succeededLock: NSLock = NSLock()
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...10_000)
            .map { _ in
                URLSession(configuration: urlSessionCfg).dataTask(with: url) { data, response, error in
                    if
                        error == nil && (200..<300).contains((response as? HTTPURLResponse)?.statusCode ?? -1),
                        let data: Data = data,
                        let text: String = String(data: data, encoding: .utf8)
                    {
                        succeededLock.lock()
                        defer { succeededLock.unlock() }
                        
                        if !succeeded {
                            scenarioHandler(scenario, text)
                            succeeded = true
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
        
        // Notify failure if all requests completed without a single successful response
        allRequestsGroup.notify(queue: .global()) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario4(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 4
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let urlSession1SecTimeout: URLSession = URLSession(
            configuration: {
                let configuration: URLSessionConfiguration = .ephemeral
                configuration.timeoutIntervalForRequest = 1
                return configuration
            }()
        )
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        var succeeded: Bool = false
        let succeededLock: NSLock = NSLock()
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = [urlSession, urlSession1SecTimeout]
            .map { urlSession in
                urlSession.dataTask(with: url) { data, response, error in
                    if
                        error == nil && (200..<300).contains((response as? HTTPURLResponse)?.statusCode ?? -1),
                        let data: Data = data,
                        let text: String = String(data: data, encoding: .utf8)
                    {
                        succeededLock.lock()
                        defer { succeededLock.unlock() }
                        
                        if !succeeded {
                            scenarioHandler(scenario, text)
                            succeeded = true
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
        
        // Notify failure if all requests completed without a single successful response
        allRequestsGroup.notify(queue: .global()) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario5(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 5
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        var succeeded: Bool = false
        let succeededLock: NSLock = NSLock()
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...2)
            .map { _ in
                urlSession.dataTask(with: url) { data, response, error in
                    if
                        error == nil && (200..<300).contains((response as? HTTPURLResponse)?.statusCode ?? -1),
                        let data: Data = data,
                        let text: String = String(data: data, encoding: .utf8)
                    {
                        succeededLock.lock()
                        defer { succeededLock.unlock() }
                        
                        if !succeeded {
                            scenarioHandler(scenario, text)
                            succeeded = true
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
        
        // Notify failure if all requests completed without a single successful response
        allRequestsGroup.notify(queue: .global()) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario6(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 6
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        var succeeded: Bool = false
        let succeededLock: NSLock = NSLock()
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...3)
            .map { _ in
                urlSession.dataTask(with: url) { data, response, error in
                    if
                        error == nil && (200..<300).contains((response as? HTTPURLResponse)?.statusCode ?? -1),
                        let data: Data = data,
                        let text: String = String(data: data, encoding: .utf8)
                    {
                        succeededLock.lock()
                        defer { succeededLock.unlock() }
                        
                        if !succeeded {
                            scenarioHandler(scenario, text)
                            succeeded = true
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
        // Notify failure if all requests completed without a single successful response
        allRequestsGroup.notify(queue: .global()) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario7(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 7
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        
        // Execute first HTTP request
        urlSession
            .dataTask(with: url) { data, response, error in
                if
                    error == nil && (200..<300).contains((response as? HTTPURLResponse)?.statusCode ?? -1),
                    let data: Data = data,
                    let text: String = String(data: data, encoding: .utf8)
                {
                    scenarioHandler(scenario, text)
                } else {
                    scenarioHandler(scenario, nil)
                }
            }
            .resume()
        
        // Execute second request after 3 seconds
        DispatchQueue.global().asyncAfter(deadline: .now() + 3.0) {
            urlSession.dataTask(with: url).resume()
        }
    }
    
    func scenario8(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 8
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        var succeeded: Bool = false
        let succeededLock: NSLock = NSLock()
        
        // Build "open" URL
        guard
            let urlComps: URLComponents = URLComponents(
                url: url, resolvingAgainstBaseURL: false
            )
        else {
            scenarioHandler(scenario, nil)
            return
        }
        
        var openURLComps = urlComps
        openURLComps.queryItems = [URLQueryItem(name: "open", value: nil)]
        
        guard
            let openURL: URL = openURLComps.url
        else {
            scenarioHandler(scenario, nil)
            return
        }
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...2)
            .map { _ in
                // Open
                urlSession.dataTask(with: openURL) { openData, openResponse, openError in
                    if
                        openError == nil && (200..<300).contains((openResponse as? HTTPURLResponse)?.statusCode ?? -1),
                        let openData: Data = openData,
                        let id: String = String(data: openData, encoding: .utf8)
                    {
                        // Use
                        var useURLComps = urlComps
                        useURLComps.queryItems = [URLQueryItem(name: "use", value: id)]
                        
                        guard
                            let useURL: URL = useURLComps.url
                        else {
                            allRequestsGroup.leave()
                            return
                        }
                        urlSession.dataTask(with: useURL) { useData, useResponse, useError in
                            if
                                useError == nil && (200..<300).contains((useResponse as? HTTPURLResponse)?.statusCode ?? -1),
                                let useData: Data = useData
                            {
                                succeededLock.lock()
                                defer { succeededLock.unlock() }
                                
                                if !succeeded {
                                    scenarioHandler(scenario, String(data: useData, encoding: .utf8))
                                    succeeded = true
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
                            urlSession.dataTask(with: closeURL).resume()
                        }.resume()
                    } else {
                        allRequestsGroup.leave()
                    }
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Notify failure if all requests completed without a single successful response
        allRequestsGroup.notify(queue: .global()) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario9(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 9
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(
            configuration: {
                let configuration: URLSessionConfiguration = .ephemeral
                configuration.httpMaximumConnectionsPerHost = 10
                return configuration
            }()
        )
        let allRequestsGroup: DispatchGroup = DispatchGroup()
        let expectedRequestsGroup: DispatchGroup = DispatchGroup()
        var resultRemaining: Int = 5
        var resultAccum: String = ""
        let resultLock: NSLock = NSLock()
        for _ in 1...resultRemaining {
            expectedRequestsGroup.enter()
        }
        
        // Set up HTTP requests without executing
        let dataTasks: [URLSessionDataTask] = (1...10)
            .map { _ in
                urlSession.dataTask(with: url) { data, response, error in
                    if
                        error == nil && (200..<300).contains((response as? HTTPURLResponse)?.statusCode ?? -1),
                        let data: Data = data,
                        let text: String = String(data: data, encoding: .utf8)
                    {
                        resultLock.lock()
                        defer { resultLock.unlock() }
                        
                        resultRemaining -= 1
                        resultAccum += text
                        expectedRequestsGroup.leave()
                    }
                    allRequestsGroup.leave()
                }
            }
        
        // Executing requests, adding them to the DispatchGroup
        for dataTask in dataTasks {
            allRequestsGroup.enter()
            dataTask.resume()
        }
        
        // Expected number of requests completed, notify success
        expectedRequestsGroup.notify(queue: .global()) {
            scenarioHandler(scenario, resultAccum)
        }
        
        // Notify failure if all requests completed before expected number of successful requests
        allRequestsGroup.notify(queue: .global()) {
            if resultRemaining > 0 {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    // Runs scenarios one by one, blocks until they are all complete
    public func scenarios(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenarios = [
            scenario1,
            scenario2,
            scenario3,
            scenario4,
            scenario5,
            scenario6,
            scenario7,
            scenario8,
            scenario9,
        ]
        let completions: DispatchSemaphore = DispatchSemaphore(value: 0)
        scenarios.reversed().reduce({ () in }) { nextScenarios, currentScenario in
            {
                currentScenario { scenarioNumber, result in
                    scenarioHandler(scenarioNumber, result)
                    completions.signal()
                    nextScenarios()
                }
            }
        }()
        for _ in scenarios {
            completions.wait()
        }
    }
    
    public static func main() {
        guard
            let baseURL = URL(string: "http://localhost:8080")
        else { return }
        
        EasyRacer(baseURL: baseURL).scenarios { scenarioNumber, result in
            print("Scenario \(scenarioNumber): \(result ?? "error")")
        }
    }
}
