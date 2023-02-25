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
        let group: DispatchGroup = DispatchGroup()
        let succeededLock: NSLock = NSLock()
        var succeeded: Bool = false
        
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
                    group.leave()
                }
            }
        dataTasks.forEach { dataTask in
            group.enter()
            dataTask.resume()
        }
        group.notify(queue: .main) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario2(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 2
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let group: DispatchGroup = DispatchGroup()
        let succeededLock: NSLock = NSLock()
        var succeeded: Bool = false
        
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
                    group.leave()
                }
            }
        dataTasks.forEach { dataTask in
            group.enter()
            dataTask.resume()
        }
        group.notify(queue: .main) {
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
        let group: DispatchGroup = DispatchGroup()
        let succeededLock: NSLock = NSLock()
        var succeeded: Bool = false
        
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
                    group.leave()
                }
            }
        dataTasks.forEach { dataTask in
            group.enter()
            dataTask.resume()
        }
        group.notify(queue: .main) {
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
        let group: DispatchGroup = DispatchGroup()
        let succeededLock: NSLock = NSLock()
        var succeeded: Bool = false
        
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
                    group.leave()
                }
            }
        dataTasks.forEach { dataTask in
            group.enter()
            dataTask.resume()
        }
        group.notify(queue: .main) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario5(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 5
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let group: DispatchGroup = DispatchGroup()
        let succeededLock: NSLock = NSLock()
        var succeeded: Bool = false
        
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
                    group.leave()
                }
            }
        dataTasks.forEach { dataTask in
            group.enter()
            dataTask.resume()
        }
        group.notify(queue: .main) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario6(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 6
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let group: DispatchGroup = DispatchGroup()
        let succeededLock: NSLock = NSLock()
        var succeeded: Bool = false
        
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
                    group.leave()
                }
            }
        dataTasks.forEach { dataTask in
            group.enter()
            dataTask.resume()
        }
        group.notify(queue: .main) {
            if !succeeded {
                scenarioHandler(scenario, nil)
            }
        }
    }
    
    func scenario7(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 7
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        
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
        DispatchQueue.global().asyncAfter(deadline: .now() + 3.0) {
            urlSession.dataTask(with: url).resume()
        }
    }
    
    func scenario8(scenarioHandler: @escaping @Sendable (Int, String?) -> Void) {
        let scenario: Int = 8
        let url: URL = baseURL.appendingPathComponent("\(scenario)")
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        let group: DispatchGroup = DispatchGroup()
        let succeededLock: NSLock = NSLock()
        var succeeded: Bool = false
        
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
                            group.leave()
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
                                group.leave()
                                return
                            }
                            urlSession.dataTask(with: closeURL).resume()
                        }.resume()
                    } else {
                        group.leave()
                    }
                }
            }
        dataTasks.forEach { dataTask in
            group.enter()
            dataTask.resume()
        }
        group.notify(queue: .main) {
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
        let group: DispatchGroup = DispatchGroup()
        let resultReceived: DispatchSemaphore = DispatchSemaphore(value: 0)
        let resultLock: NSLock = NSLock()
        var result: String = ""
        
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
                        
                        result += text
                        resultReceived.signal()
                    }
                    group.leave()
                }
            }
        dataTasks.forEach { dataTask in
            group.enter()
            dataTask.resume()
        }
        group.notify(queue: .main) {
            if result.count < 5 {
                scenarioHandler(scenario, nil)
            }
        }
        for _ in 1...5 {
            resultReceived.wait()
        }
        scenarioHandler(scenario, result)
    }
    //
    //    func scenario9() async -> String? {
    //        let result: String? = await withTaskGroup(of: String?.self) { group in
    //            defer { group.cancelAll() }
    //
    //            let url: URL = baseURL.appendingPathComponent("9")
    //            let urlSessionCfg: URLSessionConfiguration = .ephemeral
    //            urlSessionCfg.httpMaximumConnectionsPerHost = 10
    //            let urlSession: URLSession = URLSession(
    //                configuration: urlSessionCfg
    //            )
    //            @Sendable func doHTTPGet() async throws -> String {
    //                let (data, response) = try await urlSession.data(from: url)
    //                guard
    //                    let response = response as? HTTPURLResponse,
    //                    (200..<300).contains(response.statusCode),
    //                    let dataUTF8: String = String(data: data, encoding: .utf8)
    //                else {
    //                    throw EasyRacerError.error("invalid HTTP response")
    //                }
    //
    //                return dataUTF8
    //            }
    //            for _ in 1...10 {
    //                group.addTask { try? await doHTTPGet() }
    //            }
    //
    //            return await group
    //                .compactMap { $0 }
    //                .prefix(5)
    //                .reduce("") { $0 + $1 }
    //        }
    //
    //        return result
    //    }
    
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
