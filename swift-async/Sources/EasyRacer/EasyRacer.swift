import Foundation

enum EasyRacerError : Error {
    case error(String)
}

@main
public struct EasyRacer {
    let baseURL: URL
    
    func scenario1() async -> String? {
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            let url: URL = baseURL.appendingPathComponent("1")
            let urlSession: URLSession = URLSession(configuration: .ephemeral)
            @Sendable func doHTTPGet() async throws -> String {
                let (data, response) = try await urlSession.data(from: url)
                guard
                    let response = response as? HTTPURLResponse,
                    (200..<300).contains(response.statusCode),
                    let dataUTF8: String = String(data: data, encoding: .utf8)
                else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                
                return dataUTF8
            }
            group.addTask { try? await doHTTPGet() }
            group.addTask { try? await doHTTPGet() }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario2() async -> String? {
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            let url: URL = baseURL.appendingPathComponent("2")
            let urlSession: URLSession = URLSession(configuration: .ephemeral)
            @Sendable func doHTTPGet() async throws -> String {
                let (data, response) = try await urlSession.data(from: url)
                guard
                    let response = response as? HTTPURLResponse,
                    (200..<300).contains(response.statusCode),
                    let dataUTF8: String = String(data: data, encoding: .utf8)
                else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                
                return dataUTF8
            }
            group.addTask { try? await doHTTPGet() }
            group.addTask { try? await doHTTPGet() }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario3() async -> String? {
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
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
            @Sendable func doHTTPGet() async throws -> String {
                // Using URLSession per connection to get around connection limit
                let urlSession: URLSession = URLSession(configuration: urlSessionCfg)
                let (data, response) = try await urlSession.data(from: url)
                guard
                    let response = response as? HTTPURLResponse,
                    (200..<300).contains(response.statusCode),
                    let dataUTF8: String = String(data: data, encoding: .utf8)
                else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                
                return dataUTF8
            }
            for _ in 1...10_000 {
                group.addTask { try? await doHTTPGet() }
            }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario4() async -> String? {
        let url: URL = baseURL.appendingPathComponent("4")
        @Sendable func doHTTPGet(_ urlSession: URLSession) async throws -> String {
            let (data, response) = try await urlSession.data(from: url)
            guard
                let response = response as? HTTPURLResponse,
                (200..<300).contains(response.statusCode),
                let dataUTF8: String = String(data: data, encoding: .utf8)
            else {
                throw EasyRacerError.error("invalid HTTP response")
            }
            
            return dataUTF8
        }
        
        let urlSession: URLSession = URLSession(configuration: .ephemeral)
        async let result: String = doHTTPGet(urlSession)
        
        let urlSessionCfg1SecTimeout: URLSessionConfiguration = .ephemeral
        urlSessionCfg1SecTimeout.timeoutIntervalForRequest = 1
        let urlSession1SecTimeout: URLSession = URLSession(configuration: urlSessionCfg1SecTimeout)
        _ = try? await doHTTPGet(urlSession1SecTimeout)
        
        return try? await result
    }
    
    func scenario5() async -> String? {
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            let url: URL = baseURL.appendingPathComponent("5")
            let urlSession: URLSession = URLSession(configuration: .ephemeral)
            @Sendable func doHTTPGet() async throws -> String {
                let (data, response) = try await urlSession.data(from: url)
                guard
                    let response = response as? HTTPURLResponse,
                    (200..<300).contains(response.statusCode),
                    let dataUTF8: String = String(data: data, encoding: .utf8)
                else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                
                return dataUTF8
            }
            group.addTask { try? await doHTTPGet() }
            group.addTask { try? await doHTTPGet() }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario6() async -> String? {
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            let url: URL = baseURL.appendingPathComponent("6")
            let urlSession: URLSession = URLSession(configuration: .ephemeral)
            @Sendable func doHTTPGet() async throws -> String {
                let (data, response) = try await urlSession.data(from: url)
                guard
                    let response = response as? HTTPURLResponse,
                    (200..<300).contains(response.statusCode),
                    let dataUTF8: String = String(data: data, encoding: .utf8)
                else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                
                return dataUTF8
            }
            group.addTask { try? await doHTTPGet() }
            group.addTask { try? await doHTTPGet() }
            group.addTask { try? await doHTTPGet() }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario7() async -> String? {
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            let url: URL = baseURL.appendingPathComponent("7")
            let urlSession: URLSession = URLSession(configuration: .ephemeral)
            @Sendable func doHTTPGet() async throws -> String {
                let (data, response) = try await urlSession.data(from: url)
                guard
                    let response = response as? HTTPURLResponse,
                    (200..<300).contains(response.statusCode),
                    let dataUTF8: String = String(data: data, encoding: .utf8)
                else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                
                return dataUTF8
            }
            group.addTask { try? await doHTTPGet() }
            try? await Task.sleep(nanoseconds: 3_000_000_000) // 3 seconds
            group.addTask { try? await doHTTPGet() }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario8() async -> String? {
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            let url: URL = baseURL.appendingPathComponent("8")
            let urlSession: URLSession = URLSession(configuration: .ephemeral)
            @Sendable func doOpenUseAndClose() async throws -> String {
                guard
                    let urlComps: URLComponents = URLComponents(
                        url: url, resolvingAgainstBaseURL: false
                    )
                else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                
                // Open
                var openURLComps = urlComps
                openURLComps.queryItems = [URLQueryItem(name: "open", value: nil)]
                
                guard
                    let openURL: URL = openURLComps.url
                else {
                    throw EasyRacerError.error("bad open URL")
                }
                let (openData, openResponse) = try await urlSession.data(from: openURL)
                
                guard
                    let response = openResponse as? HTTPURLResponse,
                    (200..<300).contains(response.statusCode),
                    let id: String = String(data: openData, encoding: .utf8)
                else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                
                // Use
                var useURLComps = urlComps
                useURLComps.queryItems = [URLQueryItem(name: "use", value: id)]
                
                guard
                    let useURL: URL = useURLComps.url
                else {
                    throw EasyRacerError.error("bad use URL")
                }
                let (useData, useResponse) = try await urlSession.data(from: useURL)
                
                let dataUTF8: String?
                if
                    let response = useResponse as? HTTPURLResponse,
                    (200..<300).contains(response.statusCode)
                {
                    dataUTF8 = String(data: useData, encoding: .utf8)
                } else {
                    dataUTF8 = nil
                }
                
                // Close
                var closeURLComps = urlComps
                closeURLComps.queryItems = [URLQueryItem(name: "close", value: id)]
                
                guard
                    let closeURL: URL = closeURLComps.url
                else {
                    throw EasyRacerError.error("bad close URL")
                }
                let _ = try await urlSession.data(from: closeURL)
                
                guard let dataUTF8: String = dataUTF8 else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                return dataUTF8
            }
            group.addTask { try? await doOpenUseAndClose() }
            group.addTask { try? await doOpenUseAndClose() }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario9() async -> String? {
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            let url: URL = baseURL.appendingPathComponent("9")
            let urlSessionCfg: URLSessionConfiguration = .ephemeral
            urlSessionCfg.httpMaximumConnectionsPerHost = 10
            let urlSession: URLSession = URLSession(
                configuration: urlSessionCfg
            )
            @Sendable func doHTTPGet() async throws -> String {
                let (data, response) = try await urlSession.data(from: url)
                guard
                    let response = response as? HTTPURLResponse,
                    (200..<300).contains(response.statusCode),
                    let dataUTF8: String = String(data: data, encoding: .utf8)
                else {
                    throw EasyRacerError.error("invalid HTTP response")
                }
                
                return dataUTF8
            }
            for _ in 1...10 {
                group.addTask { try? await doHTTPGet() }
            }
            
            return await group
                .compactMap { $0 }
                .prefix(5)
                .reduce("") { $0 + $1 }
        }
        
        return result
    }
    
    public func scenarios() async -> [(Int, String?)] {
        [
            (1, await scenario1()),
            (2, await scenario2()),
            (4, await scenario4()),
            (5, await scenario5()),
            (6, await scenario6()),
            (7, await scenario7()),
            (8, await scenario8()),
            (9, await scenario9()),
            (3, await scenario3()), // Scenario 3 makes Swift tired, resulting in subsequent scenarios failing - run it last
        ].sorted { $0.0 < $1.0 }
    }
    
    public static func main() async throws {
        guard
            let baseURL = URL(string: "http://localhost:8080")
        else { return }
        
        for (scenarioNumber, result) in await EasyRacer(baseURL: baseURL).scenarios() {
            print("Scenario \(scenarioNumber): \(result ?? "error")")
        }
    }
}
