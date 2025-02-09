import Foundation

extension URLSession {
    func bodyText(from url: URL) async throws -> String {
        let (data, response) = try await data(from: url)
        
        guard
            let response = response as? HTTPURLResponse,
            200..<300 ~= response.statusCode
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

@main
public struct EasyRacer: Sendable {
    let baseURL: URL
    let urlSession: some URLSession = ScalableURLSession(
        configuration: {
            let configuration = URLSessionConfiguration.ephemeral
            configuration.httpMaximumConnectionsPerHost = 1_000
            configuration.timeoutIntervalForRequest = 120
            return configuration
        }(),
        requestsPerSession: 100,
        timeIntervalBetweenRequests: 0.005 // 5ms
    )
    
    func scenario1() async -> String? {
        let url: URL = baseURL.appending(path: "1")
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            group.addTask { try? await urlSession.bodyText(from: url) }
            group.addTask { try? await urlSession.bodyText(from: url) }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario2() async -> String? {
        let url: URL = baseURL.appending(path: "2")
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            group.addTask { try? await urlSession.bodyText(from: url) }
            group.addTask { try? await urlSession.bodyText(from: url) }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario3() async -> String? {
        let url: URL = baseURL.appending(path: "3")
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            for _ in 1...10_000 {
                group.addTask {
                    return try? await urlSession.bodyText(from: url)
                }
            }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario4() async -> String? {
        let url: URL = baseURL.appending(path: "4")
        let urlSession1SecTimeout: URLSession = Foundation.URLSession(configuration: {
            let configuration: URLSessionConfiguration = .ephemeral
            configuration.timeoutIntervalForRequest = 1
            return configuration
        }())
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            group.addTask { try? await urlSession.bodyText(from: url) }
            group.addTask { try? await urlSession1SecTimeout.bodyText(from: url) }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario5() async -> String? {
        let url: URL = baseURL.appending(path: "5")
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            group.addTask { try? await urlSession.bodyText(from: url) }
            group.addTask { try? await urlSession.bodyText(from: url) }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario6() async -> String? {
        let url: URL = baseURL.appending(path: "6")
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            group.addTask { try? await urlSession.bodyText(from: url) }
            group.addTask { try? await urlSession.bodyText(from: url) }
            group.addTask { try? await urlSession.bodyText(from: url) }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario7() async -> String? {
        let url: URL = baseURL.appending(path: "7")
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            group.addTask { try? await urlSession.bodyText(from: url) }
            group.addTask {
                try? await Task.sleep(nanoseconds: 3_000_000_000) // 3 seconds
                return try? await urlSession.bodyText(from: url)
            }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario8() async -> String? {
        let url: URL = baseURL.appending(path: "8")
        @Sendable func doOpenUseAndClose() async throws -> String {
            // Open
            let id = try await urlSession.bodyText(
                from: url.appending(queryItems: [.init(name: "open", value: nil)])
            )
            
            // Use
            let text: String? = try? await urlSession.bodyText(
                from: url.appending(queryItems: [.init(name: "use", value: id)])
            )
            
            // Close
            let _ = try await urlSession.data(
                from: url.appending(queryItems: [.init(name: "close", value: id)])
            )
            
            guard let text: String = text else {
                throw URLError(.badServerResponse)
            }
            return text
        }
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            group.addTask { try? await doOpenUseAndClose() }
            group.addTask { try? await doOpenUseAndClose() }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    func scenario9() async -> String? {
        let url: URL = baseURL.appending(path: "9")
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            for _ in 1...10 {
                group.addTask { try? await urlSession.bodyText(from: url) }
            }
            
            return await group
                .compactMap { $0 }
                .reduce("") { $0 + $1 }
        }
        
        return result
    }
    
    func scenario10() async -> String? {
        let url: URL = baseURL.appending(path: "10")
        let id: String = UUID().uuidString
        
        @Sendable func request() async throws {
            let _ = try await urlSession.data(
                from: url.appending(queryItems: [.init(name: id, value: nil)])
            )
        }
        @Sendable func blocking() async throws {
            while true {
                try Task.checkCancellation()
                // busy wait
            }
        }
        
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
        ) async throws -> String {
            let endWallTime: TimeInterval = currentWallTime()
            let endCPUTime: TimeInterval = currentCPUTime()
            let totalUsageOfCPU: Double = (endCPUTime - startCPUTime) / (endWallTime - startWallTime)
            
            let (data, response) = try await urlSession.data(
                from: url.appending(queryItems: [.init(name: id, value: "\(totalUsageOfCPU)")])
            )
            
            guard
                let response: HTTPURLResponse = response as? HTTPURLResponse,
                200..<400 ~= response.statusCode
            else {
                throw URLError(.badServerResponse)
            }
            
            if 300..<400 ~= response.statusCode {
                try await Task.sleep(nanoseconds: 1_000_000_000)
                return try await reportProcessLoad(
                    startWallTime: endWallTime, startCPUTime: endCPUTime
                )
            }
            
            guard
                let text: String = String(data: data, encoding: .utf8)
            else {
                throw URLError(.cannotDecodeContentData)
            }
            
            return text
        }
        
        // Run blocker
        async let blockerResult: Void? = withTaskGroup(of: Void?.self) { group in
            defer { group.cancelAll() }
            
            group.addTask { try? await request() }
            group.addTask { try? await blocking() }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        // Run reporter
        let result: String? = try? await reportProcessLoad(
            startWallTime: currentWallTime(), startCPUTime: currentCPUTime()
        )
        
        return await blockerResult.flatMap { result }
    }
    
    func scenario11() async -> String? {
        let url: URL = baseURL.appending(path: "11")
        
        let result: String? = await withTaskGroup(of: String?.self) { group in
            defer { group.cancelAll() }
            
            group.addTask {
                await withTaskGroup(of: String?.self) { subgroup in
                    subgroup.addTask { try? await urlSession.bodyText(from: url) }
                    subgroup.addTask { try? await urlSession.bodyText(from: url) }
                    
                    return await subgroup.first { $0 != nil }.flatMap { $0 }
                }
            }
            group.addTask { try? await urlSession.bodyText(from: url) }
            
            return await group.first { $0 != nil }.flatMap { $0 }
        }
        
        return result
    }
    
    public func scenarios() async -> [String?] {
        [
            (1, await scenario1()),
            (2, await scenario2()),
            (4, await scenario4()),
            (5, await scenario5()),
            (6, await scenario6()),
            (7, await scenario7()),
            (8, await scenario8()),
            (9, await scenario9()),
            (10, await scenario10()),
            (11, await scenario11()),
            (3, await scenario3()), // This has to come last, as it frequently causes other scenarios to fail
        ].sorted { $0.0 < $1.0 }.map { $0.1 }
    }
    
    public static func main() async throws {
        guard
            let baseURL = URL(string: "http://localhost:8080")
        else { return }
        
        let results = await EasyRacer(baseURL: baseURL).scenarios()
        for (idx, result) in results.enumerated() {
            print("Scenario \(idx + 1): \(result ?? "error")")
        }
        exit(results.allSatisfy { $0 != nil } ? EXIT_SUCCESS : EXIT_FAILURE)
    }
}
