import Foundation

/// URLSession operations we actually use in Easy Racer
protocol URLSession: Sendable {
    func data(from url: URL) async throws -> (Data, URLResponse)
}

/// Make sure the URLSession protocol isn't defining incompatible methods
extension Foundation.URLSession: URLSession {
}

/// URLSession implementation that is able to handle 10k concurrent connections
///
///  It does this by delegating to Foundation.URLSession, ensuring:
///   - Each delegatee handles no more than requestsPerSession requests
///   - Requests are at least timeIntervalBetweenRequests apart
///     (Needed in some environments, e.g., GitHub Actions)
actor ScalableURLSession: URLSession {
    private static let nanosecondsPerSecond: Double = 1_000_000_000
    
    private let configuration: URLSessionConfiguration
    private let requestsPerSession: UInt
    private let timeIntervalBetweenRequests: TimeInterval
    
    private var currentDelegatee: Foundation.URLSession
    private var currentRequestCount: UInt = 0
    private var nextRequestNotBefore: Date = .distantPast
    private var delegatee: Foundation.URLSession {
        get {
            if currentRequestCount < requestsPerSession {
                currentRequestCount += 1
                return currentDelegatee
            } else {
                currentDelegatee.finishTasksAndInvalidate()
                currentDelegatee = Foundation.URLSession(configuration: configuration)
                currentRequestCount = 0
                
                return currentDelegatee
            }
        }
    }
    
    init(
        configuration: URLSessionConfiguration,
        requestsPerSession: UInt = 100,
        timeIntervalBetweenRequests: TimeInterval = 0.001
    ) {
        self.configuration = configuration
        self.requestsPerSession = requestsPerSession
        self.timeIntervalBetweenRequests = timeIntervalBetweenRequests
        self.currentDelegatee = Foundation.URLSession(
            configuration: configuration
        )
    }
    
    func data(from url: URL) async throws -> (Data, URLResponse) {
        let delay: TimeInterval = nextRequestNotBefore.timeIntervalSinceNow
        if delay > 0 {
            nextRequestNotBefore = nextRequestNotBefore
                .addingTimeInterval(timeIntervalBetweenRequests)
            try await Task.sleep(
                nanoseconds: UInt64(delay * Self.nanosecondsPerSecond)
            )
        } else {
            nextRequestNotBefore = Date()
                .addingTimeInterval(timeIntervalBetweenRequests)
        }
        
        return try await delegatee.data(from: url)
    }
}
