import Combine
import Foundation

/// URLSession operations we actually use in Easy Racer
protocol URLSession {
    associatedtype DataTaskPublisher: Publisher<(data: Data, response: URLResponse), URLError>
    
    func dataTaskPublisher(for url: URL) -> DataTaskPublisher
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
class ScalableURLSession: URLSession {
    private let configuration: URLSessionConfiguration
    private let requestsPerSession: UInt
    private let timeIntervalBetweenRequests: TimeInterval
    private let syncQueue: DispatchQueue = .init(
        label: "urlsession-serialize", attributes: .concurrent
    )
    private let delayQueue: DispatchQueue = .init(label: "urlsession-delay")
    
    private var currentDelegatee: Foundation.URLSession
    private var currentRequestCount: UInt = 0
    private var nextRequestNotBefore: Date = .distantPast
    private var delegatee: Foundation.URLSession {
        get {
            if currentRequestCount < requestsPerSession {
                currentRequestCount += 1
                return currentDelegatee
            } else {
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
    
    func dataTaskPublisher(for url: URL) -> some Publisher<
        (data: Data, response: URLResponse), URLError
    > {
        syncQueue.sync(flags: .barrier) {
            let delay: TimeInterval = nextRequestNotBefore.timeIntervalSinceNow
            let requestTime: Date = delay > 0 ? nextRequestNotBefore : Date()
            nextRequestNotBefore = requestTime
                .addingTimeInterval(timeIntervalBetweenRequests)
            
            return Just(())
                .delay(for: .seconds(max(0, delay)), scheduler: delayQueue)
                .flatMap { _ in
                    self.delegatee.dataTaskPublisher(for: url)
                }
        }
    }
}
