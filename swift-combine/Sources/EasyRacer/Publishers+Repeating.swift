#if canImport(Combine)
import Combine
#else
import OpenCombine
#endif
import Synchronization

extension Publishers {
    struct Repeating<Output>: Publisher where Output : Sendable {
        typealias Failure = Never
        
        private let element: Output
        
        init(_ element: Output) {
            self.element = element
        }
        
        func receive<S>(subscriber: S) where S : Subscriber & Sendable, Never == S.Failure, Output == S.Input {
            let subscription: some Subscription = RepeatingSubscription(element, subscriber)
            subscriber.receive(subscription: subscription)
        }
        
        final class RepeatingSubscription<Downstream>: Subscription, Sendable where Downstream : Subscriber & Sendable, Downstream.Input == Output, Downstream.Failure == Never {
            private let element: Output
            private let subscriber: Downstream
            private let active: Atomic<Bool> = Atomic(true)
            
            init(_ element: Output, _ subscriber: Downstream) {
                self.element = element
                self.subscriber = subscriber
            }
            
            func request(_ demand: Subscribers.Demand) {
                if active.load(ordering: .relaxed) {
                    Task {
                        if let count: Int = demand.max {
                            let nextDemand: Subscribers.Demand? = (0..<count)
                                .map { _ in subscriber.receive(element) }
                                .last
                            if let nextDemand: Subscribers.Demand = nextDemand {
                                request(nextDemand)
                            }
                        } else {
                            let nextDemand: Subscribers.Demand = subscriber.receive(element)
                            request(nextDemand.max ?? 0 > 0 ? nextDemand : demand)
                        }
                    }
                }
            }
            
            func cancel() {
                active.store(false, ordering: .relaxed)
            }
        }
    }
}
