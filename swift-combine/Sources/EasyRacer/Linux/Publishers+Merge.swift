#if !canImport(Combine)
import OpenCombine

extension Publisher {
    func merge<P>(
        with other: P
    ) -> some Publisher<Output, Failure> where P : Publisher, Self.Failure == P.Failure, Self.Output == P.Output
    {
        [self.eraseToAnyPublisher(), other.eraseToAnyPublisher()].publisher.flatMap { $0 }
    }
    
    func merge<B, C>(
        with b: B, _ c: C
    ) -> some Publisher<Output, Failure> where B : Publisher, C : Publisher, Self.Failure == B.Failure, Self.Output == B.Output, B.Failure == C.Failure, B.Output == C.Output
    {
        [self.eraseToAnyPublisher(), b.eraseToAnyPublisher(), c.eraseToAnyPublisher()].publisher.flatMap { $0 }
    }
}

extension Publishers {
    static func MergeMany<Output, Failure>(
        _ upstreams: some Swift.Sequence<some Publisher<Output, Failure>>
    ) -> some Publisher<Output, Failure> {
        upstreams.publisher.flatMap { $0 }
    }
}
#endif
