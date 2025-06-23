import Foundation

// getrusage accepts different parameter type on Darwin vs Linux
#if !canImport(Darwin)
public func getrusage(_ who: __rusage_who, _ usage: UnsafeMutablePointer<rusage>!) -> Int32 {
    Foundation.getrusage(who.rawValue, usage)
}
#endif

