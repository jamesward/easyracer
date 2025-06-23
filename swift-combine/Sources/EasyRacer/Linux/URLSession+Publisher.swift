#if canImport(FoundationNetworking)
import Foundation
import FoundationNetworking
@preconcurrency import OpenCombine

/// Make sure the URLSession protocol isn't defining incompatible methods
extension FoundationURLSession {
    public typealias DataTaskPublisher = PassthroughSubject<(data: Data, response: URLResponse), URLError>
    
    public func dataTaskPublisher(for url: URL) -> DataTaskPublisher {
        let dataTaskPublisher: DataTaskPublisher = PassthroughSubject()
        dataTask(with: url) { data, urlResponse, error in
            if let error = error {
                if let urlError = error as? URLError {
                    dataTaskPublisher.send(completion: .failure(urlError))
                } else {
                    fatalError()
                }
            }
            
            if let data = data, let urlResponse = urlResponse {
                dataTaskPublisher.send((data: data, response: urlResponse))
            }
        }.resume()
        
        return dataTaskPublisher
    }
}
#endif
