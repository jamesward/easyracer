Swift Async/Await
-----------------

Swift implementation that uses only Swift's native [concurrency](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/) constructs (`async`/`await`, TaskGroups).
As of 2022, this is the recommended way of doing concurrency in Swift, if you are not using any libraries.

Swift's HTTP/concurrency model seems to rely on file descriptors under the hood, in order for scenario 3 to succeed,
the file descriptors limit must be increased:
```
ulimit -n 10000
``` 

Run the tests (manages scenario server in Docker):
```
swift test -c release
```

Run against a manually started scenario server:
```
swift run -c release
```
