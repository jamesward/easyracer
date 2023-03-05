Swift Combine
-------------

Swift implementation that uses [Combine](https://developer.apple.com/documentation/combine) - Apple's Function Reactive Programming framework.
As of 2022, this is an alternative approach to solving concurrency problems (in addition to async/await), especially if you are already using Combine.

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
