Swift Grand Central Dispatch (Legacy)
-------------------------------------

Swift implementation that uses only Swift's [Grand Central Dispatch](https://developer.apple.com/documentation/DISPATCH) framework.
Before the introduction of `async`/`await`, this was the recommended way of doing concurrency in Swift,
and is still the only option for running Swift on Linux, Windows or MacOS 10.14 or earlier.

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
