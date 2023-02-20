Swift Async
-----------

Swift implementation that uses only Swift's basic concurrency constructs (`async`/`await`, TaskGroups)

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
