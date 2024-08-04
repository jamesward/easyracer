Go conc
-------

Go implementation that uses [conc](https://github.com/sourcegraph/conc) - a library that promises "better structured concurrency for go".

It is worth noting that while conc is an improvement over plain vanilla Go, it can still be improved upon (some of that improvement, you can find in `internal/conc`), and at the end of the day, it does nothing to address the fact that Go is still Go.

Run the tests (using a Testcontainers scenario server):
```
go test ./...
```

Run against a manually started scenario server:
```
go run main.go
```