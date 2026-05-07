Clojure + core.async
---------------------

## How to run in local

```bash
sdk env install
docker ps
clojure -M:test
```

## Docker

Prepared to test in OSX

```bash
docker compose build verify
docker compose build --no-cache verify
docker compose run --rm verify
docker compose down
```

## Interesting links

- [Requirements to implement the scenarios](../README.md)
- https://github.com/clojure/core.async
- https://github.com/babashka/http-client
- https://github.com/javahippie/clj-test-containers
