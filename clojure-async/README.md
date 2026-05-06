Clojure + core.async
---------------------

This folder includes a Clojure implementation of the [EasyRacer](https://github.com/jamesward/easyracer) scenarios under `info.jab.easyracer.scenarios`, built on:

- [`babashka.http-client`](https://github.com/babashka/http-client) — Clojure HTTP client used by the EasyRacer scenario implementation.
- [`org.clojure/core.async`](https://github.com/clojure/core.async) — channels and `alts!` for racing concurrent requests.
- [`clj-test-containers`](https://github.com/javahippie/clj-test-containers) — boots `ghcr.io/jamesward/easyracer` for integration tests.

Sources live under `src/main/clojure/`. Tests live under `src/test/clojure/`.

## Prerequisites (Java 21)

Use **JDK 21** for this project.

1. **Use SDKMAN** with the checked-in `.sdkmanrc`:

   ```bash
   sdk env install
   sdk env
   java -version
   ```

   This repo pins `java=21.0.2-graalce`, and `java -version` should show a Java 21 runtime.

2. **Clojure CLI** (`clojure` on your `PATH`) is required for local runs and test commands. Install the CLI from the [official getting started guide](https://clojure.org/guides/install_clojure).

```bash
brew install clojure/tools/clojure
```

## EasyRacer

The project currently verifies EasyRacer scenarios through the integration test suite (no standalone CLI entrypoint in `info.jab.easyracer.scenarios`).

### Tests (Testcontainers)

Docker must be running; the test fixture starts/stops the server automatically.

```bash
clojure -M:test
```

#### Run tests locally

Local test run checklist:

```bash
# 1) Ensure Docker is available
docker ps

# 2) Run the default integration suite locally
clojure -M:test
```

Test timeouts (analogue of Surefire's `forkedProcessTimeoutInSeconds` and JUnit `@Timeout`):

- Per-test cap, set via metadata: `^{:timeout-ms 30000}` on each `deftest`. Default 60 s.
- Run-wide cap, hard-exits the JVM if exceeded:

  ```bash
  clojure -J-Deasyracer.run.timeout.ms=300000 -M:test
  ```

## Docker

Prepared to test in OSX:

```bash
docker compose build verify
docker compose build --no-cache verify
docker compose run --rm verify
docker compose down
```
