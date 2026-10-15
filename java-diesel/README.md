# Easy Racer --- Java Diesel

A Java implementation of the [Easy
Racer](https://github.com/tonivade/easyracer) concurrency scenarios
using [Diesel](https://github.com/tonivade/diesel), a Java library for
composing and interpreting programs with structured-concurrency-style
operations.

Easy Racer exercises concurrent request racing, loser cancellation,
resource management, hedged requests, error handling, and CPU-bound work
cancellation. This module implements the scenarios against the Easy
Racer HTTP server.

## Requirements

-   **JDK 25**
-   **Docker**, for the integration-test container
-   The Gradle wrapper included in this directory

The build resolves Diesel from the Maven repositories configured in
`build.gradle`. The current `gradle.properties` uses the `0.12-SNAPSHOT`
Diesel version, so access to the configured snapshot repository may be
required.

## Project layout

``` text
java-diesel/
├── build.gradle
├── gradle.properties
├── gradlew
├── gradlew.bat
└── src/
    ├── main/java/es/tonivade/racer/
    │   ├── Http.java
    │   ├── HttpImpl.java
    │   └── Scenarios.java
    └── test/java/es/tonivade/racer/
        └── ScenariosTest.java
```

-   **`Http`** declares the HTTP effect used by Diesel programs. The
    `@Diesel` annotation is processed to generate the DSL used by the
    scenarios.
-   **`HttpImpl`** interprets that effect using Java's `HttpClient` and
    `sendAsync`. Redirects are not followed automatically, and
    connection establishment has a five-second timeout. Individual
    requests do not have a global request timeout.
-   **`Scenarios`** defines the eleven scenario programs using Diesel
    combinators. It owns a virtual-thread-per-task executor and
    implements `AutoCloseable`.
-   **`ScenariosTest`** runs the scenarios as integration tests against
    the official Easy Racer container.

## Implementation approach

The scenarios are expressed as `Program` values rather than as separate
blocking workflows. Diesel combinators describe how those programs are
composed and run.

The implementation uses:

-   **`parAny`** to race concurrent programs and return the first
    successful result, cancelling losing work according to Diesel's
    concurrency semantics.
-   **`parZip`** to run two programs concurrently and combine their
    results.
-   **`parSequence`** to execute a collection of programs and collect
    their results.
-   **`delayed`** to introduce a delay before starting work, as required
    by the hedging scenario.
-   **`bracket`** to acquire a resource, use it, and release it.
-   **`recursive`** and **`catchAll`** to implement the polling loop and
    handle unsuccessful requests.
-   **`interruptibleTask`** for the CPU-intensive SHA-512 loop in
    scenario 10, which must stop when it loses the race.

A virtual-thread-per-task executor is shared by the scenarios.
`Scenarios.close()` closes the executor after use.

### Scenario-specific details

-   **Scenario 3** races 10,000 requests.
-   **Scenario 4** applies a one-second timeout to the designated
    request, rather than imposing a timeout on every HTTP request.
-   **Scenario 7** starts a second request after a three-second delay.
-   **Scenario 8** uses `bracket` to associate resource release with the
    resource's use.
-   **Scenario 9** records a timestamp with each response and assembles
    the response values in completion-time order.
-   **Scenario 10** races the initial HTTP request against the
    interruptible CPU task. In parallel, it polls the server with the
    current process CPU load until the server reports success or a
    failure occurs.
-   **Scenario 11** races one request against a nested race of two
    requests.

## Run the tests

Run these commands from the `java-diesel` directory.

Run the full integration-test suite:

``` bash
./gradlew test
```

On Windows, use:

``` bat
gradlew.bat test
```

The tests use Testcontainers to start `ghcr.io/jamesward/easyracer`
automatically. Docker must be running and able to pull and run that
image.

Each scenario test has a 30-second JUnit timeout. Scenario 4 also has
its own one-second timeout on the designated request.

## Generate the coverage report

Run:

``` bash
./gradlew test jacocoTestReport
```

The JaCoCo HTML report is generated at:

``` text
build/reports/jacoco/test/html/index.html
```

The XML report is generated at:

``` text
build/reports/jacoco/report.xml
```

## Run against a local Easy Racer server

The integration tests start their own server container. If you want to
start the server manually for development, run this from a terminal with
Docker:

``` bash
docker run --rm -it -p 8080:8080 ghcr.io/jamesward/easyracer --debug
```

The server will be available at `http://localhost:8080`.

The `Scenarios` class accepts a base URI in its constructor, so it can
be configured to target a local server:

``` java
var scenarios = new Scenarios(URI.create("http://localhost:8080"));
try {
  var result = scenarios.scenario1().eval(new HttpImpl());
  System.out.println(result);
} finally {
  scenarios.close();
}
```

## Notes

-   These are integration tests: they exercise the actual Easy Racer
    server rather than mocking its responses.
-   The HTTP client disables automatic redirect following because some
    scenarios use redirect status codes as part of their protocol.
-   Request timeouts should be applied where a scenario requires them; a
    global per-request timeout can interfere with scenarios that
    intentionally keep a request open.