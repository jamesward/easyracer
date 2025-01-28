Easy Racer
----------

A series of obstactle courses as a way to compare how different languages and frameworks handle structured concurrency, including:
 - loser cancellation
 - resource management
 - efficient thread utilization (i.e. reactive, non-blocking)
 - explicit timeouts
 - errors causing a race loss

A scenario server validates the implementations of 11 scenarios:

1. Race 2 concurrent requests
    ```
    GET /1
    ```
    The winner returns a 200 response with a body containing `right`

2. Race 2 concurrent requests, where one produces a connection error
    ```
    GET /2
    ```
    The winner returns a 200 response with a body containing `right`

3. Race 10,000 concurrent requests
    ```
    GET /3
    ```
    The winner returns a 200 response with a body containing `right`

4. Race 2 concurrent requests but 1 of them should have a 1 second timeout
    ```
    GET /4
    ```
    The winner returns a 200 response with a body containing `right`

5. Race 2 concurrent requests where a non-200 response is a loser
    ```
    GET /5
    ```
    The winner returns a 200 response with a body containing `right`

6. Race 3 concurrent requests where a non-200 response is a loser
    ```
    GET /6
    ```
    The winner returns a 200 response with a body containing `right`

7. Start a request, wait at least 3 seconds then start a second request (hedging)
    ```
    GET /7
    ```
    The winner returns a 200 response with a body containing `right`

8. Race 2 concurrent requests that "use" a resource which is obtained and released through other requests. The "use" request can return a non-20x request, in which case it is not a winner.
    ```
    GET /8?open
    GET /8?use=<id obtained from open request>
    GET /8?close=<id obtained from open request>
    ```
    The winner returns a 200 response with a body containing `right`

9. Make 10 concurrent requests where 5 return a 200 response with a letter
    ```
    GET /9
    ```
    When assembled in order of when they responded, form the "right" answer

10. This scenario validates that a computationally heavy task can be run in parallel to another task, and then cancelled.

    **Part 1)** Make a request and while the connection is open, perform something computationally heavy (e.g. repeated SHA calculation), then cancel the task when the connection closes
    ```
    GET /10?{some_id}
    ```
    
    **Part 2)** In parallel to **Part 1**, every 1 second, make a request with the current process load (0 to 1)
    ```
    GET /10?{same_id_as_part_1}={load}
    ```

    The request in **Part 2** will respond with a 20x response if it looks like **Part 1** was done correctly (in which case you can stop sending load values), otherwise it will respond with a 30x response if you should continue sending values, or with a 40x response if something has gone wrong.

11. This scenario validates that a race where all racers fail, is handled correctly.  Race a request with another race of 2 requests.
    ```
    GET /11
    ```
    The winner returns a 200 response with a body containing `right`

The scenario server has a public container `ghcr.io/jamesward/easyracer` and if you contribute your client to this repo, use Testcontainers and include automated integration tests.

For local dev you can spin up the server via Docker:
```
docker run -it -p8080:8080 ghcr.io/jamesward/easyracer --debug
```

## Clients
| Source                                                   | Scenario Coverage                                                                                                | Author(s)                                                                                                                                  | Notes                                 |
|----------------------------------------------------------|------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|---------------------------------------|
| [Scala 3 + ZIO](scala-zio)                               | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-zio.yaml/badge.svg)                | [James Ward](https://github.com/jamesward)                                                                                                 |                                       |
| [Scala 3 + Ox](scala-ox)                                 | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-ox.yaml/badge.svg)                 | [Adam Warski](https://github.com/adamw)                                                                                                    |                                       |
| [Scala 3 + Kyo](scala-kyo)                               | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-kyo.yaml/badge.svg)                | [James Ward](https://github.com/jamesward)                                                                                                 |                                       |
| [Scala 3 + Gears](scala-gears)                           | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-gears.yaml/badge.svg)              | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Scala 3 + Akka Streams](scala-akkastreams)              | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-akkastreams.yaml/badge.svg)        | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Scala + Cats Effects 3](scala-ce3)                      | 9/11  ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-ce3.yaml/badge.svg)                | [Paul Snively](https://github.com/paul-snively)                                                                                            | Needs Scenario 10, 11                 |
| [Scala 3 + Soundness](scala-akkastreams)                 | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-soundness.yaml/badge.svg)          | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Kotlin + Coroutines](kotlin-coroutines)                 | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/kotlin-coroutines.yaml/badge.svg)        | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Kotlin + Flow](kotlin-flow)                             | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/kotlin-flow.yaml/badge.svg)              | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Kotlin + Splitties](kotlin-splitties)                   | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/kotlin-splitties.yaml/badge.svg)         | [James Ward](https://github.com/jamesward)                                                                                                 |                                       |
| [Kotlin + Arrow](kotlin-arrow)                           | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/kotlin-arrow.yaml/badge.svg)             | [James Ward](https://github.com/jamesward)                                                                                                 |                                       |
| [Java + Loom](java-loom)                                 | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/java-loom.yaml/badge.svg)                | [James Ward](https://github.com/jamesward)                                                                                                 |                                       |
| [Java + Jox](java-jox)                                   | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/java-jox.yaml/badge.svg)                 | [James Ward](https://github.com/jamesward)                                                                                                 |                                       |
| [Java + CompletableFuture](java-cf)                      | 9/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/java-cf.yaml/badge.svg)                   | [James Ward](https://github.com/jabrena)                                                                                                   | Needs Scenario 4, 10                  |
| [Rust + Tokio](rust-tokio)                               | 10/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/rust-tokio.yaml/badge.svg)               | [James Ward](https://github.com/jamesward) and Rust Developer Retreat Participants                                                         | Needs Scenario 11                     |
| [C#](dotnet)                                             | 9/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/dotnet.yaml/badge.svg)                    | [Jason De Lorme](https://github.com/delormej)                                                                                              | Scenario 10: CPU Not Pegged, Needs 11 |
| [F# + Reactive Extensions](fsharp-reactive)              | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/fsharp-reactive.yaml/badge.svg)          | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [OCaml + Lwt + Cohttp](ocaml-cohttp-lwt)                 | 10/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/ocaml-cohttp-lwt.yaml/badge.svg)         | [Puneeth Chaganti](https://github.com/punchagan)                                                                                           | Needs Scenario 11                     |
| [OCaml + Eio + Cohttp](ocaml-cohttp-eio)                 | 10/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/ocaml-cohttp-eio.yaml/badge.svg)         | [Puneeth Chaganti](https://github.com/punchagan)                                                                                           | Needs Scenario 11                     |
| [Python + AIOHTTP + TaskGroup](python-aiohttp-taskgroup) | 9/11  ![tests](https://github.com/jamesward/easyracer/actions/workflows/python-aiohttp-taskgroup.yaml/badge.svg) | [James Ward](https://github.com/jamesward) [Bruce Eckel](https://github.com/BruceEckel)                                                    | Needs Scenario 10, 11                 |
| [Python + RxPy](python-rxpy)                             | 11/11  ![tests](https://github.com/jamesward/easyracer/actions/workflows/python-rxpy.yaml/badge.svg)             | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Go](go-stdlib)                                          | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/go-stdlib.yaml/badge.svg)                | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Go conc](go-conc)                                       | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/go-conc.yaml/badge.svg)                  | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Swift + Grand Central Dispatch](swift-dispatch)         | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/swift-dispatch.yaml/badge.svg)           | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Swift + async/await](swift-async)                       | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/swift-async.yaml/badge.svg)              | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Swift + Combine](swift-combine)                         | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/swift-combine.yaml/badge.svg)            | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [Elm](elm-worker)                                        | 11/11 ![tests](https://github.com/jamesward/easyracer/actions/workflows/elm-worker.yaml/badge.svg)               | [Jack Leow](https://github.com/jackgene)                                                                                                   |                                       |
| [JavaScript](javascript-stdlib)                          | 9/11  ![tests](https://github.com/jamesward/easyracer/actions/workflows/javascript-stdlib.yaml/badge.svg)        | [James Ward](https://github.com/jamesward)                                                                                                 | Needs Scenario 10, 11                 |
| [TypeScript + Effect](typescript-effect)                 | 8/11  ![tests](https://github.com/jamesward/easyracer/actions/workflows/javascript-stdlib.yaml/badge.svg)        | [James Ward](https://github.com/jamesward) [Michael Arnaldi](https://github.com/mikearnaldi) [Sebastian Lorenz](https://github.com/fubhy)  | Needs Scenario 9, 10, 11              |
| [Python + HTTPX + Trio](python-httpx-trio)               | 8/11  ![tests](https://github.com/jamesward/easyracer/actions/workflows/python-httpx-trio.yaml/badge.svg)        | [James Ward](https://github.com/jamesward)                                                                                                 | Needs Scenarios 3, 10, 11             |
| [Python + AIOHTTP](python-aiohttp)                       | 6/11  ![tests](https://github.com/jamesward/easyracer/actions/workflows/python-aiohttp.yaml/badge.svg)           | [James Ward](https://github.com/jamesward)                                                                                                 | Needs Scenarios 3, 4, 8, 10, 11       |
| [Python + HTTPX + asyncio](python-httpx-asyncio)         | 2/11  ![tests](https://github.com/jamesward/easyracer/actions/workflows/python-httpx-asyncio.yaml/badge.svg)     | [James Ward](https://github.com/jamesward)                                                                                                 | Needs Scenarios 3-11                  |
