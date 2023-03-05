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

2. Race 2 concurrent requests, where one produces a connection error
    ```
    GET /2
    ```

3. Race 10,000 concurrent requests
    ```
    GET /3
    ```

4. Race 2 concurrent requests but 1 of them should have a 1 second timeout
    ```
    GET /4
    ```

5. Race 2 concurrent requests where the winner is a 20x response
    ```
    GET /5
    ```

6. Race 3 concurrent requests where the winner is a 20x response
    ```
    GET /6
    ```

7. Start a request, wait at least 3 seconds then start a second request (hedging)
    ```
    GET /7
    ```

8. Race 2 concurrent requests that "use" a resource which is obtained and released through other requests. The "use" request can return a non-20x request, in which case it is not a winner.
    ```
    GET /8?open
    GET /8?use=<id obtained from open request>
    GET /8?close=<id obtained from open request>
    ```

9. Make 10 concurrent requests where 5 return a 200 response with a letter, when assembled in order of when they responded, form the "right" answer
    ```
    GET /9
    ```

10. Make a request, get the integer returned in the response body, calculate the Fibonacci and make a second request with the result, get the integer returned in the response body, calculate the Fibonacci for this second number, make a thir request with the second Fibonacci to get the "right" answer. (Note: Not really structured concurrency, but is a easy precursor to Scenario 11.)
    ```
    GET /10
    GET /10?<num>=<fib>
    GET /10?<num>=<fib>
    ```

11. Make a request, get two numbers comma-separated.  Calculate the Fibonacci for each in parallel, send then in a second request to get the "right" answer.
    ```
    GET /11
    GET /11?<num1>=<fib1>&<num2>=<fib2>
    ```

If your implementation is correct, each race will result in a 200 response with a body:
```
right
```


The scenario server has a public container `ghcr.io/jamesward/easyracer` and if you contribute your client to this repo, use Testcontainers and include automated integration tests.

For local dev you can spin up the server via Docker:
```
docker run -it -p8080:8080 ghcr.io/jamesward/easyracer --debug
```

## Clients
| Name | Source | Tests | Author | Notes |
| ---- | ------ | ----- | ------ | ----- |
| Scala 3 + ZIO | [scala-zio](scala-zio) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-zio.yaml/badge.svg) | [James Ward](https://github.com/jamesward) | Using an unreleased zio-http branch waiting on [zio-http#1994](https://github.com/zio/zio-http/pull/1994) |
| Scala 3 + ox | [scala-ox](scala-ox) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-ox.yaml/badge.svg) | [Adam Warski](https://github.com/adamw) | |
| Kotlin + Splitties | [kotlin-splitties](kotlin-splitties) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/kotlin-splitties.yaml/badge.svg) | [James Ward](https://github.com/jamesward) | |
| Kotlin + Arrow | [kotlin-arrow](kotlin-arrow) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/kotlin-arrow.yaml/badge.svg) | [James Ward](https://github.com/jamesward) | |
| Java + Loom | [java-loom](java-loom) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/java-loom.yaml/badge.svg) | [James Ward](https://github.com/jamesward) | |
| Python + AIOHTTP | [python-aiohttp](python-aiohttp) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/python-aiohttp.yaml/badge.svg) | [James Ward](https://github.com/jamesward) | Unable to get it working |
| Python + HTTPX + asyncio | [python-httpx-asyncio](python-httpx-asyncio) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/python-httpx-asyncio.yaml/badge.svg) | [James Ward](https://github.com/jamesward) | Unable to get it working |
| Python + HTTPX + Trio | [python-httpx-trio](python-httpx-trio) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/python-httpx-trio.yaml/badge.svg) | [James Ward](https://github.com/jamesward) | Unable to get it working |
| Go | [go-stdlib](go-stdlib) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/go-stdlib.yaml/badge.svg) | [Jack Leow](https://github.com/jackgene) | |
| Swift + Grand Central Dispatch | [swift-dispatch](swift-dispatch) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/swift-dispatch.yaml/badge.svg) | [Jack Leow](https://github.com/jackgene) | |
| Swift + async/await | [swift-async](swift-async) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/swift-async.yaml/badge.svg) | [Jack Leow](https://github.com/jackgene) | |
