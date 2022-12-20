Easy Racer
----------

This project explores how easy it is to build programs which can "race" two or more concurrent computations while providing:
 - loser cancellation
 - resource management
 - efficient thread utilization (i.e. reactive, non-blocking)
 - explicit timeouts
 - errors causing a race loss

A scenario server validates the implementations of 6 scenarios:

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

6. Start a request, wait at least 3 seconds then start a second request (hedging)
    ```
    GET /6
    ```

7. Race 2 concurrent requests that "use" a resource which is obtained and released through other requests. The "use" request can return a non-20x request, in which case it is not a winner.
    ```
    GET /7?open
    GET /7?use=<id obtained from open request>
    GET /7?close=<id obtained from open request>
    ```

If your implementation is correct, each race will result in a 200 response with a body:
```
right
```


The scenario server has a public container `ghcr.io/jamesward/easyracer` and if you contribute your client to this repo, use Testcontainers and include automated integration tests.

## Clients
| Name | Source | Tests |
| ---- | ------ | ----- |
| Scala 3 + ZIO | [scala-zio](scala-zio) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/scala-zio.yaml/badge.svg) |
| Kotlin + Splitties | [kotlin-splitties](kotlin-splitties) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/kotlin-splitties.yaml/badge.svg) |
| Python + AIOHTTP | [python-aiohttp](python-aiohttp) | ![tests](https://github.com/jamesward/easyracer/actions/workflows/python-aiohttp.yaml/badge.svg) |
