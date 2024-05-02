## Comparing Approaches to Structured Concurrency

#### Adam Hearn
*Software Engineer @ Amazon*
<a href="https://twitter.com/adamhearn_?ref_src=twsrc%5Etfw" class="twitter-follow-button" data-size="large">@adamhearn_</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

#### James Ward
*Developer Advocate @ AWS*
<a href="https://twitter.com/_JamesWard?ref_src=twsrc%5Etfw" class="twitter-follow-button" data-size="large">@_JamesWard</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

---

## What is Structured Concurrency?

#### Hierarchical Concurrency Which Generally Supports:

* Cancellation e.g. Races (loser cancellation)
* Resource management
* Efficient thread utilization (i.e. reactive, non-blocking)
* Explicit timeouts
* Semantic Errors

---

## Easy Racer

[github.com/jamesward/easyracer](https://github.com/jamesward/easyracer)

> Ten Structured Concurrency "obstacle courses"

|                                     |                                          |                                           |
|-------------------------------------|------------------------------------------|-------------------------------------------|
| [Scala 3 + ZIO](scala-zio)          | [Kotlin + Coroutines](kotlin-coroutines) | [OCaml + Lwt + Cohttp](ocaml-cohttp-lwt)  |
| [Scala 3 + Ox](scala-ox)            | [Kotlin + Splitties](kotlin-splitties)   | [OCaml + Eio + Cohttp](ocaml-cohttp-eio)  |
| [Scala 3 + Kyo](scala-kyo)          | [Kotlin + Arrow](kotlin-arrow)           | Python (Various)                          |
| [Scala + Cats Effects 3](scala-ce3) | [Rust + Tokio](rust-tokio)               | [C#](dotnet)                              |
| [Java + Loom](java-loom)            | [Go](go-stdlib)                          | [Elm](elm-worker)                         |

<!-- todo: fix links -->

---

## Scenario 1

### Race 2 concurrent requests

<!--
* First one wins
* What is a race?
    * Do multiple things at the same time, get the first result
* Loser cancellation (but not validated in this scenario)

* Result Oriented Programming
* ZIO, Kyo, Ox
    * ZIO & Kyo - Effect Oriented
    * Direct syntax explanation
* Ox
    * non effect oriented
    * race isn’t on a datatype
    * def instead of val
    * Loom
* Kyo
    * Multiple effect types
    * val that absolves the Requests
* Rust
    * Macro for race
    * async functions
* Java
    * Scopes to define SC
        * ShutdownOnSuccess is the race
    * Direct Loom usage
        * client.send is blocking but not really
-->

---

## Scenario 1 - Scala ZIO

@[code lang=scala transclude={16-21}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 1 - Scala Ox

@[code lang=scala transclude={19-22}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 1 - Scala Kyo

@[code lang=scala transclude={16-19}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 1 - Java Loom

@[code lang=java transclude={35-43}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 1 - Rust Tokio

@[code lang=rust transclude={19-30}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Scenario 2

### Race 2 concurrent requests, where one produces a connection error

<!--
* An error loser does not win or cancel the race
-->

---

## Scenario 2 - Scala ZIO

@[code lang=scala transclude={24-29}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 2 - Scala Ox

@[code lang=scala transclude={24-27}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 2 - Scala Kyo

@[code lang=scala transclude={21-24}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 2 - Java Loom

@[code lang=java transclude={46-54}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 2 - Rust Tokio

@[code lang=rust transclude={32-43}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Scenario 3

### Race 10,000 concurrent requests

<!--
* 10000 concurrent requires efficient resource utilization
* ZIO
    * raceAll to send a Seq
* Rust
    * No macro for you. Multiple Producer, Single Consumer Channel
-->

---

## Scenario 3 - Scala ZIO

@[code lang=scala transclude={32-37}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 3 - Scala Ox

@[code lang=scala transclude={29-33}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 3 - Scala Kyo

@[code lang=scala transclude={26-30}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 3 - Java Loom

@[code lang=java transclude={57-70}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 3 - Rust Tokio

@[code lang=rust transclude={47-66}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Scenario 4

### Race 2 concurrent requests but 1 of them should have a 1 second timeout

<!--
* Talking points
    * Validating that a connection is open for 1 second, then closed
    * Timeout’d racer doesn’t fail the race
    * Timeout shouldn’t block the main thread
    * Timeout with SC is generally implemented with a race
* ZIO
    * we want the loser to fail (default .timeout returns an Option)
* Ox
    * timeout is not on a datatype
* Java
    * The timeout is a race within the request race
* Rust
    * Error types get combined
-->

---

## Scenario 4 - Scala ZIO

@[code lang=scala transclude={40-45}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 4 - Scala Ox

@[code lang=scala transclude={35-38}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 4 - Scala Kyo

@[code lang=scala transclude={32-35}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 4 - Java Loom

@[code lang=java transclude={73-90}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 4 - Rust Tokio

@[code lang=rust transclude={68-91}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Scenario 5

### Race 2 concurrent requests where a non-200 response is a loser

<!--
* Modifying the task based on the value it produces
* Different HTTP clients handle response codes differently and some mapping of non-2xx to fail the request is sometimes necessary
-->

---

## Scenario 5 - Scala ZIO

@[code lang=scala transclude={48-53}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 5 - Scala Ox

@[code lang=scala transclude={40-43}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 5 - Scala Kyo

@[code lang=scala transclude={37-40}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 5 - Java Loom

@[code lang=java transclude={93-113}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 5 - Rust Tokio

@[code lang=rust transclude={93-105}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Scenario 6

### Race 3 concurrent requests where a non-200 response is a loser

<!--
Some use different libraries when your arity goes from 2 to 3
-->

---

## Scenario 6 - Scala ZIO

@[code lang=scala transclude={56-61}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 6 - Scala Ox

@[code lang=scala transclude={45-48}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 6 - Scala Kyo

@[code lang=scala transclude={42-45}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 6 - Java Loom

@[code lang=java transclude={116-137}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 6 - Rust Tokio

@[code lang=rust transclude={107-120}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Scenario 7

### Start a request, wait at least 3 seconds then start a second request (hedging)

<!--
* Hedging is a common use case for race
* why & example of hedging. P99
* Different approaches to a “delay” and like timeout, it shouldn’t block the main thread
-->

---

## Scenario 7 - Scala ZIO

@[code lang=scala transclude={64-69}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 7 - Scala Ox

@[code lang=scala transclude={50-56}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 7 - Scala Kyo

@[code lang=scala transclude={47-51}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 7 - Java Loom

@[code lang=java transclude={140-151}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 7 - Rust Tokio

@[code lang=rust transclude={122-138}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Scenario 8

### Race 2 concurrent requests that "use" a resource which is obtained and released through other requests. The "use" request can return a non-20x request, in which case it is not a winner.

<!--
* Resource management - how hard is it to be sure open resources get closed with success & failures
* Effect systems make resources management + concurrency easy
* ZIO
    * Managed with acquireReleaseWith
    * Semantic blocking on close
        * Uninterruptible on acquire
        * .disconnect will fork the close
* Ox
    * unsupervised & forkPlain
-->

---

## Scenario 8 - Scala ZIO

@[code lang=scala transclude={72-84}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 8 - Scala Ox

@[code lang=scala transclude={58-70}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 8 - Scala Kyo

@[code lang=scala transclude={53-72}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 8 - Java Loom

@[code lang=java transclude={154-200}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 8 - Rust Tokio

@[code lang=rust transclude={140-169}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Scenario 9

### Make 10 concurrent requests where 5 return a 200 response with a letter

<!--
* Different APIs to make reqs in parallel and return all/some results
    * Different semantics for only keeping successes
* ZIO
    * collectAllSuccessesPar
-->

---

## Scenario 9 - Scala ZIO

@[code lang=scala transclude={87-103}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 9 - Scala Ox

@[code lang=scala transclude={72-82}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 9 - Scala Kyo

@[code lang=scala transclude={74-88}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 9 - Java Loom

@[code lang=java transclude={202-227}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 9 - Rust Tokio

@[code lang=rust transclude={171-211}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Scenario 10

This scenario validates that a computationally heavy task can be run in parallel to another task, and then cancelled.

**Part 1)** Make a request and while the connection is open, perform something computationally heavy (e.g. repeated SHA calculation), then cancel the task when the connection closes
**Part 2)** In parallel to **Part 1**, every 1 second, make a request with the current process load (0 to 1)

The request in **Part 2** will respond with a 20x response if it looks like **Part 1** was done correctly (in which case you can stop sending load values), otherwise it will respond with a 30x response if you should continue sending values, or with a 40x response if something has gone wrong.

<!--
* Cancellation of blocking operations
    * Is it possible? How?
    * ZIO
        * attemptBlockingInterrupt
    * Java & Ox
        * Thread.interrupted
    * Rust
        * Cancellation token
* Recursion of async stuff
    * Stack safe?
    * Java
        * Yuck
    * Rust
        * #[async_recursion] ?
-->

---

## Scenario 10 - Scala ZIO

@[code lang=scala transclude={106-135}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 10 - Scala Ox

@[code lang=scala transclude={84-115}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---
## Scenario 10 - Scala Kyo

@[code lang=scala transclude={90-134}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 10 - Java Loom

@[code lang=java transclude={229-290}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 10 - Rust Tokio

@[code lang=rust transclude={213-278}](@/../rust-tokio/src/lib.rs)

<!--

-->

---

## Schedulers

* Abstractions
* Limiting costly context switching & thread overhead
* Expressing effects in terms of values / operations, not concerned with how the scheduler does its thing
* Upgrades can change the impl

---

## Composability


---

## Performance

