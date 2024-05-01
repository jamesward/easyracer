# Comparing Approaches to Structured Concurrency

Adam Hearn

James Ward

---

## What is Structured Concurrency?

* Basics on Structured Concurrency (5 min)
    * loser cancellation
    * resource management
    * efficient thread utilization (i.e. reactive, non-blocking)
    * explicit timeouts
    * errors causing a race loss
---

## Easy Racer

* What is Easy Racer
    * Different impls
---

## Scenario 1

---

## Scenario 1 - Scala ZIO

@[code lang=scala transclude={16-21}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

---
## Scenario 1 - Scala Ox

@[code lang=scala transclude={19-22}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

---
## Scenario 1 - Scala Kyo

@[code lang=scala transclude={16-19}](@/../scala-kyo/src/main/scala/EasyRacerClient.scala)

---

## Scenario 1 - Java Loom

@[code lang=java transclude={35-43}](@/../java-loom/src/main/java/Main.java)

---

## Scenario 1 - Rust Tokio

@[code lang=rust transclude={19-30}](@/../rust-tokio/src/lib.rs)

---

* Scenario 1 (10 - 15 minute)
    * Talking points
        * Server scenario
            * Two concurrent requests
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


* Scenario 2
    * Talking points
        * A loser does not fail the race
* Scenario 3
    * Talking points
        * 10000 concurrent requires efficient resource utilization
        * ZIO
            * raceAll to send a Seq
        * Rust
            * No macro for you. Multiple Producer, Single Consumer Channel
* Scenario 4
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
* Scenario 5
    * Modifying the task based on the value it produces
    * Different HTTP clients handle response codes differently and some mapping of non-2xx to fail the request is sometimes necessary
* Scenario 6
    * Some use different libraries when your arity goes from 2 to 3
* Scenario 7
    * Hedging is a common use case for race
        * why & example of hedging. P99
    * Different approaches to a “delay” and like timeout, it shouldn’t block the main thread
* Scenario 8
    * Resource management - how hard is it to be sure open resources get closed with success & failures
    * Effect systems make resources management + concurrency easy
    * ZIO
        * Managed with acquireReleaseWith
        * Semantic blocking on close
            * Uninterruptible on acquire
            * .disconnect will fork the close
    * Ox
        * unsupervised & forkPlain
* Scenario 9
    * Different APIs to make reqs in parallel and return all/some results
        * Different semantics for only keeping successes
    * ZIO
        * collectAllSuccessesPar
    * 
* Scenario 10
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


* Schedulers
    * Abstractions
    * Limiting costly context switching & thread overhead
    * Expressing effects in terms of values / operations, not concerned with how the scheduler does its thing
    * Upgrades can change the impl


* Composability
* Performance
