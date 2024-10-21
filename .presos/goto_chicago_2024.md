## Structured Concurrency: Managing the Hierarchical Cancellation and Error Handling

#### James Ward
*DX for Q Developer @ AWS*
<a href="https://twitter.com/_JamesWard" class="twitter-follow-button" data-size="large">@_JamesWard</a><script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

---

## What is Structured Concurrency?

<!--
Why concurrency is / has been hard (shared mutable state, mutexes, etc)
Hierarchical Concurrency (diagram)
-->

<pre class="mermaid">
graph TD
    A[Main Task] --> B[Parent Scope]
    B --> C[Child Task 1]
    B --> D[Child Task 2]
    B --> E[Child Task 3]
    C --> F[Complete]
    D --> G[Complete]
    E --> H[Complete]
    F --> I[All Child Tasks Complete]
    G --> I
    H --> I
    I --> J[Parent Scope Completes]
    J --> K[Main Task Continues]

    style A fill:#f9f,stroke:#333,stroke-width:2px
    style B fill:#ccf,stroke:#333,stroke-width:2px
    style C fill:#cfc,stroke:#333,stroke-width:2px
    style D fill:#cfc,stroke:#333,stroke-width:2px
    style E fill:#cfc,stroke:#333,stroke-width:2px
    style J fill:#ccf,stroke:#333,stroke-width:2px
    style K fill:#f9f,stroke:#333,stroke-width:2px
</pre>

---

## Races

<pre class="mermaid">
graph TD
    A[Main Task] --> B[Parent Scope]
    B --> C[Child Task 1]
    B --> D[Child Task 2]
    B --> E[Child Task 3]
    C --> F[Complete]
    D --> G[Cancelled]
    E --> H[Cancelled]
    F --> I[Result of Child Task 1]
    I --> J[Parent Scope Completes]
    J --> K[Main Task Continues]

    style A fill:#f9f,stroke:#333,stroke-width:2px
    style B fill:#ccf,stroke:#333,stroke-width:2px
    style C fill:#cfc,stroke:#333,stroke-width:2px
    style D fill:#cfc,stroke:#333,stroke-width:2px
    style E fill:#cfc,stroke:#333,stroke-width:2px
    style J fill:#ccf,stroke:#333,stroke-width:2px
    style K fill:#f9f,stroke:#333,stroke-width:2px
</pre>

---

## Easy Racer

[github.com/jamesward/easyracer](https://github.com/jamesward/easyracer)

> Ten Structured Concurrency "obstacle courses"

|                                                                                      |                                                                                           |                                                                                           |
|--------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------|
| [Scala 3 + ZIO](https://github.com/jamesward/easyracer/tree/main/scala-zio)          | [Kotlin + Coroutines](https://github.com/jamesward/easyracer/tree/main/kotlin-coroutines) | [OCaml + Lwt + Cohttp](https://github.com/jamesward/easyracer/tree/main/ocaml-cohttp-lwt) |
| [Scala 3 + Ox](https://github.com/jamesward/easyracer/tree/main/scala-ox)            | [Kotlin + Splitties](https://github.com/jamesward/easyracer/tree/main/kotlin-splitties)   | [OCaml + Eio + Cohttp](https://github.com/jamesward/easyracer/tree/main/ocaml-cohttp-eio) |
| [Scala 3 + Kyo](https://github.com/jamesward/easyracer/tree/main/scala-kyo)          | [Kotlin + Arrow](https://github.com/jamesward/easyracer/tree/main/kotlin-arrow)           | Python (Various)                                                                          |
| [Scala + Cats Effects 3](https://github.com/jamesward/easyracer/tree/main/scala-ce3) | [Rust + Tokio](https://github.com/jamesward/easyracer/tree/main/rust-tokio)               | [C#](https://github.com/jamesward/easyracer/tree/main/dotnet)                             |
| [Java + Loom](https://github.com/jamesward/easyracer/tree/main/java-loom)            | [Go](https://github.com/jamesward/easyracer/tree/main/go-stdlib)                          | [Elm](https://github.com/jamesward/easyracer/tree/main/elm-worker)                        |

---

## Approaches to Structured Concurrency

* Scoped Driven
  - Java Loom (JEP 453)
* Direct Style (Imperative / Monad free!)
  - Jox & Scala Ox
    - Built on Loom, JDK21+ only
  - Rust (Future based syntax)
* Effect Oriented
  - Scala ZIO
    - Monadic Effect
  - Scala Kyo
    - Algebraic Effects / single monad

---

## Scenario 1

### Race 2 concurrent requests

<!--
* First one wins
* What is a race?
    * Do multiple things at the same time, get the first result
* Loser cancellation (but not validated in this scenario)
    * Cancellation means stopping and cleaning up

* Java
    * Scopes to define SC
        * ShutdownOnSuccess is the race
    * Direct Loom usage
        * client.send is blocking but not really
* Kotlin
    * Also Scope Based
    * But explicit cancellation of loser
-->

---

## Scenario 1 - Scala Ox

@[code lang=scala transclude={19-22}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--
Higher level abstraction on Loom
No special datatype or syntax
-->

---

## Scenario 1 - Java Jox

@[code lang=java transclude={35-41}](@/../java-jox/src/main/java/Main.java)

<!--
Higher level abstraction on Loom
No special datatype or syntax
-->

---

## Scenario 1 - Java Loom

@[code lang=java transclude={35-43}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---
## Scenario 1 - Kotlin Coroutines

@[code lang=kotlin transclude={23-32}](@/../kotlin-coroutines/src/main/kotlin/Main.kt)

<!--

-->

---
## Scenario 1 - Scala ZIO

@[code lang=scala transclude={16-21}](@/../scala-zio/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 2

### Race 2 concurrent requests, where one produces a connection error

<!--
* An error loser does not win or cancel the race
-->

---

## Race Shutdown on Success

<pre class="mermaid">
graph TD
    A[Main Task] --> B[Parent Scope]
    B --> C[Child Task 1]
    B --> D[Child Task 2]
    B --> E[Child Task 3]
    C --> F[Complete]
    D --> G[Error]
    E --> H[Cancelled]
    F --> I[Result of Child Task 1]
    I --> J[Parent Scope Completes]
    J --> K[Main Task Continues]

    style A fill:#f9f,stroke:#333,stroke-width:2px
    style B fill:#ccf,stroke:#333,stroke-width:2px
    style C fill:#cfc,stroke:#333,stroke-width:2px
    style D fill:#cfc,stroke:#333,stroke-width:2px
    style E fill:#cfc,stroke:#333,stroke-width:2px
    style J fill:#ccf,stroke:#333,stroke-width:2px
    style K fill:#f9f,stroke:#333,stroke-width:2px
</pre>

---

## Race Shutdown on Error

<pre class="mermaid">
graph TD
    A[Main Task] --> B[Parent Scope]
    B --> C[Child Task 1]
    B --> D[Child Task 2]
    B --> E[Child Task 3]
    C --> F[Error]
    D --> G[Cancelled]
    E --> H[Cancelled]
    F --> I[Error of Child Task 1]
    I --> J[Parent Scope Completes]
    J --> K[Main Task Continues]

    style A fill:#f9f,stroke:#333,stroke-width:2px
    style B fill:#ccf,stroke:#333,stroke-width:2px
    style C fill:#cfc,stroke:#333,stroke-width:2px
    style D fill:#cfc,stroke:#333,stroke-width:2px
    style E fill:#cfc,stroke:#333,stroke-width:2px
    style J fill:#ccf,stroke:#333,stroke-width:2px
    style K fill:#f9f,stroke:#333,stroke-width:2px
</pre>

---

## Scenario 2 - Java Loom

@[code lang=java transclude={46-54}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 2 - Kotlin Splitties

@[code lang=kotlin transclude={38-51}](@/../kotlin-splitties/src/main/kotlin/Main.kt)

---

## Scenario 3

### Race 10,000 concurrent requests

<!--
* 10000 concurrent requires efficient resource utilization
-->

---

## Scenario 3 - Java Loom

@[code lang=java transclude={57-70}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 3 - Java Jox

@[code lang=java transclude={53-60}](@/../java-jox/src/main/java/Main.java)

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
* Java
    * The timeout is a race within the request race
-->

---

## Scenario 4 - Java Loom

@[code lang=java transclude={73-90}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 4 - Java Jox

@[code lang=java transclude={63-69}](@/../java-jox/src/main/java/Main.java)

<!--

-->

---

## Scenario 7

### Hedging: Start a request, wait at least 3 seconds then start a second request (hedging), and race the two

<!--

-->

---

## Scenario 7 - Java Loom

@[code lang=java transclude={140-151}](@/../java-loom/src/main/java/Main.java)

---

## Scenario 8

### Race 2 concurrent requests that "use" a resource which is obtained and released through other requests. The "use" request can return a non-20x request, in which case it is not a winner.

<!--
* Resource management - how hard is it to be sure open resources get closed with success & failures
* Effect systems make resources management + concurrency easy
* Java
    * ???
* Ox
    * unsupervised & forkPlain
-->

---
## Scenario 8 - Scala Ox

@[code lang=scala transclude={58-69}](@/../scala-ox/src/main/scala/EasyRacerClient.scala)

<!--

-->

---

## Scenario 8 - Java Loom

@[code lang=java transclude={155-182}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 8 - Java Loom

@[code lang=java transclude={184-199}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 9

### Make 10 concurrent requests where 5 return a 200 response with a letter. Assemble the letters in the order they were received.

<!--

-->

---

## Scenario 9 - Java Loom

@[code lang=java transclude={202-227}](@/../java-loom/src/main/java/Main.java)

<!--

-->

---

## Scenario 9 - Kotlin Coroutines

@[code lang=kotlin transclude={163-175}](@/../kotlin-coroutines/src/main/kotlin/Main.kt)

<!--

-->

---

## So what?

- Concurrency is hard
- Higher level abstractions are helpful
- Effects: Most flexibility (Race anything, Resource Management, Standard Error Handling, Abstraction over Scheduler, OOTB High-Level Combinators)


<style>
  pre.mermaid {
    all: unset;
    
  }
  .flowchart {
    max-height: -webkit-fill-available;
  }
</style>

<script type="module">
import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11.3.0/dist/mermaid.esm.min.mjs';
mermaid.initialize({
  startOnLoad: true,
  flowchart: {
    width: '100%'
  }
});

window.addEventListener('vscode.markdown.updateContent', function() { mermaid.init() });
</script>
