# Comparing Approaches to Structured Concurrency

Adam Hearn

James Ward

---

## What is Structured Concurrency?


---

## Easy Racer

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
