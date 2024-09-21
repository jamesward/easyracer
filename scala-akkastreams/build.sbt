scalaVersion := "3.5.1"

resolvers += "Akka library repository".at("https://repo.akka.io/maven")

val AkkaVersion = "2.9.3"
val AkkaHttpVersion = "10.6.3"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
  "org.asynchttpclient" % "async-http-client" % "3.0.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.16" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.4" % Test
)
