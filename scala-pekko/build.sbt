scalaVersion := "3.9.0"

val PekkoVersion = "1.7.0"
val PekkoHttpVersion = "1.4.0"

libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-stream" % PekkoVersion,
  "org.apache.pekko" %% "pekko-http" % PekkoHttpVersion,
  "org.asynchttpclient" % "async-http-client" % "3.0.11",
  "org.scalatest" %% "scalatest" % "3.2.20" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.18" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.44.1" % Test
)
