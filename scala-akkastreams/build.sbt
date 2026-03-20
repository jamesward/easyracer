scalaVersion := "3.7.4"

val PekkoVersion = "1.1.5"
val PekkoHttpVersion = "1.1.0"
libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-stream" % PekkoVersion,
  "org.apache.pekko" %% "pekko-http" % PekkoHttpVersion,
  "org.asynchttpclient" % "async-http-client" % "3.0.7",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.43.6" % Test
)
