scalaVersion := "3.9.0"

libraryDependencies ++= Seq(
  "ch.epfl.lamp" %% "gears" % "0.3.1",
  "com.squareup.okhttp3" % "okhttp" % "5.5.0",
  "org.asynchttpclient" % "async-http-client" % "3.0.13",
  "org.scalatest" %% "scalatest" % "3.2.20" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.18" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.44.1" % Test
)
