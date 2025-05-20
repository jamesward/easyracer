scalaVersion := "3.7.0"

libraryDependencies ++= Seq(
  "ch.epfl.lamp" %% "gears" % "0.2.0",
  "com.squareup.okhttp3" % "okhttp" % "4.12.0",
  "org.asynchttpclient" % "async-http-client" % "3.0.2",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.43.0" % Test
)
