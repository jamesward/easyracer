scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "com.softwaremill.ox" %% "core" % "0.0.14",
  "com.softwaremill.sttp.client3" %% "core" % "3.9.0",
  "org.scalatest" %% "scalatest" % "3.2.17" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.9" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.0" % Test
)

fork := true

Test / fork := true

javacOptions += "--enable-preview"

javaOptions += "--enable-preview"
