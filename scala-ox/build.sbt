scalaVersion := "3.7.3"

libraryDependencies ++= Seq(
  "com.softwaremill.ox" %% "core" % "0.7.3",
  "com.softwaremill.sttp.client3" %% "core" % "3.11.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.43.0" % Test
)

fork := true

Test / fork := true

javacOptions += "--enable-preview"

javaOptions += "--enable-preview"
