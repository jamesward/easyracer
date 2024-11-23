scalaVersion := "3.5.2"

libraryDependencies ++= Seq(
  "com.softwaremill.ox" %% "core" % "0.5.4",
  "com.softwaremill.sttp.client3" %% "core" % "3.10.1",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.16" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.4" % Test
)

fork := true

Test / fork := true

javacOptions += "--enable-preview"

javaOptions += "--enable-preview"
