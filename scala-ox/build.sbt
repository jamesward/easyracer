scalaVersion := "3.4.2"

libraryDependencies ++= Seq(
  "com.softwaremill.ox" %% "core" % "0.1.0",
  "com.softwaremill.sttp.client3" %% "core" % "3.9.6",
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.13" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.3" % Test
)

fork := true

Test / fork := true

javacOptions += "--enable-preview"

javaOptions += "--enable-preview"
