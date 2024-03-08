scalaVersion := "3.4.0"

libraryDependencies ++= Seq(
  "com.softwaremill.ox" %% "core" % "0.0.22",
  "com.softwaremill.sttp.client3" %% "core" % "3.9.4",
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.12" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.3" % Test
)

fork := true

Test / fork := true

javacOptions += "--enable-preview"

javaOptions += "--enable-preview"
