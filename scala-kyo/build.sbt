scalaVersion := "3.4.0"

libraryDependencies ++= Seq(
  "io.getkyo" %% "kyo-core" % "0.9.0",
  "io.getkyo" %% "kyo-direct" % "0.9.0",
  "io.getkyo" %% "kyo-sttp" % "0.9.0",
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.12" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.3" % Test
)
