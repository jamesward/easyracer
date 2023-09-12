scalaVersion := "3.3.1"

libraryDependencies ++= Seq(
  "com.softwaremill.ox" %% "core" % "0.0.13",
  "com.softwaremill.sttp.client3" %% "core" % "3.8.16",
  "org.scalatest" %% "scalatest" % "3.2.16" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.9" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.40.12" % Test
)

fork := true

Test / fork := true

javacOptions += "--enable-preview --add-modules jdk.incubator.concurrent"

javaOptions ++= Seq("--enable-preview", "--add-modules", "jdk.incubator.concurrent")
