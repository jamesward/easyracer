scalaVersion := "3.8.4"

libraryDependencies ++= Seq(
  "io.getkyo" %% "kyo-core" % "1.0.0-RC4",
  "io.getkyo" %% "kyo-direct" % "1.0.0-RC4",
  "io.getkyo" %% "kyo-http" % "1.0.0-RC4",
  "io.getkyo" %% "kyo-logging-slf4j" % "1.0.0-RC4",
  "org.slf4j" % "slf4j-simple" % "2.0.18",
  "org.scalatest" %% "scalatest" % "3.2.20" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.44.1" % Test
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

scalacOptions ++= Seq(
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Wconf:msg=(discarded.*value|pure.*statement):error"
)
