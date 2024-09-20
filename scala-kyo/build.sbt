scalaVersion := "3.5.1"

libraryDependencies ++= Seq(
  "io.getkyo" %% "kyo-core" % "0.10.0",
  "io.getkyo" %% "kyo-direct" % "0.10.0",
  "io.getkyo" %% "kyo-sttp" % "0.10.0",
  "org.slf4j" % "slf4j-simple" % "2.0.16",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.4" % Test
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

scalacOptions += "-deprecation"
