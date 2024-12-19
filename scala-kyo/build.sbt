scalaVersion := "3.6.2"

libraryDependencies ++= Seq(
  "io.getkyo" %% "kyo-core" % "0.15.1",
  "io.getkyo" %% "kyo-direct" % "0.15.1",
  "io.getkyo" %% "kyo-sttp" % "0.15.1",
  "org.slf4j" % "slf4j-simple" % "2.0.16",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.5" % Test
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

scalacOptions ++= Seq(
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Wconf:msg=(discarded.*value|pure.*statement):error"
)
