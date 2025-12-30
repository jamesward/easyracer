scalaVersion := "3.7.4"

libraryDependencies ++= Seq(
  "io.getkyo" %% "kyo-core" % "1.0-RC1",
  "io.getkyo" %% "kyo-direct" % "1.0-RC1",
  "io.getkyo" %% "kyo-sttp" % "1.0-RC1",
  "org.slf4j" % "slf4j-simple" % "2.0.17",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.43.6" % Test
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")

scalacOptions ++= Seq(
  "-Wvalue-discard",
  "-Wnonunit-statement",
  "-Wconf:msg=(discarded.*value|pure.*statement):error"
)
