scalaVersion := "3.8.3"

libraryDependencySchemes += "io.getkyo" %% "kyo-core" % "always"

libraryDependencies ++= Seq(
  "io.getkyo" %% "kyo-core" % "1.0.0-RC2",
  "io.getkyo" %% "kyo-direct" % "1.0.0-RC2",
  "io.getkyo" %% "kyo-sttp" % "1.0-RC1",
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
