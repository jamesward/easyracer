scalaVersion := "3.4.2"

libraryDependencies ++= Seq(
  "io.getkyo" %% "kyo-core" % "0.9.2",
  "io.getkyo" %% "kyo-direct" % "0.9.2",
  "io.getkyo" %% "kyo-sttp" % "0.9.2",
  "org.slf4j" % "slf4j-simple" % "2.0.13",
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.3" % Test
)

Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
