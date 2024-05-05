scalaVersion := "3.4.1"

libraryDependencies ++= Seq(
  "io.getkyo" %% "kyo-core" % "0.9.3",
  "io.getkyo" %% "kyo-direct" % "0.9.3",
  "io.getkyo" %% "kyo-sttp" % "0.9.3",
  "org.slf4j" % "slf4j-simple" % "2.0.13",
  "io.getkyo" %% "kyo-test" % "0.9.3" % Test,
  "dev.zio" %% "zio-test-sbt" % "2.0.22" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.3" % Test
)


Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
