scalaVersion := "3.2.2"

fork := true

val zioVersion = "2.0.10"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % zioVersion,

  "dev.zio" %% "zio-http"     % "0.0.5",

  "dev.zio" %% "zio-test"     % zioVersion % Test,

  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,

  "dev.zio" %% "zio-http-testkit" % "0.0.5" % Test,

  "org.slf4j" % "slf4j-simple" % "2.0.5" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.40.12" % Test,
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

Test / fork := true
