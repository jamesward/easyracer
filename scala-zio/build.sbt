scalaVersion := "3.7.4"

fork := true

val zioVersion = "2.1.24"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-concurrent" % zioVersion,
  "dev.zio" %% "zio-http" % "3.7.4",
  "dev.zio" %% "zio-direct" % "1.0.0-RC7",
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.44.1" % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

Test / fork := true
