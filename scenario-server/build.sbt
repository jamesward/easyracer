scalaVersion := "3.2.1"

fork := true

val zioVersion = "2.0.4"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"              % zioVersion,
  "dev.zio" %% "zio-concurrent"   % zioVersion,

  "dev.zio" %% "zio-http"         % "0.0.3",

  "dev.zio" %% "zio-test"         % zioVersion % Test,

  "dev.zio" %% "zio-test-sbt"     % zioVersion % Test,

  "dev.zio" %% "zio-http-testkit" % "0.0.3"    % Test,
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
