lazy val zioHttp = ProjectRef(uri("https://github.com/jamesward/zio-http.git#logging-fix"), "zioHttpLogging")

dependsOn(zioHttp)

scalaVersion := "3.2.1"

fork := true

val zioVersion = "2.0.3"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % zioVersion,

  "dev.zio" %% "zio-http"     % "0.0.1" exclude ("dev.zio", "zio-http-logging_3"),

  "dev.zio" %% "zio-test"     % zioVersion % Test,

  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,

  "dev.zio" %% "zio-http-testkit" % "0.0.1" % Test exclude ("dev.zio", "zio-http-logging_3"),
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
