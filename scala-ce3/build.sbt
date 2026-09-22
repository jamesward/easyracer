scalaVersion := "3.9.0"

libraryDependencies ++= List(
  "org.http4s" %% "http4s-ember-client" % "0.23.36",
  "org.typelevel" %% "weaver-cats" % "0.13.0" % Test,
  "com.dimafeng" %% "testcontainers-scala" % "0.44.1" % Test,
  "ch.qos.logback" % "logback-classic" % "1.6.3" % Runtime
)

testFrameworks += new TestFramework("weaver.framework.CatsEffect")

Test / fork := true

Test / run / javaOptions ++= Seq("-Xms256M", "-Xmx2G", "-XX:MaxPermSize=1024M", "-XX:+UseConcMarkSweepGC")

Test / envVars := Map("TESTCONTAINERS_RYUK_DISABLED" -> "true")
