scalaVersion := "2.13.16"

libraryDependencies ++= List(
  "org.http4s"          %% "http4s-ember-client"      % "0.23.30",
  "com.disneystreaming" %% "weaver-cats"              % "0.8.4"    % Test,
  "com.dimafeng"        %% "testcontainers-scala"     % "0.41.8"  % Test,
  "ch.qos.logback"       % "logback-classic"          % "1.5.16"    % Runtime,
)

testFrameworks += new TestFramework("weaver.framework.CatsEffect")

Test / fork := true

Test / run / javaOptions ++= Seq(
    "-Xms256M", "-Xmx2G", "-XX:MaxPermSize=1024M", "-XX:+UseConcMarkSweepGC")

Test / envVars := Map("TESTCONTAINERS_RYUK_DISABLED" -> "true")


