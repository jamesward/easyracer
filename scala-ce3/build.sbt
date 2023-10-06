scalaVersion := "2.13.10"

libraryDependencies ++= List(
  "org.http4s"          %% "http4s-ember-client"      % "0.23.18",
  "com.disneystreaming" %% "weaver-cats"              % "0.8.1"    % "test,it",
  "com.dimafeng"        %% "testcontainers-scala"     % "0.40.12"  % "it",
  "ch.qos.logback"       %  "logback-classic"         % "1.4.7"    % Runtime

)

testFrameworks += new TestFramework("weaver.framework.CatsEffect")

IntegrationTest / fork := true
IntegrationTest / run / javaOptions ++= Seq(
    "-Xms256M", "-Xmx2G", "-XX:MaxPermSize=1024M", "-XX:+UseConcMarkSweepGC")
IntegrationTest / envVars := Map("TESTCONTAINERS_RYUK_DISABLED" -> "true")

lazy val root = (project in file("."))
  .configs(IntegrationTest)
  .settings(Defaults.itSettings)

