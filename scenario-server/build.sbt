enablePlugins(JavaAppPackaging, DockerPlugin)

name := "easyracer-server"

scalaVersion := "3.8.3"

fork := true

reStartArgs := Seq("--debug")

val zioVersion = "2.1.25"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-concurrent" % zioVersion,
  "dev.zio" %% "zio-direct" % "1.0.0-RC7" exclude ("org.scalameta", "scalafmt-core_2.13"),
  "dev.zio" %% "zio-logging" % "2.5.3",
  "dev.zio" %% "zio-http" % "3.11.0"
    exclude ("io.netty", "netty-pkitesting")
    exclude ("com.lihaoyi", "unroll-plugin_3")
    exclude ("io.netty", "netty-transport-native-kqueue"),
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

Compile / packageDoc / publishArtifact := false

Compile / doc / sources := Seq.empty

// Docker: multi-stage build with jlink inside the container
dockerBaseImage := "eclipse-temurin:25-jdk"
dockerExposedPorts := Seq(8080)
Docker / daemonUser := "app"
Docker / packageName := sys.env.getOrElse("DOCKER_IMAGE", "easyracer-server")
Docker / version := sys.env.getOrElse("DOCKER_TAG", "latest")
Docker / dockerCommands := {
  import com.typesafe.sbt.packager.docker._
  Seq(
    // Stage 1: jlink to create minimal JVM
    Cmd("FROM", "eclipse-temurin:25-jdk AS builder"),
    Cmd("WORKDIR", "/build"),
    Cmd("COPY", "2/opt/docker/lib", "/build/lib"),
    Cmd("COPY", "4/opt/docker/lib", "/build/lib"),
    ExecCmd(
      "RUN",
      "bash",
      "-c",
      """modules=$(jdeps --ignore-missing-deps --multi-release 25 -R --print-module-deps /build/lib/*.jar | sed 's/java.desktop,//g; s/,java.desktop//g; s/java.desktop//g') && \
        |jlink --output /opt/jre --add-modules $modules --compress zip-6 --no-header-files --no-man-pages""".stripMargin
    ),
    // Stage 2: distroless runtime (just glibc, no JRE)
    Cmd("FROM", "gcr.io/distroless/base-debian12:latest"),
    Cmd("WORKDIR", "/opt/app"),
    Cmd("COPY", "--from=builder", "/opt/jre", "/opt/jre"),
    Cmd("COPY", "--from=builder", "/build/lib", "/opt/app/lib"),
    Cmd("EXPOSE", "8080"),
    Cmd("USER", "65532"),
    ExecCmd("ENTRYPOINT", "/opt/jre/bin/java", "--sun-misc-unsafe-memory-access=allow", "-cp", "/opt/app/lib/*", "EasyRacerServer")
  )
}

run / javaOptions += "--enable-native-access=ALL-UNNAMED"
run / javaOptions += "--sun-misc-unsafe-memory-access=allow"
