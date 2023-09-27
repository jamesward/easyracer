enablePlugins(GraalVMNativeImagePlugin)

name := "easyracer-server"

scalaVersion := "3.3.1"

fork := true

reStartArgs := Seq("--debug")

val zioVersion = "2.0.18"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-concurrent" % zioVersion,
  "dev.zio" %% "zio-direct" % "1.0.0-RC7",
  "dev.zio" %% "zio-logging" % "2.1.14",
  "dev.zio" %% "zio-http" % "3.0.0-RC2",
  "dev.zio" %% "zio-test" % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

Compile / packageDoc / publishArtifact := false

Compile / doc / sources := Seq.empty

graalVMNativeImageOptions ++= Seq(
  "--no-fallback",
  "--install-exit-handlers",
  "--initialize-at-run-time=io.netty.channel.DefaultFileRegion",
  "--initialize-at-run-time=io.netty.channel.epoll.Native",
  "--initialize-at-run-time=io.netty.channel.epoll.Epoll",
  "--initialize-at-run-time=io.netty.channel.epoll.EpollEventLoop",
  "--initialize-at-run-time=io.netty.channel.epoll.EpollEventArray",
  "--initialize-at-run-time=io.netty.channel.kqueue.KQueue",
  "--initialize-at-run-time=io.netty.channel.kqueue.KQueueEventLoop",
  "--initialize-at-run-time=io.netty.channel.kqueue.KQueueEventArray",
  "--initialize-at-run-time=io.netty.channel.kqueue.Native",
  "--initialize-at-run-time=io.netty.channel.unix.Limits",
  "--initialize-at-run-time=io.netty.channel.unix.Errors",
  "--initialize-at-run-time=io.netty.channel.unix.IovArray",
  "--initialize-at-run-time=io.netty.handler.codec.compression.ZstdOptions",
  "--initialize-at-run-time=io.netty.handler.ssl.BouncyCastleAlpnSslUtils",
  "-H:+ReportExceptionStackTraces"
)

if (sys.env.get("TARGETARCH").contains("arm64")) {
  graalVMNativeImageOptions ++= Seq(
    "-H:+StaticExecutableWithDynamicLibC",
    "--initialize-at-run-time=io.netty.incubator.channel.uring.IOUringEventLoopGroup",
    "--initialize-at-run-time=io.netty.incubator.channel.uring.Native"
  )
} else if (sys.env.get("NO_STATIC").contains("true")) {
  graalVMNativeImageOptions ++= Seq(
    "-H:+StaticExecutableWithDynamicLibC"
  )
} else {
  graalVMNativeImageOptions ++= Seq(
    "--static",
    "--libc=musl"
  )
}

//fork := true

//run / javaOptions += s"-agentlib:native-image-agent=config-output-dir=src/main/resources/META-INF/native-image"
//javaOptions += s"-agentlib:native-image-agent=trace-output=${(target in GraalVMNativeImage).value}/trace-output.json"
