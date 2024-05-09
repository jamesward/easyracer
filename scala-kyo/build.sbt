scalaVersion := "3.4.1"

resolvers ++= Resolver.sonatypeOssRepos("snapshots")
val kyoVersion = "0.9.3+41-5cd05254-SNAPSHOT"

libraryDependencies ++= Seq(
  "io.getkyo" %% "kyo-core" % kyoVersion,
  "io.getkyo" %% "kyo-direct" % kyoVersion,
  "io.getkyo" %% "kyo-sttp" % kyoVersion,
  "org.slf4j" % "slf4j-simple" % "2.0.13",
  "org.scalatest" %% "scalatest" % "3.2.18" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.41.3" % Test
)


Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
