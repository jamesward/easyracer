scalaVersion := "3.5.1"

val SoundnessVersion = "0.28.0"
libraryDependencies ++= Seq(
  "dev.soundness" % "exoskeleton-core" % SoundnessVersion,
  "dev.soundness" % "nomenclature-core" % SoundnessVersion,
  "dev.soundness" % "parasite-core" % SoundnessVersion,
  "dev.soundness" % "quantitative-units" % SoundnessVersion,
  "dev.soundness" % "telekinesis-core" % SoundnessVersion,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.17" % Test,
  "com.dimafeng" %% "testcontainers-scala-core" % "0.43.0" % Test
)
scalacOptions ++= Seq(
  "-experimental",
  "-Yexplicit-nulls",
  "-Xprint-suspension",
  "-language:experimental.modularity",
  "-language:experimental.clauseInterleaving",
  "-language:experimental.genericNumberLiterals",
  "-language:experimental.saferExceptions",
  "-language:experimental.erasedDefinitions",
  "-language:experimental.namedTypeArguments",
  "-language:experimental.namedTuples",
  "-Xmax-inlines",
  "100"
)
