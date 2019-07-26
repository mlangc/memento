name := "memento"

version := "0.1.0"

scalaVersion := "2.12.8"

mainClass in Compile := Some("com.github.mlangc.memento.trainer.console.ConsoleTrainer")

import sbtassembly.AssemblyPlugin.defaultUniversalScript
assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultUniversalScript(shebang = true)))
assemblyJarName in assembly := s"${name.value}-${version.value}"

assemblyMergeStrategy in assembly := {
  case PathList(ps @ _*) if ps.last.startsWith("jansi") || ps.last.startsWith("libjansi") => MergeStrategy.first
  case PathList("zio", "BuildInfo$.class") => MergeStrategy.last
  case PathList("scala", "tools", "nsc", _*) => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding", "utf-8", // Specify character encoding used by source files.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Xfuture", // Turn on future language features.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification", // Enable partial unification in type constructor inference
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
  "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
)

scalacOptions in(Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")

val catsVersion = "1.6.1"
val zioVersion = "1.0.0-RC10-1"
val refinedVersion = "0.9.8"
val silencerVersion = "1.4.1"
val log4jVersion = "2.12.0"

libraryDependencies += "dev.zio" %% "zio-interop-cats" % "1.3.1.0-RC3"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test

libraryDependencies += "com.google.oauth-client" % "google-oauth-client-jetty" % "1.29.0"
libraryDependencies += "com.google.apis" % "google-api-services-sheets" % "v4-rev579-1.25.0"

libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.1"

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % refinedVersion,
  "eu.timepit" %% "refined-scalacheck" % refinedVersion % Test
)

val cirisVersion = "0.12.1"

libraryDependencies ++= Seq(
  "is.cir" %% "ciris-core",
  "is.cir" %% "ciris-refined",
).map(_ % cirisVersion)

val enumeratumVersion = "1.5.13"
libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion
)

libraryDependencies += "com.statemachinesystems" % "mock-clock" % "1.0" % Test
libraryDependencies += "info.debatty" % "java-string-similarity" % "1.2.1"
libraryDependencies += "io.github.java-diff-utils" % "java-diff-utils" % "4.0"

libraryDependencies += "com.github.mlangc" %% "slf4zio" % "0.2.0-SNAPSHOT"
libraryDependencies += "org.fusesource.jansi" % "jansi" % "1.18"
libraryDependencies += "org.jline" % "jline-reader" % "3.11.0"

libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % log4jVersion
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % log4jVersion

libraryDependencies ++= Seq(
  compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
  "com.github.ghik" %% "silencer-lib" % silencerVersion % Provided
)


