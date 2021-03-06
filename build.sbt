name := "memento"

version := "0.2.1"

scalaVersion := "2.13.1"

mainClass in Compile := Some("com.github.mlangc.memento.trainer.console.ConsoleTrainer")

import sbtassembly.AssemblyPlugin.defaultUniversalScript

assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultUniversalScript(shebang = true)))
assemblyJarName in assembly := s"${name.value}-${version.value}"

assemblyMergeStrategy in assembly := {
  case PathList(ps@_*) if ps.last.startsWith("jansi") || ps.last.startsWith("libjansi") => MergeStrategy.first
  case PathList("zio", "BuildInfo$.class") => MergeStrategy.last
  case PathList("scala", "tools", "nsc", _*) => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros", // Allow macro definition (besides implementation and application)
  "-language:higherKinds", // Allow higher-kinded types
  "-language:implicitConversions", // Allow definition of implicit functions called views
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-numeric-widen", // Warn when numerics are widened.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates", // Warn if a private member is unused.
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused.
  "-Ybackend-parallelism", "8", // Enable paralellisation — change to desired number!
  "-Ycache-plugin-class-loader:last-modified", // Enables caching of classloaders for compiler plugins
  "-Ycache-macro-class-loader:last-modified", // and macro definitions. This can lead to performance improvements.
)

scalacOptions in(Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings")

val catsVersion = "2.0.0"
val zioVersion = "1.0.0-RC17+387-b8979ea4-SNAPSHOT"
val slf4zioVersion = "0.4.0+17-0a3345d7-SNAPSHOT"
val refinedVersion = "0.9.10"
val silencerVersion = "1.4.4"
val log4jVersion = "2.13.0"
val circeVersion = "0.12.3"
val jmhVersion = "1.23"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "com.google.oauth-client" % "google-oauth-client-jetty" % "1.29.2"
libraryDependencies += "com.google.apis" % "google-api-services-sheets" % "v4-rev579-1.25.0"
libraryDependencies += "com.google.apis" % "google-api-services-drive" % "v3-rev188-1.25.0" % Test

libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"

libraryDependencies += "io.circe" %% "circe-refined" % circeVersion
libraryDependencies += "io.circe" %% "circe-generic" % circeVersion
libraryDependencies += "io.circe" %% "circe-parser" % circeVersion

libraryDependencies += "org.openjdk.jmh" % "jmh-core" % jmhVersion % Test
libraryDependencies += "org.openjdk.jmh" % "jmh-generator-annprocess" % jmhVersion % Test

libraryDependencies ++= Seq(
  "eu.timepit" %% "refined" % refinedVersion,
  "eu.timepit" %% "refined-scalacheck" % refinedVersion % Test
)

val cirisVersion = "0.13.0-RC1"

libraryDependencies ++= Seq(
  "is.cir" %% "ciris-core",
  "is.cir" %% "ciris-refined",
).map(_ % cirisVersion)

val enumeratumVersion = "1.5.15"
libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion
)

libraryDependencies += "com.statemachinesystems" % "mock-clock" % "1.0" % Test
libraryDependencies += "info.debatty" % "java-string-similarity" % "1.2.1"
libraryDependencies += "io.github.java-diff-utils" % "java-diff-utils" % "4.0"

libraryDependencies += "com.github.mlangc" %% "slf4zio" % slf4zioVersion
libraryDependencies += "org.jline" % "jline-reader" % "3.12.1"

libraryDependencies += "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % log4jVersion
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % log4jVersion

libraryDependencies ++= Seq(
  compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
  "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.3" % Test
libraryDependencies += "dev.zio" %% "zio-test" % zioVersion % Test
libraryDependencies += "dev.zio" %% "zio-test-sbt" % zioVersion % Test
libraryDependencies += "commons-io" % "commons-io" % "2.6"
libraryDependencies += "com.outr" %% "hasher" % "1.2.2"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

enablePlugins(JmhPlugin)
enablePlugins(BuildInfoPlugin)
sourceDirectory in Jmh := (sourceDirectory in Test).value
classDirectory in Jmh := (classDirectory in Test).value
dependencyClasspath in Jmh := (dependencyClasspath in Test).value
compile in Jmh := (compile in Jmh).dependsOn(compile in Test).value
run in Jmh := (run in Jmh).dependsOn(Keys.compile in Jmh).evaluated

buildInfoKeys := Seq[BuildInfoKey](
  name, version, scalaVersion, sbtVersion,
  BuildInfoKey("head" -> git.gitHeadCommit.value.getOrElse("???")),
  BuildInfoKey("dirty" -> git.gitUncommittedChanges.value))


buildInfoPackage := "com.github.mlangc.memento"
buildInfoOptions += BuildInfoOption.BuildTime
