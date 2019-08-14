// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val catsVersion = "1.6.1"
val catsCollectionsVersion = "0.8.0"
val scalacheckVersion = "1.13.5"
val drosteVersion = "0.7.0"
val fastParseVersion = "2.1.2"
val scalaJsDomVersion = "0.9.7"

val catsOrg = "org.typelevel"
val scalacheckOrg = "org.scalacheck"
val drosteOrg = "io.higherkindness"
val scalaJsOrg = "org.scala-js"

val isTravisBuild =
  settingKey[Boolean]("Flag indicating whether the current build is running under Travis")

inThisBuild(
  List(
    organization := "net.ceedubs",
    homepage := Some(url("https://github.com/ceedubs/irrec")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "ceedubs",
        "Cody Allen",
        "ceedubs@gmail.com",
        url("https://github.com/ceedubs")
      )
    ),
    isTravisBuild := sys.env.get("TRAVIS").isDefined
  ))

coverageExcludedPackages in ThisBuild := "ceedubs.irrec.bench"

scalaVersion in Global := "2.12.8"

lazy val kleene = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("kleene"))
  .settings(
    moduleName := "irrec-kleene",
    libraryDependencies += catsOrg %%% "cats-core" % catsVersion)
  .settings(commonSettings)

lazy val regex = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("regex"))
  .dependsOn(kleene)
  .settings(
    moduleName := "irrec-regex",
    libraryDependencies ++= Seq(
      drosteOrg %%% "droste-core" % drosteVersion,
      catsOrg %%% "cats-collections-core" % catsCollectionsVersion))
  .settings(commonSettings)

lazy val tests = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("tests"))
  .dependsOn(regexGen, parser)
  .settings(
    moduleName := "irrec-tests",
    libraryDependencies += catsOrg %%% "cats-testkit" % catsVersion % Test)
  .settings(commonSettings)
  .settings(noPublishSettings)

lazy val regexGen = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(kleene, regex)
  .in(file("regex-gen"))
  .settings(
    moduleName := "irrec-regex-gen",
    libraryDependencies += scalacheckOrg %%% "scalacheck" % scalacheckVersion)
  .settings(commonSettings)

lazy val parser = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("parser"))
  .settings(
    moduleName := "irrec-parser",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % fastParseVersion)
  .settings(commonSettings)
  .dependsOn(kleene, regex)

lazy val jsDocs = project
  .in(file("irrec-js-docs"))
  .settings(commonSettings)
  .settings(
    moduleName := "irrec-js-docs",
    libraryDependencies += scalaJsOrg %%% "scalajs-dom" % scalaJsDomVersion
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(regexGen.js, parser.js)
  .settings(noPublishSettings)

lazy val docs = project
  .in(file("irrec-docs"))
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
  .dependsOn(regex.jvm, regexGen.jvm, parser.jvm)
  .settings(commonSettings)
  .settings(
    scalacOptions in (ScalaUnidoc, unidoc) += "-diagrams",
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(regex.jvm, regexGen.jvm, parser.jvm),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(unidoc in Compile).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(unidoc in Compile).value,
    mdocJS := Some(jsDocs),
    mdocVariables := Map(
      "ORG" -> organization.value,
      "VERSION" -> version.value
    )
  )
  .settings(noPublishSettings)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(moduleName := "irrec-benchmarks")
  .enablePlugins(JmhPlugin)
  .settings(commonSettings)
  .settings(noPublishSettings)
  .dependsOn(regex.jvm)

lazy val jvm = project
  .settings(
    moduleName := "irrec-root-jvm"
  )
  .aggregate(kleene.jvm, regex.jvm, regexGen.jvm, parser.jvm, tests.jvm, benchmarks)
  .settings(commonSettings)
  .settings(noPublishSettings)

lazy val js = project
  .settings(
    moduleName := "irrec-root-js"
  )
  .aggregate(kleene.js, regex.js, regexGen.js, parser.js, tests.js)
  .settings(commonSettings)
  .settings(noPublishSettings)

lazy val root = project
  .settings(
    moduleName := "irrec-root"
  )
  .aggregate(jvm, js)
  .settings(commonSettings)
  .settings(noPublishSettings)

// Thanks, Rob! https://tpolecat.github.io/2017/04/25/scalac-flags.html
val scalac212Options: Seq[String] = Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8", // Specify character encoding used by source files.
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
  "-Xlint:package-object-classes", // Class or object defined in package object.
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
  "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
)

val scalac211Options: Seq[String] = {
  val exclusions = Set(
    "-Xlint:constant",
    "-Ywarn-extra-implicit"
  )
  scalac212Options.filter(
    s =>
      !s.startsWith("-Ywarn-unused") &&
        !exclusions.contains(s))
}

val scalacOptionExclusionsForConsole: Seq[String] = Seq("-Ywarn-unused:imports", "-Xfatal-warnings")

val scalacOptionSettings: Seq[Setting[_]] = {
  def baseScalaCOptions(scalaVersion: String) = CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 11)) => scalac211Options
    case _ => scalac212Options
  }

  Seq(
    scalacOptions ++= baseScalaCOptions(scalaVersion.value),
    scalacOptions in (Compile, console) --= scalacOptionExclusionsForConsole,
    scalacOptions in (Test, console) --= scalacOptionExclusionsForConsole
  )
}

val commonSettings: Seq[Setting[_]] = Seq(
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10"),
  scalaVersion := "2.12.8",
  crossScalaVersions := List("2.11.12", "2.12.8"),
  autoAPIMappings := true
) ++ scalacOptionSettings

val commonJsSettings: Seq[Setting[_]] = Seq(
  parallelExecution := false,
  // batch mode decreases the amount of memory needed to compile Scala.js code
  scalaJSOptimizerOptions := scalaJSOptimizerOptions.value.withBatchMode(isTravisBuild.value)
)

val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false
)

addCommandAlias("format", ";scalafmtSbt;scalafmtAll")
addCommandAlias("lint", ";scalafmtSbtCheck;scalafmtCheckAll")
addCommandAlias("validate", ";lint;docs;test")
addCommandAlias("docs", ";docs/clean;docs/docusaurusCreateSite")
