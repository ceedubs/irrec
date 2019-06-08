import sbt._

object dependencies {
  object cats {
    val version = "1.4.0"
    val org = "org.typelevel"

    val core = org %% "cats-core" % version
    val testkit = org %% "cats-testkit" % version
  }

  val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.5"

  val droste = "io.higherkindness" %% "droste-core" % "0.6.0"

  val fastparse = "com.lihaoyi" %% "fastparse" % "2.1.0"
}
