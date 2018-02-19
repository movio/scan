name := "scan"
organization := "co.movio"
version := "0.2.0"

scalaVersion := "2.11.11"
crossScalaVersions := Seq("2.11.11", "2.12.3")

scalacOptions := Seq(
  "-Xlint",
  "-deprecation",
  "-feature",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused"
)

fork in Test := true

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.scalatest" %% "scalatest" % "3.0.4" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
)

// Override the directory structure settings so that subprojects have the
// following flattened layout:
//
// build.sbt
// resources/
//   application.conf
// src/
//   A.scala
// test/
//   ATests.scala

sourceDirectory in Compile := baseDirectory.value / "src"
sourceDirectory in Test := baseDirectory.value / "test"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"

resourceDirectory in Compile := baseDirectory.value / "resources"
resourceDirectory in Test := baseDirectory.value / "resources_test"

// Configure Scaladoc and GitHub pages publishing.
// Run `scaladoc` in SBT to push.

enablePlugins(SiteScaladocPlugin)
enablePlugins(GhpagesPlugin)

autoAPIMappings := true

git.remoteRepo := "git@github.com:movio/scan.git"

scalacOptions in (Compile, doc) ++= Seq(
  "-groups"
)

def scaladoc = Command.command("scaladoc") { state =>
  val extracted = Project.extract(state)
  // Use pretty scaladocs from 2.12 when publishing.
  val newState = extracted.append(Seq(scalaVersion := "2.12.3"), state)
  extracted.runTask(ghpagesPushSite in doc, newState)
  state
}

commands += scaladoc

// Configure other plugins.

scalafmtOnCompile in ThisProject := true
