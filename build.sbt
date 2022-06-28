name := "scan"
organization := "co.movio"
version := "0.3.0"

scalaVersion := "2.12.15"

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

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.6.1",
  "org.scalatest" %% "scalatest" % "3.2.12" % Test,
  "org.scalacheck" %% "scalacheck" % "1.16.0" % Test
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

Compile / sourceDirectory := baseDirectory.value / "src"
Test / sourceDirectory := baseDirectory.value / "test"

Compile / scalaSource := baseDirectory.value / "src"
Test / scalaSource := baseDirectory.value / "test"

Compile / resourceDirectory := baseDirectory.value / "resources"
Test / resourceDirectory := baseDirectory.value / "resources_test"

// Configure Scaladoc and GitHub pages publishing.
// Run `scaladoc` in SBT to push.

enablePlugins(SiteScaladocPlugin)
enablePlugins(GhpagesPlugin)

autoAPIMappings := true

git.remoteRepo := "git@github.com:movio/scan.git"

scalacOptions in (Compile, doc) ++= Seq(
  "-groups"
)

// Configure other plugins.

ThisProject / scalafmtOnCompile := true
