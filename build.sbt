scalaVersion := "2.11.11"

organization := "co.movio"

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
  "org.typelevel" %% "cats" % "0.9.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
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

scalafmtOnCompile := true
