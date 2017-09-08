// Coursier changes SBT's resolution mechanism to download dependencies in
// parallel.

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC10")

// Scalariform is a code formatter.

addSbtPlugin("com.lucidchart" % "sbt-scalafmt-coursier" % "1.10")

// sbt-site is used to manage static site generation from SBT. It is used in
// this project to publish the Scaladocs to Github.

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.0")
