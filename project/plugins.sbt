// Coursier changes SBT's resolution mechanism to download dependencies in
// parallel.

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.3")

// Scalariform is a code formatter.

addSbtPlugin("com.lucidchart" % "sbt-scalafmt-coursier" % "1.15")

// sbt-site and sbt-ghpages are used to publish the Scaladocs to Github.

addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.0")
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.2")
