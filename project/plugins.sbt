// Coursier changes SBT's resolution mechanism to download dependencies in
// parallel.

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC10")

// Scalariform is a code formatter.

addSbtPlugin("com.lucidchart" % "sbt-scalafmt-coursier" % "1.10")
