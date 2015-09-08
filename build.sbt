name := "lms-utils"

scalaVersion := "2.11.2"

scalaOrganization := "org.scala-lang.virtualized"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scala-lang.lms" %% "lms-core" % "1.0.0-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.2",
  "org.scala-lang.plugins" % "scala-continuations-library_2.11" % "1.0.2"
)

scalacOptions ++= Seq(
  "-Yvirtualize",
  //"-optimize",
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions"
  //"-Yinline-warnings"
)

/**
 * tests are not thread safe
 * this applies to all lms tests that write
 * to a file, and do diff tests
 */
parallelExecution in Test := false

defaultScalariformSettings
