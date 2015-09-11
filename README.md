LMS Utils
==================

[![Build Status](https://travis-ci.org/manojo/lms-utils.svg?branch=master)](https://travis-ci.org/manojo/lms-utils)

This repo contains various additions to the [LMS](http://scala-lms.github.io)
that I have found useful during my experiments, and that are sufficiently
generic that I want to use them from multiple projects.

Among the more important things are

 * [MyScalaCompile.scala](https://github.com/manojo/lms-utils/blob/master/src/main/scala/lms/MyScalaCompile.scala): An add-on to be
 able to compile generated code that has multiple input parameters. It also
 generates data structures used, if any.

 * [ZeroVal.scala](https://github.com/manojo/lms-utils/blob/master/src/main/scala/lms/ZeroVal.scala): Allows an easy way to
 generate default null values for types, primitive and otherwise.

 * [FileDiffSpec.scala](https://github.com/manojo/lms-utils/blob/master/testutil/src/main/scala/lms/testutil/FileDiffSpec.scala): a file for doing diff tests for generated code.

It also contains CPS-encodings and struct-like encodings for some basic
datatypes, like Option and Either.

Running the code
================
To run the code, please follow these steps:

1. Clone this here repo in a separate folder: `git clone git@github.com:manojo/lms-utils.git`.
2. Profit:
```
  $ cd lms-utils
  $ sbt
  > test
```

Using the code in your project
==============================

This repo has been snapshot on Sonatype. If you want to have access to
lms-utils, add the following lines to your `sbt` build to start using:

    libraryDependencies += "com.github.manojo" % "lms-utils_2.11" % "0.1-SNAPSHOT"
    resolvers += Resolver.sonatypeRepo("snapshots")

If you want to use the diff test spec, then add the following:

  libraryDependencies += "com.github.manojo" % "lms-testutils_2.11" % "0.1-SNAPSHOT" % "test"
  resolvers += Resolver.sonatypeRepo("snapshots")
