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
