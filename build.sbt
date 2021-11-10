import Dependencies._

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "teatime",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % Test,
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % Test,
    libraryDependencies += "dev.zio" %% "zio-test" % "1.0.1" % Test,
    libraryDependencies += "dev.zio" %% "zio-test-sbt" % "1.0.1" % Test,
    libraryDependencies += "dev.zio" %% "zio-test-magnolia" % "1.0.1" % Test,
    libraryDependencies += "dev.zio" %% "zio" % "1.0.1"
  )

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
