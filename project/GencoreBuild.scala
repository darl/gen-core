import sbt._
import sbt.Keys._

object GencoreBuild extends Build {

  lazy val gencore = Project(
    id = "gen-core",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "gen-core",
      organization := "com.github.darl",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.1",
      // add other settings here
      libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.1",
      libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.10.1",
      libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.5",
      libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.11"
    )
  )
}
