name := "Smala"

organization := "com.faacets"

version := "0.1"

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "com.googlecode.kiama" %% "kiama" % "1.6.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
  "org.scalacheck" %% "scalacheck" % "1.11.4" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.1.6" % "test",
  "org.spire-math" %% "spire" % "0.7.6-SNAPSHOT"
)

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 
