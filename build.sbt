name := "Smala"

organization := "com.faacets"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.4"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "com.googlecode.kiama" %% "kiama" % "1.8.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
  "org.scalacheck" %% "scalacheck" % "1.11.6" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
  "org.spire-math" %% "spire" % "0.9.1-SNAPSHOT"
)

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 
