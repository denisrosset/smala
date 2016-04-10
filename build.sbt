val kiamaVersion = "1.8.0"
val parserCombinatorsVersion = "1.0.3"
val scalaCheckVersion = "1.12.4"
val scalaTestVersion = "3.0.0-M7"
val spireVersion = "0.11.0"

name := "Smala"

organization := "net.alasc"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "com.googlecode.kiama" %% "kiama" % kiamaVersion,
  "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion,
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
  "org.scalatest" % "scalatest_2.11" % scalaTestVersion % "test",
  "org.spire-math" %% "spire" % spireVersion
)

scalacOptions ++= Seq("-unchecked", "-feature", "-deprecation") 

bintrayRepository := "maven"

publishArtifact in Test := false

homepage := Some(url("http://github.com/denisrosset/smala"))

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
