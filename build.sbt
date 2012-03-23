import AssemblyKeys._

seq(assemblySettings: _*)

name := "mn2gt"

version := "0.1"

organization := "org.opengeo"

scalaVersion := "2.9.1"

fork in run := true

resolvers += ScalaToolsSnapshots

mainClass /* in assembly */ := Some("me.winslow.d.mn2gt.GUI")

libraryDependencies ++= Seq(
  "commons-httpclient" % "commons-httpclient" % "3.1",
  "org.scala-tools.testing" %% "specs" % "[1.6.0,1.7[" % "test"
)

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-swing" % _)
