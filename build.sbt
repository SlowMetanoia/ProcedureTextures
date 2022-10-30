ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

//enablePlugins(ScalaNativePlugin)

//import scala.scalanative.build._
/*
nativeConfig ~= {
  _.withLTO(LTO.thin)
   .withMode(Mode.releaseFast)
   .withGC(GC.commix)
}

lazy val root = (project in file("."))
  .settings(
    name := "ProcedureTextures"
  )
*/
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

Compile / mainClass := Some("ProcTextures")

assembly / mainClass := Some("ProcTextures")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}