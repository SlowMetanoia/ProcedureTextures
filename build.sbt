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
//libraryDependencies += "org.scalatest" % "scalatest_2.9.2" % "1.9.2" % "test" withSources() withJavadoc()
libraryDependencies  ++= Seq(
  // other dependencies here
  "org.scalanlp" %% "breeze" % "2.1.0",
  // native libraries are not included by default. add this if you want them (as of 0.7)
  // native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "2.1.0",
  // the visualization library is distributed separately as well.
  // It depends on LGPL code.
  "org.scalanlp" %% "breeze-viz" % "2.1.0"
  )

Compile / mainClass := Some("ProcTextures")//"SIFPanel"

assembly / mainClass := Some("ProcTextures")//"SIFPanel"

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}