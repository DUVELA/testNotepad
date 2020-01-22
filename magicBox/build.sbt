// scalajs, kind-projector, graph

enablePlugins(ScalaJSPlugin)
scalaJSUseMainModuleInitializer := true
skip in packageJSDependencies := false
jsDependencies += "org.webjars" % "jquery" % "3.3.1" / "3.3.1/jquery.js" minified "3.3.1/jquery.min.js"
jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()


import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "scala-js-tutorial",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
    libraryDependencies += "org.scala-graph" %% "graph-core" % "1.13.0",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7",
    libraryDependencies += "org.querki" %%% "jquery-facade" % "1.2",
    libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.9.5",
    libraryDependencies += "io.udash" %%% "udash-jquery" % "3.0.2",
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.7.2" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  
resolvers += Resolver.sonatypeRepo("releases")


// if your project uses multiple Scala versions, use this for cross building
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

// if your project uses multiple Scala versions, use this for cross building
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.0" cross CrossVersion.full)

// if your project uses both 2.10 and polymorphic lambdas
libraryDependencies ++= (scalaBinaryVersion.value match {
  case "2.10" =>
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full) :: Nil
  case _ =>
    Nil
})
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
