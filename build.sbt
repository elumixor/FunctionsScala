name := "Functions"

version := "0.1"

enablePlugins(ScalaJSPlugin)
scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scala-js" %% "scalajs-stubs" % scalaJSVersion % "provided"
libraryDependencies += "org.scalafx" %% "scalafx" % "10.0.2-R15"
scalaJSUseMainModuleInitializer := true
//scalaJSModuleKind := ModuleKind.CommonJSModule
//scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) }