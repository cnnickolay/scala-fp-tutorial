name := "edu"

version := "0.1"

scalaVersion := "2.13.6"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full)

scalacOptions += "-Xfatal-warnings"
scalacOptions += "-feature"

