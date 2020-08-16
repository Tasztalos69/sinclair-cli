name := "scli"
packageSummary := "scli"

maintainer := "bmk@jelszo.co"
packageDescription := "A Scala CLI application for controlling Sinclair ACs."

version := "0.1"
scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
)

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.5.0",
  "io.spray" %% "spray-json" % "1.3.5",
  "commons-codec" % "commons-codec" % "1.14"
)

enablePlugins(JavaAppPackaging)
enablePlugins(UniversalPlugin)
enablePlugins(WindowsPlugin)