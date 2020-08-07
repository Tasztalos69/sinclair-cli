name := "sinclair-cli"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.rogach" %% "scallop" % "3.5.0",
  "io.spray" %%  "spray-json" % "1.3.5",
  "commons-codec" % "commons-codec" % "1.14"
)