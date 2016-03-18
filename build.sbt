name := "scalac-cosmetics"

version := "0.1"

libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % "0.0.2",
  "org.scala-lang" % "scala-compiler" % "2.11.7",
  "com.lihaoyi" %% "fastparse" % "0.2.1",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "com.lihaoyi" %% "ammonite-ops" % "0.4.6"
)

scalaVersion := "2.11.7"
