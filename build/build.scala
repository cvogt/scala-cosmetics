import cbt._
import java.net.URL
import java.io.File
import scala.collection.immutable.Seq

class Build( context: Context ) extends BasicBuild( context ){
  //override def name = "scalac-cosmetics"
  //override def defaultVersion = "0.1"
  override def defaultScalaVersion = "2.11.7"

  override def sources = Seq( projectDirectory ++ "/src/main/scala" )
  
  override def dependencies = (
    super.dependencies // don't forget super.dependencies here
    ++
    Resolver( mavenCentral ).bind(
      "org.scalameta" %% "scalameta" % "0.0.2",
      "org.scala-lang" % "scala-compiler" % "2.11.7",
      "com.lihaoyi" %% "fastparse" % "0.2.1",
      "com.lihaoyi" %% "ammonite-ops" % "0.4.6"
    )
  )
}
