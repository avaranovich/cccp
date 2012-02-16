import sbt._
import Keys._

object CCCPBuild extends Build {
  lazy val root = Project(id = "cccp", base = file(".")) aggregate(server, agent)
  lazy val server = Project(id = "cccp-server", base = file("server"))
  lazy val agent = Project(id = "cccp-agent", base = file("agent")) dependsOn server

  val virtualVoid = "Virtual-Void repository" at "http://mvn.virtual-void.net"
  val es = compilerPlugin("net.virtualvoid" %% "scala-enhanced-strings" % "0.5.2")
  
  val stage = TaskKey[Unit]("stage", "Copy files into staging directory for a release.")
}

