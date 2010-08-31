import sbt._

class LiftProject(info: ProjectInfo) extends DefaultWebProject(info) {
  val snapshots = ScalaToolsSnapshots
  val liftVersion = "2.1-SNAPSHOT"

  override def libraryDependencies = Set(
    "net.liftweb" %% "lift-mapper" % liftVersion % "compile->default",
    "net.liftweb" %% "lift-wizard" % liftVersion % "compile->default",
    "org.mortbay.jetty" % "jetty" % "6.1.25" % "test->default",
    "junit" % "junit" % "4.5" % "test->default",
    "org.scala-tools.testing" % "specs" % "1.6.2.1" % "test->default",
    "com.h2database" % "h2" % "1.2.121",
    "org.apache.httpcomponents" % "httpclient" % "4.0.1"
  ) ++ super.libraryDependencies
}
