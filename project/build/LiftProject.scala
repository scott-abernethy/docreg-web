import sbt._

class LiftProject(info: ProjectInfo) extends DefaultWebProject(info) with IdeaProject {
  val liftVersion = property[Version]

  // uncomment the following if you want to use the snapshot repo
  //  val scalatoolsSnapshot = ScalaToolsSnapshots

  // If you're using JRebel for Lift development, uncomment
  // this line
  override def scanDirectories = Nil

  lazy val JavaNet = "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"
  lazy val Typesafe = "Typesafe Maven2 Repository" at "http://repo.typesafe.com/typesafe/releases/"

  override def libraryDependencies = Set(
    "net.liftweb" %% "lift-webkit" % liftVersion.value.toString % "compile",
    "net.liftweb" %% "lift-record" % liftVersion.value.toString % "compile",
    "net.liftweb" %% "lift-ldap" % liftVersion.value.toString % "compile",
    "net.liftweb" %% "lift-widgets" % liftVersion.value.toString % "compile",
    "org.mortbay.jetty" % "jetty" % "6.1.26" % "test",
    "junit" % "junit" % "4.7" % "test",
    "org.scala-tools.testing" %% "specs" % "1.6.9" % "test",
    "org.squeryl" %% "squeryl" % "0.9.5" % "compile",
    "com.h2database" % "h2" % "1.2.147",
    "mysql" % "mysql-connector-java" % "5.1.9",
    "org.mockito" % "mockito-all" % "1.8.5" % "test",
    "c3p0" % "c3p0" % "0.9.1.2",
    "org.jboss.netty" % "netty" % "3.2.3.Final",
    "com.typesafe.akka" % "akka-actor" % "2.0.1",
    "com.typesafe.akka" % "akka-testkit" % "2.0.1",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.0",
    "net.databinder" %% "dispatch-http" % "0.8.8",
    "ch.ethz.ganymed" % "ganymed-ssh2" % "build210",
    "org.streum" %% "configrity-core" % "0.10.2",
//    "org.apache.httpcomponents" % "httpclient" % "4.1.2",  // <- comes in via dispatch-http now.
    "log4j" % "log4j" % "1.2.16",
    "org.slf4j" % "slf4j-log4j12" % "1.6.1"
  ) ++ super.libraryDependencies

  override def jettyPort = 9333
}
