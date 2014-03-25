name := "DocReg+Web"

version := "0.8.4"

organization := "vvv"

scalaVersion := "2.10.0"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases",
                "Java.net Maven2 Repository" at "http://download.java.net/maven/2/",
                "Typesafe Maven2 Repository" at "http://repo.typesafe.com/typesafe/releases/"
                )

seq(com.github.siasia.WebPlugin.webSettings :_*)

unmanagedResourceDirectories in Test <+= (baseDirectory) { _ / "src/main/webapp" }

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= {
  val liftVersion = "2.5.1"
  Seq(
    "net.liftweb"       %% "lift-webkit"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-record"        % liftVersion        % "compile",
    "net.liftweb"       %% "lift-ldap"        % liftVersion        % "compile",
    "net.liftmodules"       %% "widgets_2.5"        % "1.3"        % "compile",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.7.v20120910"  % "container,test",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    "org.specs2"        %% "specs2"             % "1.14"            % "test",
    "org.squeryl" %% "squeryl" % "0.9.5-6" % "compile",
    "com.h2database" % "h2" % "1.3.172" % "test",
    "mysql" % "mysql-connector-java" % "5.1.21",
    "org.mockito" % "mockito-all" % "1.9.5" % "test",
    "c3p0" % "c3p0" % "0.9.1.2",
    "org.jboss.netty" % "netty" % "3.2.3.Final",
    "com.typesafe.akka" %% "akka-actor" % "2.1.4",
    "com.typesafe.akka" %% "akka-slf4j" % "2.1.4",
    "com.typesafe.akka" %% "akka-testkit" % "2.1.4" % "test",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2",
    "org.apache.tika" % "tika-core" % "1.2",
    "org.slf4j" % "slf4j-log4j12" % "1.7.2"
  )
}

// fork in Test := true

parallelExecution in Test := false

// By default, it listens on port 8080; use the following to override
port in container.Configuration := 9090

