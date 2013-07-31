/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import vvv.docreg.backend._
import vvv.docreg.agent._
import vvv.docreg.agent.faux._
import akka.actor.{PoisonPill, Props, ActorSystem}
import com.typesafe.config.{ConfigFactory}
import vvv.docreg.directory.faux.FauxDirectoryComponentImpl
import net.liftweb.common.Loggable

trait Environment extends BackendComponent with DocumentStreamComponent with DirectoryComponent with DaemonAgentComponent
{
  def start()

  def exit()
}

class EnvironmentImpl extends Environment with BackendComponent with DirectoryComponentImpl with DaemonAgentComponent with Loggable
{
  val akkaConfig = ConfigFactory.load("akka.conf")
  val system = ActorSystem("DocRegWebSystem", akkaConfig.getConfig("docreg-web").withFallback(akkaConfig))
  import system._
  val fileDatabase = actorOf(Props[FileDatabase], "FileDatabase")
  val daemonAgent = actorOf(Props[DaemonAgentImpl], "DaemonAgent")
  val documentStream = actorOf(Props[DocumentStream], "DocumentStream")
  val backend = actorOf(Props(new Backend(directory, daemonAgent, documentStream, fileDatabase)), "Backend")
  val poller = actorOf(Props(new ChangePoller(AgentVendor.server, backend, daemonAgent)))
  val userStorage = actorOf(Props[UserStorage], "UserStorage")

  def start()
  {
    poller ! 'Reset
    logger.info("Environment started - ***PRODUCTION MODE*** - Connecting to production docregd servers")
  }

  def exit()
  {
    backend ! 'Die
    poller ! 'Die
    daemonAgent ! 'Die
    system.shutdown()
  }
}

class FauxEnvironmentImpl extends Environment with BackendComponent with FauxDirectoryComponentImpl with DaemonAgentComponent with Loggable
{
  val akkaConfig = ConfigFactory.load("akka.conf")
  val system = ActorSystem("DocRegWebSystem", akkaConfig.getConfig("docreg-web").withFallback(akkaConfig))
  import system._
  val fileDatabase = actorOf(Props[FauxFileDatabase], "FileDatabase")
  val daemonAgent = actorOf(Props[FauxDaemonAgent], "DaemonAgent")
  val documentStream = actorOf(Props[DocumentStream], "DocumentStream")
  val backend = actorOf(Props(new Backend(directory, daemonAgent, documentStream, fileDatabase)), "Backend")
  val poller = actorOf(Props(new ChangePoller(AgentVendor.server, backend, daemonAgent)))
  val userStorage = actorOf(Props[UserStorage], "UserStorage")

  def start()
  {
    poller ! 'Reset
    logger.info("Environment started - Developer Mode - Using faked data for external systems (LDAP, docregd)")
  }

  def exit()
  {
    backend ! 'Die
    poller ! 'Die
    daemonAgent ! 'Die
    system.shutdown()
  }
}

object Environment
{
  var env: Environment = _
}
