/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import vvv.docreg.backend._
import vvv.docreg.agent._
import akka.actor.{PoisonPill, Props, ActorSystem}
import com.typesafe.config.{ConfigFactory}

trait Environment extends BackendComponent with DocumentStreamComponent with DirectoryComponent with DaemonAgentComponent
{
  def start()

  def exit()
}

trait EnvironmentImpl extends Environment with BackendComponent with DirectoryComponentImpl with DaemonAgentComponent
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
