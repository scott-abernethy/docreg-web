package vvv.docreg.util

import vvv.docreg.backend._
import vvv.docreg.agent._
import akka.actor.{PoisonPill, Props, ActorSystem}
import com.typesafe.config.{Config, ConfigFactory}

trait Environment extends BackendComponent with DocumentServerComponent with DirectoryComponent with DaemonAgentComponent
{
  def start()

  def exit()
}

trait EnvironmentImpl extends Environment with BackendComponent with DocumentServerComponentImpl with DirectoryComponentImpl with DaemonAgentComponent
{
  val akkaConfig: Config = ConfigFactory.load("akka.conf")
  val system = ActorSystem("DocRegWebSystem", akkaConfig.getConfig("docreg-web").withFallback(akkaConfig))
  import system._
  val daemonAgent = actorOf(Props[DaemonAgentImpl], "DaemonAgent")
  val backend = actorOf(Props(new Backend(directory, daemonAgent, documentServer)), "Backend")
  val poller = actorOf(Props(new ChangePoller(AgentVendor.server, backend, daemonAgent)))
  val userStorage = actorOf(Props[UserStorage], "UserStorage")

  def start()
  {
    documentServer.start()
    poller ! 'Reset
  }

  def exit()
  {
    documentServer ! 'Die
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