package vvv.docreg.util

import vvv.docreg.backend.Connect._
import vvv.docreg.backend._
import vvv.docreg.agent._
import akka.actor.{PoisonPill, Props, ActorSystem}

trait Environment extends BackendComponent with DocumentServerComponent with DirectoryComponent with DaemonAgentComponent
{
  def start()

  def exit()
}

trait EnvironmentImpl extends Environment with BackendComponent with DocumentServerComponentImpl with DirectoryComponentImpl with DaemonAgentComponent
{
  val system = ActorSystem("DocRegWebSystem")
  import system._
  val daemonAgent = actorOf(Props[DaemonAgentImpl], "DaemonAgent")
  val backend = actorOf(Props(new Backend(directory, daemonAgent, documentServer)), "Backend")
  val poller = actorOf(Props(new ChangePoller(Backend.server, backend, daemonAgent)))

  def start()
  {
    documentServer.start()
    backend ! Connect()
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