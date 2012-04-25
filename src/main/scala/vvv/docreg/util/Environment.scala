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

trait EnvironmentImpl extends Environment with BackendComponentImpl with DocumentServerComponentImpl with DirectoryComponentImpl with DaemonAgentComponentImpl
{
  val poller = new ChangePoller(Backend.server, backend, daemonAgent)

  def start()
  {
    documentServer.start()
    backend.start()
    backend ! Connect()
    daemonAgent.start()
    poller.start() ! 'Reset
  }

  def exit()
  {
    documentServer ! 'Die
    backend !? (5000L, 'Die)
    poller ! 'Die
    daemonAgent ! 'Die
  }
}

object Environment
{
  var env: Environment = _
}