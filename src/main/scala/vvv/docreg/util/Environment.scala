package vvv.docreg.util

import vvv.docreg.backend.Connect._
import vvv.docreg.agent.{ChangePoller, DaemonAgentComponentImpl, DaemonAgentComponent}
import vvv.docreg.backend._

trait Environment extends BackendComponent with DocumentServerComponent with AgentComponent with DirectoryComponent with DaemonAgentComponent
{
  def start()

  def exit()
}

trait EnvironmentImpl extends Environment with BackendComponentImpl with DocumentServerComponentImpl with AgentComponentImpl with DirectoryComponentImpl with DaemonAgentComponentImpl {

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