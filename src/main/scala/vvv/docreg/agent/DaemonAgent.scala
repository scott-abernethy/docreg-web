package vvv.docreg.agent

import actors.Actor
import vvv.docreg.backend.Agent

trait DaemonAgent extends Actor

case class Changed(documentInfo: DocumentInfo)

case class NextChange(consumer: Actor, hostname: String, lastChange: Int)

class DaemonAgentImpl extends DaemonAgent
{
  def act()
  {
    loop
    {
      // todo change to react when DaemonProtocol doesn't block
      receive
      {
        case NextChange(consumer, hostname, lastChange) =>
        {
          DaemonProtocol.getNextChange(consumer, hostname, lastChange)
        }

        case _ =>
      }
    }
  }
}