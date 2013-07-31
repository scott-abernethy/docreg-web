package vvv.docreg.agent.faux

import akka.actor.Actor
import vvv.docreg.agent.RequestPackage
import vvv.docreg.agent.NextChangeRequest
import vvv.docreg.agent.NextChangeReply

class FauxDaemonAgent extends Actor {
  def receive = {
    case RequestPackage(replyTo, target, NextChangeRequest(id)) => {
      // Sending back the same id means no change.
      replyTo ! NextChangeReply(id, null)
    }
    case other => {
      unhandled(other)
    }
  }
}