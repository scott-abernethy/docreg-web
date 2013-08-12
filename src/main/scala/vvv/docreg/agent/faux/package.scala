package vvv.docreg.agent

import akka.actor.ActorRef
import vvv.docreg.agent.DocumentInfo

package object faux {
  case class AddDocument(info: DocumentInfo, username: String, notifyTo: ActorRef)
  case class AddDocumentChange(info: DocumentInfo)
}
