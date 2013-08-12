package vvv.docreg.agent.faux

import akka.actor._
import vvv.docreg.agent._

case class AddDocument(info: DocumentInfo, username: String, notifyTo: ActorRef)
case class AddDocumentChange(info: DocumentInfo)

class FauxDaemonAgent(db: ActorRef) extends Actor {

  var submits: Map[String, RegisterRequest] = Map.empty
  var change: (Int, DocumentInfo) = (-1, null)

  def receive = {
    case RequestPackage(replyTo, target, NextChangeRequest(id)) => {
      // Sending back the same id means no change.
      replyTo ! NextChangeReply(change._1, change._2)
    }
    case RequestPackage(replyTo, target, request @ RegisterRequest(fileName, project, comment, access, author, userName, clientHost, clientVersion)) => {
      submits = submits + (fileName -> request) 
      replyTo ! RegisterReply("Accepted", fileName)
    }
    case RequestPackage(replyTo, target, SubmitRequest(fileName, crc)) => {
      replyTo ! SubmitReply("Done", fileName)
      (submits get fileName) match {
        case Some(request) => {
          // Update the file datebase -- it will respond with an AddDocumentChange msg
          db ! AddDocument(DocumentInfo(-1, 0, request.fileName, request.project, request.fileName, request.comment, request.access, request.author, new java.util.Date(), "shelob", request.clientHost, null, null), request.userName, self)

          // Remove stored submit
          submits = submits - fileName
        }
        case _ => {
          // Not found
        }
      }
    }
    case AddDocumentChange(info) => {
      // Update change notification for next poll (note: dev mode  only supports single change at a time)
      change = (change._1 + 1, info)
    }
    case other => {
      unhandled(other)
    }
  }
}
