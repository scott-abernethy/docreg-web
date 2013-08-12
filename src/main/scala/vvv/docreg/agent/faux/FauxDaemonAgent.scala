package vvv.docreg.agent.faux

import akka.actor._
import vvv.docreg.agent._

class FauxDaemonAgent(db: ActorRef) extends Actor {

  var submits: Map[String, RegisterRequest] = Map.empty
  var change: (Int, DocumentInfo) = (-1, null)

  def receive = {
    case RequestPackage(replyTo, _, NextChangeRequest(id)) => {
      // Reply with the latest stored change.
      replyTo ! NextChangeReply(change._1, change._2)
    }
    case RequestPackage(replyTo, _, r @ RegisterRequest(fileName, _, _, _, _, _, _, _)) => {
      submits = submits + (fileName -> r) 
      replyTo ! RegisterReply("Accepted", fileName)
    }
    case RequestPackage(replyTo, _, SubmitRequest(filename, crc)) => {
      replyTo ! SubmitReply("Done", filename)
      submits.get(filename) match {
        case Some(r) => {
          // Update the file datebase -- it will respond with an AddDocumentChange msg
          db ! AddDocument(DocumentInfo(-1, 0, r.fileName, r.project, r.fileName, r.comment, r.access, r.author, new java.util.Date(), "shelob", r.clientHost, null, null), r.userName, self)

          // Remove stored submit
          submits = submits - filename
        }
        case _ => {
          // Not found
        }
      }
    }
    case RequestPackage(replyTo, _, EditRequest(_, username)) => {
      replyTo ! EditReply(username)
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
