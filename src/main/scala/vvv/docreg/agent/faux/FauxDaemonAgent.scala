package vvv.docreg.agent.faux

import akka.actor._
import vvv.docreg.agent._
import vvv.docreg.model.Document._
import vvv.docreg.util.StringUtil.filenameWithoutExt
import net.liftweb.common.Loggable

class FauxDaemonAgent(db: ActorRef) extends Actor with Loggable {

  var submits: Map[String, RegisterRequest] = Map.empty
  var change: (Int, DocumentInfo) = (-1, null)

  def receive = {
    case RequestPackage(replyTo, _, NextChangeRequest(id)) => {
      // Reply with the latest stored change.
      replyTo ! NextChangeReply(change._1, change._2)
    }
    case RequestPackage(replyTo, _, r @ RegisterRequest(fileName, _, _, _, _, _, _, _)) => {
      // Store request for later, reply accepted.
      submits = submits + (fileName -> r) 
      replyTo ! RegisterReply("Accepted", fileName)
    }
    case RequestPackage(replyTo, _, SubmitRequest(filename, crc)) => {
      replyTo ! SubmitReply("Done", filename)
      submits.get(filename) match {
        case Some(r) => {
          // Parse document details from filename
          val (num, ver, title) = r.fileName match {
            case ValidDocumentFileName(num, ver, fn) => (num.toInt, ver.toInt, filenameWithoutExt(fn))
            case _ => (-1, 1, filenameWithoutExt(filename))
          }

          // Update the file datebase -- it will respond with an AddDocumentChange msg
          db ! AddDocument(DocumentInfo(num, ver, r.fileName, r.project, title, r.comment, r.access, r.author, new java.util.Date(), "shelob", r.clientHost, null, null), r.userName, self)

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
    case RequestPackage(_, _, other) => {
      logger.warn("Dev mode DaemonAgent does not support this type of request yet: " + other)
    }
    case AddDocumentChange(info) => {
      // Update change notification for next poll (note: dev mode  only supports single change at a time)
      change = (change._1 + 1, info)
    }
  }
}
