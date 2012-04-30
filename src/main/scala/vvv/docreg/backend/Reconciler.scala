package vvv.docreg.backend

import vvv.docreg.model._
import scala.collection.JavaConversions._
import scala.actors.Futures._
import net.liftweb.common.Loggable
import akka.pattern.{ask,pipe}
import akka.util.Timeout
import akka.util.duration._
import akka.dispatch.Await
import vvv.docreg.agent._
import akka.actor.{Actor, ActorRef}

case class Prepare(d: DocumentInfo, fileDatabase: ActorRef)

class Reconciler(private val backend: ActorRef) extends Actor with Loggable {

  protected def receive = {
        case Prepare(document, fileDatabase) => {
          logger.debug("Preparing " + document.getKey())
          val key = document.getKey()

          val timeout = Timeout(60 seconds)

          val futureRevisions = ask(fileDatabase, GetLog(key, document.access))(timeout).map(_ match {
            case ResponseLog(_, items) => Some(items)
            case _ => {
              // A log should not fail.
              // TODO worse than this?!
              logger.warn("Failed to get log for " + List(key, document.access))
              None
            }
          })
          val futureApprovals = ask(fileDatabase, GetApproval(key))(timeout).map(_ match {
            case ResponseApproval(_, items) => items
            case _ => Nil
          })
          val futureSubscriptions = ask(fileDatabase, GetMail(key))(timeout).map(_ match {
            case ResponseMail(_, items) => items
            case _ => Nil
          })

          val result = for {
            rs_? <- futureRevisions
            as <- futureApprovals
            ss <- futureSubscriptions
          } yield rs_?.map(Reconcile(document, _, as, ss))

          // The pipe pattern sends the future result to the actor on future completion, so non-blocking
          pipe(result) to backend
        }
        case _ => {}
      }
}

// these can be done in parallel
/*

just load it all.

wait for reply first.

createDocument(ad)
 - loadRevisions: List[ar]
 - loadSubscribers: List[as]
 - loadApprovals: List[aa]
 */
