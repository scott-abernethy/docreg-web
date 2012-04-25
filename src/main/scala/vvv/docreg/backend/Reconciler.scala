package vvv.docreg.backend

import scala.actors._
import scala.actors.Actor._
import vvv.docreg.model._
import com.hstx.docregsx.{Document => AgentDocument, Revision => AgentRevision, Approval => AgentApproval, Subscriber => AgentSubscriber, ApprovalStatus => AgentApprovalState}
import scala.collection.JavaConversions._
import scala.actors.Futures._
import net.liftweb.common.Loggable
import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import akka.dispatch.Await
import vvv.docreg.agent._

case class Prepare(d: AgentDocument, fileDatabase: ActorRef)

class Reconciler(private val backend: Backend) extends Actor with Loggable {
  def act() {
    loop {
      receive {
        case Prepare(document, fileDatabase) =>
          logger.debug("Preparing " + document.getKey)
          val key = document.getKey

          implicit val timeout = Timeout(60 seconds)

          val futureRevisions  = fileDatabase ? GetLog(key)
          val futureApprovals = fileDatabase ? GetApproval(key)
          val futureSubscriptions = fileDatabase ? GetMail(key)

          // TODO change to non-blocking
          val revisions = Await.result(futureRevisions, timeout.duration) match {
            case ResponseLog(_, items) => items
            case _ => Nil
          }
          val approvals = Await.result(futureApprovals, timeout.duration) match {
            case ResponseApproval(_, items) => items
            case _ => Nil
          }
          val subscriptions = Await.result(futureSubscriptions, timeout.duration) match {
            case ResponseMail(_, items) => items
            case _ => Nil
          }

//          val revisions = future { agent.loadRevisions(key).toList }
//          val approvals = future { agent.loadApprovals(key).toList }
//          val subscriptions = future { agent.loadSubscribers(key).toList }
          // todo what if one of these futures never finishes? react within?
          backend ! Reconcile(document, revisions, approvals, subscriptions)
        case _ =>
      }
    }
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
