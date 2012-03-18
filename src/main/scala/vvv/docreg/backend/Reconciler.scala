package vvv.docreg.backend

import scala.actors._
import scala.actors.Actor._
import vvv.docreg.model._
import com.hstx.docregsx.{Document => AgentDocument, Revision => AgentRevision, Approval => AgentApproval, Subscriber => AgentSubscriber, ApprovalStatus => AgentApprovalState}
import scala.collection.JavaConversions._
import scala.actors.Futures._
import net.liftweb.common.Loggable

case class Prepare(d: AgentDocument, a: Agent)

class Reconciler(private val backend: Backend) extends Actor with Loggable {
  def act() {
    loop {
      receive {
        case Prepare(document, agent) =>
          logger.debug("Preparing " + document.getKey)
          val key = document.getKey
          val revisions = future { agent.loadRevisions(key).toList }
          val approvals = future { agent.loadApprovals(key).toList }
          val subscriptions = future { agent.loadSubscribers(key).toList }
          // todo what if one of these futures never finishes? react within?
          backend ! Reconcile(document, revisions(), approvals(), subscriptions())
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
