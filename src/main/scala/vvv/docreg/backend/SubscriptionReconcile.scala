package vvv.docreg.backend

import net.liftweb.common.Loggable
import vvv.docreg.model._
import vvv.docreg.agent.SubscriberInfo
import org.squeryl.PrimitiveTypeMode._

trait SubscriptionReconcile extends Loggable {
  val userLookup: UserLookupProvider

  def reconcileSubscriptions(document: Document, subscriptions: List[SubscriberInfo]) {
    val subscribers: List[(User,String)] = for {
      s <- subscriptions
      u <- userLookup.lookup(Some(s.userName), Some(s.email), None, "subscription on " + document + " for " + s)
    } yield (u -> s.options)

    inTransaction {
      var userSubscriptions = Subscription.forDocument(document).map(s => s.userId -> s).toMap

      subscribers.foreach { i =>
        val u = i._1
        val options = i._2.split(" ")
        val notification = options exists ("always".equalsIgnoreCase)
        val bookmark = options exists ("bookmark".equalsIgnoreCase)

        userSubscriptions.get(u.id) match {
          case Some(s) if (s.notification != notification || s.bookmark != bookmark) => {
            s.notification = options exists ("always".equalsIgnoreCase)
            s.bookmark = options exists ("bookmark".equalsIgnoreCase)
            Subscription.dbTable.update(s)
          }
          case None => {
            val s = new Subscription
            s.documentId = document.id
            s.userId = u.id
            s.notification = options exists ("always".equalsIgnoreCase)
            s.bookmark = options exists ("bookmark".equalsIgnoreCase)

            Subscription.dbTable.insert(s)
          }
          case _ => {} // No change
        }

        userSubscriptions -= u.id
      }

      if (userSubscriptions.size > 0) {
        Subscription.dbTable.deleteWhere(s => (s.documentId === document.id) and (s.userId in userSubscriptions.keySet))
      }
    }
  }
}
