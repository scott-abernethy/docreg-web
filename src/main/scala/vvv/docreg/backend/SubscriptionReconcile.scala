/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import net.liftweb.common.Loggable
import vvv.docreg.model._
import vvv.docreg.agent.SubscriberInfo
import org.squeryl.PrimitiveTypeMode._

trait SubscriptionReconcile extends Loggable {
  val userLookup: UserLookupProvider

  def reconcileSubscriptions(document: Document, subscriptions: List[SubscriberInfo]) {
    val subscribers: List[(Long,String)] = for {
      s <- subscriptions
      u <- userLookup.lookup(Some(s.userName), Some(s.email), None, "subscription on " + document + " for " + s)
    } yield (u.id -> s.options)

    inTransaction {
      var userSubscriptions = Subscription.forDocument(document).map(s => s.userId -> s).toMap

      // converting to a map makes the user distinct, and takes the last user option as the valid option.
      subscribers.toMap.foreach { i =>
        val uid = i._1
        val options = i._2.toLowerCase
        val notification = options contains "always"
        val bookmark = options contains "bookmark"

        userSubscriptions.get(uid) match {
          case Some(s) if (s.notification != notification || s.bookmark != bookmark) => {
            s.notification = notification
            s.bookmark = bookmark
            Subscription.dbTable.update(s)
          }
          case None => {
            val s = new Subscription
            s.documentId = document.id
            s.userId = uid
            s.notification = notification
            s.bookmark = bookmark

            Subscription.dbTable.insert(s)
          }
          case _ => {} // No change
        }

        userSubscriptions -= uid
      }

      if (userSubscriptions.size > 0) {
        Subscription.dbTable.deleteWhere(s => (s.documentId === document.id) and (s.userId in userSubscriptions.keySet))
      }
    }
  }
}
