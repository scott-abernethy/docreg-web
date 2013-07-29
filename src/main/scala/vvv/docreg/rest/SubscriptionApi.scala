/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.rest

import net.liftweb.common.{Full, Box, Empty, Loggable}
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{BadResponse, PlainTextResponse, RedirectResponse, LiftResponse}
import vvv.docreg.model.{Subscription, Document, User}
import vvv.docreg.backend.{UnsubscribeRequested, SubscribeRequested}
import vvv.docreg.util.Environment

object SubscriptionApi extends RestHelper with Loggable {

  serve ("doc" / "unwatch" prefix {
    case Nil Get req => {
      val docO = req.param("key").flatMap(key => Document.forKey(key)).toOption
      (docO, User.loggedInUser.is) match {
        case (None, _) => {
          // TODO note that the document key is probably valid due to earlier validation.
          Full(BadResponse())
        }
        case (Some(doc), Full(user)) => {
          watch(doc, user, false)
          Full(PlainTextResponse("You are no longer watching " + doc.fullTitle))
        }
        case _ => {
          Full(RedirectResponse("/user/signin"))
        }
      }
    }
  })

  def watch(d: Document, u: User, yes: Boolean = true) {
    logger.debug("%s : %s %s".format(if (yes) "Watch" else "Unwatch", d.fullTitle, u.username))
    if (yes) {
      Subscription.addNotification(d, u)
    }
    else {
      Subscription.removeNotification(d, u)
    }
    // TODO could return faster
    val msg = Subscription.optionsFor(d, u) match {
      case Some(options) => SubscribeRequested(d, u, options)
      case None => UnsubscribeRequested(d, u)
    }
    Environment.env.backend ! msg
  }

}
