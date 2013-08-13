/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import net.liftweb.common.Loggable
import akka.pattern.{ask,pipe}
import akka.util.Timeout
import vvv.docreg.agent._
import vvv.docreg.agent.FileDatabaseApi._
import akka.actor.{Actor, ActorRef}
import scala.concurrent.duration._
import scala.concurrent.Await
import vvv.docreg.backend.BackendApi._

case class Filing(db: ActorRef)
case class Prepare(d: DocumentInfo)
case class PrepareAlt(number: String)

class Clerk(private val backend: ActorRef) extends Actor with Loggable {

  var fileDatabase: ActorRef = context.system.deadLetters
  val fetchInParallel = false
  val dbTimeout = Timeout(60.seconds)

  def receive = {
    case Filing(db) => {
      fileDatabase = db
    }
        case Prepare(document) => {
          // leave this code as is for 0.8.1, it works, too risky to change.
          logger.debug("Preparing " + document.getKey())
          val key = document.getKey()

          import context.dispatcher
          val futureRevisions = ask(fileDatabase, GetLog(key, document.access))(dbTimeout).map(_ match {
            case ResponseLog(_, items) => Some(items)
            case _ => {
              // A log should not fail.
              // TODO worse than this?!
              logger.warn("Failed to get log for " + List(key, document.access))
              None
            }
          })
          val futureApprovals = ask(fileDatabase, GetApproval(key))(dbTimeout).map(_ match {
            case ResponseApproval(_, items) => items
            case _ => Nil
          })
          val futureSubscriptions = ask(fileDatabase, GetMail(key))(dbTimeout).map(_ match {
            case ResponseMail(_, items) => items
            case _ => Nil
          })

          val result = for {
            rs_? <- futureRevisions
            as <- futureApprovals
            ss <- futureSubscriptions
          } yield rs_?.map(Reconcile(document, _, as, ss))

          if (fetchInParallel) {
            // The pipe pattern sends the future result to the actor on future completion, so non-blocking
            pipe(result) to backend
          }
          else {
            // Alternatively block, useful in dev mode on slow VPN connection
            val x = Await.result(result, dbTimeout.duration)
            backend ! x
          }
        }
        case PrepareAlt(number) => {
          logger.debug("Preparing (alt) " + number)
          // do this later.
          // add to a list.
        }
        case msg => {
          unhandled(msg)
        }
      }
}
