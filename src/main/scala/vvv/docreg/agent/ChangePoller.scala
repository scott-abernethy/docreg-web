/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.agent

import net.liftweb.util.Schedule
import net.liftweb.common.Loggable
import vvv.docreg.util.Millis
import scala.concurrent.duration._
import java.util.concurrent.{TimeUnit}
import akka.actor.{Cancellable, PoisonPill, Actor, ActorRef}
import net.liftweb.http.js.JsCmds._Noop

case class Changed(documentInfo: DocumentInfo)

class ChangePoller(hostname: String, consumer: ActorRef, agent: ActorRef) extends Actor with Loggable {

  // todo use akka watchdog
  val pollInterval = 5000L
  val wakeInterval: Long = pollInterval * 5
  val pollReplyTimeout: Long = pollInterval * 20

  var lastChangeNumber: Int = -1
  var lastPoll = Millis.zero()
  var lastReply = Millis.zero()
  var lastDocumentInfo: Option[DocumentInfo] = None
  var wakeCancellable: Option[Cancellable] = None

  def receive = {

    case 'Reset => {
      lastChangeNumber = -1
      lastPoll.zero()
      lastReply.zero()
      lastDocumentInfo = None
      self ! 'Poll
    }

    case 'Poll if lastPoll.elapsed_?(0.9 * pollInterval toLong) => {
      //logger.debug("Poll, next change request {" + lastChangeNumber + "}")
      lastPoll.mark()
      agent ! RequestPackage(self, hostname, NextChangeRequest(lastChangeNumber))
      schedulePoll
      scheduleWake
    }

    case 'Wake => {
      wakeCancellable = None
      if (lastReply.elapsed_?(pollReplyTimeout)) {
        logger.warn("Change reply not received in a timely fashion")
        self ! 'Reset
        // todo warn consumer to resync
      }
      else {
        schedulePoll
      }
      scheduleWake
    }

    case NextChangeReply(changeNumber, documentInfo) => {
      lastReply.mark()
      if (changeNumber != lastChangeNumber) {
        lastChangeNumber = changeNumber
        lastPoll.zero()
        self ! 'Poll

        // Daemon can repeat last changed document message, so ignore repeats
        if (!lastDocumentInfo.exists(_ == documentInfo))
        {
          logger.debug("Change detected in " + documentInfo)
          lastDocumentInfo = Some(documentInfo)
          consumer ! Changed(documentInfo)
        }
      }
    }

    case 'Ping => sender ! 'Pong

    case 'Die => {
      logger.info("ChangePoller killed")
      self ! PoisonPill
    }
  }
  
  def scheduleWake {
    if (wakeCancellable.isEmpty) {
      val cancellable = context.system.scheduler.scheduleOnce(Duration(wakeInterval, TimeUnit.MILLISECONDS), self, 'Wake)(context.dispatcher)
      wakeCancellable = Some(cancellable)
    }
  }

  def schedulePoll {
    context.system.scheduler.scheduleOnce(Duration(pollInterval, TimeUnit.MILLISECONDS), self, 'Poll)(context.dispatcher)
  }

}

object ChangePollerRig {

  def main(args: Array[String]) {
    import akka.actor.ActorDSL._
    import akka.actor._

    implicit val system = ActorSystem("temp")

    val foo = actor(new Act {
      become {
        case Changed(d) => println("Received... " + d)
        case other => println("Unexpected...? " + other)
      }
    })

    val agent = system.actorOf(Props[DaemonAgentImpl])
    val x = system.actorOf(Props(new ChangePoller("shelob", foo, agent)))
    println("Starting...")
    x ! 'Reset
  }

}
