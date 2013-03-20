/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package vvv.docreg.agent

import net.liftweb.util.Schedule
import net.liftweb.common.Loggable
import vvv.docreg.util.Millis
import akka.util.duration._
import akka.util.Duration
import java.util.concurrent.{TimeUnit}
import akka.actor.{Cancellable, PoisonPill, Actor, ActorRef}
import net.liftweb.http.js.JsCmds._Noop

case class Changed(documentInfo: DocumentInfo)

class ChangePoller(hostname: String, consumer: ActorRef, agent: ActorRef) extends Actor with Loggable
{
  // todo use akka watchdog
  val pollInterval = 5000L
  val wakeInterval: Long = pollInterval * 5
  val pollReplyTimeout: Long = pollInterval * 20

  var lastChangeNumber: Int = -1
  var lastPoll = Millis.zero()
  var lastReply = Millis.zero()
  var lastDocumentInfo: Option[DocumentInfo] = None
  var wakeCancellable: Option[Cancellable] = None

  protected def receive = {
        case 'Reset =>
        {
          lastChangeNumber = -1
          lastPoll.zero()
          lastReply.zero()
          lastDocumentInfo = None
          self ! 'Poll
        }

        case 'Poll if lastPoll.elapsed_?(0.9 * pollInterval toLong) =>
        {
          //logger.debug("Poll, next change request {" + lastChangeNumber + "}")
          lastPoll.mark()
          agent ! RequestPackage(self, hostname, NextChangeRequest(lastChangeNumber))
          schedulePoll
          scheduleWake
        }

        case 'Wake =>
        {
          wakeCancellable = None
          if (lastReply.elapsed_?(pollReplyTimeout))
          {
            logger.warn("Change reply not received in a timely fashion")
            self ! 'Reset
            // todo warn consumer to resync
          }
          else
          {
            schedulePoll
          }
          scheduleWake
        }

        case NextChangeReply(changeNumber, documentInfo) =>
        {
          lastReply.mark()
          if (changeNumber != lastChangeNumber)
          {
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

        case 'Die =>
        {
          logger.info("ChangePoller killed")
          self ! PoisonPill
        }

        case other =>
  }
  
  def scheduleWake
  {
    if (wakeCancellable.isEmpty)
    {
      val cancellable = context.system.scheduler.scheduleOnce(Duration(wakeInterval, TimeUnit.MILLISECONDS), self, 'Wake)
      wakeCancellable = Some(cancellable)
    }
  }

  def schedulePoll
  {
    context.system.scheduler.scheduleOnce(Duration(pollInterval, TimeUnit.MILLISECONDS), self, 'Poll)
  }
}

object ChangePoller
{
  def main(args: Array[String])
  {
//    import Actor._
//    val foo = Actor.actor
//    {
//      loop
//      {
//        receive
//        {
//          case Changed(d) => println(">>>> " + d)
//          case other => println(">>>? " + other)
//        }
//      }
//    }
//
//    val agent = new DaemonAgentImpl().start
//    val x = new ChangePoller("shelob", foo, agent).start
//    x ! 'Reset
  }
}
