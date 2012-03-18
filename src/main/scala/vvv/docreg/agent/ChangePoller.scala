package vvv.docreg.agent

import actors.Actor
import net.liftweb.util.Schedule
import java.util.concurrent.ScheduledFuture
import net.liftweb.common.Loggable
import vvv.docreg.util.Millis
import com.hstx.docregsx.Document

case class Changed(documentInfo: DocumentInfo)

class ChangePoller(hostname: String, consumer: Actor, agent: Actor) extends Actor with Loggable
{
  // todo use akka watchdog
  val pollInterval = 5000L
  val wakeInterval: Long = pollInterval * 5
  val pollReplyTimeout: Long = pollInterval * 20

  var lastChangeNumber: Int = -1
  var lastPoll = Millis.zero()
  var lastReply = Millis.zero()
  var lastDocumentInfo: Option[DocumentInfo] = None
  var wakeFuture: Option[ScheduledFuture[Unit]] = None

  def act()
  {
    loop
    {
      react
      {
        case 'Reset =>
        {
          lastChangeNumber = -1
          lastPoll.zero()
          lastReply.zero()
          lastDocumentInfo = None
          this ! 'Poll
        }

        case 'Poll if lastPoll.elapsed_?(0.9 * pollInterval toLong) =>
        {
          //logger.debug("Poll, next change request {" + lastChangeNumber + "}")
          lastPoll.mark()
          agent ! RequestPackage(Actor.self, hostname, NextChangeRequest(lastChangeNumber))
          schedulePoll
          scheduleWake
        }

        case 'Wake =>
        {
          if (lastReply.elapsed_?(pollReplyTimeout))
          {
            logger.warn("Change reply not received in a timely fashion")
            this ! 'Reset
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
            this ! 'Poll

            // Daemon can repeat last changed document message, so ignore repeats
            if (!lastDocumentInfo.exists(_ == documentInfo))
            {
              logger.debug("Change detected in " + documentInfo)
              lastDocumentInfo = Some(documentInfo)
              consumer ! Changed(documentInfo)
            }
          }
        }

        case 'Ping => reply('Pong)

        case 'Die =>
        {
          logger.info("ChangePoller killed")
          exit()
        }

        case other =>
      }
    }
  }
  
  def scheduleWake
  {
    if (wakeFuture.isEmpty || wakeFuture.exists(_.isDone))
    {
      val thiz = Actor.self
      wakeFuture = Some(Schedule.schedule(() => thiz ! 'Wake, wakeInterval))
    }
  }

  def schedulePoll
  {
    val thiz = Actor.self
    Schedule.schedule(() => thiz ! 'Poll, pollInterval)
  }
}

object ChangePoller
{
  def main(args: Array[String])
  {
    import Actor._
    val foo = Actor.actor
    {
      loop
      {
        receive
        {
          case Changed(d) => println(">>>> " + d)
          case other => println(">>>? " + other)
        }
      }
    }

    val agent = new DaemonAgentImpl().start
    val x = new ChangePoller("shelob", foo, agent).start
    x ! 'Reset
  }
}
