package vvv.docreg.agent

import actors.Actor
import net.liftweb.util.Schedule
import java.util.concurrent.ScheduledFuture
import vvv.docreg.backend.Updated
import vvv.docreg.agent.DaemonProtocol.documentInfoToAgentDocument
import net.liftweb.common.Loggable
import vvv.docreg.util.Millis
import com.hstx.docregsx.Document

/*
wait 1 sec between polls
if no response, watch dog will poll every 10 secs
 */
class ChangePoller(hostname: String, consumer: Actor) extends Actor with Loggable
{
  // todo use akka watchdog
  val pollInterval = 2000L
  var currentChangeNumber: Int = -1
  var lastPoll = Millis.zero()
  var lastReply = Millis.zero()
  var wakeFuture: ScheduledFuture[Unit] = _

  def act()
  {
    loop
    {
      receive
      {
        case 'Reset =>
        {
          currentChangeNumber = -1
          lastPoll.zero()
          lastReply.zero()
          this ! 'Poll
        }

        case 'Poll if lastPoll.elapsed_?(pollInterval) =>
        {
          lastPoll.mark()
          DaemonProtocol.getNextChange(this, hostname, currentChangeNumber)
          scheduleWake
        }

        case 'Wake =>
        {
          if (lastReply.elapsed_?(pollInterval * 30))
          {
            logger.warn("Change reply not received in a timely fashion")
            this ! 'Reset
            // todo warn consumer to resync
          }
          scheduleWake
        }

        case ChangeReply(header, changeNumber, documentInfo) =>
        {
          lastReply.mark()
          if (changeNumber != currentChangeNumber)
          {
            logger.info("Change detected in " + documentInfo)
            currentChangeNumber = changeNumber
            lastPoll.zero()
            this ! 'Poll
            consumer ! Updated(documentInfo)
          }
          else
          {
            schedulePoll
          }
        }

        case other =>
      }
    }
  }
  
  def scheduleWake
  {
    if (wakeFuture == null || wakeFuture.isDone)
    {
      val thiz = Actor.self
      wakeFuture = Schedule.schedule(() => thiz ! 'Wake, pollInterval * 10)
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
          case Updated(d) => println(">>>> " + d.getDocument + " , " + d.getDate + " , " + d.getEditor)
          case other => println(">>>? " + other)
        }
      }
    }

    val x = new ChangePoller("shelob", foo).start
    x ! 'Reset

  }
}
