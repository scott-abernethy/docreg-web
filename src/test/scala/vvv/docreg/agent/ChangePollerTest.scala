package vvv.docreg.agent

import org.specs.Specification
import actors.Actor._
import actors.{TIMEOUT, Actor}
import java.util.Date

object ChangePollerTest extends Specification
{
  "ChangePoller" should
  {
    "don't notify if change number is same as last" >>
    {
      val x = new ChangePoller("1.2.3.4", Actor.self, Actor.actor())
      x.start()

      val a: DocumentInfo = DocumentInfo(2, 1, "", "", "", "", "", "", new Date, "", "", "", new Date)
      x ! NextChangeReply(1, a)
      x !? 'Ping
      receiveWithin(1000)
      {
        case Changed(d) => d must be_==(a)
        case _ => fail("Changed msg expected")
      }

      val b: DocumentInfo = DocumentInfo(456, 2, "sd", "", "", "", "", "", new Date, "", "", "", new Date)
      x ! NextChangeReply(1, b)
      x !? 'Ping
      receiveWithin(1000)
      {
        case TIMEOUT =>
        case other => fail("no msg expected " + other)
      }

      x ! NextChangeReply(3, b)
      x !? 'Ping
      receiveWithin(1000)
      {
        case Changed(d) => d must be_==(b)
        case _ => fail("Changed msg expected")
      }
    }

    "not notify repeated changes" >>
    {
      val x = new ChangePoller("1.2.3.4", Actor.self, Actor.actor())
      x.start()

      val d = new Date

      val a: DocumentInfo = DocumentInfo(5174,193,"5174-193-Performance Review Process Check List.xlsx","Eclipse","Performance Review Process Check List","updated for SC SP group","Everyone","RVann",d,"boromir","10.15.153.122","",null)
      val a2: DocumentInfo = DocumentInfo(5174,193,"5174-193-Performance Review Process Check List.xlsx","Eclipse","Performance Review Process Check List","updated for SC SP group","Everyone","RVann",d,"boromir","10.15.153.122","",null)

      x ! NextChangeReply(1, a)
      x !? 'Ping
      receiveWithin(1000)
      {
        case Changed(d) => d must be_==(a)
        case _ => fail("Changed msg expected")
      }

      x ! NextChangeReply(2, a2)
      x !? 'Ping
      receiveWithin(1000)
      {
        case TIMEOUT =>
        case other => fail("no msg expected " + other)
      }

      x ! NextChangeReply(2, a2)
      x !? 'Ping
      receiveWithin(1000)
      {
        case TIMEOUT =>
        case other => fail("no msg expected " + other)
      }
    }
  }
}