package vvv.docreg.agent

import org.specs.Specification
import actors.Actor._
import vvv.docreg.backend.Updated
import actors.{TIMEOUT, Actor}

object ChangePollerTest extends Specification
{
  "ChangePoller" should
  {
    "don't notify if change number is same as last" >>
    {
      val x = new ChangePoller("1.2.3.4", Actor.self, Actor.actor())
      x.start()

      val a: DocumentInfo = DocumentInfo(2, 1, "", "", "", "", "", "", "", "", "", "", "")
      x ! ChangeReply(null, 1, a)
      x !? 'Ping
      receiveWithin(1000)
      {
        case Changed(d) => d must be_==(a)
        case _ => fail("Changed msg expected")
      }

      val b: DocumentInfo = DocumentInfo(456, 2, "sd", "", "", "", "", "", "", "", "", "", "")
      x ! ChangeReply(null, 1, b)
      x !? 'Ping
      receiveWithin(1000)
      {
        case TIMEOUT =>
        case other => fail("no msg expected " + other)
      }

      x ! ChangeReply(null, 3, b)
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

      val info: DocumentInfo = DocumentInfo(2, 1, "", "", "", "", "", "", "", "", "", "", "")

      x ! ChangeReply(null, 1, info)
      x !? 'Ping
      receiveWithin(1000)
      {
        case Changed(d) => d must be_==(info)
        case _ => fail("Changed msg expected")
      }

      x ! ChangeReply(null, 2, info)
      x !? 'Ping
      receiveWithin(1000)
      {
        case TIMEOUT =>
        case other => fail("no msg expected " + other)
      }
    }
  }
}