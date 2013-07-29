/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.agent

import org.specs2.mutable._
import java.util.Date
import akka.testkit.TestProbe
import akka.actor.{Props, ActorSystem}
import scala.concurrent.duration._

class ChangePollerTest extends Specification
{
  "ChangePoller" should
  {
    val timeout: FiniteDuration = FiniteDuration.apply(1, "seconds")

    "don't notify if change number is same as last" >>
    {
      val system = ActorSystem()
      val probe = new TestProbe(system)
      val x = system.actorOf(Props(new ChangePoller("1.2.3.4", probe.ref, system.deadLetters)))

      val a: DocumentInfo = DocumentInfo(2, 1, "", "", "", "", "", "", new Date, "", "", "", new Date)
      x ! NextChangeReply(1, a)

      probe.expectMsg(timeout, Changed(a))

      val b: DocumentInfo = DocumentInfo(456, 2, "sd", "", "", "", "", "", new Date, "", "", "", new Date)
      x ! NextChangeReply(1, b)
      probe.expectNoMsg(timeout)

      x ! NextChangeReply(3, b)
      probe.expectMsg(timeout, Changed(b))

      1 must be_==(1)
    }

    "not notify repeated changes" >>
    {
      val system = ActorSystem()
      val probe = new TestProbe(system)
      val x = system.actorOf(Props(new ChangePoller("1.2.3.4", probe.ref, system.deadLetters)))

      val d = new Date

      val a: DocumentInfo = DocumentInfo(5174,193,"5174-193-Performance Review Process Check List.xlsx","Eclipse","Performance Review Process Check List","updated for SC SP group","Everyone","RVann",d,"boromir","10.15.153.122","",null)
      val a2: DocumentInfo = DocumentInfo(5174,193,"5174-193-Performance Review Process Check List.xlsx","Eclipse","Performance Review Process Check List","updated for SC SP group","Everyone","RVann",d,"boromir","10.15.153.122","",null)

      x ! NextChangeReply(1, a)
      probe.expectMsg(timeout, Changed(a))

      x ! NextChangeReply(2, a2)
      probe.expectNoMsg(timeout)

      x ! NextChangeReply(2, a2)
      probe.expectNoMsg(timeout)

      1 must be_==(1)
    }
  }
}