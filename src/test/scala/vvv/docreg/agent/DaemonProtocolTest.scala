/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.agent

import org.specs2.mutable._
import akka.testkit.TestActorRef
import akka.actor.ActorSystem

class DaemonProtocolTest extends Specification
{
  "DaemonAgentImpl" should
  {
    "have valid transaction ids" >>
    {
      implicit val system = ActorSystem()
      val x = TestActorRef(new DaemonAgentImpl()).underlyingActor
      x.previousTransaction = 0
      x.nextTransaction() must be_==(1)

      x.previousTransaction = 98765
      x.nextTransaction() must be_==(98766)
      x.nextTransaction() must be_==(98767)

      x.previousTransaction = Int.MaxValue - 2
      x.nextTransaction() must be_==(Int.MaxValue - 1)
      x.nextTransaction() must be_==(0)
      x.nextTransaction() must be_==(1)
    }
  }
}