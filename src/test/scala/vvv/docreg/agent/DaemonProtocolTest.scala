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

import org.specs.Specification
import akka.testkit.TestActorRef
import akka.actor.ActorSystem

object DaemonProtocolTest extends Specification
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