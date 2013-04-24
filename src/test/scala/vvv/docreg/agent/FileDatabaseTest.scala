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
import akka.actor.{Props, ActorSystem}
import akka.testkit.{TestProbe, TestKit}
import akka.util.Duration
import java.util.concurrent.TimeUnit

class FileDatabaseTest extends Specification
{
  "FileDatabase" should {
    "bar" >> {
//        val system = ActorSystem()
//        import system._
//      val probe = new TestProbe(system)
//        val fileDatabase = actorOf(Props[FileDatabase])
//        fileDatabase.tell(GetRegister, probe.ref)
//      probe.expectNoMsg(Duration.create(60L, TimeUnit.SECONDS))
//      println("lala")
    }
  }
}
