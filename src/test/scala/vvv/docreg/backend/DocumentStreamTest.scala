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

package vvv.docreg.backend

import org.specs._
import akka.testkit.{TestProbe, TestActorRef}
import akka.actor.{ActorSystem, Props}
import vvv.docreg.model.{Project, Revision, Document}
import vvv.docreg.db.TestDbVendor
import org.squeryl.PrimitiveTypeMode._
import java.sql.Timestamp

class DocumentStreamTest extends Specification {
  "DocumentStream" should {
    "check most recent" >> {
      TestDbVendor.initAndClean()
      implicit val system = ActorSystem()
      val ref = TestActorRef[DocumentStream]
      val x = ref.underlyingActor
      val now = System.currentTimeMillis
      x.stream must beEmpty
      x.mostRecent(new Timestamp(now - 50000)) must beTrue
      system.shutdown
    }

    "insert and put most recent at top" >> {
      TestDbVendor.initAndClean()

      implicit val system = ActorSystem()
      val ref = TestActorRef[DocumentStream]
      val x = ref.underlyingActor

      transaction {
        val (u,_) = TestDbVendor.createUsers
        val (p,_,_) = TestDbVendor.createProjects
        val (d, r1, r2, r3) = TestDbVendor.createDocument(p, u)

        val now = System.currentTimeMillis
        r1.date = new Timestamp(now - 50000)
        r2.date = new Timestamp(now - 30000)
        r3.date = new Timestamp(now - 123)

        x.stream must beEmpty
        x.insertInStream(d, r1, Some(p)) must beSome(StreamAddition( d,r1,p ))
        x.insertInStream(d, r2, Some(p)) must beSome(StreamAddition( d,r2,p ))
        x.insertInStream(d, r3, Some(p)) must beSome(StreamAddition( d,r3,p ))
        x.stream must haveSize(3)
      }

      system.shutdown
    }

    "insert and if not most recent, send insert" >> {
      TestDbVendor.initAndClean()

      implicit val system = ActorSystem()
      val ref = TestActorRef[DocumentStream]
      val x = ref.underlyingActor

      transaction {
        val (u,_) = TestDbVendor.createUsers
        val (p,_,_) = TestDbVendor.createProjects
        val (d, r1, r2, r3) = TestDbVendor.createDocument(p, u)

        val now = System.currentTimeMillis
        r1.date = new Timestamp(now - 50000)
        r2.date = new Timestamp(now - 30000)
        r3.date = new Timestamp(now - 123)

        x.insertInStream(d, r1, Some(p)) must beSome(StreamAddition( d,r1,p ))
        x.insertInStream(d, r3, Some(p)) must beSome(StreamAddition( d,r3,p ))
        x.insertInStream(d, r2, Some(p)) must beSome(StreamInsert( d,r2,p, (d,r3,p) :: (d,r2,p) :: (d,r1,p) :: Nil ))
        x.stream must haveSize(3)
      }

      system.shutdown
    }

    "insert and ignore those outside of scope" >> {
      TestDbVendor.initAndClean()

      implicit val system = ActorSystem()
      val ref = TestActorRef[DocumentStream]
      val x = ref.underlyingActor

      val now = System.currentTimeMillis
      x.start = new Timestamp(now - 40000)

      transaction {
        val (u,_) = TestDbVendor.createUsers
        val (p,_,_) = TestDbVendor.createProjects
        val (d, r1, r2, r3) = TestDbVendor.createDocument(p, u)

        r1.date = new Timestamp(now - 50000)
        r2.date = new Timestamp(now - 30000)
        r3.date = new Timestamp(now - 123)

        x.insertInStream(d, r1, Some(p)) must beNone
        x.insertInStream(d, r2, Some(p)) must beSome(StreamAddition( d,r2,p ))
        x.insertInStream(d, r3, Some(p)) must beSome(StreamAddition( d,r3,p ))
        x.stream must haveSize(2)
      }

      "and retain only within scope" >> {
        x.retainOnlyWithinScope must haveSize(2)
        x.start = new Timestamp(now - 200)
        x.retainOnlyWithinScope must haveSize(1)
        x.start = new Timestamp(now - 100)
        x.retainOnlyWithinScope must beEmpty
      }

      system.shutdown
    }
  }
}
