package vvv.docreg.backend

import org.specs._
import akka.testkit.{TestProbe, TestActorRef}
import akka.actor.{ActorSystem, Props}
import vvv.docreg.model.{Project, Revision, Document}
import vvv.docreg.db.TestDbVendor
import org.squeryl.PrimitiveTypeMode._
import java.sql.Timestamp

object DocumentStreamTest extends Specification {
  "DocumentStream" should {
    "check most recent" >> {
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

      system.shutdown
    }
  }
}
