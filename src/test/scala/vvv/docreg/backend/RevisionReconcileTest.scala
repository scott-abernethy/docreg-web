package vvv.docreg.backend

import org.specs.Specification
import org.specs.mock.Mockito
import vvv.docreg.db.TestDbVendor
import vvv.docreg.agent.RevisionInfo
import java.util.Date
import net.liftweb.common.Full
import org.mockito.Matchers
import vvv.docreg.model._
import java.sql.Timestamp
import vvv.docreg.util.T
import org.squeryl.PrimitiveTypeMode._

object RevisionReconcileTest extends Specification with Mockito
{
  "RevisionReconcile" should
  {
    "Handle smite" >>
    {
      TestDbVendor.initAndClean()
      transaction{
      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val (d, r1, r2, r3) = TestDbVendor.createDocument(p1, u1)
      val s = new Subscription
      s.documentId = (d.id)
      s.userId = (u2.id)
      Subscription.dbTable.insert(s)

      val lookup = mock[UserLookupProvider]
      val x = new RevisionReconcile {
        val userLookup = lookup
      }

      Document.forKey("234").toOption must beSomething
      Revision.forDocument(d) must haveSize(3)
      Subscription.forDocument(d) must haveSize(1)

      // Reconcile with same key, but the smite info
      val result = x.reconcileRevisions(d, RevisionInfo("0234-000-Free Document Number!", "DocReg", "B4 First version", "Everyone", "System", new Date(), "boromir",	"10.16.9.68",	"smite", "smite", "smite", "") :: Nil)

      result must containAll(ReconcileDocumentRemoved :: Nil)

      Document.forKey("234").toOption must beNone
      Revision.forDocument(d) must beEmpty
      Subscription.forDocument(d) must beEmpty
      }
    }

    "Smite if there are Nil revisions?" >>
    {

    }

    "Remove missing revisions" >>
    {
      TestDbVendor.initAndClean()
      transaction{
      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val (d, r1, r2, r3) = TestDbVendor.createDocument(p1, u1)
      val s = new Subscription
      s.documentId = (d.id)
      s.userId = (u2.id)
      Subscription.dbTable.insert(s)

      val lookup = mock[UserLookupProvider]
      val x = new RevisionReconcile {
        val userLookup = lookup
      }

      lookup.lookup(Matchers.eq(Some("aaa")), Matchers.eq(None), Matchers.eq(Some("foo")), Matchers.anyString()) returns(Full(u1))

      Document.forKey("234").toOption must beSomething
      Revision.forDocument(d) must haveSize(3)
      Subscription.forDocument(d) must haveSize(1)

      // only 2 revisions.
      val result = x.reconcileRevisions(d,
        RevisionInfo("0234-001-foo.txt", "p1", "ONe", "Everyone", "foo", new Date(), "boromir",	"10.16.9.68",	"pc123", "aaa", "123", "356345") ::
        RevisionInfo("0234-002-foo.txt", "p1", "22222", "Everyone", "foo", new Date(), "boromir",	"10.16.9.68",	"pc123", "aaa", "123", "35352") ::
          Nil
      )

      result must containAll(ReconcileRevisionUpdated :: Nil)

      Document.forKey("234").toOption must beSomething
      Revision.forDocument(d) must haveSize(2)
      Revision.forDocument(d, 3) must beNone
      Subscription.forDocument(d) must haveSize(1)
      }
    }

    "Add first revision" >>
    {
      TestDbVendor.initAndClean()
      transaction{
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      var d = new Document
      d.number = ("336")
      d.projectId = (p2.id)
      d.title = ("Foo bar")
      d.access = ("Everyone")
      d = Document.dbTable.insert(d)

      val lookup = mock[UserLookupProvider]
      val x = new RevisionReconcile {
        val userLookup = lookup
      }

      lookup.lookup(Matchers.eq(Some("jroads")), Matchers.eq(None), Matchers.eq(Some("Johnty Roads")), Matchers.anyString()) returns(Full(u1))

      val now: Date = new Date()

      val result = x.reconcileRevisions(d, RevisionInfo("0336-001-Foo bar.txt", "p2", "Initial version of foo bar baz doco.", "Everyone", "Johnty Roads", now, "boromir", "1.2.3.4", "pc983", "jroads", "v9", "1314324") :: Nil)

      result must containAll(ReconcileRevisionAdded(1) :: Nil)

      Revision.forDocument(d) must haveSize(1)
      val r = Revision.forDocument(d, 1).getOrElse(null)
      r.documentId must be_==(d.id)
      r.version must be_==(1)
      r.filename must be_==("0336-001-Foo bar.txt")
      r.authorId must be_==(u1.id)
      r.date must be_==(now)
      r.comment must be_==("Initial version of foo bar baz doco.")
      r.rawAuthor must be_==("Johnty Roads")
      r.clientVersion must be_==("v9")
      }
    }

    "Do nothing for existing revision that has not changed" >>
    {
      TestDbVendor.initAndClean()
      transaction{
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      var d = new Document
      d.number = ("336")
      d.projectId = (p2.id)
      d.title = ("Foo bar")
      d.access = ("Everyone")
      d = Document.dbTable.insert(d)

      val now = T.now()
      val r = new Revision
      r.documentId = (d.id)
      r.version = (1)
      r.filename = ("0336-001-Foo bar.txt")
      r.authorId = (u1.id)
      r.date = (now)
      r.comment = ("Initial version of foo bar baz doco.")
      r.rawAuthor = "Johnty Roads"
      r.clientVersion = "v9"
      Revision.dbTable.insert(r)

      val lookup = mock[UserLookupProvider]
      val x = new RevisionReconcile {
        val userLookup = lookup
      }

      lookup.lookup(Matchers.eq(Some("jroads")), Matchers.eq(None), Matchers.eq(Some("Johnty Roads")), Matchers.anyString()) returns(Full(u1))

      val result = x.reconcileRevisions(d, RevisionInfo("0336-001-Foo bar.txt", "p2", "Initial version of foo bar baz doco.", "Everyone", "Johnty Roads", now, "boromir", "1.2.3.4", "pc983", "jroads", "v9", "1314324") :: Nil)

      result must beEmpty

      Revision.forDocument(d) must haveSize(1)
      val r2 = Revision.forDocument(d, 1).getOrElse(null)
      r2.documentId must be_==(d.id)
      r2.version must be_==(1)
      r2.filename must be_==("0336-001-Foo bar.txt")
      r2.authorId must be_==(u1.id)
      r2.date must be_==(now)
      r2.comment must be_==("Initial version of foo bar baz doco.")
      }
    }

    "Add and update if detected" >>
    {
      TestDbVendor.initAndClean()
      transaction{
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      var d = new Document
      d.number = ("336")
      d.projectId = (p2.id)
      d.title = ("Foo bar")
      d.access = ("Everyone")
      d = Document.dbTable.insert(d)

      val now = T.now
      val earlier = T.ago(100000)
      val r = new Revision
      r.documentId = (d.id)
      r.version = (1)
      r.filename = ("0336-001-Foo bar.txt")
      r.authorId = (u1.id)
      r.date = (earlier)
      r.comment = ("Initial version of foo bar baz doco.")
      Revision.dbTable.insert(r)

      val lookup = mock[UserLookupProvider]
      val x = new RevisionReconcile {
        val userLookup = lookup
      }

      lookup.lookup(Matchers.eq(Some("jroads")), Matchers.eq(None), Matchers.eq(Some("Johnty Roads")), Matchers.anyString()) returns(Full(u1))
      lookup.lookup(Matchers.eq(Some("shawking")), Matchers.eq(None), Matchers.eq(Some("Dr. Hawking")), Matchers.anyString()) returns(Full(u2))

      val result = x.reconcileRevisions(d,
        RevisionInfo("0336-001-Foo bar.txt", "p2", "Initial version.", "Everyone", "Johnty Roads", earlier, "boromir", "1.2.3.4", "pc983", "jroads", "v9", "1314324") ::
        RevisionInfo("0336-002-Foo bar baz.txt", "p2", "UPdated.", "Everyone", "Dr. Hawking", now, "boromir", "99.99.99.99", "pc111-ubu", "shawking", "v9.1", "989876") ::
          Nil)

      result must containAll(ReconcileRevisionUpdated :: ReconcileRevisionAdded(2) :: Nil)

      Revision.forDocument(d) must haveSize(2)
      Revision.forDocument(d, 1).map(_.comment) must beSome("Initial version.")
      Revision.forDocument(d, 2).map(_.comment) must beSome("UPdated.")
      }
    }

    "When duplicate info found for a revision, ignore earlier ones" >>
    {
      TestDbVendor.initAndClean()
      transaction{
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      var d = new Document
      d.number = ("336")
      d.projectId = (p2.id)
      d.title = ("Foo bar")
      d.access = ("Everyone")
      d = Document.dbTable.insert(d)

      val now = T.now()
      val earlier = T.ago(100000)
      val r = new Revision
      r.documentId = (d.id)
      r.version = (1)
      r.filename = ("0336-001-Foo bar.txt")
      r.authorId = (u1.id)
      r.date = (earlier)
      r.comment = ("Initial version of foo bar baz doco.")
      Revision.dbTable.insert(r)

      val lookup = mock[UserLookupProvider]
      val x = new RevisionReconcile {
        val userLookup = lookup
      }

      // Only one lookup will be performed
      lookup.lookup(Matchers.eq(Some("shawking")), Matchers.eq(None), Matchers.eq(Some("Dr. Hawking")), Matchers.anyString()) returns(Full(u2))

      val result = x.reconcileRevisions(d,
        RevisionInfo("0336-001-Foo bar.txt", "p2", "Initial version.", "Everyone", "Johnty Roads", earlier, "boromir", "1.2.3.4", "pc983", "jroads", "v9", "1314324") ::
          RevisionInfo("0336-001-IGNORED FILE NAME.txt", "fooos", "-------------", "---", "--", now, "boromir", "99.99.99.99", "pc111-ubu", "shawking", "v9.1", "989876") ::
          RevisionInfo("0336-001-Foo bar.txt", "p2", "Initial version 2.", "Everyone", "Dr. Hawking", now, "boromir", "99.99.99.99", "pc111-ubu", "shawking", "v9.1", "989876") ::
          Nil)

      result must containAll(ReconcileRevisionUpdated :: Nil)

      Revision.forDocument(d) must haveSize(1)
      Revision.forDocument(d, 1).map(_.comment) must beSome("Initial version 2.")
      }
    }
  }
}
