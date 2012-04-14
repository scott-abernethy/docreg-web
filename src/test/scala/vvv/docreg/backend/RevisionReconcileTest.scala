package vvv.docreg.backend

import org.specs.Specification
import org.specs.mock.Mockito
import vvv.docreg.db.TestDbVendor
import vvv.docreg.agent.RevisionInfo
import java.util.Date
import vvv.docreg.model.{Subscription, Revision, Document, UserLookupProvider}
import net.liftweb.common.Full
import org.mockito.Matchers

object RevisionReconcileTest extends Specification with Mockito
{
  "RevisionReconcile" should
  {
    "Handle smite" >>
    {
      TestDbVendor.initAndClean()
      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val (d, r1, r2, r3) = TestDbVendor.createDocument(p1, u1)
      Subscription.create.document(d).user(u2).save

      val lookup = mock[UserLookupProvider]
      val x = new RevisionReconcile {
        val userLookup = lookup
      }

      Document.forKey("234").toOption must beSomething
      Revision.forDocument(d.id.is) must haveSize(3)
      Subscription.forDocument(d) must haveSize(1)

      // Reconcile with same key, but the smite info
      val result = x.reconcileRevisions(d, RevisionInfo("0234-000-Free Document Number!", "DocReg", "B4 First version", "Everyone", "System", new Date(), "boromir",	"10.16.9.68",	"smite", "smite", "smite", "") :: Nil)

      result must containAll(DocumentRemoved :: Nil)

      // Delete the doco?
      Document.forKey("234").toOption must beNone
      Revision.forDocument(d.id.is) must beEmpty
      Subscription.forDocument(d) must beEmpty
    }

    "Smite if there are Nil revisions?" >>
    {

    }

    "Add first revision" >>
    {
      TestDbVendor.initAndClean()
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val d = Document.create.key("336").project(p2).title("Foo bar").access("Everyone")
      d.save;

      val lookup = mock[UserLookupProvider]
      val x = new RevisionReconcile {
        val userLookup = lookup
      }

      lookup.lookup(Matchers.eq(Some("jroads")), Matchers.eq(None), Matchers.eq(Some("Johnty Roads")), Matchers.anyString()) returns(Full(u1))

      val now: Date = new Date()

      val result = x.reconcileRevisions(d, RevisionInfo("0336-001-Foo bar.txt", "p2", "Initial version of foo bar baz doco.", "Everyone", "Johnty Roads", now, "boromir", "1.2.3.4", "pc983", "jroads", "v9", "1314324") :: Nil)

      result must containAll(RevisionAdded :: Nil)

      Revision.forDocument(d) must haveSize(1)
      val r = Revision.forDocument(d, 1).getOrElse(null)
      r.document.is must be_==(d.id.is)
      r.version.is must be_==(1)
      r.filename.is must be_==("0336-001-Foo bar.txt")
      r.author.is must be_==(u1.id.is)
      r.date.is must be_==(now)
      r.comment.is must be_==("Initial version of foo bar baz doco.")
    }

    "Do nothing for existing revision that has not changed" >>
    {
      TestDbVendor.initAndClean()
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val d = Document.create.key("336").project(p2).title("Foo bar").access("Everyone")
      d.save;

      val now: Date = new Date()
      Revision.create.document(d).version(1).filename("0336-001-Foo bar.txt").author(u1).date(now).comment("Initial version of foo bar baz doco.").save

      val lookup = mock[UserLookupProvider]
      val x = new RevisionReconcile {
        val userLookup = lookup
      }

      lookup.lookup(Matchers.eq(Some("jroads")), Matchers.eq(None), Matchers.eq(Some("Johnty Roads")), Matchers.anyString()) returns(Full(u1))

      val result = x.reconcileRevisions(d, RevisionInfo("0336-001-Foo bar.txt", "p2", "Initial version of foo bar baz doco.", "Everyone", "Johnty Roads", now, "boromir", "1.2.3.4", "pc983", "jroads", "v9", "1314324") :: Nil)

      result must beEmpty

      Revision.forDocument(d) must haveSize(1)
      val r = Revision.forDocument(d, 1).getOrElse(null)
      r.document.is must be_==(d.id.is)
      r.version.is must be_==(1)
      r.filename.is must be_==("0336-001-Foo bar.txt")
      r.author.is must be_==(u1.id.is)
      r.date.is must be_==(now)
      r.comment.is must be_==("Initial version of foo bar baz doco.")
    }

    "Add and update if detected" >>
    {
      TestDbVendor.initAndClean()
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val d = Document.create.key("336").project(p2).title("Foo bar").access("Everyone")
      d.save;

      val now: Date = new Date()
      val earlier: Date = new Date(now.getTime - 100000)
      Revision.create.document(d).version(1).filename("0336-001-Foo bar.txt").author(u1).date(earlier).comment("Initial version of foo bar baz doco.").save

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

      result must containAll(RevisionUpdated :: RevisionAdded :: Nil)

      Revision.forDocument(d) must haveSize(2)
      Revision.forDocument(d, 1).map(_.comment.is).toOption must beSome("Initial version.")
      Revision.forDocument(d, 2).map(_.comment.is).toOption must beSome("UPdated.")
    }

    "When duplicate info found for a revision, ignore earlier ones" >>
    {
      TestDbVendor.initAndClean()
      val (p1, p2, p3) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val d = Document.create.key("336").project(p2).title("Foo bar").access("Everyone")
      d.save;

      val now: Date = new Date()
      val earlier: Date = new Date(now.getTime - 100000)
      Revision.create.document(d).version(1).filename("0336-001-Foo bar.txt").author(u1).date(earlier).comment("Initial version of foo bar baz doco.").save

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

      result must containAll(RevisionUpdated :: Nil)

      Revision.forDocument(d) must haveSize(1)
      Revision.forDocument(d, 1).map(_.comment.is).toOption must beSome("Initial version 2.")
    }
  }
}
