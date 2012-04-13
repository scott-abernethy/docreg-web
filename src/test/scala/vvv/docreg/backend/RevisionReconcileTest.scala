package vvv.docreg.backend

import org.specs.Specification
import org.specs.mock.Mockito
import vvv.docreg.db.TestDbVendor
import vvv.docreg.agent.RevisionInfo
import java.util.Date
import vvv.docreg.model.{Subscription, Revision, Document, UserLookupProvider}

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

      // Hmm?
//      x.reconcileRevisions(d, Nil)

      Document.forKey("234").toOption must beSomething
      Revision.forDocument(d.id.is) must haveSize(3)
      Subscription.forDocument(d) must haveSize(1)

      // Reconcile with same key, but the smite info
      x.reconcileRevisions(d, RevisionInfo("0234-000-Free Document Number!", "DocReg", "B4 First version", "Everyone", "System", new Date(), "boromir",	"10.16.9.68",	"smite", "smite", "smite", "") :: Nil)

      // Delete the doco?
      Document.forKey("234").toOption must beNone
      Revision.forDocument(d.id.is) must beEmpty
      Subscription.forDocument(d) must beEmpty
    }
  }
}
