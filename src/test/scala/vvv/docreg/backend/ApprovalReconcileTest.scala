package vvv.docreg.backend

import org.specs._
import mock.Mockito
import vvv.docreg.db.TestDbVendor
import com.hstx.docregsx.{Approval => AgentApproval}
import vvv.docreg.agent.ApprovalInfo
import java.util.Date
import net.liftweb.common.{Full, Empty}
import org.mockito.Matchers
import vvv.docreg.model.{ApprovalState, UserLookupProvider, Approval, Document}

object ApprovalReconcileTest extends Specification with Mockito
{
  "ApprovalReconcile" should
  {
    "Do nothing if no approvals and no agentapprovals" >>
    {
      TestDbVendor.initAndClean()

      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val (d, r1, r2, r3) = TestDbVendor.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      
      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      val approvals: List[ApprovalInfo] = Nil

      x.reconcileApprovals(d, approvals)
      
      Approval.findAll() must haveSize(0)
    }

    "Add new approvals" >>
    {
      TestDbVendor.initAndClean()

      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val (d, r1, r2, r3) = TestDbVendor.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("aemail")), Matchers.eq(Some("aname")), Matchers.anyString()) returns(Full(u2))
      
      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      val date1 = new Date()
      
      val approvals: List[ApprovalInfo] = List(
        ApprovalInfo(r1.filename.is, "aname", "aemail", "Approved", "Just because", date1, "1.2.3.4", "pcFoo", "me"),
        ApprovalInfo(r3.filename.is, "aname", "aemail", "Not Approved", "Spelling is terrible!", date1, "1.2.3.4", "pcFoo", "me")
      )

      x.reconcileApprovals(d, approvals)

      Approval.findAll() must haveSize(2)
      Approval.forRevision(r1).head.revision.obj must be_==(Full(r1))
      Approval.forRevision(r1).head.by.obj must be_==(Full(u2))
      Approval.forRevision(r1).head.comment.is must be_==("Just because")
      Approval.forRevision(r1).head.state.is must be_==(ApprovalState.approved)
      Approval.forRevision(r1).head.date.is must be_==(date1)
    }

    "Do nothing for existing approvals" >>
    {
      TestDbVendor.initAndClean()

      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val (d, r1, r2, r3) = TestDbVendor.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("aemail")), Matchers.eq(Some("aname")), Matchers.anyString()) returns(Full(u2))
      
      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      val date1 = new Date()
      
      Approval.create.revision(r1).by(u2).state(ApprovalState.approved).comment("Just because").date(date1).save
      
      val approvals: List[ApprovalInfo] = List(
        ApprovalInfo(r1.filename.is, "aname", "aemail", "Approved", "Just because", date1, "1.2.3.4", "pcFoo", "me"),
        ApprovalInfo(r3.filename.is, "aname", "aemail", "Not Approved", "Spelling is terrible!", date1, "1.2.3.4", "pcFoo", "me")
      )

      x.reconcileApprovals(d, approvals)

      Approval.findAll() must haveSize(2)
    }

    "Purge non-existant approvals" >>
    {
      TestDbVendor.initAndClean()

      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val (d, r1, r2, r3) = TestDbVendor.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("aemail")), Matchers.eq(Some("aname")), Matchers.anyString()) returns(None)

      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      val date1 = new Date()

      Approval.create.revision(r1).by(u2).state(ApprovalState.approved).comment("Just because").date(date1).save

      x.reconcileApprovals(d, List.empty)

      Approval.findAll() must haveSize(0)
    }

    "Correct updated approvals" >>
    {
      // Bug reported by Robert Brown 28.03.2012 where approval username changed from system to RB.
      TestDbVendor.initAndClean()

      val (p1, _, _) = TestDbVendor.createProjects
      val (u1, u2) = TestDbVendor.createUsers
      val (d, r1, r2, r3) = TestDbVendor.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("aemail")), Matchers.eq(Some("aname")), Matchers.anyString()) returns(Full(u1))
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("bemail")), Matchers.eq(Some("bname")), Matchers.anyString()) returns(Full(u2))

      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      x.reconcileApprovals(d, List(
        ApprovalInfo(r3.filename.is, "aname", "aemail", "Pending", "", new Date(), "1.2.3.4", "pcFoo", "me2")
      ))

      Approval.findAll() must haveSize(1)
      Approval.forRevision(r3).head.by.obj must be_==(Full(u1))

      x.reconcileApprovals(d, List(
        ApprovalInfo(r3.filename.is, "bname", "bemail", "Pending", "", new Date(), "1.2.3.4", "pcFoo", "me2")
      ))

      Approval.findAll() must haveSize(1)
      Approval.forRevision(r3).head.by.obj must be_==(Full(u2))
    }
  }
}
