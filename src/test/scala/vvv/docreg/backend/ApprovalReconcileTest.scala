/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import org.specs2.mutable._
import org.specs2.mock._
import vvv.docreg.db.{TestDbScope, TestDbVendor}
import vvv.docreg.agent.ApprovalInfo
import java.util.Date
import net.liftweb.common.{Full, Empty}
import org.mockito.Matchers
import vvv.docreg.model.{ApprovalState, UserLookupProvider, Approval, Document}
import java.sql.Timestamp

class ApprovalReconcileTest extends Specification with Mockito
{
  sequential

  "ApprovalReconcile" should
  {
    "Do nothing if no approvals and no agentapprovals" >> new TestDbScope
    {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
      val (p1, _, _) = db.createProjects
      val (u1, u2) = db.createUsers
      val (d, r1, r2, r3) = db.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      
      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      val approvals: List[ApprovalInfo] = Nil

      x.reconcileApprovals(d, approvals)
      
      Approval.forDocument(d) must haveSize(0)
      }
    }

    "Add new approvals" >> new TestDbScope
    {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
      val (p1, _, _) = db.createProjects
      val (u1, u2) = db.createUsers
      val (d, r1, r2, r3) = db.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("aemail")), Matchers.eq(Some("aname")), Matchers.anyString()) returns(Full(u2))
      
      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      val date1 = new Timestamp(45677)
      
      val approvals: List[ApprovalInfo] = List(
        ApprovalInfo(r1.filename, "aname", "aemail", "Approved", "Just because", date1, "1.2.3.4", "pcFoo", "me"),
        ApprovalInfo(r3.filename, "aname", "aemail", "Not Approved", "Spelling is terrible!", date1, "1.2.3.4", "pcFoo", "me")
      )

      x.reconcileApprovals(d, approvals)

      Approval.forDocument(d) must haveSize(2)
      Approval.forRevision(r1).head.revisionId must be_==(r1.id)
      Approval.forRevision(r1).head.userId must be_==(u2.id)
      Approval.forRevision(r1).head.comment must be_==("Just because")
      Approval.forRevision(r1).head.state must be_==(ApprovalState.approved)
      Approval.forRevision(r1).head.date must be_==(date1)
      Approval.forRevision(r1).head.rawUser must be_==("aname")
      }
    }

    "Do nothing for existing approvals" >> new TestDbScope
    {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
      val (p1, _, _) = db.createProjects
      val (u1, u2) = db.createUsers
      val (d, r1, r2, r3) = db.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("aemail")), Matchers.eq(Some("aname")), Matchers.anyString()) returns(Full(u2))
      
      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      val date1 = new Timestamp(System.currentTimeMillis())
      
      val aa = new Approval
      aa.revisionId = (r1.id)
      aa.userId = (u2.id)
      aa.state = (ApprovalState.approved)
      aa.comment = ("Just because")
      aa.date = (date1)
      aa.rawUser = "aname"
      Approval.dbTable.insert(aa)
      
      val approvals: List[ApprovalInfo] = List(
        ApprovalInfo(r1.filename, "aname", "aemail", "Approved", "Just because", date1, "1.2.3.4", "pcFoo", "me"),
        ApprovalInfo(r3.filename, "aname", "aemail", "Not Approved", "Spelling is terrible!", date1, "1.2.3.4", "pcFoo", "me")
      )

      x.reconcileApprovals(d, approvals)

      Approval.forDocument(d) must haveSize(2)
      }
    }

    "Purge non-existant approvals" >> new TestDbScope
    {
      import org.squeryl.PrimitiveTypeMode._
      transaction{
      val (p1, _, _) = db.createProjects
      val (u1, u2) = db.createUsers
      val (d, r1, r2, r3) = db.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("aemail")), Matchers.eq(Some("aname")), Matchers.anyString()) returns(None)

      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      val date1 = new Timestamp(System.currentTimeMillis())

      val aa = new Approval
      aa.revisionId = (r1.id)
      aa.userId = (u2.id)
      aa.state = (ApprovalState.approved)
      aa.comment = ("Just because")
      aa.date = (date1)
      Approval.dbTable.insert(aa)

      x.reconcileApprovals(d, List.empty)

      Approval.forDocument(d) must haveSize(0)
      }
    }

    "Correct updated approvals" >> new TestDbScope
    {
      // Bug reported by Robert Brown 28.03.2012 where approval username changed from system to RB.
      import org.squeryl.PrimitiveTypeMode._
      transaction{
      val (p1, _, _) = db.createProjects
      val (u1, u2) = db.createUsers
      val (d, r1, r2, r3) = db.createDocument(p1, u1)

      val lookup = mock[UserLookupProvider]
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("aemail")), Matchers.eq(Some("aname")), Matchers.anyString()) returns(Full(u1))
      lookup.lookup(Matchers.eq(None), Matchers.eq(Some("bemail")), Matchers.eq(Some("bname")), Matchers.anyString()) returns(Full(u2))

      val x = new ApprovalReconcile{
        val userLookup = lookup
      }

      x.reconcileApprovals(d, List(
        ApprovalInfo(r3.filename, "aname", "aemail", "Pending", "", new Date(), "1.2.3.4", "pcFoo", "me2")
      ))

      Approval.forDocument(d) must haveSize(1)
      Approval.forRevision(r3).headOption.map(_.userId) must beSome(u1.id)

      x.reconcileApprovals(d, List(
        ApprovalInfo(r3.filename, "bname", "bemail", "Pending", "", new Date(), "1.2.3.4", "pcFoo", "me2")
      ))

      Approval.forDocument(d) must haveSize(1)
      Approval.forRevision(r3).head.userId must be_==(u2.id)
      Approval.forRevision(r3).head.rawUser must be_==("bname")
      }
    }
  }
}
