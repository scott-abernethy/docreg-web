/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import vvv.docreg.agent.ApprovalInfo
import vvv.docreg.model._
import net.liftweb.common.{Box, Loggable, Full}
import java.sql.Timestamp
import org.squeryl.PrimitiveTypeMode._

/*
  private def applyApprovals(document: Document, approvals: Iterable[AgentApproval]) = approvals foreach { a =>
    // The agent returns a single approval item per revision/user pair, so no need to cull.
    Revision.forDocument(document, a.getVersion) match {
      case Full(revision) =>
        val user = UserLookup.lookup(Some(a.getUsername()), Some(a.getApproverEmail), Some(a.getApproverName), directory, "approval " + a + " on " + document) openOr null
        val approval = Approval.forRevisionBy(revision, user) match {
          case Full(a) => a
          case _ => Approval.create.revision(revision).by(user)
        } 
        approval.state(ApprovalState.parse(a.getStatus.toString))
        approval.date(a.getDate)
        approval.comment(a.getComment)
        approval.save
      case _ => 
        logger.warn("Approval found with no matching revision: " + a)
    }
  }
 */

trait ApprovalReconcile extends Loggable
{
  val userLookup: UserLookupProvider
  
  def reconcileApprovals(document: Document, approvals: List[ApprovalInfo])
  {
    var existing = Approval.forDocument(document).toSet

    approvals.foreach
    {
      case i @ ApprovalInfo(Document.ValidDocumentFileName(key, version, _), name, email, status, comment, date, _, _, _) =>
      {
        // The agent returns a single approval item per revision/user pair, so no need to cull.
        Revision.forDocument(document, version.toLong) match 
        {
          case Some(revision) =>
          {
            val user = userLookup.lookup(None, Some(email), Some(name), "approval " + i + " on " + document) openOr null
            val approval = Approval.forRevisionBy(revision, user) match {
              case Some(a) =>
              {
                existing -= a
                a
              }
              case _ =>
              {
                val a = new Approval
                a.revisionId = revision.id
                a.userId = user.id
                a
              }
            } 
            approval.state = ApprovalState.parse(status)
            approval.date = new Timestamp(date.getTime)
            approval.comment = comment
            approval.rawUser = name
            Approval.dbTable.insertOrUpdate(approval)
          }
          case _ =>
          {
            logger.warn("Approval found with no matching revision: " + i)
          }
        }
      }
    }

    // Remove left overs
    // TODO perhaps this is a bit harsh? Replace with invalid db flag? Either that or only reconcile when file successfully received? This will probably remove the approval that the user just added.
    if (existing.size > 0) {
      logger.warn("Purging invalid approvals " + existing)
      Approval.dbTable.deleteWhere(a => a.id in existing.map(_.id))
    }
  }
}