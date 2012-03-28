package vvv.docreg.backend

import vvv.docreg.agent.ApprovalInfo
import vvv.docreg.model._
import net.liftweb.common.{Box, Loggable, Full}

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
          case Full(revision) =>
          {
            val user = userLookup.lookup(None, Some(email), Some(name), "approval " + i + " on " + document) openOr null
            val approval = Approval.forRevisionBy(revision, user) match {
              case Full(a) => 
              {
                existing -= a
                a
              }
              case _ =>
              {
                Approval.create.revision(revision).by(user)
              }
            } 
            approval.state(ApprovalState.parse(status))
            approval.date(date)
            approval.comment(comment)
            approval.save
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
    existing.foreach{ invalid =>
      logger.warn("Purging invalid approval " + invalid)
      invalid.delete_!
    }
  }
}

object ApprovalReconcile
{
  import com.hstx.docregsx.{Approval => AgentApproval}
  
  implicit def agentToInfo(in: AgentApproval): ApprovalInfo =
  {
    ApprovalInfo(in.getFilename, in.getApproverName, in.getApproverEmail, in.getStatus.toString, in.getComment, in.getDate, in.getClientIp, in.getClientPc, in.getUsername)
  }
}
