package vvv.docreg.backend

import vvv.docreg.agent.{ApprovalInfo, RevisionInfo}
import net.liftweb.util.True
import vvv.docreg.model.{Subscription, Revision, Document}

trait RevisionReconcile
{
  def reconcileRevisions(document: Document, revisions: List[RevisionInfo])
  {
    if (isSmite(revisions))
    {
      Subscription.forDocument(document).foreach(_.delete_!)
      Revision.forDocument(document).foreach(_.delete_!)
      document.delete_!
    }
  }

  def isSmite(revisions: List[RevisionInfo]): Boolean =
  {
    revisions match
    {
      case RevisionInfo(Document.ValidDocumentFileName(key, "000", "Free Document Number!"), _, "B4 First version", _, "System", _, _, _, _, "smite", _, _) :: Nil => true
      case _ => false
    }
  }
}

object RevisionReconcile
{
  import com.hstx.docregsx.{Revision => AgentRevision}

  implicit def agentToInfo(in: AgentRevision): RevisionInfo =
  {
    RevisionInfo(in.getFilename, in.getProject, in.getComment, in.getAccess, in.getAuthor, in.getDate, in.getServer, in.getClientIp, in.getClientPc, in.getUsername, in.getClientVersion, in.getCrc)
  }
}