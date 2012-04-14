package vvv.docreg.backend

import vvv.docreg.agent.{ApprovalInfo, RevisionInfo}
import net.liftweb.util.True
import vvv.docreg.model.{UserLookupProvider, Subscription, Revision, Document}

abstract class ReconcileAction
case object RevisionAdded extends ReconcileAction
case object RevisionUpdated extends ReconcileAction
case object DocumentRemoved extends ReconcileAction

trait RevisionReconcile
{
  val userLookup: UserLookupProvider

  def reconcileRevisions(document: Document, revisions: List[RevisionInfo]): Set[ReconcileAction] =
  {
    if (isSmite(revisions))
    {
      Subscription.forDocument(document).foreach(_.delete_!)
      Revision.forDocument(document).foreach(_.delete_!)
      document.delete_!
      Set(DocumentRemoved)
    }
    else
    {
      val filtered = revisions
      .groupBy{ _.fileName match {
        case Document.ValidDocumentFileName(_, version, _) => version.toLong
        case _ => -1
      }}
      .filter(_._1 > 0)
      .map(_._2.last)

      var results = Set.empty[ReconcileAction]
      filtered.foreach{
        case r @ RevisionInfo(fullFileName @ Document.ValidDocumentFileName(key, version, file), project, comment, access, author, date, _, _, _, clientUserName, _, _) =>
        {
          val record = document.revision(version.toLong).getOrElse(Revision.create)
          record.document(document)
          val user = userLookup.lookup(Some(r.clientUserName), None, Some(r.author), "revision on " + record + " for " + r)
    record.version(version.toLong)
          record.filename(fullFileName)
          record.author(user)
          record.date(date)
          record.comment(comment)
          if (record.dirty_?)
          {
            results += (if (!record.id.defined_?) RevisionAdded else RevisionUpdated)
            record.save
          }
        }
      }
      results
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