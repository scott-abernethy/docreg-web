package vvv.docreg.backend

import vvv.docreg.agent.{ApprovalInfo, RevisionInfo}
import net.liftweb.util.True
import net.liftweb.common.Loggable
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.model._
import java.sql.Timestamp

abstract class ReconcileAction
case class ReconcileRevisionAdded(added: Long) extends ReconcileAction
case object ReconcileRevisionUpdated extends ReconcileAction
case object ReconcileDocumentRemoved extends ReconcileAction

trait RevisionReconcile extends Loggable
{
  val userLookup: UserLookupProvider

  def reconcileRevisions(document: Document, revisions: List[RevisionInfo]): Set[ReconcileAction] =
  {
    if (isSmite(revisions))
    {
      logger.warn("Smiting document " + document.key + " due to " + revisions)
      Subscription.dbTable.deleteWhere(s => s.documentId === document.id)
      Pending.dbTable.deleteWhere(p => p. documentId === document.id)
      // Approvals
      Revision.dbTable.deleteWhere(r => r.documentId === document.id)
      Document.dbTable.deleteWhere(d => d.id === document.id)
      Set(ReconcileDocumentRemoved)
    }
    else
    {
      var existing = Revision.forDocument(document).toSet

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
          val record = document.revision(version.toLong).getOrElse(new Revision)
          record.documentId = document.id
          val user = userLookup.lookup(Some(r.clientUserName), None, Some(r.author), "revision on " + record + " for " + r)
          record.version = version.toLong
          record.filename = fullFileName
          record.authorId = user.map(_.id).getOrElse(-1)
          record.date = new Timestamp(date.getTime)
          record.comment = comment

          Revision.dbTable.insertOrUpdate(record)
//          {
//            results += (if (!record.id.defined_?) ReconcileRevisionAdded(version.toLong) else ReconcileRevisionUpdated)
//            record.save
//          }
          existing -= record
        }
      }

      // Remove left overs
      if (existing.size > 0) {
        logger.warn("Purging invalid revisions " + existing)
        Revision.dbTable.deleteWhere(r => r.id in existing.map(_.id))
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