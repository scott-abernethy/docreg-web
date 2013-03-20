/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package vvv.docreg.backend

import vvv.docreg.agent.{ApprovalInfo, RevisionInfo}
import net.liftweb.util.True
import net.liftweb.common.Loggable
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.model._
import java.sql.Timestamp
import vvv.docreg.util.T

abstract class ReconcileAction
case class ReconcileRevisionAdded(added: Long) extends ReconcileAction
case object ReconcileRevisionUpdated extends ReconcileAction
case object ReconcileDocumentRemoved extends ReconcileAction
case object ReconcileRevisionPurged extends ReconcileAction

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
      // todo approvals
      Revision.dbTable.deleteWhere(r => r.documentId === document.id)
      Document.dbTable.deleteWhere(d => d.id === document.id)
      Set(ReconcileDocumentRemoved)
    }
    else
    {
      val existing = Revision.forDocument(document).toSet

      val filtered = revisions
      .groupBy{ _.fileName match {
        case Document.ValidDocumentFileName(_, version, _) => version.toLong
        case _ => -1
      }}
      .filter(_._1 > 0)
      .map(_._2.last)

      var found = Set.empty[Revision]
      var results = Set.empty[ReconcileAction]
      filtered.foreach{
        case r @ RevisionInfo(fullFileName @ Document.ValidDocumentFileName(key, version, file), project, comment, access, author, date, _, _, _, clientUserName, clientVersion, _) =>
        {
          val record = document.revision(version.toLong).getOrElse{
            val x = new Revision
            x.documentId = document.id
            x.version = version.toLong
            x
          }
          val user = userLookup.lookup(Some(clientUserName), None, Some(author), "revision on " + record + " for " + r).map(_.id).getOrElse(-1L)
          var changed = false
          if (record.filename != fullFileName) {
            record.filename = fullFileName
            changed = true
          }
          if (record.authorId != user) {
            record.authorId = user
            changed = true
          }
          if (record.rawAuthor != author) {
            record.rawAuthor = author
            changed = true
          }
          if (record.clientVersion != clientVersion) {
            record.clientVersion = clientVersion
            changed = true
          }
          if (record.date != T.at(date)) {
            record.date = T.at(date)
            changed = true
          }
          if (record.comment != comment) {
            record.comment = comment
            changed = true
          }

          if (!record.isPersisted) {
            Revision.dbTable.insert(record)
            results += ReconcileRevisionAdded(version.toLong)
          }
          else if (changed) {
            Revision.dbTable.update(record)
            results += ReconcileRevisionUpdated
          }
          found += record
        }
      }

      // Remove left overs, unless there would be nothing left, which looks like an error.
      val purge = existing -- found
      if (found.size > 0 && purge.size > 0) {
        logger.warn("Purging invalid revisions " + purge)
        Revision.dbTable.deleteWhere(r => r.id in purge.map(_.id))
        results += ReconcileRevisionPurged
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