package vvv.docreg.model

import _root_.net.liftweb.common._
import java.text._
import java.util.TimeZone
import scala.xml.{NodeSeq, Text}
import vvv.docreg.util.DatePresentation
import net.liftweb.util._
import java.sql.Timestamp
import vvv.docreg.db.{DbSchema, DbObject}
import org.squeryl.PrimitiveTypeMode._

class Revision extends DbObject[Revision] {
  def dbTable = DbSchema.revisions
  var documentId: Long = 0
  var version: Long = 0
  var filename: String = ""
  var authorId: Long = 0
  var date: Timestamp = new Timestamp(0)
  var comment: String = ""

  def document(): Option[Document] = {
    inTransaction( DbSchema.documentsToRevisions.right(this).headOption )
  }

  def author(): Option[User] = {
    inTransaction( DbSchema.usersToRevisions.right(this).headOption )
  }

  def when() = DatePresentation.short(date)

  def dateOnly() = DatePresentation.formatDay(date)

  def dateAsDT(): String = DatePresentation.formatDateTime(date)
}

object Revision extends Revision {
  def forDocument(document: Document): List[Revision] = forDocument(document.id)

  def forDocument(documentId: Long): List[Revision] =
  {
    inTransaction( from(dbTable)(r => where(r.documentId === documentId) select(r) orderBy(r.version desc)).toList )
  }

  def forDocument(document: Document, version: Long): Option[Revision] =
  {
    inTransaction( from(dbTable)(r => where(r.documentId === document.id and r.version === version) select(r)).headOption )
  }
}

object FilteredRevision {
  import vvv.docreg.helper.ProjectSelection
  def findRecent(limit: Long): List[Revision] = {
    if (ProjectSelection.showAll.is) {
      inTransaction( from(Revision.dbTable)(r => select(r) orderBy(r.date desc)).page(0, limit.toInt).toList )
    } else {
      val checked = ProjectSelection.projects.is.toList
      inTransaction(
        join(Revision.dbTable, Document.dbTable)( (r, d) =>
          where(d.projectId in checked.map(_.id))
          select(r)
          orderBy(r.date desc)
          on(r.documentId === d.id)
        ).page(0, limit.toInt).toList
      )
    }
  }
}

object EmptyRevision extends Revision {
  authorId = inTransaction( UserLookup.unknownUser.map(_.id).getOrElse(0L) )
}
