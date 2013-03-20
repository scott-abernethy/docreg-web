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

package vvv.docreg.model

import _root_.net.liftweb.common._
import java.text._
import java.util.TimeZone
import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import java.sql.Timestamp
import vvv.docreg.db.{DbSchema, DbObject}
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.util.{T, DatePresentation}

class Revision extends DbObject[Revision] {
  def dbTable = DbSchema.revisions
  var documentId: Long = 0
  var version: Long = 0
  var filename: String = ""
  var authorId: Long = 0
  var rawAuthor: String = ""
  var date: Timestamp = new Timestamp(0)
  var comment: String = ""
  var clientVersion: String = ""

  def document(): Option[Document] = {
    inTransaction( DbSchema.documentsToRevisions.right(this).headOption )
  }

  def author(): Option[User] = {
    inTransaction( DbSchema.usersToRevisions.right(this).headOption )
  }

  def when() = DatePresentation.short(date)

  def dateOnly() = DatePresentation.formatDay(date)

  def dateAsDT(): String = DatePresentation.formatDateTime(date)

  def dateOnlyWithHint() = <abbr title={dateAsDT()}>{dateOnly()}</abbr>
}

object Revision extends Revision {
  def forDocument(document: Document): List[Revision] = forDocument(document.id)

  def forDocument(documentId: Long): List[Revision] =
  {
    inTransaction( from(dbTable)(r => where(r.documentId === documentId) select(r) orderBy(r.version desc)).toList )
  }

  def latestFor(document: Document): Option[Revision] = {
    inTransaction {
      from(dbTable)(r =>
        where(r.documentId === document.id)
        select(r)
        orderBy(r.version desc)
      ).headOption
    }
  }

  def forDocument(document: Document, version: Long): Option[Revision] =
  {
    if (version == Long.MaxValue) {
      latestFor(document)
    }
    else {
      inTransaction( from(dbTable)(r => where(r.documentId === document.id and r.version === version) select(r)).headOption )
    }
  }
}

object FilteredRevision {
  def findRecent(userId: Long): List[(Document,Revision,Project)] = {
    val recentCutoff: Timestamp = T.ago(1000L * 60 * 60 * 24 * 31)
      inTransaction(
        join(Revision.dbTable, Document.dbTable, Project.dbTable)( (r, d, p) =>
          where(r.date > recentCutoff)
          select((d,r,p))
          orderBy(r.date desc)
          on(r.documentId === d.id, d.projectId === p.id)
        ).toList
      )
  }
}

object EmptyRevision extends Revision {
  authorId = inTransaction( UserLookup.unknownUser.map(_.id).getOrElse(0L) )
}
