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

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import scala.xml.Text
import vvv.docreg.util.DatePresentation
import vvv.docreg.db.{DbSchema, DbObject}
import java.sql.Timestamp
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Query

class Approval extends DbObject[Approval] {
  def dbTable = DbSchema.approvals
  var revisionId: Long = 0
  var userId: Long = 0
  var rawUser: String = ""
  var state: ApprovalState.Value = ApprovalState.pending
  var date: Timestamp = new Timestamp(0)
  var comment: String = ""

  def user(): Option[User] = {
    inTransaction( DbSchema.usersToApprovals.right(this).headOption )
  }

  def dateOnly() = DatePresentation.formatDay(date)

  def dateAsDT(): String = DatePresentation.formatDateTime(date)

  def dateOnlyWithHint() = <abbr title={dateAsDT()}>{dateOnly()}</abbr>
}

object Approval extends Approval with Loggable {
  def forRevision(r: Revision): List[Approval] =
  {
    inTransaction( dbTable.where(a => a.revisionId === r.id).toList )
  }
  
  def forRevisionBy(r: Revision, by: User): Option[Approval] =
  {
    inTransaction( dbTable.where(a => a.revisionId === r.id and a.userId === by.id).headOption )
  }

  def forDocument(document: Document): List[Approval] =
  {
    inTransaction(
      join(Document.dbTable, Revision.dbTable, Approval.dbTable)( (d,r,a) =>
        where(d.id === document.id)
        select(a)
        on(d.id === r.documentId, r.id === a.revisionId)
      ).toList
    )
  }
}

object ApprovalState extends Enumeration {
  type ApprovalState = Value
  val approved = Value("Approved")
  val notApproved = Value("Not Approved")
  val pending = Value("Pending")
  def parse(text: String): ApprovalState = text match {
    case "Approved" => approved
    case "Pending" => pending
    case _ => notApproved
  }
  def style(state: Any): String = state match {
    case ApprovalState.approved => "label label-success"
    case ApprovalState.pending => "label"
    case _ => "label label-important"
  }
}
