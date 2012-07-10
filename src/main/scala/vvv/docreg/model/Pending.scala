package vvv.docreg.model

import java.util.Date
import scala.xml.Text
import vvv.docreg.util.DatePresentation
import java.sql.Timestamp
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.db.{DbSchema, DbObject}

class Pending extends DbObject[Pending] {
  def dbTable = DbSchema.pendings
  var userId: Long = 0
  var documentId: Long = 0
  var action: PendingAction.Value = PendingAction.editCancel
  var date: Timestamp = new Timestamp(0)

  def dateOnly() = DatePresentation.formatDay(date)

  def dateAsDT(): String = DatePresentation.formatDateTime(date)

  def dateOnlyWithHint() = <abbr title={dateAsDT()}>{dateOnly()}</abbr>

  def user(): Option[User] = {
    inTransaction( DbSchema.usersToPendings.right(this).headOption )
  }

  def document(): Option[Document] = {
    inTransaction( DbSchema.documentsToPendings.right(this).headOption )
  }
}

object Pending extends Pending {
  def editRequest(u: User, d: Document) {
    inTransaction {
      // perhaps backend will wait and do in order?
      unaction(u, d, PendingAction.editCancel)
      actionNow(u, d, PendingAction.editRequest)
    }
  }

  def editCancel(u: User, d: Document) {
    inTransaction {
      unaction(u, d, PendingAction.editRequest)
      actionNow(u, d, PendingAction.editCancel)
    }
  }

  def unassignEditor(d: Document): Boolean = {
    inTransaction {
      unaction(d, PendingAction.editCancel) // don't care about these for change indication
      unaction(d, PendingAction.editRequest) // fix bug where after submit use is still the editor.
      unaction(d, PendingAction.editing)
    }
  }

  def assignEditor(u: User, d: Document, at: Date): Boolean = {
    inTransaction {
      unaction(u, d, PendingAction.editRequest) // don't care about these for change indication
      val existing = forAction(d, PendingAction.editing)
      ensureEditorOnly(existing, u, d, at)
    }
  }

  private def ensureEditorOnly(pendings: List[Pending], u: User, d: Document, at: Date): Boolean = {
    pendings match {
      case Nil => {
        set(u, d, PendingAction.editing, at)
       true // changed
      }
      case p :: ps if (p.userId == u.id) => {
        Pending.dbTable.deleteWhere(p => p.id in ps.map(_.id))
        false // no change
      }
      case p :: ps => {
        Pending.dbTable.deleteWhere(x => x.id === p.id)
        ensureEditorOnly(ps, u, d, at)
      }
    }
  }

  def forUserDocument(u: User, d: Document): List[Pending] = {
    inTransaction( Pending.dbTable.where(p => p.userId === u.id and p.documentId === d.id).toList )
  }

  def forAction(d: Document, a: PendingAction.Value): List[Pending] = {
    inTransaction( Pending.dbTable.where(p => p.action === a and p.documentId === d.id).toList )
  }

  def forUserAction(u: User, a: PendingAction.Value): List[Pending] = {
    inTransaction( from(Pending.dbTable)( p => where(p.action === a and p.userId === u.id) select(p) orderBy(p.date desc)).toList )
  }

  def forUserAction(u: User, d: Document, a: PendingAction.Value): Option[Pending] = {
    inTransaction( Pending.dbTable.where(p => p.userId === u.id and p.documentId === d.id and p.action === a).headOption )
  }

  private def unaction(d: Document, a: PendingAction.Value): Boolean = {
    val removable: List[Pending] = forAction(d, a)
    Pending.dbTable.deleteWhere(x => x.id in removable.map(_.id))
    removable.size > 0
  }

  private def unaction(u: User, d: Document, a: PendingAction.Value): Boolean = {
    val removable: Option[Pending] = forUserAction(u, d, a)
    removable.foreach( x => Pending.dbTable.deleteWhere(p => p.id === x.id) )
    removable.size > 0
  }

  private def actionNow(u: User, d: Document, a: PendingAction.Value) {
    forUserAction(u, d, a) match {
      case Some(p) => {
        p.date = new Timestamp(System.currentTimeMillis())
        dbTable.update(p)
      }
      case None => {
        set(u, d, a, new Date)
      }
    }
  }

  private def set(u: User, d: Document, a: PendingAction.Value, date: Date) {
    val p = new Pending
    p.userId = u.id
    p.documentId = d.id
    p.action = a
    p.date = new Timestamp(date.getTime)
    Pending.dbTable.insert(p)
  }
}

object PendingAction extends Enumeration {
  val editRequest = Value("Edit Requested")
  val editing = Value("Editing")
  val editCancel = Value("Edit Cancelled")
}