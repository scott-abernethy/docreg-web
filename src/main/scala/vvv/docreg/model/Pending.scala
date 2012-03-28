package vvv.docreg.model

import net.liftweb.mapper._
import java.util.Date
import scala.xml.Text
import vvv.docreg.util.DatePresentation

class Pending extends LongKeyedMapper[Pending] with IdPK {
  def getSingleton = {
    Pending
  }

  object user extends MappedLongForeignKey(this, User)
  object document extends MappedLongForeignKey(this, Document)
  object action extends MappedEnum(this, PendingAction)
  object date extends MappedDateTime(this) {
    override def asHtml = Text(if (is != null) DatePresentation.formatDateTime(is) else "?")
  }
}

object Pending extends Pending with LongKeyedMetaMapper[Pending] {
  override def dbIndexes = {
    UniqueIndex(user, document, action) :: super.dbIndexes
  }

  def editRequest(u: User, d: Document) {
    DB.use(DefaultConnectionIdentifier) { c =>
      // perhaps backend will wait and do in order?
      unaction(u, d, PendingAction.editCancel)
      actionNow(u, d, PendingAction.editRequest)
    }
  }

  def editCancel(u: User, d: Document) {
    DB.use(DefaultConnectionIdentifier) { c =>
      unaction(u, d, PendingAction.editRequest)
      actionNow(u, d, PendingAction.editCancel)
    }
  }

  def unassignEditor(d: Document): Boolean = {
    unaction(d, PendingAction.editCancel) // don't care about these for change indication
    unaction(d, PendingAction.editRequest) // fix bug where after submit use is still the editor.
    unaction(d, PendingAction.editing)
  }

  def assignEditor(u: User, d: Document, at: Date): Boolean = {
    unaction(u, d, PendingAction.editRequest) // don't care about these for change indication
    val existing = forAction(d, PendingAction.editing)
    ensureEditorOnly(existing, u, d, at)
  }

  private def ensureEditorOnly(pendings: List[Pending], u: User, d: Document, at: Date): Boolean = {
    pendings match {
      case Nil => {
        Pending.create.user(u).document(d).action(PendingAction.editing).date(at).save
       true // changed
      }
      case p :: ps if (p.user == u) => {
        for (pending <- ps) {
          Pending.delete_!(pending)
        }
        false // no change
      }
      case p :: ps => {
        Pending.delete_!(p)
        ensureEditorOnly(ps, u, d, at)
      }
    }
  }

  def forUserDocument(u: User, d: Document): List[Pending] = {
    Pending.findAll(By(user, u), By(document, d))
  }

  def forAction(d: Document, a: PendingAction.Value): List[Pending] = {
    Pending.findAll(By(document, d), By(action, a))
  }

  def forUserAction(u: User, a: PendingAction.Value): List[Pending] = {
    Pending.findAll(By(user, u), By(action, a), OrderBy(date, Descending), PreCache(document))
  }

  def forUserAction(u: User, d: Document, a: PendingAction.Value): Option[Pending] = {
    Pending.find(By(user, u), By(document, d), By(action, a)).toOption
  }

  private def unaction(d: Document, a: PendingAction.Value): Boolean = {
    val removable: List[Pending] = forAction(d, a)
    for (p <- removable) {
      Pending.delete_!(p)
    }
    removable.size > 0
  }

  private def unaction(u: User, d: Document, a: PendingAction.Value): Boolean = {
    val removable: Option[Pending] = forUserAction(u, d, a)
    for (p <- removable) {
      Pending.delete_!(p)
    }
    removable.size > 0
  }

  private def actionNow(u: User, d: Document, a: PendingAction.Value) {
    forUserAction(u, d, a) match {
      case Some(p) => {
        p.date(new Date)
        p.save
      }
      case None => {
        Pending.create.user(u).document(d).action(a).date(new Date).save
      }
    }
  }
}

object PendingAction extends Enumeration {
  val editRequest = Value("Edit Requested")
  val editing = Value("Editing")
  val editCancel = Value("Edit Cancelled")
}