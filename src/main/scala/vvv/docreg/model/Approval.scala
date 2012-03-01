package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import scala.xml.Text
import vvv.docreg.util.DatePresentation

class Approval extends LongKeyedMapper[Approval] with IdPK {
  def getSingleton = Approval
  object revision extends MappedLongForeignKey(this, Revision)
  object by extends MappedLongForeignKey(this, User)
  object state extends MappedEnum(this, ApprovalState)
  object date extends MappedDateTime(this) {
    override def asHtml = Text(if (is != null) DatePresentation.formatTime(is) else "?")
  }
  object comment extends MappedString(this, 128)
}

object Approval extends Approval with LongKeyedMetaMapper[Approval] with Loggable {
  override def fieldOrder = List(revision, state, by, date, comment)
  override def dbIndexes = UniqueIndex(revision, by) :: super.dbIndexes
  def forRevision(r: Revision): List[Approval] = findAll(By(Approval.revision, r))
  def forRevisionBy(r: Revision, by: User): Box[Approval] = findAll(By(Approval.revision, r), By(Approval.by, by)) match {
    case Nil => Empty 
    case a :: Nil => Full(a)
    case a :: as => logger.warn("Found duplicate approvals for " + r + " by " + by) ; Full(a)
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
