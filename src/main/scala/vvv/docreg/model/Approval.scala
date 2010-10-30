package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Approval extends LongKeyedMapper[Approval] with IdPK {
  def getSingleton = Approval
  object revision extends LongMappedMapper(this, Revision)
  object by extends LongMappedMapper(this, User)
  object state extends MappedEnum(this, ApprovalState)
  object date extends MappedDateTime(this)
  object comment extends MappedString(this, 128)
}

object Approval extends Approval with LongKeyedMetaMapper[Approval] {
  override def fieldOrder = List(revision, state, by, date, comment)
  def forRevision(r: Revision): List[Approval] = findAll(By(Approval.revision, r))
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
}
