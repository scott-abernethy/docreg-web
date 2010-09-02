package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import java.text._
import scala.xml.{NodeSeq, Text}

class Revision extends LongKeyedMapper[Revision] with IdPK {
  def getSingleton = Revision

  object document extends MappedLongForeignKey(this, Document) {
    override def dbIndexed_? = true
  }
  object version extends MappedLong(this) // unique?
  object filename extends MappedString(this, 200)
  object author extends MappedString(this, 100)
  object date extends MappedDateTime(this) {
    final val dateFormat = DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT)
    override def asHtml = Text(if (is != null) dateFormat format is else "?")
  }
  object comment extends MappedText(this) {
    /** The default apply for MappedText seems to always dirty the field, odd */
    override def apply(v: String) = {
      if (is != v) super.apply(v) else fieldOwner
    }
  }
  //object server extends MappedLong(this)
}

object Revision extends Revision with LongKeyedMetaMapper[Revision] {
  override def fieldOrder = List(version, filename, author, date, comment)
  def forDocument(document: Document): List[Revision] = {
    Revision.findAll(By(Revision.document, document.id))
  }
  def forDocument(document: Document, version: Long): Revision = {
    val rs = Revision.findAll(By(Revision.document, document.id), By(Revision.version, version))
    if (rs nonEmpty) rs head else null
  }
}

object EmptyRevision extends Revision {
  def EmptyRevision {
    version(0)
    date(new java.util.Date(0))
    author("-")
    filename("")
    comment("")
  }
}
