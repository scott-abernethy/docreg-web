package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import java.text._
import java.util.TimeZone
import scala.xml.{NodeSeq, Text}
import vvv.docreg.util.DatePresentation

class Revision extends LongKeyedMapper[Revision] with IdPK {
  def getSingleton = Revision

  object document extends LongMappedMapper(this, Document) {
    override def dbIndexed_? = true
  }
  object version extends MappedLong(this)
  object filename extends MappedString(this, 200)
  object author extends MappedString(this, 100)
  object date extends MappedDateTime(this) {
    override def asHtml = Text(if (is != null) DatePresentation.dateTimeF format is else "?")
  }
  object comment extends MappedText(this) {
    /** The default apply for MappedText seems to always dirty the field, odd */
    override def apply(v: String) = {
      if (is != v) super.apply(v) else fieldOwner
    }
  }
  def when: String = DatePresentation.short(date.is)
  def info: String = "/d/" + (document.obj.map(_.key.is) openOr "?") + "/v/" + version
  def link: String = "/d/" + (document.obj.map(_.key.is) openOr "?") + "/v/" + version + "/download"
  def fullTitle: String = (document.obj.map(_.key.is) openOr "?") + "-" + version.is + ": " + (document.obj.map(_.title.is) openOr "?")
}

object Revision extends Revision with LongKeyedMetaMapper[Revision] {
  override def dbIndexes = UniqueIndex(document, version) :: super.dbIndexes
  override def fieldOrder = List(version, filename, author, date, comment)
  def forDocument(document: Document): List[Revision] = {
    Revision.findAll(By(Revision.document, document.id), OrderBy(Revision.version, Descending))
  }
  def forDocument(document: Document, version: Long): Box[Revision] = {
    // Use find instead?
    val rs = Revision.findAll(By(Revision.document, document.id), By(Revision.version, version))
    if (rs nonEmpty) Full(rs head) else Empty
  }
}

object FilteredRevision {
  import vvv.docreg.helper.ProjectSelection
  def findRecent(): List[Revision] = {
    val checked = ProjectSelection.projects.is.toList
    Revision.findAll(In(Revision.document, Document.id, In(Document.project, Project.id, ByList(Project.id, checked map ( _.id.is )))), OrderBy(Revision.date, Descending), MaxRows(20))
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
