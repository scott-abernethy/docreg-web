package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Document extends LongKeyedMapper[Document] with IdPK {
  def getSingleton = Document

  object name extends MappedString(this, 20) // unique
  object project extends MappedLongForeignKey(this, Project)
  object title extends MappedString(this, 200)
  object editor extends MappedString(this, 100) 
  def revisions = Revision.forDocument(this)
  def revision(version: Long) = Revision.forDocument(this, version)
  def hasRevision(version: Long, date: java.util.Date): Boolean = {
    val r = revision(version)
    r != null && r.date.is == date
  }
  def latest = if (revisions nonEmpty) revisions head else EmptyRevision
  def author = latest author 
  def dateRevised = latest date 
  def projectName = project.obj.map(_.name.is) openOr "?"
}

object Document extends Document with LongKeyedMetaMapper[Document] {
  override def fieldOrder = List(name, project, title)
  def forName(name: String) = {
    val xs = findAll(By(Document.name, name))
    if (xs isEmpty) null else xs head
  }
}
