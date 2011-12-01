package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Project extends LongKeyedMapper[Project] with IdPK {
  def getSingleton = Project

  object name extends MappedString(this, 60)

  def documents(): List[Document] = {
    Document.findAll(By(Document.project, this), OrderBy(Document.key, Descending))
  }

  def contributors(): List[User] = {
    documents().flatMap(_.latest.author.obj).distinct.sortWith(User.sort)
  }
}

object Project extends Project with LongKeyedMetaMapper[Project] {
  override def dbIndexes = UniqueIndex(name) :: super.dbIndexes
  def forName(name: String) = {
    val xs = findAll(By(Project.name, name), OrderBy(Project.name, Ascending))
    if (xs isEmpty) null else xs head
  }
  override def findAll = findAll(OrderBy(Project.name, Ascending))
}
