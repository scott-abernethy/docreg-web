package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class Project extends LongKeyedMapper[Project] with IdPK {
  def getSingleton = Project

  object name extends MappedString(this, 60)
}

object Project extends Project with LongKeyedMetaMapper[Project] {
  def forName(name: String) = {
    val xs = findAll(By(Project.name, name), OrderBy(Project.name, Ascending))
    if (xs isEmpty) null else xs head
  }
  override def findAll = findAll(OrderBy(Project.name, Ascending))
}
