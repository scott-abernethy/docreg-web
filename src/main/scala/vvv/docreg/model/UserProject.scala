package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class UserProject extends LongKeyedMapper[UserProject] with IdPK {
  def getSingleton = UserProject

  object user extends LongMappedMapper(this, User) {
    override def dbIndexed_? = true
  }
  object project extends LongMappedMapper(this, Project) {
    override def dbIndexed_? = true
  }
  object selected extends MappedBoolean(this)
}

object UserProject extends UserProject with LongKeyedMetaMapper[UserProject] {
  override def dbIndexes = UniqueIndex(user, project) :: super.dbIndexes

  def userSelected(user: User): Seq[Project] = {
    for {
      userProject <- findAll(By(UserProject.user, user.id), By(UserProject.selected, true))
      project <- userProject.project.obj
    } yield project 
  }
}
