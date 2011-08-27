package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

class UserProject extends LongKeyedMapper[UserProject] with IdPK {
  def getSingleton = UserProject

  object user extends MappedLongForeignKey(this, User) {
    override def dbIndexed_? = true
  }
  object project extends MappedLongForeignKey(this, Project) {
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

  def set(user: User, project: Project, s: Boolean) {
    UserProject.find(By(UserProject.user, user), By(UserProject.project, project)) match {
      case Full(up) => up.selected(s).save
      case _ => UserProject.create.user(user).project(project).selected(s).save
    }
  }

  def listFor(user: User): List[(Project, Boolean)] = {
    val selectedProjects = userSelected(user).toSet
    for {
      project <- Project.findAll(OrderBy(Project.name, Ascending))
    } yield (project, selectedProjects.contains(project))
  }
}
