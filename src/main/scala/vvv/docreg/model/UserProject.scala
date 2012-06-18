package vvv.docreg.model

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import vvv.docreg.db.{DbSchema, DbObject}
import net.liftweb.mapper.{OrderBy, By}
import org.squeryl.PrimitiveTypeMode._

/**
 * User selected favourite projects. See ProjectAuthorization for user authorized projects.
 */
class UserProject extends DbObject[UserProject] {
  def dbTable = DbSchema.userProjects
  var userId: Long = 0
  var projectId: Long = 0
  var selected: Boolean = true
}

object UserProject extends UserProject {
  def userSelected(user: User): Seq[Project] = {
    inTransaction(
      join(UserProject.dbTable, Project.dbTable)( (up,p) =>
        where(up.userId === user.id and up.selected === true)
        select(p)
        on(up.projectId === p.id)
      ).toSeq
    )
  }

  def set(user: User, project: Project, s: Boolean) {
    inTransaction {
      UserProject.dbTable.where(up => up.userId === user.id and up.projectId === project.id).headOption match {
        case Some(x) => {
          x.selected = s
          dbTable.update(x)
        }
        case _ => {
          val x = new UserProject
          x.userId = user.id
          x.projectId = project.id
          x.selected = s
          dbTable.insert(x)
        }
      }
    }
  }

  def listFor(usero: Option[User]): List[(Project, Boolean)] = {
    usero match {
      case None => {
        Nil
      }
      case Some(user) => {
        inTransaction {
          /*
          p1
          p2  u1
          p3
          p4  u1  u2

          p1
          p2 u1
          p3
          p4 u1
          p4 u2
           */

          val all = Project.findAll()
          val selected = from(UserProject.dbTable)(up =>
            where(up.userId === user.id and up.selected === true)
            select(up.projectId)
          ).toSet

          all.map(p => (p, selected.contains(p.id)))
        }
      }
    }
  }

  def find(user: User, project: Project): Option[UserProject] = {
    inTransaction( dbTable.where(up => up.userId === user.id and up.projectId === project.id).headOption )
  }
}
