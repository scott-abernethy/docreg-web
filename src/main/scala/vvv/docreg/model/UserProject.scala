/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

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
  var interested: Boolean = true
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
          x.interested = true
          dbTable.update(x)
        }
        case _ => {
          val x = new UserProject
          x.userId = user.id
          x.projectId = project.id
          x.selected = s
          x.interested = true
          dbTable.insert(x)
        }
      }
    }
  }

  def clear(userId: Long, projectId: Long) {
    inTransaction {
      UserProject.dbTable.update(up =>
        where(up.userId === userId and up.projectId === projectId)
        set(up.selected := false, up.interested := false)
      )
    }
  }

  def listFor(usero: Option[User], showAll: Boolean): List[(Project, Boolean)] = {
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

          val all = Project.findAllUsed()
          val interest = from(UserProject.dbTable)(up =>
            where(up.userId === user.id and (up.selected === true or up.interested === true))
            select( (up.projectId, up) )
          ).toMap

          all
            .map(p => (p, interest.get(p.id)))
            .filter(x => showAll || x._2.isDefined)
            .map(x => (x._1, x._2.map(_.selected).getOrElse(false)))
        }
      }
    }
  }

  def find(user: User, project: Project): Option[UserProject] = {
    inTransaction( dbTable.where(up => up.userId === user.id and up.projectId === project.id).headOption )
  }
}
