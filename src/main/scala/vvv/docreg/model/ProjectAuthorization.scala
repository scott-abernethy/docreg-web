package vvv.docreg.model

import vvv.docreg.db.{DbObject, DbSchema}
import org.squeryl.PrimitiveTypeMode._
import java.sql.Timestamp
import vvv.docreg.util.T

/**
 * User authorized projects. See UserProject for user selected favourite projects.
 */
class ProjectAuthorization extends DbObject[ProjectAuthorization] {
  def dbTable = DbSchema.projectAuthorizations
  var projectId: Long = 0
  var userId: Long = 0
  var granted: Timestamp = T.now()
  var revoked: Option[Timestamp] = None
}

object ProjectAuthorization extends ProjectAuthorization {

  def grant(user: User, project: Project) {
    inTransaction{
      authorizationFor(user, project) match {
        case Some(existing) if (existing.revoked.isDefined) => {
          existing.revoked = None
          dbTable.update(existing)
        }
        case None => {
          val x = new ProjectAuthorization
          x.projectId = project.id
          x.userId = user.id
          dbTable.insert(x)
        }
        case _ => {}
      }
    }
  }

  def revoke(user: User, project: Project) {
    revoke(user.id, project.id)
  }

  def revoke(userId: Long, projectId: Long) {
    inTransaction{
      authorizationFor(userId, projectId) match {
        case Some(existing) if (existing.revoked.isEmpty) => {
          existing.revoked = Some(T.now())
          dbTable.update(existing)
        }
        case _ => {}
      }
    }
  }

  def authorizedFor_?(user: User, project: Project): Boolean = authorizedFor_?(user.id, project.id)

  def authorizedFor_?(userId: Long, projectId: Long): Boolean = {
    authorizationFor(userId, projectId).exists(_.revoked.isEmpty)
  }

  def authorizationFor(user: User, project: Project): Option[ProjectAuthorization] = authorizationFor(user.id, project.id)

  def authorizationFor(userId: Long, projectId: Long): Option[ProjectAuthorization] = {
    inTransaction{
      from(dbTable)(pa =>
        where(pa.userId === userId and pa.projectId === projectId)
        select(pa)
      ).headOption
    }
  }

  def authorizedProjectsFor(user: User): List[Project] = {
    inTransaction{
      join(dbTable, Project.dbTable)( (pa, p) =>
        where(pa.userId === user.id and pa.revoked.isNull)
        select(p)
        on(pa.projectId === p.id)
      ).toList
    }
  }

  def authorizedUsersFor(projectId: Long): List[User] = {
    inTransaction{
      join(dbTable, User.dbTable)( (pa, u) =>
        where(pa.projectId === projectId and pa.revoked.isNull)
        select(u)
        orderBy(u.displayName asc)
        on(pa.userId === u.id)
      ).toList
    }
  }

  def allAuthorizations(): List[(ProjectAuthorization,Project)] = {
    inTransaction{
      join(dbTable, Project.dbTable)( (pa, p) =>
        where(pa.revoked.isNull)
          select( (pa,p) )
          on(pa.projectId === p.id)
      ).toList
    }
  }
}
