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
    inTransaction{
      authorizationFor(user, project) match {
        case Some(existing) if (existing.revoked.isEmpty) => {
          existing.revoked = Some(T.now())
          dbTable.update(existing)
        }
        case _ => {}
      }
    }
  }

  def authorizedFor_?(user: User, project: Project): Boolean = {
    authorizationFor(user, project).exists(_.revoked.isEmpty)
  }

  def authorizationFor(user: User, project: Project): Option[ProjectAuthorization] = {
    inTransaction{
      from(dbTable)(pa =>
        where(pa.userId === user.id and pa.projectId === project.id)
        select(pa)
      ).headOption
    }
  }
}
