/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package vvv.docreg.backend

import akka.actor.Actor
import akka.util.Duration
import vvv.docreg.util.Environment
import net.liftweb.common.{Empty, Full}
import vvv.docreg.model.{ProjectAuthorization, Project, User, UserLookup}
import org.squeryl.PrimitiveTypeMode._

class UserStorage extends Actor {
  override def preStart() {
    context.system.scheduler.schedule(Duration("15 min"), Duration("24 hour"), self, 'Check)
    super.preStart()
  }

  protected def receive = {
    case 'Check => {
      val directory = Environment.env.directory

      directory.groupMembers(DirectoryConfig.userGroup()) match {
        case Full(dns) => {
          for (dn <- dns; attributes <- directory.findAttributes(dn)) {
            // Cheap and nasty lookup that should set things right for most users.
            transaction( UserLookup.fromAttributes(attributes, true) )
          }
        }
        case _ => {
          // Ignore
        }
      }

//      for (p <- Project.findAll()) {
//        directory.groupMembers(DirectoryConfig.projectAuthorizationGroup(p.name)) match {
//          case Full(dns) => {
//            val authorized = ProjectAuthorization.authorizedUsersFor(p.id)
//            val users = for {
//              dn <- dns
//              attributes <- directory.findAttributes(dn)
//              user <- transaction( UserLookup.fromAttributes(attributes, true) )
//            } yield user
//            println("Authorized for " + p.name + " = " + users.map(_.displayName))
//          }
//          case Empty => {
//            // Remove all authorizations
//          }
//          case _ => {
//            // Ignore
//          }
//        }
//      }

    }
    case other => {
      unhandled(other)
    }
  }
}
