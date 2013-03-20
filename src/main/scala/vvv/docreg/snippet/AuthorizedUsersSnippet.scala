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

package vvv.docreg.snippet

import net.liftweb.util.Helpers._
import xml.{Unparsed, NodeSeq, Text}
import vvv.docreg.model.{ProjectAuthorization, User, UserLookup}
import org.squeryl.PrimitiveTypeMode._
import net.liftweb.util.{PassThru, ClearClearable}
import net.liftweb.http.js.JsCmds.RedirectTo
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.{Box, Empty, Full}

class AuthorizedUsersSnippet {
  def list = {
    val users = User.authorized()
    val authorizations = ProjectAuthorization.allAuthorizations().groupBy(_._1.userId)
    ClearClearable &
    ".x-count *" #> users.size &
    ".x-user" #> users.map{ u =>
      val projects = authorizations.get(u.id).getOrElse(Nil).map(_._2).sortBy(_.name)
      ".x-username *" #> u.shortUsername() &
      ".x-name *" #> u.profileLabel(-1) &
      ".x-access *" #> u.accessLevel().toString() &
      ".x-authorizations *" #> (projects.foldLeft(NodeSeq.Empty)( (xml,p) => xml ++ p.infoLink() ++ Unparsed(", ")))
    }
  }
}
