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
import xml.Text
import vvv.docreg.model.{User, UserLookup}
import org.squeryl.PrimitiveTypeMode._
import net.liftweb.util.{PassThru, ClearClearable}
import net.liftweb.http.js.JsCmds.RedirectTo
import net.liftweb.http.{SHtml, S}
import net.liftweb.common.{Box, Empty, Full}

class UserLookupSnippet
{
  def unknown =
  {
    val items = inTransaction{
      join(User.dbTable, UserLookup.dbTable)( (u,ul) =>
        where(u.name === "[Unknown]")
        select(ul)
        orderBy(ul.id desc)
        on(u.id === ul.userId)
      ).toList
    }

    ".x-count *" #> (items.size.toString + " records") &
    ".x-item" #> items.map { i=>
      ".x-username *" #> i.username &
      ".x-name *" #> i.name &
      ".x-email *" #> i.email &
      ".x-founduser *" #> "???" &
      ".x-change [href]" #> ("/admin/user-lookup-change?id=" + i.id)
    }
  }

  def change = {
    var changeTo = ""
    S.param("id").toOption.flatMap(id => inTransaction(UserLookup.lookup(id.toLong))) match {
      case Some(i) => {
        val allUsers: List[User] = inTransaction(from(User.dbTable)(u => select(u) orderBy(u.username asc)).toList)
        val selectOptions = allUsers.map(u => (u.id.toString,u.shortUsername() + " = " + u.displayName))
        val selected = Box(allUsers.find(x => i.username.exists(_ equalsIgnoreCase x.shortUsername())).map(_.id.toString))
        ".x-username *" #> i.username &
        ".x-name *" #> i.name &
        ".x-email *" #> i.email &
        ".x-user" #> SHtml.select(selectOptions.toSeq, selected, changeTo = _) &
        ".x-submit" #> SHtml.submit("Submit", () => processChange(i.id, changeTo), "class" -> "btn primary") &
        ".x-cancel" #> SHtml.submit("Cancel", () => S.redirectTo("/admin/user-lookup"), "class" -> "btn")
      }
      case _ => {
        PassThru
      }
    }
  }

  def processChange(id: Long, userId: String) {
    transaction(
      update(UserLookup.dbTable)(ul =>
        where(ul.id === id)
        set(ul.userId := userId.toLong)
      )
    )
    S.notice("User Lookup Changed")
    S.redirectTo("/admin/user-lookup")
  }
}
