package vvv.docreg.snippet

import net.liftweb.util.Helpers._
import xml.Text
import vvv.docreg.model.{User, UserLookup}
import net.liftweb.util.{PassThru, ClearClearable}
import net.liftweb.http.js.JsCmds.RedirectTo
import net.liftweb.http.{SHtml, S}
import net.liftweb.mapper._
import net.liftweb.common.{Box, Empty, Full}

class UserLookupSnippet
{
  def unknown =
  {
    val items = UserLookup.findAll(
      In(UserLookup.user, User.id, In(User.name, User.name, Like(User.name, "[Unknown]"))),
      OrderBy(UserLookup.id, Descending)
    )
    ".x-count *" #> (items.size + " records") &
    ".x-item" #> items.map { i=>
      ".x-username *" #> i.username.is &
      ".x-name *" #> i.name.is &
      ".x-email *" #> i.email.is &
      ".x-founduser *" #> i.user.map(_.profileLink).getOrElse(Text("???")) &
      ".x-change [href]" #> ("/admin/user-lookup-change?id=" + i.id)
    }
  }

  def change = {
    var changeTo = ""
    S.param("id").flatMap(UserLookup.find(_)) match {
      case Full(i) => {
        val allUsers: List[User] = User.findAll(OrderBy(User.username, Ascending))
        val selectOptions = allUsers.map(u => (u.id.is.toString,u.shortUsername() + " = " + u.displayName))
        val selected = Box(allUsers.find(_.shortUsername() equalsIgnoreCase i.username).map(_.id.is.toString))
        ".x-username *" #> i.username.is &
        ".x-name *" #> i.name.is &
        ".x-email *" #> i.email.is &
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
    for {
      record <- UserLookup.find(id)
      u <- User.find(userId)
    } {
      record.user(u).save
    }
    S.notice("User Lookup Changed")
    S.redirectTo("/admin/user-lookup")
  }
}
