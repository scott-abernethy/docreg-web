package vvv.docreg.snippet

import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._
import xml.Text
import vvv.docreg.model.{User, UserLookup}
import net.liftweb.mapper.{Like, In, Descending, OrderBy}

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
      ".x-founduser *" #> i.user.map(_.profileLink).getOrElse(Text("???"))
    }
  }
}
