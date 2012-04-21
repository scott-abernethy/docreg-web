package vvv.docreg.snippet

import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._
import xml.Text
import vvv.docreg.model.{User, UserLookup}
import net.liftweb.mapper.{Like, In, Descending, OrderBy}
import org.squeryl.PrimitiveTypeMode._

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
      ".x-founduser *" #> "???"
    }
  }
}
