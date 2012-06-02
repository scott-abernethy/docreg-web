package vvv.docreg.model

import org.specs.runner.{ConsoleRunner, JUnit4}
import org.specs.Specification
import vvv.docreg.db.TestDbVendor
import org.specs.mock.Mockito
import vvv.docreg.backend.{UserAttributes, Directory}
import net.liftweb.common.{Full, Failure, Empty}
import org.squeryl.PrimitiveTypeMode._

class FakeUserAttributes(username: String, mail: String, display: String) extends UserAttributes {
  def userName() = Some(username)

  def email() = Some(mail)

  def displayName() = Some(display)

  def dn() = None

  def department() = None

  def description() = None

  def location() = None

  def memberOf() = Nil
}

object UserLookupTest extends Specification with Mockito {
  "UserLookup" should {
    "Reject empty lookup" >> {
      TestDbVendor.initAndClean()
      val directory = mock[Directory]
      transaction{
        UserLookup.lookup(None, None, None, directory, "") must be_==(Failure("Invalid input"))
      }
    }

    "Recognise system user" >> {
      TestDbVendor.initAndClean()
      transaction{
      UserLookup.installDefaults()
      UserLookup.installDefaults()
      val directory = mock[Directory]
      UserLookup.lookup(Some("smite"), None, Some("System"), directory, "") match {
        case Full(x) =>
          x.username must be_==("system.docreg")
        case _ =>
          fail()
      }
      }
    }
    "Check db first" >> {
      TestDbVendor.initAndClean()
      val directory = mock[Directory]
      transaction{
      var a = new User
      a.name = ("a")
      a.email = ("a@no.com")
      a.username = ("aaa")
      a = User.dbTable.insert(a)
      var b = new User
      b.name = ("b")
      b.email = ("b@no.com")
      b.username = ("bbb")
      b = User.dbTable.insert(b)
      val ul = new UserLookup
      ul.username = Some("uUu")
      ul.email = None
      ul.name = None
      ul.userId = (a.id)
      UserLookup.dbTable.insert(ul)
      UserLookup.lookup(Some("uUu"), None, None, directory, "") must be_==(Full(a))
      }
    }
    "First look up directory for username" >> {
      TestDbVendor.initAndClean()
      val directory = mock[Directory]
      transaction{
      val a = new User
      a.name = ("a")
      a.email = ("a@no.com")
      a.username = ("aaa")
      User.dbTable.insert(a)
      val b = new User
      b.name = ("b")
      b.email = ("b@no.com")
      b.username = ("bbb")
      User.dbTable.insert(b)

      directory.findFromUserName("uUu") returns(Full(new FakeUserAttributes("uUu", "u@hoo.org", "u u u")))
//      directory.findFromUserName("uUu") returns(Empty)
//      directory.findFromUserName("uUu") returns(Empty)
      UserLookup.lookup(Some("uUu"), None, None, directory, "") match {
        case Full(user) =>
          user.email must be_==("u@hoo.org")
          user.name must be_==("u u u")
          user.username must be_==("uUu")
        case _ => 
          fail()
      }
//      there was no(directory).findFromMail(any[String])
//      there was no(directory).findFromPartialName(any[String])
      }
    }
    "Second look up directory for email" >> {
      TestDbVendor.initAndClean()
      val directory = mock[Directory]
      transaction{
      val a = new User
      a.name = ("a")
      a.email = ("a@no.com")
      a.username = ("aaa")
      User.dbTable.insert(a)
      val b = new User
      b.name = ("b")
      b.email = ("b@no.com")
      b.username = ("bbb")
      User.dbTable.insert(b)

      directory.findFromMail("l@la.la") returns(Full(new FakeUserAttributes("lala", "l@la.la", "la la la lah")))
      UserLookup.lookup(None, Some("l@la.la"), None, directory, "") match {
        case Full(user) =>
          user.email must be_==("l@la.la")
          user.name must be_==("la la la lah")
          user.username must be_==("lala")
        case _ => 
          fail()
      }
      }
    }
  }
}
