package vvv.docreg.model

import org.specs.runner.{ConsoleRunner, JUnit4}
import org.specs.Specification
import vvv.docreg.db.TestDbVendor
import net.liftweb.common.{Full, Failure, Empty}
import org.specs.mock.Mockito
import vvv.docreg.backend.{UserAttributes, Directory}

class UserLookupTestSpecsAsTest extends JUnit4(UserLookupTestSpecs)
object UserLookupTestSpecsRunner extends ConsoleRunner(UserLookupTestSpecs)

object UserLookupTestSpecs extends Specification with Mockito {
  "UserLookup" should {
    "Reject empty lookup" >> {
      TestDbVendor.initAndClean()
      val directory = mock[Directory]
      UserLookup.lookup(None, None, None, directory) must be_==(Failure("Invalid input"))
    }
    "Recognise system user" >> {
      TestDbVendor.initAndClean()
      UserLookup.installDefaults()
      UserLookup.installDefaults()
      val directory = mock[Directory]
      UserLookup.lookup(Some("smite"), None, Some("System"), directory) match {
        case Full(x) =>
          x.username.is must be_==("system.docreg")
        case _ =>
          fail()
      }
    }
    "Check db first" >> {
      TestDbVendor.initAndClean()
      val directory = mock[Directory]
      val a = User.create.name("a").email("a@no.com").username("aaa")
      a.save
      val b = User.create.name("b").email("b@no.com").username("bbb")
      b.save
      UserLookup.create.username("uUu").email("").name("").user(a).save
      UserLookup.lookup(Some("uUu"), None, None, directory) must be_==(Full(a))
    }
    "First look up directory for username" >> {
      TestDbVendor.initAndClean()
      val directory = mock[Directory]
      val a = User.create.name("a").email("a@no.com").username("aaa")
      a.save
      val b = User.create.name("b").email("b@no.com").username("bbb")
      b.save
      
      directory.findFromUserName("uUu") returns(Full(UserAttributes("uUu", "u@hoo.org", "u u u")))
//      directory.findFromUserName("uUu") returns(Empty)
//      directory.findFromUserName("uUu") returns(Empty)
      UserLookup.lookup(Some("uUu"), None, None, directory) match {
        case Full(user) =>
          user.email.is must be_==("u@hoo.org")
          user.name.is must be_==("u u u")
          user.username.is must be_==("uUu")
        case _ => 
          fail()
      }
//      there was no(directory).findFromMail(any[String])
//      there was no(directory).findFromPartialName(any[String])
    }
    "Second look up directory for email" >> {
      TestDbVendor.initAndClean()
      val directory = mock[Directory]
      val a = User.create.name("a").email("a@no.com").username("aaa")
      a.save
      val b = User.create.name("b").email("b@no.com").username("bbb")
      b.save

      directory.findFromMail("l@la.la") returns(Full(UserAttributes("lala", "l@la.la", "la la la lah")))
      UserLookup.lookup(None, Some("l@la.la"), None, directory) match {
        case Full(user) =>
          user.email.is must be_==("l@la.la")
          user.name.is must be_==("la la la lah")
          user.username.is must be_==("lala")
        case _ => 
          fail()
      }
    }
  }
}
