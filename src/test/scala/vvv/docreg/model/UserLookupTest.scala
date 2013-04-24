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

package vvv.docreg.model

import org.specs.runner.{ConsoleRunner, JUnit4}
import org.specs.Specification
import vvv.docreg.db.TestDbVendor
import org.specs.mock.Mockito
import vvv.docreg.backend.{UserAttributes, Directory}
import net.liftweb.common.{Full, Failure, Empty}
import org.squeryl.PrimitiveTypeMode._

class FakeUserAttributes(username: String, mail: String, display: String) extends NothingUserAttributes {
  override def userName() = Some(username)

  override def email() = Some(mail)

  override def displayName() = Some(display)
}

class NothingUserAttributes extends UserAttributes {
  def userName(): Option[String] = None

  def email(): Option[String] = None

  def displayName(): Option[String] = None

  def dn(): Option[String] = None

  def department(): Option[String] = None

  def description(): Option[String] = None

  def location(): Option[String] = None

  def memberOf(): List[String] = Nil
}

class UserLookupTest extends Specification with Mockito {
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

    "Remove authorizations if none provided via LDAP" >> {
      TestDbVendor.initAndClean()
      inTransaction{
        val (p1, p2, p3) = TestDbVendor.createProjects
        val (u1, u2) = TestDbVendor.createUsers
        ProjectAuthorization.grant(u1, p3)
        ProjectAuthorization.grant(u2, p3)
        ProjectAuthorization.authorizedFor_?(u1, p1) must beFalse
        ProjectAuthorization.authorizedFor_?(u1, p3) must beTrue
        ProjectAuthorization.authorizedFor_?(u2, p3) must beTrue

        val attrs = new NothingUserAttributes{
          override def memberOf() = Nil
        }
        UserLookup.parseUserAuthorizations(u1, attrs)
        ProjectAuthorization.authorizedFor_?(u1, p3) must beFalse
        ProjectAuthorization.authorizedFor_?(u2, p3) must beTrue
      }
    }

    "Add authorizations provided via LDAP" >> {
      TestDbVendor.initAndClean()
      inTransaction{
        val (p1, p2, p3) = TestDbVendor.createProjects
        val (u1, u2) = TestDbVendor.createUsers
        ProjectAuthorization.authorizedFor_?(u1, p2) must beFalse
        ProjectAuthorization.authorizedFor_?(u2, p2) must beFalse

        val attrs = new NothingUserAttributes{
          override def memberOf() = List("CN=DocRegProjectUnknown,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectp2,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectt,OU=OpenKM,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=STXN Provision Development Team,OU=Distribution List,OU=Messaging,OU=APAC,DC=HSTX,DC=global,DC=vpn")
        }
        UserLookup.parseUserAuthorizations(u2, attrs)
        ProjectAuthorization.authorizedFor_?(u1, p2) must beFalse
        ProjectAuthorization.authorizedFor_?(u2, p2) must beTrue
      }
    }

    "Merge authorizations provided via LDAP" >> {
      TestDbVendor.initAndClean()
      inTransaction{
        val (p1, p2, p3) = TestDbVendor.createProjects
        val (u1, u2) = TestDbVendor.createUsers
        ProjectAuthorization.grant(u1, p2)
        ProjectAuthorization.grant(u1, p3)

        val attrs = new NothingUserAttributes{
          override def memberOf() = List("CN=DocRegProjectUnknown,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectp2,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectt,OU=OpenKM,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectp1,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=STXN Provision Development Team,OU=Distribution List,OU=Messaging,OU=APAC,DC=HSTX,DC=global,DC=vpn")
        }
        UserLookup.parseUserAuthorizations(u1, attrs)
        ProjectAuthorization.authorizedFor_?(u1, p1) must beTrue
        ProjectAuthorization.authorizedFor_?(u1, p2) must beTrue
        ProjectAuthorization.authorizedFor_?(u1, p3) must beFalse
      }
    }

    "Parse user access via LDAP" >> {
      UserLookup.parseUserAccess(new NothingUserAttributes()) must beFalse

      val nope = new NothingUserAttributes{
        override def memberOf() = List("CN=DocRegProjectUnknown,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectp2,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectt,OU=OpenKM,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectp1,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=STXN Provision Development Team,OU=Distribution List,OU=Messaging,OU=APAC,DC=HSTX,DC=global,DC=vpn")
      }
      UserLookup.parseUserAccess(nope) must beFalse

      val yep = new NothingUserAttributes{
        override def memberOf() = List("CN=DocRegProjectUnknown,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegUser,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectt,OU=OpenKM,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=DocRegProjectp1,OU=DocReg,OU=New Zealand,OU=Groups,OU=APAC,DC=GNET,DC=global,DC=vpn", "CN=STXN Provision Development Team,OU=Distribution List,OU=Messaging,OU=APAC,DC=HSTX,DC=global,DC=vpn")
      }
      UserLookup.parseUserAccess(yep) must beTrue
    }
  }
}
