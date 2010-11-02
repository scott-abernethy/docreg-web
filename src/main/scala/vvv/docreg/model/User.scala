package vvv.docreg.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._

// http://www.assembla.com/wiki/show/liftweb/How_to_use_Container_Managed_Security
// http://wiki.eclipse.org/Jetty/Tutorial/JAAS#LdapLoginModule
// http://www.mail-archive.com/openbd@googlegroups.com/msg05268.html

// Custom simple user, authentication handled against domain by jetty.
class User extends LongKeyedMapper[User] with IdPK {
  def getSingleton = User
  object name extends MappedString(this, 64)
  object email extends MappedEmail(this, 64) {
    override def apply(s: String) = super.apply(s.toLowerCase)
    override def apply(b: Box[String]) = super.apply(b map { _.toLowerCase })
    override def dbIndexed_? = true
    override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations
  }
  def displayName = name.is
}

object User extends User with LongKeyedMetaMapper[User] {
  object loggedInUser extends SessionVar[Box[User]](Empty)
  override def dbTableName = "users"
  override def fieldOrder = List(id, name, email)
  def loggedIn_? = !loggedInUser.is.isEmpty
  def login(user: User) = loggedInUser(Full(user))
  def logout() = loggedInUser(Empty)
  def forEmail(email: String): Box[User] = find(By(User.email, email.toLowerCase)) 
}
