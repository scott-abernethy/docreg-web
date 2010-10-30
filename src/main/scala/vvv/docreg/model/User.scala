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
    override def dbIndexed_? = true
    override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations
  }
  def displayName = name.is
}

object User extends User with LongKeyedMetaMapper[User] {
  override def dbTableName = "users"
  override def fieldOrder = List(id, name, email)
  def loggedIn_? = false
}
