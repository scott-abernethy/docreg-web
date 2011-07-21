package vvv.docreg.model

import net.liftweb._
import mapper._
import util._
import common._
import Helpers._
import http._
import provider.HTTPCookie
import vvv.docreg.util.StringUtil

// http://www.assembla.com/wiki/show/liftweb/How_to_use_Container_Managed_Security
// http://wiki.eclipse.org/Jetty/Tutorial/JAAS#LdapLoginModule
// http://www.mail-archive.com/openbd@googlegroups.com/msg05268.html

// Custom simple user, authentication handled against domain by jetty.
class User extends LongKeyedMapper[User] with IdPK with ManyToMany {
  def getSingleton = User
  object name extends MappedString(this, 64)
  object username extends MappedString(this, 64)
  object email extends MappedEmail(this, 64) {
    override def apply(s: String) = super.apply(s.toLowerCase)
    override def apply(b: Box[String]) = super.apply(b map { _.toLowerCase })
    override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations // Doesn't seem to work.
  }
  object host extends MappedString(this, 64)
  object subscriptions extends MappedManyToMany(Subscription, Subscription.user, Subscription.document, Document)

  def subscribed_?(d: Document) = Subscription.forDocumentBy(d, this).nonEmpty

  def displayName = name.is
  def profileLink = "/user/" + id + "/profile"
}

object User extends User with LongKeyedMetaMapper[User] {
  val docRegUserCookie = "DocRegUser"
  object loggedInUser extends SessionVar[Box[User]](checkForUserCookie)
  override def dbIndexes = UniqueIndex(email) :: UniqueIndex(username) :: super.dbIndexes
  override def fieldOrder = List(id, name, email)
  def loggedIn_? = !loggedInUser.is.isEmpty
  def login(user: User) = loggedInUser(Full(user))
  def logout() = loggedInUser(Empty)
  def forEmail(email: String): Box[User] = find(By(User.email, email.toLowerCase))
  // todo remove, and uses
  def forEmailOrCreate(email: String): Box[User] = forEmail(email) match {
    case existing @ Full(_) => existing 
    case _ => 
      val placeholder = User.create
      placeholder.email(email)
      placeholder.name(StringUtil nameFromEmail email)
      placeholder asValid match {
        case Full(u) => 
          u.save
          Full(u)
        case _ => Empty // TODO if invalid email, use Unknown Author special user.
      }
  }
  def saveUserCookie() {
    loggedInUser.is match {
      case Full(u) => S.addCookie(HTTPCookie(docRegUserCookie, u.id.is.toString).setMaxAge(3600 * 24 * 365).setPath("/"))
      case _ => S.addCookie(HTTPCookie(docRegUserCookie, "###").setPath("/"))
    }
  }
  def checkForUserCookie: Box[User] = {
    S.cookieValue(docRegUserCookie) match {
      case Full(id) =>
        val existing = User.find(id)
        existing.foreach { u =>
          u.host(User.parseHost)
          u.save
        }
        existing
      case _ =>
        Empty
    }
  }
  def parseHost: String = {
    val host = S.request match {
      case Full(req: Req) => req.remoteAddr
      case _ => "?"
    }
    println("????? " + host)
    host
  }
}
