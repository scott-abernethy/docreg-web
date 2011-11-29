package vvv.docreg.model

import net.liftweb._
import mapper._
import util._
import common._
import Helpers._
import http._
import provider.HTTPCookie
import vvv.docreg.util.{Environment, StringUtil}
import vvv.docreg.util.StringUtil.ValidEmail
import java.util.Date
import xml.NodeSeq

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
    override def validations = valUnique(S.??("unique.email.address")) _ :: super.validations // Doesn't seem to work.
  }
  object active extends MappedBoolean(this)
  object host extends MappedString(this, 64)
  object lastSession extends MappedDateTime(this)
  object sessionCount extends MappedLong(this)
  object subscriptions extends MappedManyToMany(Subscription, Subscription.user, Subscription.document, Document)

  def subscribed_?(d: Document) = Subscription.forDocumentBy(d, this).nonEmpty

  def displayName = name.is

  def shortUsername(): String =
  {
    username.is match {
      case ValidEmail(name, domain) => name
      case other => other
    }
  }

  def profileLink(): NodeSeq = profileLink(displayName)

  def profileLink(text: String): NodeSeq = <a href={"/user/" + id + "/profile"}>{text}</a>

  def revisions(): List[Revision] = {
    Revision.findAll(By(Revision.author, this), OrderBy(Revision.date, Descending), PreCache(Revision.document))
  }

  def activity(): Long = {
    Revision.count(By(Revision.author, this))
  }

  def impact(): Long = {
    Document.count(In(Document.id, Revision.document, By(Revision.author, this)))
  }

  def history(): List[Document] = {
    Document.findAll(In(Document.id, Revision.document, By(Revision.author, this), OrderBy(Revision.date, Descending)))
  }
}

object User extends User with LongKeyedMetaMapper[User] with Loggable {
  val docRegUserCookie = "DocRegWebUser"
  val domain = "@GNET.global.vpn"

  object loggedInUser extends SessionVar[Box[User]](checkForUserCookie)
  object requestUri extends SessionVar[Option[String]](None)
  
  override def dbIndexes = UniqueIndex(email) :: UniqueIndex(username) :: super.dbIndexes
  override def fieldOrder = List(id, name, email)
  def loggedIn_? = !loggedInUser.is.isEmpty
  def login(user: User) = loggedInUser(Full(user))
  def logout() = loggedInUser(Empty)

  def forUsernameOrCreate(username: String): Box[User] = {
    find(By(User.username, username + domain)) match {
      case Full(user) =>
        Full(user)
      case _ =>
        UserLookup.lookup(Some(username), None, None, Environment.env.directory)
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
        val existing: Box[User] = User.find(id)
        existing.foreach { u =>
          u.lastSession(new Date)
          u.sessionCount(u.sessionCount.is + 1L)
          u.host(User.parseHost)
          u.save
          logger.info("User '" + u.displayName + "' started session " + host)
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
    host
  }

  def sort(a: User, b: User): Boolean =
  {
    val x = a.displayName.split(" ").head
    val y = b.displayName.split(" ").head
    x.compareToIgnoreCase(y) < 0
  }
}
