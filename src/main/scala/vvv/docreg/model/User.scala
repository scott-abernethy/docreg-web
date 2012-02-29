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
import xml.NodeSeq
import java.util.{TimeZone, Date}


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
  object localServer extends MappedString(this, 64)
  object subscriptions extends MappedManyToMany(Subscription, Subscription.user, Subscription.document, Document)
  object timeZone extends MappedTimeZone(this)

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

  def profileLink(text: String): NodeSeq = <a href={ profile() }>{ text }</a>

  def profile(): String = "/user/" + username.split("@")(0)  + "/profile"

  def preferences(): String = {
    "/user/" + username.split("@")(0) + "/preferences"
  }

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
    val documents = for {
      revision <- Revision.findAll(By(Revision.author, this), OrderBy(Revision.date, Descending), PreCache(Revision.document))
      document <- revision.document.obj
    } yield document
    documents.distinct
  }

  def editing(): List[Document] = {
    val documents = for {
      pending <- Pending.forUserAction(this, PendingAction.editing)
      document <- pending.document.obj
    } yield document
    documents.distinct
  }
  
  def getTimeZone(): TimeZone = {
    if (timeZone.is == null){
      TimeZone.getDefault
    } else {
      timeZone.isAsTimeZone
    }
  }
}

object User extends User with LongKeyedMetaMapper[User] with Loggable {
  val docRegUserCookie = "DocRegWebUser"
  val domain = "@GNET.global.vpn"

  object loggedInUser extends SessionVar[Box[User]](checkForUserCookie)
  object requestUri extends SessionVar[Option[String]](None)
  
  override def dbIndexes = UniqueIndex(email) :: UniqueIndex(username) :: super.dbIndexes
  override def fieldOrder = List(id, name, email)

  def reloadLoggedInUser() {
    for (u <- loggedInUser.is) {
      loggedInUser(Full(u.reload))
    }
  }

  def loggedIn_? = !loggedInUser.is.isEmpty

  def login(user: User) = {
    markSession(user)
    loggedInUser(Full(user))
  }

  def logout() = {
    loggedInUser(Empty)
  }

  private def markSession(in: User)
  {
    val u = in.reload
    u.lastSession(new Date)
    u.sessionCount(u.sessionCount.is + 1L)
    u.host(User.parseHost)
    u.save
    logger.info("User '" + u.displayName + "' started session " + host)
  }

  def forUsernameOrCreate(username: String): Box[User] = {
    find(By(User.username, username + domain)) match {
      case Full(user) =>
        Full(user)
      case _ =>
        UserLookup.lookup(Some(username), None, None, Environment.env.directory, "forUsernameOrCreate")
    }
  }

  def saveUserCookie() {
    loggedInUser.is match {
      case Full(u) => S.addCookie(HTTPCookie(docRegUserCookie, u.username.is).setMaxAge(3600 * 24 * 365).setPath("/"))
      case _ => S.addCookie(HTTPCookie(docRegUserCookie, "###").setPath("/"))
    }
  }

  def checkForUserCookie: Box[User] = {
    S.cookieValue(docRegUserCookie) match {
      case Full(id) =>
        val existing: Box[User] = User.find(By(User.username, id))
        existing.foreach { u => markSession(u) }
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
