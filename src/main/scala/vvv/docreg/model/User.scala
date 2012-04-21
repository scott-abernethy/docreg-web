package vvv.docreg.model

import net.liftweb._
import util._
import common._
import Helpers._
import http._
import provider.HTTPCookie
import vvv.docreg.util.{Environment, StringUtil}
import vvv.docreg.util.StringUtil.ValidEmail
import xml.NodeSeq
import java.util.{TimeZone, Date}
import java.sql.Timestamp
import vvv.docreg.db.{DbObject, DbSchema}
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.model.User.loggedInUser
import scala.Predef._

// http://www.assembla.com/wiki/show/liftweb/How_to_use_Container_Managed_Security
// http://wiki.eclipse.org/Jetty/Tutorial/JAAS#LdapLoginModule
// http://www.mail-archive.com/openbd@googlegroups.com/msg05268.html

// Custom simple user, authentication handled against domain by jetty.
class User extends DbObject[User] {
  def dbTable = DbSchema.users
  var username: String = ""
  var name: String =  ""
  var email: String = ""
  var active: Boolean = true
  var host: String = ""
  var lastSession: Timestamp = new Timestamp(0)
  var sessionCount: Long = 0
  var localServer: String = ""
  var timeZone: String = ""

  //var subscriptions = MappedManyToMany(Subscription, Subscription.user, Subscription.document, Document)

  def subscribed_?(d: Document) = {
    inTransaction( Subscription.dbTable.where(s => s.userId === id and s.documentId === d.id).isEmpty )
  }

  def displayName = name

  def shortUsername(): String =
  {
    username match {
      case ValidEmail(name, domain) => name
      case other => other
    }
  }

  def profileLink(): NodeSeq = profileLink(displayName)

  def profileLink(text: String): NodeSeq = <a href={ profile() }>{ text }</a>

  def profile(): String = "/user/" + username.split("@")(0)

  def preferences(): String = {
    "/user/" + username.split("@")(0) + "/preferences"
  }

  def revisions(): List[Revision] = {
    inTransaction( from(Revision.dbTable)(r => where(r.authorId === id) select(r) orderBy(r.date desc)).toList )
  }

  def activity(): Long = {
    inTransaction( from(Revision.dbTable)(r => where(r.authorId === id) compute(countDistinct(r.id))) )
  }

  def impact(): Long = {
    inTransaction( from(Revision.dbTable)(r => where(r.authorId === id) compute(countDistinct(r.documentId))) )
  }

  def history(): List[Document] = {
    inTransaction {
      join(Document.dbTable, Revision.dbTable)( (d, r) =>
        where(r.authorId === id)
        select(d)
        orderBy(r.date desc)
        on(d.id === r.documentId)
      ).toList.distinct
    }
  }

  def editing(): List[Document] = {
    inTransaction {
      join(Pending.dbTable, Document.dbTable)( (p, d) =>
        where(p.action === PendingAction.editing and p.userId === id)
        select(d)
        orderBy(p.date desc)
        on(p.documentId === d.id)
      ).toList.distinct
    }
  }

  def getTimeZone(): TimeZone = {
    if (timeZone == null){
      TimeZone.getDefault
    } else {
      TimeZone.getTimeZone(timeZone)
    }
  }
}

object User extends User with Loggable {
  val docRegUserCookie = "DocRegWebUser"
  val domain = "@GNET.global.vpn"

  object loggedInUser extends SessionVar[Box[User]](checkForUserCookie)
  object requestUri extends SessionVar[Option[String]](None)
  
  def reloadLoggedInUser() {
    val u = loggedInUser.toOption.flatMap(_.reload())
    loggedInUser(u)
  }

  def loggedIn_? = !loggedInUser.is.isEmpty

  def login(user: User) = {
    markSession(user)
    loggedInUser(Full(user))
  }

  def logout() = {
    loggedInUser(Empty)
  }

  def forUsername(username: String): Option[User] = {
    inTransaction( dbTable.where(u => u.username like username).headOption )
  }

  private def markSession(in: User)
  {
    for (u <- in.reload()) {
      u.lastSession = new Timestamp(System.currentTimeMillis())
      u.sessionCount = u.sessionCount + 1L
      u.host = User.parseHost
      User.dbTable.update(u)
      logger.info("User '" + u.displayName + "' started session " + host)
    }
  }

  def forUsernameOrCreate(username: String): Box[User] = {
    forUsername(username + domain) match {
      case Some(user) =>
        Full(user)
      case _ =>
        UserLookup.lookup(Some(username), None, None, Environment.env.directory, "forUsernameOrCreate")
    }
  }

  def saveUserCookie() {
    loggedInUser.is match {
      case Full(u) => S.addCookie(HTTPCookie(docRegUserCookie, u.username).setMaxAge(3600 * 24 * 365).setPath("/"))
      case _ => S.addCookie(HTTPCookie(docRegUserCookie, "###").setPath("/"))
    }
  }

  def checkForUserCookie: Box[User] = {
    S.cookieValue(docRegUserCookie) match {
      case Full(id) =>
        val existing: Box[User] = User.forUsername(id)
        existing.foreach { u => markSession(u) }
        existing
      case _ =>
        Empty
    }
  }

  def parseHost: String = 
  {
    // Nginx wraps the request ip as X-Real-IP and X-Forwarded-For
    S.getRequestHeader("X-Real-IP").getOrElse(S.getRequestHeader("X-Forwarded-For").getOrElse("?"))
  }

  def sort(a: User, b: User): Boolean =
  {
    val x = a.displayName.split(" ").head
    val y = b.displayName.split(" ").head
    x.compareToIgnoreCase(y) < 0
  }
}
