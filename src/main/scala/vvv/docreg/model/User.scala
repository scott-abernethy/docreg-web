package vvv.docreg.model

import net.liftweb._
import util._
import common._
import Helpers._
import http._
import provider.HTTPCookie
import vvv.docreg.util.{Environment, StringUtil}
import vvv.docreg.util.StringUtil.ValidEmail
import java.util.{TimeZone, Date}
import java.sql.Timestamp
import vvv.docreg.db.{DbObject, DbSchema}
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.model.User.loggedInUser
import scala.Predef._
import xml.{Text, NodeSeq}

// http://www.assembla.com/wiki/show/liftweb/How_to_use_Container_Managed_Security
// http://wiki.eclipse.org/Jetty/Tutorial/JAAS#LdapLoginModule
// http://www.mail-archive.com/openbd@googlegroups.com/msg05268.html

class User extends DbObject[User] {
  def dbTable = DbSchema.users
  var username: String = ""
  var dn: String = ""
  var name: String =  ""
  var description: String =  ""
  var department: String =  ""
  var location: String =  ""
  var email: String = ""
  var active: Boolean = true
  var superuser: Boolean = false
  var host: String = ""
  var lastSession: Timestamp = new Timestamp(0)
  var sessionCount: Long = 0
  var localServer: String = ""
  var timeZone: String = ""

  def watching_?(d: Document) = {
    Subscription.isNotified(d.id, id)
  }

  def bookmarked_?(d: Document) = {
    Subscription.isBookmarked(d.id, id)
  }

  def displayName = name

  def shortUsername(): String =
  {
    username match {
      case ValidEmail(name, domain) => name
      case other => other
    }
  }

  def knownOption(): Option[User] = {
    Option(this).filter(_.name != "[Unknown]")
  }

  def profileLink(): NodeSeq = profileLink(displayName)

  def profileLink(text: String): NodeSeq = profileLink(Text(text))

  def profile(): String = "/user/" + username.split("@")(0)

  def preferences(): String = {
    "/user/" + username.split("@")(0) + "/preferences"
  }

  def profileLink(content: NodeSeq): NodeSeq = <a href={ profile() }>{ content }</a>

  def profileLabel(focusUserId: Long): NodeSeq = {
    if (focusUserId == id) {
      <span class="user-selected">{ profileLink() }</span>
    }
    else {
      <span class="user">{ profileLink() }</span>
    }
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

  def canLogin_?(): Boolean = {
    // TODO need to check for DocReg Access group
    active
  }

  def accessLevel(): AccessLevel.Value = {
    if (!active) {
      AccessLevel.none
    }
    else if (superuser) {
      AccessLevel.superuser
    }
    else {
      AccessLevel.normal
    }
  }
}

case class SignInFailure(why: String, description: String)

object User extends User with Loggable {
  val docRegUserCookie = "DocRegWebUser"
  val domain = "@GNET.global.vpn"

  object loggedInUser extends SessionVar[Box[User]](checkForUserCookie)
  object requestUri extends SessionVar[Option[String]](None)

  def signIn(username: String, password: String): Either[User, SignInFailure] = {
    val directory = Environment.env.directory
    // This gives us an up to date user, i.e. LDAP attributes are reloaded
    UserLookup.lookupUser(username, directory) match {
      case Full(user) if (!user.canLogin_?()) => {
        Right(SignInFailure("Not Authorized", "Your user account is not authorized to access this service."))
      }
      case Full(user) if (directory.login(user.dn, password)) => {
        Left(user)
      }
      case _ => {
        Right(SignInFailure("Incorrect Username or Password", "Failed to login as user '" + username + "', incorrect username or password provided."))
      }
    }
  }

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

  def saveUserCookie() {
    loggedInUser.is match {
      case Full(u) => S.addCookie(HTTPCookie(docRegUserCookie, u.username).setMaxAge(3600 * 24 * 7).setPath("/"))
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

  def authorized(): List[User] = {
    from(dbTable)( u =>
      where(u.active === true)
      select(u)
      orderBy(u.name asc)
    ).toList
  }
}

object AccessLevel extends Enumeration {
  type AccessLevel = Value
  val none = Value("None")
  val normal = Value("Normal")
  val superuser = Value("Administrator")
}

object StreamMode extends Enumeration {
  val all = Value("All")
  val selected = Value("Selected")
  val watching = Value("Watching")
  val me = Value("@Me")
}

object UserSession {
  val modeCookie = "DocRegWebMode"
  object mode extends SessionVar[StreamMode.Value](loadModeCookie)
  object authorizedProjects extends SessionVar[Set[Long]](loadAuthorized)
  object selectedProjects extends SessionVar[Set[Long]](loadSelected)

  private def user: Box[User] = {
    User.loggedInUser.is
  }

  def loadAuthorized(): Set[Long] = {
    user.map(u =>
      ProjectAuthorization.authorizedProjectsFor(u).map(_.id).toSet
    ).getOrElse(Set.empty[Long])
  }

  def loadSelected(): Set[Long] = {
    user.map(u =>
      UserProject.userSelected(u).map(_.id).toSet
    ).getOrElse(Set.empty[Long])
  }

  def changeSelected(projectId: Long, selected: Boolean) {
    selectedProjects( if (selected) selectedProjects.is + projectId else selectedProjects.is - projectId )
  }

  def isAuthorized(d: Document, p: Project): Boolean = {
    d.secure_?() == false || authorizedProjects.contains(p.id)
  }

  private def loadModeCookie(): StreamMode.Value = {
    S.cookieValue(modeCookie).toOption.flatMap(x => Option(StreamMode.withName(x))).getOrElse(StreamMode.all)
  }

  private def saveModeCookie(x: StreamMode.Value) {
    S.addCookie(HTTPCookie(modeCookie, x.toString).setMaxAge(3600 * 24 * 365).setPath("/"))
  }

  def changeMode(x: StreamMode.Value) {
    mode(x)
    saveModeCookie(x)
  }

  def inStreamFilter(): (Document, Revision, Project) => Boolean = {
    inStreamFilter(UserSession.mode.is)
  }

  def inStreamFilter(mode: StreamMode.Value): (Document, Revision, Project) => Boolean = {
    val authorized = authorizedProjects.is
    def filterAuthorized(document: Document, project: Project) = !document.secure_?() || authorized.contains(project.id)

    val filterMode: (Document, Revision, Project) => Boolean = mode match {
      case StreamMode.all => {
        (_,_,_) => true
      }
      case StreamMode.selected => { // now favourites
        val selected = selectedProjects.is
        val bookmarks = user.map(Subscription.bookmarksFor(_)).getOrElse(Nil).map(_.id)
        (d,_,p) => selected.contains(p.id) || bookmarks.contains(d.id)
      }
      case StreamMode.watching => {
        val subs = user.map(Subscription.watchingFor(_)).getOrElse(Nil).map(_.id)
        (d,_,_) => subs.contains(d.id)
      }
      case StreamMode.me => {
        val uid = user.map(_.id) getOrElse -1
        (_,r,_) => r.authorId == uid
      }
      case _ => {
        (_,_,_) => false
      }
    }

    (d,r,p) => { filterMode(d,r,p) && filterAuthorized(d,p) }
  }
}