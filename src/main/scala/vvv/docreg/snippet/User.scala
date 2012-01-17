package vvv.docreg.snippet

import vvv.docreg.model._
import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import scala.xml.{NodeSeq, Text}
import vvv.docreg.util.{Environment, StringUtil}
import vvv.docreg.util.StringUtil._

class UserSnippet extends Loggable {
  val signInHint = ""
  object username extends RequestVar(signInHint)

  def signIn = {
    ".username" #> JsCmds.FocusOnLoad(SHtml.text(username.is, s => username(s)) % ("style" -> "width: 250px")) &
    ".submit" #> SHtml.submit("Sign In", processLogin _, "class" -> "btn primary")
  }

  def processLogin() {
    username.is.toLowerCase match {
      case DomainUsername("gnet", name) => {
        tryLogin(name)
      }
      case DomainUsername(domain, name) => {
        loginError(<p><strong>Invalid domain</strong>{" '" + domain + "'. Please use your GNET domain username."}</p>)
      }
      case ValidEmail(name, "gnet.global.vpn") => {
        tryLogin(name)
      }
      case ValidEmail(name, domain) => {
        loginError(<p><strong>Invalid domain</strong>{" '" + domain + "'. Please use your GNET domain username."}</p>)
      }
      case input if (input == signInHint) => {
        loginError(<p>Please enter <strong>your</strong> account username</p>)
      }
      case input => {
        tryLogin(input)
      }
    }
  }

  private def tryLogin(username: String) {
    User.forUsernameOrCreate(username) match {
      case Full(u) if u.active.is =>
        doSignIn(u)
      case _ =>
        loginError(<p><strong>Failed</strong>{" to login as user '" + username + User.domain + "'"}</p>)
    }
  }

  private def loginError(msg: NodeSeq) {
    S.error(<div class="alert-message error">{ msg }</div>)
  }

  def doSignIn(u: vvv.docreg.model.User) {
    val uri = User.requestUri.is match {
      case Some(x) => x
      case _ => "/"
    }
    User.requestUri(None)
    User.login(u)
    S.redirectTo(uri, () => (User.saveUserCookie))
  }

  def signOut(in: NodeSeq): NodeSeq = {
    User.logout()
    S.redirectTo("signin", () => (User.saveUserCookie))
  }

  def control = {
    if (User.loggedIn_?) {
      ".user-id *" #> (User.loggedInUser.map(o => o.displayName) openOr "?") &
      ".user-profile *" #> (User.loggedInUser.map(o => o.profileLink("Profile")) openOr Text("?")) &
      ".user-preferences *" #> <a href={ (User.loggedInUser.map(o => o.preferences()) openOr "#") }>Preferences</a> &
      ".user-action *" #> <a href="/user/signout">Sign out</a>
    } else {
      ".user-id *" #> "Welcome" &
      ".user-profile" #> NodeSeq.Empty &
      ".user-preferences" #> NodeSeq.Empty &
      ".user-action *" #> <a href="/user/signin">Sign in</a>
    }
  }

  def profile(in: NodeSeq): NodeSeq = {
    val user = S.param("user") match {
      case Full(uid) => User.find(uid)
      case _ => User.loggedInUser.is
    }
    user match {
      case Full(u) => {
        profileTransform(u).apply(in)
      }
      case _ => {
        <div class="alert-message error"><p>No such user found</p></div>
      }
    }
  }

  def profileTransform(u: User) = {
    val t = ".profile-name" #> u.displayName &
    ".profile-username" #> u.username &
    ".profile-email" #> u.email &
    ".profile-local-server" #> Server.description(u.localServer) &
    ".profile-activity" #> <span>{ u.activity() } submits in { u.impact() } documents</span> &
    ".profile-preferences [href]" #> u.preferences() &
    ".subscription-item" #> u.subscriptions.sortWith(Document.sort).map { s =>
      "li *" #> s.info()
    } &
    ".history-item" #> u.history.map { h =>
      "li *" #> h.info()
    } &
    ".editing-item" #> u.editing.map { d =>
      "li *" #> d.info()
    }

    if (User.loggedInUser.is.exists(_ == u)) {
      t
    }
    else {
      t & ".profile-preferences" #> NodeSeq.Empty
    }
  }

  def preferences = {
    val user = S.param("user") match {
      case Full(uid) => User.find(uid)
      case _ => User.loggedInUser.is
    }
    user match {
      case Full(u) if (User.loggedInUser.is.exists(_ == u)) => {
        var selected = u.localServer.is
        ClearClearable &
        ".profile-name" #> u.displayName &
        ".local-server" #> SHtml.select(Server.select.toSeq, Full(selected), selected = _) &
        "#submit" #> SHtml.onSubmit( x => savePreferences(u, selected) ) &
        "#cancel" #> SHtml.onSubmit( x => S.redirectTo(u.profile()) )
      }
      case _ => {
        S.warning("You do not have permission to edit that users preferences!")
        S.redirectTo("/")
      }
    }
  }

  def savePreferences(u: User, server: String) {
    u.reload.localServer(server).save
    User.reloadLoggedInUser()
    S.redirectTo(u.profile)
  }

  def history = {
    User.loggedInUser.is match {
      case Full(u) => {
        ".history-user [href]" #> u.profile() &
        ".history-item" #> u.reload.history().take(10).map { d =>
          "li *" #> d.info()
        }
      }
      case _ => {
        ".history-item" #> NodeSeq.Empty
      }
    }
  }

  def editing = {
    User.loggedInUser.is match {
      case Full(u) => {
        ".editing-user [href]" #> u.profile() &
        ".editing-item" #> u.reload.editing().map { d =>
          "li *" #> d.info()
        }
      }
      case _ => {
        ".editing-item" #> NodeSeq.Empty
      }
    }
  }

  def subscriptions = {
    User.loggedInUser.is match {
      case Full(u) => ".subscription:item" #> bindSubscriptions(u.reload) _
      case _ => ".subscription:item" #> NodeSeq.Empty
    }
  }

  def bindSubscriptions(user: vvv.docreg.model.User)(in: NodeSeq): NodeSeq = {
    user.subscriptions.sortWith(Document.sort).flatMap( d =>
      (
        ".item:title" #> d.fullTitle &
        "a [href]" #> d.infoLink
      ).apply(in)
    )
  }
}
