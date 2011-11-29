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

class UserSnippet extends Loggable {
  val signInHint = ""
  object username extends RequestVar(signInHint)

  def signIn = {
    ".username" #> JsCmds.FocusOnLoad(SHtml.text(username.is, s => username(s)) % ("style" -> "width: 250px")) &
    ".submit" #> SHtml.submit("Sign In", processLogin _, "class" -> "btn primary")
  }

  def processLogin() {
    var submittedUsername = username.is.toLowerCase

    if (submittedUsername == signInHint) {
      loginError(<p>Please enter <strong>your</strong> account username</p>)
    } else {
      User.forUsernameOrCreate(submittedUsername) match {
        case Full(u) if u.active.is =>
          u.host(User.parseHost)
          u.save
          doSignIn(u)
        case _ =>
          loginError(<p><strong>Failed</strong>{" to login as user '" + submittedUsername + User.domain + "'"}</p>)
      }
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
      ".user-action *" #> <a href="/user/signout">Sign out</a>
    } else {
      ".user-id *" #> "Welcome" &
      ".user-profile" #> NodeSeq.Empty &
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
        val t = ".profile-name" #> u.displayName &
          ".profile-username" #> u.username &
          ".profile-email" #> u.email &
          ".profile-activity" #> <span>{ u.activity() } submits in { u.impact() } documents</span> &
          ".subscription-item" #> u.subscriptions.sortWith(Document.sort).map { s =>
            "li *" #> s.info()
          } &
          ".history-item" #> u.history.map { h =>
            "li *" #> h.info()
          }

        t(in)
      }
      case _ => {
        <div class="alert-message error"><p>No such user found</p></div>
      }
    }
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
