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

  def signIn(in: NodeSeq): NodeSeq = {
    bind("signIn", in,
      "username" -> JsCmds.FocusOnLoad(SHtml.text(username.is, s => username(s)) % ("style" -> "width: 250px")),
      "submit" -> SHtml.submit("Sign In", processLogin _)
    )
  }

  def processLogin() {
    var submittedUsername = username.is.toLowerCase

    if (submittedUsername == signInHint) {
      S.error("Please enter your account username")
    } else {
      User.forUsernameOrCreate(submittedUsername) match {
        case Full(u) if u.active.is =>
          u.host(User.parseHost)
          u.save
          doSignIn(u)
        case _ =>
          S.error("Failed to login as user '" + submittedUsername + User.domain + "'")
      }
    }
  }

  def doSignIn(u: vvv.docreg.model.User) {
    val uri = User.requestUri.is match {
      case Some(x) => x
      case _ => "/"
    }
    User.requestUri(None)
    User.login(u)
    S.notice("Welcome " + u.displayName)
    S.redirectTo(uri, () => (User.saveUserCookie))
  }

  def signOut(in: NodeSeq): NodeSeq = {
    User.logout()
    S.notice("User signed out")
    S.redirectTo("signin", () => (User.saveUserCookie))
  }

  def control(in: NodeSeq): NodeSeq = {
    if (User.loggedIn_?) {
      bind("user", in,
        "id" -> (User.loggedInUser.map(o => o.profileLink) openOr Text("?")),
        "signOut" -> <a href="/user/signout">Sign out</a>
      )
    } else {
      <a href="/user/signin">Sign in</a>
    }
  }

  def profile(in: NodeSeq): NodeSeq = {
    val user = S.param("user") match {
      case Full(uid) => User.find(uid)
      case _ => User.loggedInUser.is
    }
    user map {u => bind("profile", in, 
      "name" -> u.displayName,
      "username" -> u.username,
      "email" -> u.email,
      //TODO get subscriptions working on profile page, with subscriptions snippet used on home page.
      "subscriptions" -> <ul>{ if (u.subscriptions.nonEmpty) u.subscriptions.sortWith((a, b) => a.key.is < b.key.is).map(s =>
                                      <li><a href={ s.infoLink }>{s.fullTitle}</a></li>)
                                else "-"
                        }</ul>
    )} openOr {S.error("No such user found"); NodeSeq.Empty}
  }

  def subscriptions = {
    User.loggedInUser.is match {
      case Full(u) => ".subscription:item" #> bindSubscriptions(u.reload) _
      case _ => ClearClearable
    }
  }

  def bindSubscriptions(user: vvv.docreg.model.User)(in: NodeSeq): NodeSeq = {
    user.subscriptions.flatMap( d =>
      (
        ".item:title" #> d.fullTitle &
        "a [href]" #> d.infoLink
      ).apply(in)
    )
  }
}
