package vvv.docreg.snippet

import vvv.docreg.model._
import vvv.docreg.util.StringUtil
import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import scala.xml.{NodeSeq, Text}

class User extends Loggable {
  val emailHint = "your.name@aviatnet.com"
  object email extends RequestVar(emailHint)
  object name extends RequestVar("")
  def signIn(in: NodeSeq): NodeSeq = {
    bind("signIn", in,
      "email" -> JsCmds.FocusOnLoad(SHtml.text(email.is, s => email(s)) % ("style" -> "width: 250px")),
      "submit" -> SHtml.submit("Sign In", processLogin _)
    )
  }
  def processLogin() {
    if (email.is.indexOf('@') == -1) email(email.is + "@aviatnet.com")
    var submittedEmail = email.is.toLowerCase

    if (submittedEmail == emailHint) {
      S.error("Please enter YOUR email address")
    } else {
      User.forEmail(submittedEmail) match {
        case Full(u) => doSignIn(u)
        case Empty => User.create.email(submittedEmail).asValid match {
          case Full(u) => 
            S.warning("User '" + submittedEmail + "' is not registered")
            S.redirectTo("register", () => email(submittedEmail))
          case Empty => S.error("Unknown error")
          case Failure(msg, _, _) => S.error(msg)
        }
        case Failure(msg, _, _) => S.error(msg)
      }
    }
  }
  def register(in: NodeSeq): NodeSeq = {
    val submittedEmail = email.is
    bind("register", in,
      "email" -> Text(email.is),
      "name" -> JsCmds.FocusOnLoad(SHtml.text(StringUtil nameFromEmail submittedEmail, s => name(s))),
      "submit" -> SHtml.submit("Register", () => processRegister(submittedEmail)),
      "cancel" -> SHtml.submit("Cancel", () => S.redirectTo("/"))
    )
  }
  def processRegister(e: String) {
    logger.info("Register user " + e + " = " + name.is)
    val u = User.create
    u.email(e)
    u.name(name.is)
    u.save
    S.notice("User '" + u.email + "' has been registered")
    doSignIn(u)
  }
  def doSignIn(u: vvv.docreg.model.User) {
    User.login(u)
    S.notice("Welcome " + u.email)
    S.redirectTo("/", () => (User.saveUserCookie))
  }
  def signOut(in: NodeSeq): NodeSeq = {
    User.logout()
    S.notice("User signed out")
    S.redirectTo("signin", () => (User.saveUserCookie))
  }
  def control(in: NodeSeq): NodeSeq = {
    if (User.loggedIn_?) {
      bind("user", in,
        "id" -> (User.loggedInUser.map(_.email.is) openOr "?"),
        "signOut" -> <a href="/user/signout">Sign out</a>
      )
    } else {
      <a href="/user/signin">Sign in</a>
    }
  }
}
