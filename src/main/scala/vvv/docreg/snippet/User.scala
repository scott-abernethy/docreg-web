package vvv.docreg.snippet

import vvv.docreg.model._
import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import scala.xml.{NodeSeq, Text}

class User extends Logger {
  object email extends RequestVar("Email")
  object name extends RequestVar("")
  def signIn(in: NodeSeq): NodeSeq = {
    bind("signIn", in,
      "email" -> SHtml.text(email.is, s => email(s)),
      "submit" -> SHtml.submit("Sign In", processLogin _)
    )
  }
  def processLogin() {
    val submittedEmail = email.is.toLowerCase
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
  def register(in: NodeSeq): NodeSeq = {
    val submittedEmail = email.is
    bind("register", in,
      "email" -> Text(email.is),
      "name" -> SHtml.text(name.is, s => name(s)),
      "submit" -> SHtml.submit("Register", () => processRegister(submittedEmail)),
      "cancel" -> SHtml.submit("Cancel", () => S.redirectTo("/"))
    )
  }
  def processRegister(e: String) {
    info("Register user " + e + " = " + name.is)
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
    S.redirectTo("/")
  }
  def signOut(in: NodeSeq): NodeSeq = {
    User.logout()
    S.notice("User signed out")
    S.redirectTo("/") 
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
