/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import net.liftweb._
import mapper.{Like, MappedTimeZone}
import util._
import common._
import Helpers._
import http._
import js._
import xml.{Unparsed, NodeSeq, Text}
import vvv.docreg.util.{Environment, StringUtil}
import vvv.docreg.util.StringUtil._
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.model._

class UserSnippet extends Loggable {
  val signInHint = ""
  object failure extends RequestVar[Option[(String,String)]](None)
  object username extends RequestVar(signInHint)
  object password extends RequestVar("")

  def signIn = {
    ".failure-alert" #> failure.is.map{ f =>
      ".failure-heading *" #> f._1 &
      ".failure-desc *" #> f._2
    } &
    ".username" #> JsCmds.FocusOnLoad(SHtml.text(username.is, s => username(s), "class" -> "input-xlarge")) &
    ".password" #> SHtml.password("", s => password(s), "class" -> "input-xlarge") &
    ".submit" #> SHtml.submit("Sign In", processLogin _, "class" -> "btn btn-success")
  }

  def processLogin() {
    username.is.trim.toLowerCase match {
      case "" => {
        loginFailed("Invalid Username", "Please enter your GNET account username.")
      }
      case DomainUsername("gnet", name) => {
        tryLogin(name, password.is.trim)
      }
      case DomainUsername(domain, name) => {
        loginFailed("Invalid Domain", "'" + domain + "' is not a recognised domain. Please use your GNET domain username.")
      }
      case ValidEmail(name, "gnet.global.vpn") => {
        tryLogin(name, password.is.trim)
      }
      case ValidEmail(name, "aviatnet.com") => {
        loginFailed("Invalid Username", "Email address is not supported. Please use your GNET domain username.")
      }
      case ValidEmail(name, domain) => {
        loginFailed("Invalid Domain", "'" + domain + "' is not a recognised domain. Please use your GNET domain username.")
      }
      case input => {
        tryLogin(input, password.is.trim)
      }
    }
  }

  private def tryLogin(username: String, password: String) {
    inTransaction(User.signIn(username, password)) match {
      case Left(user) => {
        doSignIn(user)
      }
      case Right(IncorrectUsernameOrPassword) => {
        loginFailed("Incorrect Username or Password", "Failed to login as user '" + username + "', incorrect username or password provided.")
      }
      case Right(NotAuthorized(user)) => {
        S.redirectTo("/user/not-authorized?u=" + user.shortUsername)
      }
    }
  }

  def loginFailed(title: String, desc: String) {
    failure.apply(Some((title, desc)))
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
      ".signed-in" #> NodeSeq.Empty &
      ".user-id *" #> "Welcome" &
      ".user-profile" #> NodeSeq.Empty &
      ".user-preferences" #> NodeSeq.Empty &
      ".user-action *" #> <a href="/user/signin">Sign in</a>
    }
  }

  def profile(in: NodeSeq): NodeSeq = {
    val user = S.param("user") match {
      case Full(uname) => User.forUsername(uname + "@%")
      case _ => User.loggedInUser.is.toOption
    }
    user match {
      case Some(u) => {
        profileTransform(u).apply(in)
      }
      case _ => {
        S.warning(<div class="alert-message error"><p>No such user found</p></div>)
        S.redirectTo("/")
      }
    }
  }

  def profileTransform(u: User) = {
    val authorzations = ProjectAuthorization.authorizedProjectsFor(u)
    val t = ".profile-name" #> u.displayName &
    ".profile-username *" #> u.username &
    ".profile-email *" #> <a href={"mailto:" + u.email}>{u.email} <i class="icon-envelope"></i></a> &
    ".profile-description" #> u.description &
    ".profile-department" #> u.department &
    ".profile-location" #> u.location &
    ".profile-time-zone *" #> u.timeZone &
    ".profile-activity *" #> <span>{ u.activity() } submits in { u.impact() } documents</span> &
    ".profile-access-level *" #> u.accessLevel().toString &
    (if (authorzations.isEmpty) ".profile-authorizations" #> NodeSeq.Empty else ".ignoredzzz" #> "ignored") &
    ".authorized-item" #> authorzations.map{ pa =>
      "li *" #> pa.infoLink()
    } &
    ".profile-preferences [href]" #> u.preferences() &
    ".subscription-item" #> Subscription.watchingFor(u).map { s =>
      "li *" #> s.info()
    } &
    ".bookmark-item" #> Subscription.bookmarksFor(u).map { s =>
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
      case Full(uname) => User.forUsername(uname + "@%")
      case _ => User.loggedInUser.is.toOption
    }
    user match {
      case Some(u) if (User.loggedInUser.is.exists(_ == u)) => {
        var selectedTime = ""
        ClearClearable &
        ".profile-name" #> u.displayName &
        ".local-time" #> SHtml.select(MappedTimeZone.timeZoneList, Option(u.timeZone), selectedTime = _) &
        "#submit" #> SHtml.onSubmit( x => savePreferences(u, selectedTime) ) &
        "#cancel" #> SHtml.onSubmit( x => S.redirectTo(u.profile()) )
      }
      case _ => {
        S.warning(<div class="alert-message error"><p>You do not have permission to edit that users preferences!</p></div>)
        S.redirectTo("/")
      }
    }
  }

  def savePreferences(u: User, timeZone: String) {
    inTransaction{
      User.dbTable.update(x =>
        where(x.id === u.id)
        set(x.timeZone := timeZone)
      )
    }

    User.reloadLoggedInUser()
    S.session.foreach(_.destroySession())
    S.redirectTo(u.profile)
  }

  def history = {
    User.loggedInUser.is match {
      case Full(u) => {
        ".history-user [href]" #> u.profile() &
        ".history-item" #> u.history().take(10).map { d =>
          "li *" #> d.info()
        }
      }
      case _ => {
        ".history-item" #> NodeSeq.Empty
      }
    }
  }

  def editing = {
    User.loggedInUser.is map(u => (u,u.editing())) match {
      case Full( (u, list) ) if (list.size > 0) => {
        ".editing-user [href]" #> u.profile() &
        ".editing-item" #> list.map { d =>
          "li *" #> d.info()
        }
      }
      case _ => ClearNodes
    }
  }

  def subscriptions = {
    User.loggedInUser.is match {
      case Full(u) => ".subscription:item" #> bindSubscriptions(u.reload)
      case _ => ".subscription:item" #> NodeSeq.Empty
    }
  }

  def bindSubscriptions(user: Option[vvv.docreg.model.User]) = {
    user match {
      case Some(u) => {
        Subscription.watchingFor(u).map(d =>
          ".item:title" #> d.fullTitle &
          "a [href]" #> d.infoLink
        )
      }
      case _ => List(ClearClearable)
    }
  }

  def favouriteDocuments = {
    (User.loggedInUser.is.map(u => Subscription.bookmarksFor(u)) getOrElse Nil) match {
      case Nil => ".item" #> Text("None")
      case list => ".item" #> list.map{ d =>
        ".item *" #> d.info()
      }
    }
  }
}
