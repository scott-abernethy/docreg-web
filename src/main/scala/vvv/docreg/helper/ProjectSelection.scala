package vvv.docreg.helper

import net.liftweb._
import common._
import util._
import Helpers._
import http._
import js._
import js.JE.JsRaw
import net.liftweb.http.js.jquery.JqJsCmds
import js.JsCmds._
import js.jquery.JqJsCmds.{Show, Hide}
import scala.xml.{NodeSeq, Text}
import vvv.docreg.model._
import vvv.docreg.util._

trait ProjectSelection extends Loggable {
  def mode = {
    import StreamMode._
    val currentMode = UserSession.mode.is
    "#mode-all [onclick]" #> SHtml.ajaxInvoke(() => showMode(all)) &
    "#mode-all [class+]" #> activeClassIfActive(all) &
    "#mode-select [onclick]" #> SHtml.ajaxInvoke(() => showMode(selected)) &
    "#mode-select [class+]" #> activeClassIfActive(selected) &
    "#mode-watch [onclick]" #> SHtml.ajaxInvoke(() => showMode(watching)) &
    "#mode-watch [class+]" #> activeClassIfActive(watching) &
    "#mode-me [onclick]" #> SHtml.ajaxInvoke(() => showMode(me)) &
    "#mode-me [class+]" #> activeClassIfActive(me)
  }

  def activeClassIfActive(mode: StreamMode.Value): Option[String] = {
    if (UserSession.mode.is == mode) Some("active") else None
  }

  def favouriteProjects(in: NodeSeq): NodeSeq = {
    (
      "#fav-more [onclick]" #> SHtml.ajaxInvoke(() => moreFavs(in)) &
      favs(false)
    ).apply(in)
  }

  def favs(edit: Boolean): CssSel = {
    UserProject.listFor(User.loggedInUser.is.toOption, edit) match {
      case Nil => {
        ".item *" #> "None"
      }
      case config => {
        ".item" #> config.map { i =>
          val project = i._1
          val selected = i._2
          val id = "fup" + project.id
          "li [id]" #> id &
            ".f-name" #> project.infoLink &
            ".f-check" #> createProjectCheck(project, selected) &
            ".f-clr" #> (if (!edit) PassThru else ClearNodes) andThen
            ".f-clr [onclick]" #> SHtml.ajaxInvoke(() => {clearFav(project.id); Hide(id)})
        }
      }
    }
  }

  def moreFavs(in: NodeSeq): JsCmd = {
    Hide("fav-more") &
    Replace("favs", ("#favs ^^" #> "ignored" & favs(true)).apply(in))
  }
  
  private def createProjectCheck(p: Project, initial: Boolean): NodeSeq = {
    SHtml.ajaxCheckbox(initial, checked => projectChecked(p, checked))
  }

  private def projectChecked(project: Project, checked: Boolean): JsCmd = {
    //logger.info("checked " + project.name.is)
    User.loggedInUser.is match {
      case Full(user) => 
        UserProject.set(user, project, checked)
        UserSession.changeSelected(project.id, checked)
        projectSelectionUpdate
      case _ => 
        JsCmds.Noop
    }
  }

  private def clearFav(projectId: Long) {
    User.loggedInUser.is.foreach { user =>
      UserProject.clear(user.id, projectId)
      UserSession.changeSelected(projectId, false)
      projectSelectionUpdate
    }
  }

  def modeSelectionUpdate: JsCmd = {
    Noop
  }

  def projectSelectionUpdate: JsCmd = {
    Noop
  }

  def showMode(mode: StreamMode.Value): JsCmd = {
    UserSession.changeMode(mode)
    modeSelectionUpdate
  }
}

//object ProjectSelection {
//  import scala.collection.immutable._
//
//  object projects extends SessionVar[Set[Project]] (findSelected()) {
//    def all() {  }
//    def none() {  }
//    def checked(p: Project) {  }
//    def unchecked(p: Project) {  }
//    def save(ps: Set[Project]) {
//    }
//  }
//
//  def findSelected(): Set[Project] = {
//    User.loggedInUser.is match {
//      case Full(user) =>
//        UserProject.userSelected(user).toSet
//      case _ =>
//        Set.empty
//    }
//  }
//
//  def isSelected(project: Project): Boolean = {
//    if (.is) return true
//    projects.is.contains(project)
//  }
//}
