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

  def projects = {
    val all = ProjectSelection.showAll.is
    "#projects-all *" #> SHtml.a(() => showAll(true), Text("All")) &
    "#projects-sel *" #> SHtml.a(() => showAll(false), Text("Selected")) &
    "#projects-all [class]" #> (if (all) "active" else "inactive") &
    "#projects-sel [class]" #> (if (!all) "active" else "inactive") &
    ".project-item" #> UserProject.listFor(User.loggedInUser.is.toOption).map { i =>
      val project = i._1
      val selected = i._2
      ".project-name" #> project.infoLink &
      ".project-check" #> createProjectCheck(project, selected, !all)
    }
  }
  
  private def createProjectCheck(p: Project, initial: Boolean, show: Boolean): NodeSeq = {
    SHtml.ajaxCheckbox(initial, checked => projectChecked(p, checked)).%("style" -> (if (show) "" else "display:none"))
  }

  private def projectChecked(project: Project, checked: Boolean): JsCmd = {
    //logger.info("checked " + project.name.is)
    User.loggedInUser.is match {
      case Full(user) => 
        UserProject.set(user, project, checked)
        // todo inefficient
        ProjectSelection.projects(ProjectSelection.findSelected())
        projectSelectionUpdate
      case _ => 
        JsCmds.Noop
    }
  }
  lazy val projectFilterXhtml = TemplateParse.parseDiv(Templates("index" :: Nil), "project_filter")
  private def updateProjects: JsCmd = {
    SetHtml("project_filter", projects(projectFilterXhtml))
  }
    
  def projectSelectionUpdate: JsCmd = {
    Noop
  }

  def showAll(s: Boolean): JsCmd = {
    ProjectSelection.showAll(s)
    val toggleCheckboxes: JsCmd = if (s) Hide(".projects input") else Show(".projects input")
    val toggleTabs: JsCmd = if (s) JsRaw("$('#projects-all').addClass('active');$('#projects-sel').removeClass('active')").cmd else JsRaw("$('#projects-all').removeClass('active');$('#projects-sel').addClass('active')").cmd
    projectSelectionUpdate & toggleTabs & toggleCheckboxes
  }
}

object ProjectSelection {
  import scala.collection.immutable._

  object showAll extends SessionVar[Boolean] (true)

  object projects extends SessionVar[Set[Project]] (findSelected()) {
    def all() {  }
    def none() {  }
    def checked(p: Project) {  }
    def unchecked(p: Project) {  }
    def save(ps: Set[Project]) {
    }
  }

  def findSelected(): Set[Project] = {
    User.loggedInUser.is match {
      case Full(user) =>
        UserProject.userSelected(user).toSet
      case _ =>
        Set.empty
    }
  }

  def isSelected(project: Project): Boolean = {
    if (showAll.is) return true
    projects.is.contains(project)
  }
}
