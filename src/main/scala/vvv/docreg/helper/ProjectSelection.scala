package vvv.docreg.helper

import net.liftweb._
import common._
import util._
import Helpers._
import http._
import js._
import js.JsCmds._
import js.jquery.JqJsCmds.{Show, Hide}
import scala.xml.{NodeSeq, Text}
import vvv.docreg.model._
import vvv.docreg.util._

trait ProjectSelection extends Loggable {
  def projects(in: NodeSeq): NodeSeq = {
    val map = Map("All" -> (() => showAll(true)),
                  "Selected" -> (() => showAll(false)))
    val default = if (ProjectSelection.showAll.is) Full("All") else Full("Selected")
    val choices = SHtml.ajaxRadio(map.keys.toSeq, default, (selected: String) => map(selected).apply())

    bind("projects", in,
      "all" -> <label>{choices("All")}All</label>,
      "none" -> <label>{choices("Selected")}Selected</label>,
      "item" -> bindProjects _)
  }
  private def bindProjects(in: NodeSeq): NodeSeq = {
    val showAll = ProjectSelection.showAll.is
    User.loggedInUser.is match {
      case Full(user) =>
        UserProject.listFor(user).flatMap { i =>
          val project = i._1
          val selected = i._2
          bind("project", in, 
            "name" -> Text(project.name),
            "check" -> createProjectCheck(project, selected, !showAll))
        }
      case _ => NodeSeq.Empty
    }
  }
  private def createProjectCheck(p: Project, initial: Boolean, show: Boolean) = {
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
  lazy val projectFilterXhtml = TemplateParse.parseDiv(TemplateFinder.findAnyTemplate("index" :: Nil), "project_filter")
  private def updateProjects: JsCmd = {
    SetHtml("project_filter", projects(projectFilterXhtml))
  }
    
  def projectSelectionUpdate: JsCmd = Noop

  def showAll(s: Boolean): JsCmd = {
    ProjectSelection.showAll(s)
    val toggleCheckboxes: JsCmd = if (s) Hide(".projects input") else Show(".projects input")
    projectSelectionUpdate & toggleCheckboxes
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
