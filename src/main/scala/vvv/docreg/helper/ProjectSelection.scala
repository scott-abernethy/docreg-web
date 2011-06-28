package vvv.docreg.helper

import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import js.JsCmds._
import scala.xml.{NodeSeq, Text}
import vvv.docreg.model._
import vvv.docreg.util._
import org.h2.engine.Session

trait ProjectSelection extends Loggable {
  def projects(in: NodeSeq): NodeSeq = {
    bind("projects", in, 
      "all" -> SHtml.ajaxButton("All", () => JsCmds.Noop),
      "none" -> SHtml.ajaxButton("Selected", () => JsCmds.Noop),
      "item" -> bindProjects _)
  }
  private def bindProjects(in: NodeSeq): NodeSeq = {
    // todo
    User.loggedInUser.is match {
      case Full(user) =>
        UserProject.listFor(user).flatMap { i =>
          val project = i._1
          val selected = i._2
          bind("project", in, 
            "name" -> createProjectFocus(project),
            "check" -> createProjectCheck(project, selected))
        }
      case _ => NodeSeq.Empty
    }
  }
  private def createProjectFocus(p: Project) = {
    //SHtml.a(() => projectFocused(p), Text(p.name))
    Text(p.name)
  }
  private def createProjectCheck(p: Project, initial: Boolean) = {
    SHtml.ajaxCheckbox(initial, checked => projectChecked(p, checked))
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
}

object ProjectSelection {
  import scala.collection.immutable._

  object showAll extends SessionVar[Boolean] (false)

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
}
