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

trait ProjectSelection extends Logger {
  def projects(in: NodeSeq): NodeSeq = {
    val selected = S.findCookie("vvv.docreg.projectsSelected")
    bind("projects", in, "item" -> bindProjects _)
  }
  private def bindProjects(in: NodeSeq): NodeSeq = {
    val checked = ProjectSelection.projects.is
    Project.findAll.flatMap { p =>
      bind("project", in, 
        "name" -> createProjectFocus(p),
        "check" -> createProjectCheck(p, checked contains p))
    }
  }
  private def createProjectFocus(p: Project) = {
    //SHtml.a(() => projectFocused(p), Text(p.name))
    Text(p.name)
  }
  private def createProjectCheck(p: Project, initial: Boolean) = {
    SHtml.ajaxCheckbox(initial, checked => projectChecked(p, checked))
  }
  private def projectFocused(project: Project): JsCmd = {
    //info("focused " + project.name.is)
    projectSelectionUpdate
  }
  private def projectChecked(project: Project, checked: Boolean): JsCmd = {
    //info("checked " + project.name.is)
    val process = if (checked) ProjectSelection.projects.checked _ else ProjectSelection.projects.unchecked _
    process(project)
    projectSelectionUpdate
  }
  def projectSelectionUpdate: JsCmd = Noop
}

object ProjectSelection {
  import scala.collection.immutable._
  import net.liftweb.http.provider.HTTPCookie

  object projects extends SessionVar[Set[Project]] (findSelected) {
    def checked(p: Project) { save(is + p) }
    def unchecked(p: Project) { save(is - p) }
    def save(ps: Set[Project]) {
      saveSelected(ps)
      this(ps) 
    }
  }

  val selectedProjectsCookie = "DocRegSelectedProjects"

  def findSelected(): Set[Project] = {
    // cookie value is list of selected project ids.
    //println("in " + S.receivedCookies) 
    S.cookieValue(selectedProjectsCookie).map(_.split(",").map(Project.find(_) openOr null).filter(_ != null).toSet[Project]) openOr Project.findAll.toSet[Project]
  }

  def saveSelected(ps: Set[Project]) {
    val cookie = HTTPCookie(selectedProjectsCookie, ps.map(_.id.is.toString).reduceRight((a, b) => a + ":" + b)).setMaxAge(3600 * 24 * 365)  
    S.addCookie(cookie) 
  }
}
