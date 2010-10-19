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

trait ProjectSelection extends Logger {
  def projects(in: NodeSeq): NodeSeq = {
    bind("projects", in, 
      "all" -> SHtml.ajaxButton("All", () => checkAll),
      "none" -> SHtml.ajaxButton("None", () => checkNone),
      "item" -> bindProjects _)
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
  private def checkAll: JsCmd = {
    ProjectSelection.projects.all
    updateProjects & projectSelectionUpdate
  }
  private def checkNone: JsCmd = {
    ProjectSelection.projects.none
    updateProjects & projectSelectionUpdate
  }
  lazy val projectFilterXhtml = TemplateParse.parseDiv(TemplateFinder.findAnyTemplate("index" :: Nil), "project_filter")
  private def updateProjects: JsCmd = {
    SetHtml("project_filter", projects(projectFilterXhtml))
  }
    
  def projectSelectionUpdate: JsCmd = Noop
}

object ProjectSelection {
  import scala.collection.immutable._
  import net.liftweb.http.provider.HTTPCookie

  object projects extends SessionVar[Set[Project]] (findSelected) {
    def all() { save(findAllProjects) }
    def none() { save(Set.empty) }
    def checked(p: Project) { save(is + p) }
    def unchecked(p: Project) { save(is - p) }
    def save(ps: Set[Project]) {
      saveSelected(ps)
      this(ps) 
    }
  }

  val selectedProjectsCookie = "DocRegSelectedProjects"

  def findAllProjects(): Set[Project] = Project.findAll.toSet[Project]

  def findSelected(): Set[Project] = {
    // cookie value is list of selected project ids.
    //println("in " + S.receivedCookies) 
    S.cookieValue(selectedProjectsCookie).map(_.split(",").map(Project.find(_) openOr null).filter(_ != null).toSet[Project]) openOr findAllProjects 
  }

  def saveSelected(ps: Set[Project]) {
    val value = if (ps.isEmpty) "" else ps.map(_.id.is.toString).reduceRight((a, b) => a + ":" + b)
    val cookie = HTTPCookie(selectedProjectsCookie, value).setMaxAge(3600 * 24 * 365)  
    S.addCookie(cookie) 
  }
}
