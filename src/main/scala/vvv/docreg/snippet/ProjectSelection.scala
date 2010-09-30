package vvv.docreg.snippet

import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import js.JsCmds._
import scala.xml.{NodeSeq, Text}
import vvv.docreg.model._

class ProjectSelection extends Logger {
  def snippet(in: NodeSeq): NodeSeq = {
    val selected = S.findCookie("vvv.docreg.projectsSelected")
    bind("projects", in, "item" -> bindProjects _)
  }
  private def bindProjects(in: NodeSeq): NodeSeq = {
    Project.findAll.flatMap { p =>
      bind("project", in, 
        "name" -> createProjectFocus(p),
        "check" -> createProjectCheck(p))
    }
  }
  private def createProjectFocus(p: Project) = {
    SHtml.a(() => projectFocused(p), Text(p.name))
  }
  private def createProjectCheck(p: Project) = {
    SHtml.ajaxCheckbox(true, checked => projectChecked(p, checked))
  }
  private def projectFocused(project: Project): JsCmd = {
    info("focused " + project.name.is)
    Noop
  }
  private def projectChecked(project: Project, checked: Boolean): JsCmd = {
    info("checked " + project.name.is)
    Noop
  }
}
