package vvv.docreg.snippet

import vvv.docreg.comet._
import vvv.docreg.model._
import vvv.docreg.helper._
import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import js.JsCmds._
import scala.xml.{NodeSeq, Text}

class Dashboard extends Loggable 
  with ProjectSelection with Search {
  def search(in: NodeSeq): NodeSeq = {
    bindSearch(in)
  }
  def filter(in: NodeSeq): NodeSeq = {
    projects(in)
  }
  def log(in: NodeSeq): NodeSeq = {
    in
  }
  lazy val indexXhtml = TemplateFinder.findAnyTemplate("index" :: Nil) openOr <div/>
  //lazy val logXhtml = indexXhtml \\ "snippet" filter ( _.attribute("type") == Some(Text("Dashboard.log")) )
  lazy val logXhtml = indexXhtml \\ "surround" \ "div"
  override def projectSelectionUpdate(): JsCmd = {
    CurrentLog.foreach(_ ! ReloadLog())
    SetHtml("primary_content", logXhtml) & processSearch
  }
  // later use this to bind? and seperate bits as seperate templates. because it is going to have to be a sethtml js response.
/*  val xhtml = TemplateFinder.findAnyTemplate("path" :: "to" :: "file" :: Nil) match {
  case Full(template) => bind("hello", template, "world" -> "Mads says hi!")
  case _ => Text("Im sorry man, but the template file was nowhere to be found")
}*/
}
