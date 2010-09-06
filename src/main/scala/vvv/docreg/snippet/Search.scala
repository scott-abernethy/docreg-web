package vvv.docreg.snippet

import _root_.scala.xml._
import _root_.net.liftweb._
import http._
import S._
import SHtml._
import common._
import util._
import Helpers._
import js._
import JsCmds._
import _root_.net.liftweb.http.js.jquery.JqJsCmds._
import _root_.net.liftweb.http.js.JE.JsRaw
import vvv.docreg.model.Document

class Search extends Logger {
  object search extends RequestVar("")
  def go(xhtml: NodeSeq): NodeSeq = {
    bind("search", xhtml, 
      "form" -> form _,
      "results" -> <div/> % ("id" -> "results"))
  }
  def form(xhtml: NodeSeq): NodeSeq = {
    ajaxForm (
      bind("search", xhtml,
      "text" -> text(search.is, s => search(s)) ,
      "submit" -> submit("Search", processSearch _)) ++ hidden(processSearch _)
    )
  }
  def processSearch(): JsCmd = {
    val ds = Document.search(search.is) 
    info("Search for '" + search.is + "' resulted in " + ds.size + " documents") 
  val x = ds.flatMap(d => bind("result", 
      <ul>
        <li><result:title /></li>
      </ul>, 
      "title" -> d.title))

    SetHtml("results", x)
  }
}
