package vvv.docreg.snippet

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
import vvv.docreg.helper.ProjectSelection
import vvv.docreg.comet._
import vvv.docreg.util.StringUtil._
import vvv.docreg.model._
import xml.{Text, NodeSeq}
import vvv.docreg.util.Bits

class Search extends Loggable with ProjectSelection {
  object searchInput extends RequestVar("")

  def input = {
    if (User.loggedIn_?) {
      ".search-input" #> SHtml.onSubmit((s) => {val x = s; S.redirectTo("/search", () => searchInput(x))})
    } else {
      ".all" #> NodeSeq.Empty
    }
  }

  def bindResults(in: NodeSeq): NodeSeq =
  {
    results(in, FilteredDocument.search(searchInput.is).filter(x => UserSession.inStream(x._1, x._3, x._2)))
  }

  var html: NodeSeq = NodeSeq.Empty

  def results(in: NodeSeq): NodeSeq = {
    html = in
    bindResults(html)
  }

  def results(in: NodeSeq, ds: List[(Document, Project, Revision, User)]): NodeSeq = {
    items(in, ds)
  }

  def items(in: NodeSeq, ds: List[(Document, Project, Revision, User)]): NodeSeq =
  {
    val inputText = Option(searchInput.is).getOrElse("")
    val (open, restricted) = ds.partition(i => UserSession.isAuthorized(i._1, i._2))
    (
      ".match-count" #> <span>Results for &quot;{ inputText }&quot; <span class="badge">{open.size}</span></span> &
      ".search-item" #> open.map { x =>
        val (d,p,r,u) = x
        ".doc-project" #> p.name &
        ".doc-author" #> u.profileLink &
        ".doc-key" #> <a href={d.infoLink}>{d.number}</a> &
        ".doc-date" #> r.dateOnly &
        ".doc-title" #> <a href={d.infoLink}>{d.title}</a>
      } &
      ".d-restricted" #> restricted.headOption.map{ x =>
        Bits.restrictedNotice(restricted.size)
      }
    ).apply(in)
  }

  override def projectSelectionUpdate(): JsCmd = {
    CurrentLog.foreach(_ ! ReloadLog())
    Replace("search_results", ("#search_results ^^" #> "noused").apply(bindResults(html)) )
  }
}
