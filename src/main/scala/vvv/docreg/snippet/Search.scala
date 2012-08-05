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
//  object searchInput extends RequestVar("")

  val searchInput = S.param("q") openOr ""

  def input = {
    if (User.loggedIn_?) {
      ".search-input" #> SHtml.onSubmit((s) => {val x = s; S.redirectTo("/search?q=" + x)})
    } else {
      ".all" #> NodeSeq.Empty
    }
  }

  def bindResults(in: NodeSeq): NodeSeq =
  {
    val f = UserSession.inStreamFilter()
    results(in, FilteredDocument.search(searchInput).filter(x => f(x._1, x._3, x._2)))
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
    val pageUserId = User.loggedInUser.is.map(_.id) getOrElse -1L
    val inputText = Option(searchInput).getOrElse("")
    val open = ds
//    val (open, restricted) = ds.partition(i => UserSession.isAuthorized(i._1, i._2))
    (
      ".search-for *" #> <span>for &quot;{ inputText }&quot;</span> &
      ".match-count" #> <span>Results <span class="badge">{open.size}</span></span> &
      ".search-item" #> open.map { x =>
        val (d,p,r,u) = x
        ".doc-project" #> p.infoLink() &
        ".doc-author" #> u.knownOption.map(_.profileLabel(pageUserId)).getOrElse(Text(r.rawAuthor)) &
        ".doc-key" #> <a href={d.infoLink}>{d.number}</a> &
        ".doc-date" #> r.dateOnlyWithHint() &
        ".doc-title" #> <a href={d.infoLink}>{d.title}</a>
      }
    ).apply(in)
  }

  override def projectSelectionUpdate(): JsCmd = {
//    CurrentLog.foreach(_ ! ResetLog)
    Replace("search_results", ("#search_results ^^" #> "noused").apply(bindResults(html)) )
  }
}
