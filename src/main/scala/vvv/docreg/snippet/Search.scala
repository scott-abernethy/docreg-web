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
import xml.NodeSeq

class Search extends Loggable with ProjectSelection {
  object searchInput extends SessionVar("")

  def input = {
    if (User.loggedIn_?) {
      ".search-input" #> SHtml.text(searchInput.is, s => searchInput(s), "class" -> "search-query input-medium") &
      ".search-submit" #> SHtml.onSubmit((s) => S.redirectTo("/search"))
    } else {
      ".all" #> NodeSeq.Empty
    }
  }

  def bindResults(in: NodeSeq): NodeSeq =
  {
    results(in, FilteredDocument.search(searchInput.is))
  }

  var html: NodeSeq = NodeSeq.Empty

  def results(in: NodeSeq): NodeSeq = {
    html = in
    bindResults(html)
  }

  def results(in: NodeSeq, ds: List[(Document, Revision, User, Project)]): NodeSeq = {
    items(in, ds)
  }

  def items(in: NodeSeq, ds: List[(Document, Revision, User, Project)]): NodeSeq =
  {
    (
      ".match-count" #> pluralise(ds.size, "match", "es") &
      ".search-item" #> ds.map { x =>
        val (d,r,u,p) = x
        ".doc-project" #> p.name &
        ".doc-author" #> u.profileLink &
        ".doc-key" #> <a href={d.infoLink}>{d.key}</a> &
        ".doc-date" #> r.dateOnly &
        ".doc-title" #> <a href={d.infoLink}>{d.title}</a>
      }
    ).apply(in)
  }

  override def projectSelectionUpdate(): JsCmd = {
    CurrentLog.foreach(_ ! ReloadLog())
    Replace("search_results", bindResults(html))
  }
}
