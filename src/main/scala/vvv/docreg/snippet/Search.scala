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
import vvv.docreg.helper.ProjectSelection
import vvv.docreg.comet._
import vvv.docreg.model.{User, FilteredDocument, Document}

class Search extends Loggable with ProjectSelection {
  object searchInput extends SessionVar("")

  def input = {
    if (User.loggedIn_?) {
      ".search-input" #> SHtml.text(searchInput.is, s => searchInput(s), "class" -> "input-small span5") &
      ".search-submit" #> SHtml.submit("Search", () => S.redirectTo("/search"), "class" -> "btn info")
    } else {
      ".all" #> NodeSeq.Empty
    }
  }

  def bindResults(in: NodeSeq): NodeSeq = {
    import vvv.docreg.util.StringUtil._
    // todo better parsing of search string, from 
    var found: List[Document] = FilteredDocument.searchLike(Document.title, formatSearch(searchInput.is))
    if (!in.isEmpty) {
      found = FilteredDocument.searchLike(Document.key, prePadTo(searchInput.is, 4, '0')) ::: found
    }
    results(in, found)
  }

  var html: NodeSeq = NodeSeq.Empty

  def results(in: NodeSeq): NodeSeq = {
    html = in
    bindResults(html)
  }

  def results(in: NodeSeq, ds: List[Document]): NodeSeq = {
    items(in, ds)
  }

  def items(in: NodeSeq, ds: List[Document]): NodeSeq = {
    (".search-item" #> ds.map { d =>
      ".doc-project" #> d.projectName &
        ".doc-author" #> d.latest.authorLink &
        ".doc-key_link" #> <a href={d.latest.link}>{d.key}</a> &
        ".doc-date" #> d.latest.dateOnly &
        ".doc-title" #> <a href={d.infoLink}>{d.title}</a>
    }).apply(in)
  }

  override def projectSelectionUpdate(): JsCmd = {
    CurrentLog.foreach(_ ! ReloadLog())
    Replace("search_results", bindResults(html))
  }

  def formatSearch(in: String): String = {
    val out: Option[String] = for {
      s <- Option(in)
      surrounded = "%" + s + "%"
    } yield surrounded.replaceAll("[ *]", "%").replaceAll("[%]+", "%")

    out.getOrElse("%")
  }
}
