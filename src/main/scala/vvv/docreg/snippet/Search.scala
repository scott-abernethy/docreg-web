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
  def input(xhtml: NodeSeq): NodeSeq = {
    if (User.loggedIn_?) {
      bind("search", xhtml, "form" -> form _)
    } else {
      NodeSeq.Empty
    }
  }
  def form(xhtml: NodeSeq): NodeSeq = {
    bind("search", xhtml,
      "text" -> JsCmds.FocusOnLoad(SHtml.text(searchInput.is, s => searchInput(s)) % ("style" -> "width: 250px")),
      "submit" -> SHtml.submit("Search", () => S.redirectTo("/search")))
  }
  def bindResults(in: NodeSeq): NodeSeq = {
    import vvv.docreg.util.StringUtil._
    // todo better parsing of search string, from 
    val titleMatches = results(in, FilteredDocument.searchLike(Document.title, formatSearch(searchInput.is)))
    if (!in.isEmpty) {
      val keyMatches = results(in, FilteredDocument.searchLike(Document.key, prePadTo(searchInput.is, 4, '0')))
      keyMatches ++ titleMatches
    } else {
      titleMatches
    }
  }
  var html: NodeSeq = NodeSeq.Empty
  def results(in: NodeSeq): NodeSeq = {
    html = in
    bindResults(in)
  }
  def results(in: NodeSeq, ds: List[Document]): NodeSeq = ds match {
    case Nil => Text("")
    case xs => bind("search", in, "result" -> (n => items(n, xs)))
  }
  def items(in: NodeSeq, ds: List[Document]): NodeSeq = {
    ds.flatMap(d => bind("doc", in,
        "project" -> d.projectName,
        "author" -> d.latest.authorLink,
        "key_link" -> <a href={d.latest.link}>{d.key}</a>,
        "date" -> d.latest.dateOnly,
        "title" -> <a href={d.infoLink}>{d.title}</a>))
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
