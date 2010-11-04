package vvv.docreg.helper

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
import vvv.docreg.model.{FilteredDocument,Document}

trait Search extends Loggable {
  private val resultPart: NodeSeq = 
        <table>
          <tr>
            <th>Project</th>
            <th>Doc</th>
            <th>Title</th>
            <th>Author</th>
            <th>Date</th>
          </tr>
          <search:result>
            <tr>
              <td><doc:project/></td>
              <td class="nowrap"><doc:key_link/></td>
              <td><doc:title/></td>
              <td class="nowrap"><doc:author/></td>
              <td class="nowrap"><doc:date/></td>
            </tr>
          </search:result>
        </table>
  object search extends SessionVar("")
  def bindSearch(xhtml: NodeSeq): NodeSeq = {
    bind("search", xhtml, 
      "form" -> form _)
  }
  def form(xhtml: NodeSeq): NodeSeq = {
    ajaxForm (
      bind("search", xhtml,
      "text" -> JsCmds.FocusOnLoad(text(search.is, s => search(s))),
      "submit" -> submit("Search", processSearch _)) ++ hidden(processSearch _)
    )
  }
  def processSearch(): JsCmd = {
    //logger.info("Search for '" + search.is + "'")
    if (search.is.size == 0) {
      Hide("secondary_content", 0) & Show("primary_content", 0)
    } else {
      val keyMatches = results(resultPart, FilteredDocument.searchLike(Document.key, pad(search.is)))
      val titleMatches = results(resultPart, FilteredDocument.searchLike(Document.title, "%" + search.is + "%"))
      SetHtml("secondary_content", keyMatches ++ titleMatches) & Show("secondary_content", 0) & Hide("primary_content", 0)
    }
  }
  def results(in: NodeSeq, ds: List[Document]): NodeSeq = ds match {
    case Nil => Text("")
    case xs => bind("search", in, "result" -> (n => items(n, xs)))
  }
  def items(in: NodeSeq, ds: List[Document]): NodeSeq = {
    ds.flatMap(d => bind("doc", in,
        "project" -> d.projectName,
        "author" -> d.latest.author,
        "key_link" -> <a href={d.latest.link}>{d.key}</a>,
        "date" -> d.latest.date,
        "title" -> <a href={d.infoLink}>{d.title}</a>))
  }
  def pad(key: String) = {
    var x = key
    List.range(x.size, 4).foreach((i) => x = "0" + x)
    x 
  }
}
