/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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

  val searchInput = S.param("q") openOr ""

  def input = {
    if (User.loggedIn_?) {
      ".search-input" #> SHtml.onSubmit((s) => {val x = s; S.redirectTo("/search?q=" + escapeInput(x))})
    } else {
      ".all" #> NodeSeq.Empty
    }
  }

  def escapeInput(text: String): String = {
    text.trim.replaceAll("#","%23")
  }

  def bindResults(in: NodeSeq): NodeSeq = {
    val f = UserSession.inStreamFilter()
    val list: List[(Document, Project, Revision, User)] = FilteredDocument.search(searchInput.trim)
    results(in, list.filter(x => f(x._1, x._3, x._2)), list.size >= FilteredDocument.searchLimit)
  }

  var html: NodeSeq = NodeSeq.Empty

  def results(in: NodeSeq): NodeSeq = {
    html = in
    bindResults(html)
  }

  def results(in: NodeSeq, ds: List[(Document, Project, Revision, User)], tooBig: Boolean): NodeSeq = {
    val pageUserId = User.loggedInUser.is.map(_.id) getOrElse -1L
    val inputText = Option(searchInput.trim).getOrElse("")
    val open = ds
//    val (open, restricted) = ds.partition(i => UserSession.isAuthorized(i._1, i._2))
    (
      ".search-for *" #> <span>for &quot;{ inputText }&quot;</span> &
      ".match-count" #> <span>Results <span class="badge">{open.size}</span></span> &
      "#search-too-big" #> (if (tooBig) PassThru else ClearNodes) &
      ".search-item" #> open.map { x =>
        val (d,p,r,u) = x
        ".doc-project" #> p.infoLink() &
        ".doc-author" #> u.knownOption.map(_.profileLabel(pageUserId)).getOrElse(Text(r.rawAuthor)) &
        ".doc-key" #> <a href={d.infoLink}>{d.number}</a> &
        ".doc-date" #> r.dateOnlyWithHint() &
        ".doc-download [href]" #> d.downloadHref() &
        ".doc-title" #> <a href={d.infoLink}>{d.title}</a>
      }
    ).apply(in)
  }

  override def modeSelectionUpdate(): JsCmd = {
    reload()
  }

  override def projectSelectionUpdate(): JsCmd = {
    reload()
  }

  def reload(): JsCmd = {
    Replace("search_results", ("#search_results ^^" #> "noused").apply(bindResults(html)) )
  }
}
