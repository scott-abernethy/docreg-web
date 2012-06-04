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
    results(in, FilteredDocument.search(searchInput.is))
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
    // TODO store the authorized projects list against the user session, no need to requery here.
    val user = User.loggedInUser.is.toOption
    val authorizedProjects: Set[Long] = user.map(u => ProjectAuthorization.authorizedProjectsFor(u).map(_.id).toSet).getOrElse(Set.empty[Long])
    val (open, restricted) = ds.partition(i => i._1.secure_?() == false || authorizedProjects.contains(i._2.id))
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
          ".d-restricted-count *" #> restricted.size
      }
    ).apply(in)
  }

  override def projectSelectionUpdate(): JsCmd = {
    CurrentLog.foreach(_ ! ReloadLog())
    Replace("search_results", bindResults(html))
  }
}
