package vvv.docreg.snippet

import vvv.docreg.backend._
import vvv.docreg.model._
import vvv.docreg.model.ApprovalState._
import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import scala.xml.{NodeSeq, Text}

class DocumentSnippet extends Logger {
  val key = S.param("key") openOr ""
  val version: Long = S.param("version") map (_.toLong) openOr -1 
  val document: Box[Document] = try {
    Document.forKey(key)
  } catch {
    case e:NumberFormatException => null 
  }
  val revision: Box[Revision] = document match {
    case Full(d) => Revision.forDocument(d, version)
    case _ => Empty
  }

  def info(xhtml: NodeSeq): NodeSeq = document match {
    case Full(d) =>
      bind("doc", xhtml,
        "key" -> d.key,
        "title" -> d.title,
        "author" -> d.latest.author,
        "revised" -> d.latest.date,
        "link" -> ((in: NodeSeq) => <a href={d.latest.link}>{in}</a>),
        "version" -> d.latest.version,
        "project" -> d.projectName,
        "edit" -> (if (d.editor.is == null) Text("-") else <span class="highlight">{d.editor.asHtml}</span>),
        "revision" -> revisions _,
        "approve" -> ((in: NodeSeq) => <a href={"/d/" + d.key + "/v/" + d.latest.version + "/approve"}>{in}</a>))
    case _ =>
      Text("Invalid document '" + key + "'")
  }

  private def revisions(xhtml: NodeSeq): NodeSeq = document match {
    case Full(d) => d.revisions flatMap { r =>
      bind("rev", xhtml,
        "version" -> r.version,
        "author" -> r.author,
        "date" -> r.date,
        "approvals" -> ((in: NodeSeq) => approvals(in, r)),
        "link" -> <a href={r.link}>{r.version.asHtml}</a>,
        "comment" -> r.comment)
    }
    case _ => Text("")
  }

  private def approvals(xhtml: NodeSeq, r: Revision): NodeSeq = {
    Approval.forRevision(r) flatMap {a =>
      bind("approval", xhtml,
        "by" -> (a.by.obj.map (_.displayName) openOr "?"),
        "state" -> <span style={ApprovalState.style(a.state.is)}>{a.state}</span>,
        "comment" -> a.comment,
        "date" -> a.date)
    }
  }

  def approve(in: NodeSeq): NodeSeq = document match {
    case Full(d) => revision match {
      case Full(r) =>
        if (!d.latest_?(r.version.is)) S.warning("Approval is not for the most recent version")
        // TODO warning if document is being editted!
        bind("doc", in,
          "key" -> d.key,
          "title" -> d.title,
          "author" -> r.author,
          "revised" -> r.date,
          "link" -> ((in: NodeSeq) => <a href={r.link}>{in}</a>),
          "version" -> r.version,
          "project" -> d.projectName,
          "edit" -> (if (d.editor.is == null) Text("-") else <span class="highlight">{d.editor.asHtml}</span>),
        "approval" -> ((in: NodeSeq) => approvalForm(in, d, r)))
      case _ => S.error("Invalid revision '" + version + "'"); NodeSeq.Empty
    }
    case _ => S.error("Invalid document '" + key + "'"); NodeSeq.Empty
  }

  private def approvalForm(in: NodeSeq, d: Document, r: Revision): NodeSeq = {
    val states = List(ApprovalState.approved, ApprovalState.notApproved) map (state => (state.toString, state.toString))
    var comment = ""
    var state = ApprovalState.approved
    bind("approval", in,
      "version" -> r.version,
      "by" -> Text(User.loggedInUser map (_.displayName) openOr "?"),
      "state" -> SHtml.select(states, Full(state.toString), (selected) => (state = ApprovalState parse selected)),
      "comment" -> SHtml.textarea(comment, comment = _) % ("class" -> "smalltextarea"),
      "submit" -> SHtml.submit("Submit", () => processApprove(d, r, state, comment)),
      "cancel" -> SHtml.submit("Cancel", () => S.redirectTo("/"))
    )
  }
  
  def processApprove(d: Document, r: Revision, state: ApprovalState, comment: String): JsCmd = {
    val user = User.loggedInUser.is openOr null
    val approval = Approval.create
    approval.revision(r)
    approval.by(user)
    approval.state(state)
    approval.comment(comment)
    approval.date(new java.util.Date)
    approval.save
    Backend ! ApprovalApproved(d, r, user, state, comment)
    S.notice("Document " + state)
    S.redirectTo(r.info)
  }
}
