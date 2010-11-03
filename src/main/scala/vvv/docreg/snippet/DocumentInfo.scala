package vvv.docreg.snippet

import vvv.docreg.backend._
import vvv.docreg.model._
import net.liftweb._
import util._
import common._
import Helpers._
import http._
import js._
import scala.xml.{NodeSeq, Text}

class DocumentView extends Logger {
  var key = S.param("key") openOr ""

  var document: Document = try {
    Document.forKey(key)
  } catch {
    case e:NumberFormatException => null 
  }

  def view(xhtml: NodeSeq): NodeSeq = {
    if (document == null) {
      Text("Invalid document '" + key + "'")
    } else {
      bind("doc", xhtml,
        "key" -> document.key,
        "title" -> document.title,
        "author" -> document.latest.author,
        "revised" -> document.latest.date,
        "link" -> ((in: NodeSeq) => <a href={document.latest.link}>{in}</a>),
        "version" -> document.latest.version,
        "project" -> document.projectName,
        "edit" -> (if (document.editor.is == null) Text("-") else <span class="highlight">{document.editor.asHtml}</span>),
        "revision" -> revisions _,
        "approval" -> approvalForm _)
    }
  }

  private def revisions(xhtml: NodeSeq): NodeSeq = {
    document.revisions flatMap { r =>
      bind("rev", xhtml,
        "version" -> r.version,
        "author" -> r.author,
        "date" -> r.date,
        "approvals" -> ((in: NodeSeq) => approvals(in, r)),
        "link" -> <a href={r.link}>{r.version.asHtml}</a>,
        "comment" -> r.comment)
    }
  }

  private def approvalForm(xhtml: NodeSeq): NodeSeq = {
    bind("approval", xhtml,
      "approve" -> SHtml.a(() => {approve}, Text("Approve")))
  }
  private def approve(): JsCmd = {
    Backend ! ApprovalApproved(document, document.latest)
    JsCmds.Noop
  }

  private def approvals(xhtml: NodeSeq, r: Revision): NodeSeq = {
    Approval.forRevision(r) flatMap {a =>
      bind("approval", xhtml,
        "by" -> a.by,
        "state" -> a.state,
        "comment" -> a.comment,
        "date" -> a.date)
    }
  }
}
