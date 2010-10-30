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
        "project" -> document.projectName,
        "edit" -> (if (document.editor.is == null) Text("") else <p><em>Editor:</em>{document.editor.is}</p>),
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
        "link" -> <span><a href={r.link}>{r.version}</a></span>,
        "comment" -> r.comment)
    }
  }

  private def approvalForm(xhtml: NodeSeq): NodeSeq = {
    SHtml.ajaxForm (
      bind("approval", xhtml,
        "approve" -> SHtml.submit("Approve", approve _)) ++ SHtml.hidden(approve _)
    )
  }
  private def approve(): JsCmd = {
    Backend ! ApprovalApproved(document, document.latest)
    JsCmds.Noop
  }

  private def approvals(xhtml: NodeSeq, r: Revision): NodeSeq = {
    Approval.forRevision(r) flatMap {a =>
      bind("approval", xhtml,
        "state" -> a.state,
        "comment" -> a.comment,
        "date" -> a.date)
    }
  }
}
