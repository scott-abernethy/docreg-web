package vvv.docreg.snippet

import vvv.docreg.model._
import net.liftweb._
import util._
import common._
import Helpers._
import http._
import scala.xml._

class DocumentView {
  var key = S.param("key") openOr ""

  var document: vvv.docreg.model.Document = try {
    Document.forKey(key)
  } catch {
    case e:NumberFormatException => null 
  }

  def view(xhtml: NodeSeq): NodeSeq = {
    if (document == null) {
      Text("Invalid document '" + key + "'")
    } else {
      bind("doc", xhtml,
        "title" -> document.title,
        "revision" -> revisions _)
    }
  }

  private def revisions(xhtml: NodeSeq): NodeSeq = {
    document.revisions flatMap { r =>
      bind("rev", xhtml,
        "version" -> r.version,
        "author" -> r.author,
        "date" -> r.date,
        "link" -> <span><a href={r.link}>{r.version}</a></span>,
        "comment" -> r.comment)
    }
  }
}
