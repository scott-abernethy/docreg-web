package vvv.docreg.comet

import vvv.docreg.model._
import vvv.docreg.backend._
import scala.xml.NodeSeq
import net.liftweb._
import http._
import actor._
import common._
import mapper._
import util.Helpers._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds._

class Log extends DocumentSubscriber {
  private var revisions: List[Revision] = Nil
  private lazy val revisionPart = deepFindKids(defaultXml, "log", "item")

  override def defaultPrefix = Full("log")

  override def localSetup {
    DocumentServer ! Subscribe(this)
    super.localSetup
  }

  override def localShutdown {
    DocumentServer ! Unsubscribe(this)
    super.localShutdown
  }

  override def lowPriority = {
    case Subscribed() => 
      revisions = Revision.findAll(OrderBy(Revision.date, Descending), MaxRows(20))
      reRender(true)
    case DocumentAdded(document) =>
      partialUpdate(PrependHtml("log", bindRevision(revisionPart, document.latest)))
    case DocumentRevised(document, latest) =>
      partialUpdate(PrependHtml("log", bindRevision(revisionPart, latest)))
    case DocumentChanged(document) =>
      //partialUpdate(PrependHtml("log", bindRevision(revisionPart, document.latest)))
    case _ =>
  }

  def render = bind("log", "item" -> bindRevisions _)

  private def bindRevisions(xml: NodeSeq): NodeSeq =
    revisions.flatMap(bindRevision(xml, _))

  private def bindRevision(xml: NodeSeq, r: Revision): NodeSeq = {
    val d: Document = r.document.obj openOr null
    bind("doc", xml, 
      AttrBindParam("id_attr", r.id.is.toString, "id"),
      "key_link" -> <a href={r.link}>{d.key}</a>,
      "project" -> d.projectName, 
      "title" -> d.title, 
      "author" -> r.author,
      "date" -> r.date)
  }
}
