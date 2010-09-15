package vvv.docreg.comet

import vvv.docreg.model._
import vvv.docreg.backend._
import scala.xml.{NodeSeq,Text}
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
      add(document.latest)
    case DocumentRevised(document, latest) =>
      add(latest)
    case DocumentChanged(document) =>
      val update = for (
        r <- revisions
        if r.document == document
      ) yield Replace(r.id.is.toString, bindRevision(revisionPart, r))
      partialUpdate(update)
    case _ =>
  }

  private def add(r: Revision) = {
    val remove = revisions.last
    revisions = r :: revisions.dropRight(1)
    partialUpdate(PrependHtml("log", bindRevision(revisionPart, r)) & Replace(remove.id.is.toString, Text("")))
  }

  def render = bind("log", "item" -> bindRevisions _)

  private def bindRevisions(xml: NodeSeq): NodeSeq =
    revisions.flatMap(bindRevision(xml, _))

  private def bindRevision(xml: NodeSeq, r: Revision): NodeSeq = {
    val d: Document = r.document.obj openOr null
    bind("doc", xml, 
      AttrBindParam("id_attr", r.id.is.toString, "id"),
      "link" -> <span><a href={d.latest.link}>{d.key}</a></span><span class="quiet">+<a href={r.link}>{r.version}</a></span>,
      "key" -> d.key,
      "version" -> r.version,
      "project" -> d.projectName, 
      "title" -> d.title, 
      "author" -> r.author,
      "date" -> r.date,
      "comment" -> r.comment)
  }
}
