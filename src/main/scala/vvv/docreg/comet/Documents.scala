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

class Documents extends DocumentSubscriber {
  private var documents: List[Document] = Nil
  private lazy val documentPart = deepFindKids(defaultXml, "docs", "doc")

  override def defaultPrefix = Full("docs")

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
      documents = Document.findAll(MaxRows(20))
      reRender(true)
    case DocumentAdded(document) =>
      documents = Document.findAll(MaxRows(20))
      //documents ::= document
      reRender(true)
    case DocumentRevised(document) =>
      // use partialUpdate instead of reRender, as much more efficient.
      partialUpdate(Replace(document.name, bindDocument(documentPart, document)))
    case DocumentChanged(document) =>
      partialUpdate(Replace(document.name, bindDocument(documentPart, document)))
    case _ =>
  }

  // load all, does it meet filter? show all.
  // update, does it meet filter? insert.

  def render = bind("docs", "doc" -> bindDocuments _)

  private def bindDocuments(xml: NodeSeq): NodeSeq =
    documents.flatMap(bindDocument(xml, _))

  private def bindDocument(xml: NodeSeq, d: Document): NodeSeq =
    bind("docs", xml, 
      AttrBindParam("name_attr", d.name.is, "id"),
      "name" -> d.name, 
      "name_link" -> <a href={d.latest.filename}>{d.name}</a>,
      "project" -> d.projectName, 
      "title" -> d.title, 
      "author" -> d.author,
      "editor" -> d.editor,
      "dateRevised" -> d.dateRevised)
}
