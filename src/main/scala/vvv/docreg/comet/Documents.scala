package vvv.docreg.comet

import vvv.docreg.model._
import vvv.docreg.backend._
import scala.xml.NodeSeq
import net.liftweb._
import http._
import actor._
import common._

class Documents extends CometActor with CometListener {
  private var documents: List[Document] = Document.findAll

  def registerWith = DocumentServer 

  override def defaultPrefix = Full("docs")

  override def lowPriority = {
    case DocumentAdded(document) =>
    case DocumentRevised(document) =>
    case DocumentNotice(document) =>
      //documents = d
      // use partialUpdate instead of reRender, as much more efficient.
      //reRender(false)
    case _ =>
  }

  // load all, does it meet filter? show all.
  // update, does it meet filter? insert.

  def render = bind("docs", "doc" -> bindDocuments _)

  private def bindDocuments(xml: NodeSeq): NodeSeq =
documents.flatMap(d => bind("docs", xml, 
      AttrBindParam("name_attr", d.name.is, "id"),
      "name" -> d.name, 
      "project" -> d.projectName, 
      "title" -> d.title, 
      "author" -> d.author,
      "dateRevised" -> d.dateRevised))
}
