package vvv.docreg.comet

import vvv.docreg.model._
import scala.xml.NodeSeq
import net.liftweb._
import http._
import actor._
import common._

object DocumentsServer extends LiftActor with ListenerManager {
  private var documents: List[Document] = Document.findAll

  def createUpdate = documents

  override def lowPriority = {
    case d: Document => 
      documents ::= d
      updateListeners
  }
}

class Documents extends CometActor with CometListener {
  override def defaultPrefix = Full("docs")
  private var documents: List[Document] = Nil

  def registerWith = DocumentsServer

  override def lowPriority = {
    case d: List[Document] => 
      documents = d
      // use partialUpdate instead of reRender, as much more efficient.
      reRender(false)
  }

  def render = bind("docs", "doc" -> bindDocuments _)

  private def bindDocuments(xml: NodeSeq): NodeSeq =
    documents.flatMap(d => bind("docs", xml, 
      "name" -> d.name, 
      "project" -> d.projectName, 
      "title" -> d.title, 
      "author" -> d.author,
      "dateRevised" -> d.dateRevised))
}
