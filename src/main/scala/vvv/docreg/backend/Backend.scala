package vvv.docreg.backend

import com.hstx.docregsx._
import scala.actors._
import scala.actors.Actor._
import scala.collection.JavaConversions._
import vvv.docreg.model._

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

case class Connect()

class Backend extends Actor {
  def act() {
    loop {
      react {
        case Connect() => 
          val server = "shelob.GNET.global.vpn"
          val agent = new Agent("0.1", server, "docreg-web")
          val library = new FileList(server, agent)
          library.addUpdateListener(new UpdateListener() {
            def updated(ds: java.util.List[Doc]) = ds.foreach(Backend.this ! _)
            def updated(d: Doc) = Backend.this ! d 
          })
        case d: Doc => 
          
          val document = Document.forName(d.getKey)
          if (document == null) {
            createDocument(d)
          } else {
            updateDocument(document, d)
          }
      }
    }
  }
  
  private def projectWithName(name: String) = {
    val existing = Project.forName(name) 
    if (existing == null) {
      val project = Project.create
      project.name(name)
      project.save
      project
      // TODO notify new project? Or do it as notification on save.
    } else {
      existing
    }
  }

  private def createDocument(d: Doc) {
    val document = Document.create
    document.name(d.getKey)
    document.project(projectWithName(d.getProject))
    document.title(d.getTitle)
    document.save

    val r1 = Revision.create
    r1.document(document)
    r1.version(1)
    r1.filename(d.getDocument)
    r1.author(d.getAuthor)
    r1.date(new java.util.Date)
    r1.comment(d.getDescription)
    r1.save

    DocumentServer ! DocumentAdded(document)
  }
  
  private def updateDocument(document: Document, d: Doc) {
  }
}
