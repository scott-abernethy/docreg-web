package vvv.docreg.backend

import scala.actors._
import net.liftweb.http._
import net.liftweb.actor._
import vvv.docreg.model._

case class DocumentAdded(document: Document)
case class DocumentRevised(document: Document)
case class DocumentNotice(document: Document)

object DocumentServer extends LiftActor with ListenerManager {
  private var message: Any = _
  override def createUpdate = message
  override def lowPriority = {
    case x @ _ =>
      message = x
      updateListeners
  }
}
