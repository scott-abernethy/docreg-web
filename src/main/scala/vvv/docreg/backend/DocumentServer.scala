package vvv.docreg.backend

import scala.actors._
import scala.actors.Actor._
import net.liftweb.http._
import net.liftweb.actor._
import net.liftweb.util._
import vvv.docreg.model._

case class Subscribe(subscriber: DocumentSubscriber)
case class Subscribed()
case class Unsubscribe(subscriber: DocumentSubscriber)
case class DocumentAdded(document: Document)
case class DocumentRevised(document: Document)
case class DocumentChanged(document: Document)

trait DocumentSubscriber extends CometActor {
}

object DocumentServer extends Actor {
  private var subscribers: List[DocumentSubscriber] = Nil
  def act() {
    loop {
      react {
        case Subscribe(subscriber) =>
          subscribers ::= subscriber
          subscriber ! Subscribed()
        case Unsubscribe(subscriber) =>
          subscribers -= subscriber
        case a @ DocumentAdded(d) =>
          println(d.name + " added")
          distribute(a)
        case r @ DocumentRevised(d) => 
          println(d.name + " revised")
          distribute(r)
        case c @ DocumentChanged(d) => 
          println(d.name + " changed")
          distribute(c)
        case _ =>
      }
    }
  }

  private def distribute(msg: AnyRef) { subscribers.foreach(_ ! msg) }
}
