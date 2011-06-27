package vvv.docreg.backend

import scala.actors._
import scala.actors.Actor._
import vvv.docreg.model._

case class PriorityReconcile(d: Document)

class Reconciler(private val backend: Actor) extends Actor {
  def act() {
    loop {
      react {
        case PriorityReconcile(d) => backend ! Reload(d)
        case _ =>
      }
    }
  }
}
