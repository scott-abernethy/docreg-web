package vvv.docreg.backend

import scala.actors._
import scala.actors.Actor._
import vvv.docreg.model._

case class PriorityReconcile(document: Document)

object Reconciler extends Actor {
  def act() {
    loop {
      react {
        case PriorityReconcile(d) =>
        case _ =>
      }
    }
  }
}
