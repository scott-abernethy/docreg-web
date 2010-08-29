package vvv.docreg.backend

import scala.actors._
import scala.actors.Actor._

class SimulatedBackend extends Backend {
  def act = loop {
    react {
      case _ =>
    }
  }
}
