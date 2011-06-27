package vvv.docreg.util

import vvv.docreg.backend.{DocumentServerComponent, Backend}

trait Environment extends Backend with DocumentServerComponent {
  def start()
}

object Environment {
  var env: Environment = _
}