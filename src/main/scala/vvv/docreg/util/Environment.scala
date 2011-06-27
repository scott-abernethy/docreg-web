package vvv.docreg.util

import vvv.docreg.backend.{AgentComponent, DocumentServerComponent, BackendComponent}

trait Environment extends BackendComponent with DocumentServerComponent with AgentComponent {
  def start()
}

object Environment {
  var env: Environment = _
}