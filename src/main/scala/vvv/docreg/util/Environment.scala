package vvv.docreg.util

import vvv.docreg.backend.{AgentComponent, DocumentServerComponent, BackendComponent, DirectoryComponent}

trait Environment extends BackendComponent with DocumentServerComponent with AgentComponent with DirectoryComponent {
  def start()
}

object Environment {
  var env: Environment = _
}