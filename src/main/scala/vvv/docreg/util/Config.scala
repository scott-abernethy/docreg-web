package vvv.docreg.util

import net.liftweb.util.Props
import org.streum.configrity.Configuration

object Config {
  lazy val is = Props.get("conf") map(Configuration.load(_)) getOrElse (Configuration())
}
