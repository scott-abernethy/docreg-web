package vvv.docreg.util

import scala.xml._
import net.liftweb.common._

object TemplateParse {
  def parseDiv(template: Box[NodeSeq], id: String): NodeSeq = template match {
    case Full(in) => in \\ "div" filter (d => (d \ "@id").text == "project_filter")
    case _ => NodeSeq.Empty
  }
}
