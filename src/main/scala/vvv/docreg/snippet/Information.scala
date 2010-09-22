package vvv.docreg.snippet

import scala.xml._
import vvv.docreg.util._

class Information {
  def name(xhtml: NodeSeq): NodeSeq = Text(ProjectProps.get("project.name") openOr "Project")
  def version(xhtml: NodeSeq): NodeSeq = Text(ProjectProps.get("project.version") openOr "v?")
}
