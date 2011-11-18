package vvv.docreg.snippet

import scala.xml._
import vvv.docreg.util._
import _root_.net.liftweb._
import http._
import S._
import SHtml._
import common._
import util._
import Helpers._
import js._
import JsCmds._

class Information {
  def print = {
    ".info-name *" #> Text(ProjectProps.get("project.name") openOr "Project") &
    ".info-version" #> Text(ProjectProps.get("project.version") openOr "?")
  }
}
