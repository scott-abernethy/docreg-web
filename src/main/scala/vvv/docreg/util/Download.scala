package vvv.docreg.util

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._

import vvv.docreg.model.Document
import vvv.docreg.backend.Backend

object Download {
  def download(key: String): Box[LiftResponse] = {
    println(key + " download")
    Document.forKey(key) map (d => RedirectResponse("http://"+Backend.server+"/docreg/release/" + d.latest.filename))
  }
  def download(key: String, version: String): Box[LiftResponse] = {
    println(key + "-" + version + " download")
    Document.forKey(key) match {
      case Full(d) => d.revision(version.toLong) map (r => RedirectResponse("http://"+Backend.server+"/docreg/release/" + r.filename))
      case _ => Empty
    }
  }
}
