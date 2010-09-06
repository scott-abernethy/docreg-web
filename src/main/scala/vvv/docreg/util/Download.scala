package vvv.docreg.util

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._

import vvv.docreg.model.Document

object Download {
  def download(key: String): Box[LiftResponse] = {
    println(key + " download")
    val d = Document.forKey(key)
    if (d == null) Empty else Full(RedirectResponse("http://shelob.GNET.global.vpn/docreg/release/" + d.latest.filename))
  }
  def download(key: String, version: String): Box[LiftResponse] = {
    println(key + "-" + version + " download")
    val d = Document.forKey(key)
    if (d == null) Empty
    else {
    val r = d.revision(version.toLong)
    if (r == null) Empty else Full(RedirectResponse("http://shelob.GNET.global.vpn/docreg/release/" + r.filename))
    }
  }
}