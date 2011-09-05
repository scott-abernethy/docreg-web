package vvv.docreg.util

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._

import vvv.docreg.backend.Backend
import vvv.docreg.model.{User, Document}

object Download extends Loggable
{
  def download(key: String): Box[LiftResponse] =
  {
    log(key)
    Document.forKey(key) map (d => RedirectResponse("http://"+Backend.server+"/docreg/release/" + d.latest.filename))
  }

  def download(key: String, version: String): Box[LiftResponse] =
  {
    log(key)
    Document.forKey(key) match {
      case Full(d) => d.revision(version.toLong) map (r => RedirectResponse("http://"+Backend.server+"/docreg/release/" + r.filename))
      case _ => Empty
    }
  }

  def log(key: String)
  {
    // todo log to backend
    User.loggedInUser.is match
    {
      case Full(user) =>
      {
        logger.info("User '" + user.displayName + "' downloaded " + key)
      }
      case _ =>
      {
        logger.info("User downloaded " + key)
      }
    }
  }
}
