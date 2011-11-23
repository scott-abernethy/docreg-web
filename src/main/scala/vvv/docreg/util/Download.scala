package vvv.docreg.util

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._

import vvv.docreg.backend.Backend
import vvv.docreg.model.{User, Document}
import org.apache.http.HttpEntity
import net.liftweb.util.ControlHelpers._
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet

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

  def downloadForEditing(key: String, user: String): Box[LiftResponse] = {
    for {
      document <- Document.forKey(key)
      url = ("http://" + Backend.server + "/docreg/release/" + document.latest.filename).replaceAll(" ", "%20")
      entity <- tryRequest(url)
      stream = entity.getContent()
    } yield StreamingResponse(stream, () => stream.close(), entity.getContentLength(), List("Content-Disposition" -> ("attachment; filename=\"" + document.editingFileName(user) + "\"")), Nil, 200)
  }

  def tryRequest(url: String): Option[HttpEntity] = {
    tryo {
      val client = new DefaultHttpClient()
      val get = new HttpGet(url)
      val response = client.execute(get)
      val entity = response.getEntity()
      if (entity == null) {
        throw new Exception()
      }
      entity
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
