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
  def download(key: String, version: Option[String]): Box[LiftResponse] =
  {
    log(key)
    (Document.forKey(key), version) match {
      case (Full(d), None) => Document.forKey(key) map (d => RedirectResponse(fileUrl(d.latest.filename)))
      case (Full(d), Some(v)) => d.revision(v.toLong) map (r => RedirectResponse(fileUrl(r.filename)))
      case _ => Empty
    }
  }

  def downloadForEditing(key: String, user: String): Box[LiftResponse] = {
    // TODO could just grab user here, as we get it for other reasons.
    for {
      document <- Document.forKey(key)
      url = fileUrl(document.latest.filename)
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

  def fileUrl(fileName: String): String = {
    val userLocalServer: Box[String] = User.loggedInUser.is.map(_.localServer.is)
    val server = userLocalServer match {
      case Full(s) if (s != null && s.length() > 0) => s
      case _ => Backend.server
    }
    ("http://" + server + "/docreg/release/" + fileName).replaceAll(" ", "%20")
  }
}
