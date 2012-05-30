package vvv.docreg.util

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._

import vvv.docreg.backend.Backend
import org.apache.http.HttpEntity
import net.liftweb.util.ControlHelpers._
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet
import vvv.docreg.agent.AgentVendor
import vvv.docreg.model._

object Download extends Loggable
{
  def download(key: String, version: String): Box[LiftResponse] =
  {
    Document.forKey(key) match {
      case Full(d) => {
        val r = Revision.forDocument(d, version.toLong) getOrElse EmptyRevision
        val url: String = fileUrl(r.filename)
        log("downloaded " + key + " via " + url)
        Full(RedirectResponse(url))
      }
      case _ => {
        log("failed to download " + key)
        Empty
      }
    }
  }

  def downloadForEditing(key: String, user: String): Box[LiftResponse] = {
    // TODO could just grab user here, as we get it for other reasons.
    for {
      document <- Document.forKey(key)
      url = fileUrl(document.latest.filename)
      entity <- tryRequest(url)
      stream = entity.getContent()
    }
    yield {
      log("downloaded for editing " + key + " via " + url)
      StreamingResponse(stream, () => stream.close(), entity.getContentLength(), List("Content-Disposition" -> ("attachment; filename=\"" + document.editingFileName(user) + "\"")), Nil, 200)
    }
  }

  def logFile(key: String): Box[LiftResponse] =
  {
    Full(RedirectResponse("http://" + userServer + "/docreg/log/" + key + ".log"))
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

  def log(text: String)
  {
    // todo log to backend
    User.loggedInUser.is match {
      case Full(user) => {
        logger.info("User '" + user.displayName + "' " + text)
      }
      case _ => {
        logger.info("User ??? " + text)
      }
    }
  }

  def userServer: String =
  {
    val userLocalServer: Box[String] = for {
      user <- User.loggedInUser.is
    }
    yield Server.address(user.localServer)

    userLocalServer match {
      case Full(s) if (s != null && s.length() > 0) => s
      case _ => AgentVendor.server
    }
  }

  def fileUrl(fileName: String): String =
  {
    ("http://" + userServer + "/docreg/release/" + fileName).replaceAll(" ", "%20")
  }
}
