package vvv.docreg.util

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._

import rest.RestHelper
import vvv.docreg.backend.Backend
import org.apache.http.HttpEntity
import net.liftweb.util.ControlHelpers._
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet
import vvv.docreg.agent.AgentVendor
import vvv.docreg.model._
import java.io.{FileInputStream, File}

object DownloadService extends RestHelper with Loggable {

  serve {
    case "doc" :: "download" :: "editing" :: key :: version :: Nil Get req => {
      User.loggedInUser.is match {
        case Full(user) => {
          fileResponse(key, d => Full(d.latest), (d,r) => d.editingFileName(user.shortUsername()))
        }
        case _ => Full(RedirectResponse("/user/signin"))
      }
    }
    case "doc" :: "download" :: key :: version :: Nil Get req => {
      User.loggedInUser.is match {
        case Full(user) => {
          fileResponse(key, d => Revision.forDocument(d, version.toLong), (d,r) => r.filename)
        }
        case _ => Full(RedirectResponse("/user/signin"))
      }
    }
    case "doc" :: "log" :: key :: Nil Get req => {
      logResponse(key)
    }
  }

  def fileResponse(key: String, revisionFunc: (Document) => Box[Revision], fileNameFunc: (Document, Revision) => String): Box[LiftResponse] = {
    for {
      document <- Document.forKey(key)
      revision <- revisionFunc(document)
      file <- releaseFile(document, revision)
      stream <- tryo(new FileInputStream(file))
      toFilename = fileNameFunc(document, revision)
    }
    yield {
      log("downloaded " + key + " as " + toFilename)
      StreamingResponse(stream, () => stream.close(), file.length(), List("Content-Disposition" -> ("attachment; filename=\"" + toFilename + "\"")), Nil, 200)
    }
  }

  def logResponse(key: String): Box[LiftResponse] = {
    for {
      document <- Document.forKey(key)
      file <- logFile(document)
      stream <- tryo(new FileInputStream(file))
    }
    yield {
      log("downloaded log file for " + key)
      StreamingResponse(stream, () => stream.close(), file.length(), List("Content-Disposition" -> ("attachment; filename=\"" + key + ".log\"")), Nil, 200)
    }
  }

  def releaseFile(document: Document, revision: Revision): Box[java.io.File] = {
    val folder = if (document.secure_?()) "/secure/release" else "/docreg/release"
    val path = AgentVendor.home + folder + "/" + revision.filename
    Box !! new File(path)
  }

  def logFile(document: Document): Box[java.io.File] = {
    val folder = if (document.secure_?()) "/secure/log" else "/docreg/log"
    val path = AgentVendor.home + folder + "/" + document.key() + ".log"
    Box !! new File(path)
  }

  private def log(text: String)
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
}

//object Download extends Loggable
//{
//  def logFile(key: String): Box[LiftResponse] =
//  {
//    Full(RedirectResponse("http://" + userServer + "/docreg/log/" + key + ".log"))
//  }
//
//  def tryRequest(url: String): Option[HttpEntity] = {
//    tryo {
//      val client = new DefaultHttpClient()
//      val get = new HttpGet(url)
//      val response = client.execute(get)
//      val entity = response.getEntity()
//      if (entity == null) {
//        throw new Exception()
//      }
//      entity
//    }
//  }
//
//  def log(text: String)
//  {
//    // todo log to backend
//    User.loggedInUser.is match {
//      case Full(user) => {
//        logger.info("User '" + user.displayName + "' " + text)
//      }
//      case _ => {
//        logger.info("User ??? " + text)
//      }
//    }
//  }
//
//  def userServer: String =
//  {
//    val userLocalServer: Box[String] = for {
//      user <- User.loggedInUser.is
//    }
//    yield Server.address(user.localServer)
//
//    userLocalServer match {
//      case Full(s) if (s != null && s.length() > 0) => s
//      case _ => AgentVendor.server
//    }
//  }
//
//  def fileUrl(fileName: String): String =
//  {
//    ("http://" + userServer + "/docreg/release/" + fileName).replaceAll(" ", "%20")
//  }
//}
