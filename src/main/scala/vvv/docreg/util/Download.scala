/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import rest.RestHelper
import vvv.docreg.backend.Backend
import net.liftweb.util.ControlHelpers._
import vvv.docreg.agent.AgentVendor
import vvv.docreg.model._
import java.io.{FileInputStream, File}
import java.io.FileNotFoundException

object DownloadService extends RestHelper with Loggable {

  serve {
    case "doc" :: "download" :: "editing" :: key :: version :: Nil Get req => {
      User.loggedInUser.is match {
        case Full(user) => {
          guardBadDownloads(() => RedirectResponse("/doc/file-not-found?ref=" + key + "-" + version))( 
              fileResponse(key, d => Full(d.latest), (d,r) => d.editingFileName(user.shortUsername())) )
        }
        case _ => Full(RedirectResponse("/user/signin"))
      }
    }
    case "doc" :: "download" :: key :: version :: Nil Get req => {
      User.loggedInUser.is match {
        case Full(user) => {
          guardBadDownloads(() => RedirectResponse("/doc/file-not-found?ref=" + key + "-" + version))( 
              fileResponse(key, d => Revision.forDocument(d, version.toLong), (d,r) => r.filename) )
        }
        case _ => Full(RedirectResponse("/user/signin"))
      }
    }
    case "doc" :: "log" :: key :: Nil Get req => {
      logResponse(key)
    }
  }
  
  def guardBadDownloads(alternate: () => LiftResponse)(toGuard: Box[LiftResponse]): Box[LiftResponse] = {
    toGuard match {
      case Failure(msg, Full(ex: FileNotFoundException), _) => {
        logger.error("%s tried to download but failed with %s".format(user(), msg))
        Full(alternate())
      } 
      case other => other
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
      logger.info("%s downloaded %s as %s".format(user(), key, toFilename))
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
      logger.info("%s downloaded log file for %s".format(user(), key))
      StreamingResponse(stream, () => stream.close(), file.length(), Nil, Nil, 200)
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

  private def user(): String = {
    User.loggedInUser.is match {
      case Full(user) => "User '" + user.displayName + "'"
      case _ => "User ???"
    }
  }
}