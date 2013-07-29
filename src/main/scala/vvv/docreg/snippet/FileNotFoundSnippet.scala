/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import net.liftweb.common.Loggable
import net.liftweb.util.PassThru
import net.liftweb.http.RequestVar
import net.liftweb.util.Helpers._
import net.liftweb.http.S
import vvv.docreg.model.Document
import scala.xml.NodeSeq

class FileNotFoundSnippet extends Loggable {

  val refOption = S.param("ref") getOrElse "???"
  
  def render: NodeSeq => NodeSeq = {
    refOption match {
      case Document.DocumentRevision(doc, rev) => {
        val latest = Some(doc.latest.version).filter(_ != rev.version)
        val previous = Some(rev.version - 1).filter(_ > 0)
        ".x-doc" #> <strong>{rev.filename}</strong> &
        ".x-latest-download [href]" #> latest.map(doc.downloadHref(_)).getOrElse("#") &
        ".x-latest-download [class+]" #> (if (latest.isDefined) None else Some("disabled")) &
        ".x-prev-download [href]" #> previous.map(doc.downloadHref(_)).getOrElse("#") &
        ".x-prev-download [class+]" #> (if (previous.isDefined) None else Some("disabled"))
      }
      case _ => {
        ".x-doc" #> <span>???</span> &
        ".x-latest-download [class+]" #> "disabled" &
        ".x-prev-download [class+]" #> "disabled"
      }
    }
  }

}