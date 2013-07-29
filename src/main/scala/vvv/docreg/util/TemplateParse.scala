/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import scala.xml._
import net.liftweb.common._

object TemplateParse {
  def parseDiv(template: Box[NodeSeq], id: String): NodeSeq = parseFor(template, "div", id)
  def parseFor(template: Box[NodeSeq], element: String, id: String): NodeSeq = template match {
    case Full(in) => in \\ element filter (x => (x \ "@id").text == id)
    case _ => NodeSeq.Empty
  }
}
