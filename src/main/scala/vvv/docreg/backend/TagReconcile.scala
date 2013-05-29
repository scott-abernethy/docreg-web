/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import net.liftweb.common.Loggable
import vvv.docreg.model._
import org.squeryl.PrimitiveTypeMode._
import scala.util.matching.Regex

trait TagReconcile extends Loggable {

  val TagExtract: Regex = """(#[a-zA-Z0-9\-]+)""".r
  val Chary: Regex = """([a-zA-Z]+)""".r

  def reconcileTags(document: Document, texts: List[String]) {
    inTransaction {
      val existing = Tag.namesForDocument(document.id).toSet

      val found: Set[String] = (
        for {
          text <- texts
          word <- text.split(" ")
          clean <- cleanTag(word)
        } yield clean
      ).toSet

      for (add <- found -- existing) Tag.ensure(add, document.id)
      for (remove <- existing -- found) Tag.remove(remove, document.id)
    }
  }

  def cleanTag(input: String): Option[String] = {
    for {
      TagExtract(name) <- TagExtract findFirstIn input
      if (Chary findFirstIn name).isDefined
    } yield name
  }

}
