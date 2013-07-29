/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import net.liftweb.common.Loggable
import xml.NodeSeq
import net.liftweb.util.Helpers._
import vvv.docreg.model._
import net.liftweb.http._
import net.liftweb.util.{ClearNodes, PassThru, CssSel}
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.util.Bits

class TagSnippet(tag: String) extends Loggable {
  val documents: List[Document] = Tag.documentsWithTag(tag)

  def info() = {
    val user = User.loggedInUser.is.toOption
    val uid = user.map(_.id).getOrElse(-1L)

    val (open,restricted) = UserSession.partitionAuthorized(documents, (x: Document) => x)
    ".t-name" #> tag &
    ".d-count *" #> (open.size) &
    listOrNone[Document](".d-items", open, d => <span>{ d.accessIcon() } { d.info() }</span>) &
    ".d-restricted" #> restricted.headOption.map{ x =>
      Bits.restrictedNotice(restricted.size)
    }
  }

  def listOrNone[T](root: String, list: List[T], listTransform: T => NodeSeq): CssSel = {
    if (list.size > 0) {
      root #> ("li *" #> list.map(listTransform(_)))
    }
    else {
      root #> <p>(None)</p>
    }
  }
}

