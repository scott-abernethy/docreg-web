/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.snippet

import net.liftweb.common.{Empty, Full, Box, Loggable}
import xml.NodeSeq
import net.liftweb.util.Helpers._
import vvv.docreg.model._
import net.liftweb.http._
import net.liftweb.util.{ClearNodes, PassThru, CssSel}
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.util.Bits

class ProjectSnippet extends Loggable {
  val key = S.param("key") openOr ""
  val project: Option[Project] = {
    val nameUn = key.replaceAll("[+ ]", "_")
    from(Project.dbTable)(p => where(p.name like nameUn) select(p)).headOption
  }

  def info(in: NodeSeq): NodeSeq = {
    val user = User.loggedInUser.is.toOption
    val uid = user.map(_.id).getOrElse(-1L)
    project match {
      case Some(p) => {
        val (open,restricted) = UserSession.partitionAuthorized(p.documents(), (x: Document) => x)
        val authorized: List[User] = p.authorized()
        val contributors: List[User] = p.contributors().filter(_.knownOption.isDefined)
        val t = ".p-name" #> p.name &
          ".p-feed [href]" #> (p.url + "/feed") &
          ".d-count *" #> (open.size) &
          listOrNone[Document](".d-items", open, d => <span>{ d.accessIcon() } { d.info() }</span>) &
          ".d-restricted" #> restricted.headOption.map{ x =>
            Bits.restrictedNotice(restricted.size)
          } &
          ".a-block" #> (if (authorized.size > 0) PassThru else ClearNodes) andThen
          ".a-count *" #> authorized.size &
          listOrNone[User](".a-items", authorized, u => u.profileLabel(uid)) &
          ".c-count *" #> contributors.size &
          listOrNone[User](".c-items", contributors, u => u.profileLabel(uid))
        t(in)
      }
      case _ => {
        <div class="alert-message error"><p><strong>Invalid</strong>{" project '" + key + "'"}</p></div>
      }
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

