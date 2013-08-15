/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.agent

import akka.actor.ActorRef

package object faux {
  case class AddDocument(info: DocumentInfo, username: String, notifyTo: ActorRef)
  case class AddDocumentChange(info: DocumentInfo)
}
