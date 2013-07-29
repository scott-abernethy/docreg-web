/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.agent

import vvv.docreg.util.Config

object AgentVendor {
  lazy val server: String = Config.is.get[String]("agent.server") getOrElse "10.16.9.179" // shelob.gnet.global.vpn
  lazy val home: String =  Config.is.get[String]("agent.home") getOrElse "/srv/docreg-fs"
  lazy val secure: Boolean = Config.is.get[Boolean]("agent.secure") getOrElse false
}