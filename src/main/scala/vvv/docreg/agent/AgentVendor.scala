package vvv.docreg.agent

import net.liftweb.util.Props

object AgentVendor {
  lazy val server: String = Props.get("agent.server") openOr "10.16.9.179" // shelob.gnet.global.vpn
  lazy val home: String =  Props.get("agent.home") openOr "/srv/docreg-fs"
  lazy val secure: Boolean = Props.get("agent.secure").map(_ equalsIgnoreCase "true") openOr false
}