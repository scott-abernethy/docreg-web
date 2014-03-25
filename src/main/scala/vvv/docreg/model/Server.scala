/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.model

class Server(val description: String, val name: String, val address: String) {
}

object Server {
  lazy val select: List[(String, String)] = {
    for (server <- all)
    yield (server.name, server.description + " (" + server.name + " = " + server.address + ")")
  }

  lazy val toAddress: List[(String, String)] = {
    for (server <- all)
    yield (server.name, server.address)
  }

  def description(name: String): String = {
    select.toMap.get(name).getOrElse(name)
  }

  def address(name: String): String = {
    toAddress.toMap.get(name).getOrElse(name)
  }

  lazy val all: List[Server] = {
    new Server("Santa Clara", "boromir", "10.15.32.243") ::
    new Server("NZ", "shelob", "10.16.9.179") ::
    new Server("RTP", "ugluk", "10.15.32.210") ::
    new Server("UK", "balrog", "10.11.65.21") ::
    new Server("Singapore", "treebeard", "10.11.48.28") ::
    new Server("Slovenia", "beregond", "10.15.201.30" /* was "192.168.180.30" */) ::
    new Server("India", "eomer", "192.168.250.51") :: Nil
  }
}