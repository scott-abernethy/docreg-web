/*
 * Copyright (c) 2013 Scott Abernethy.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    new Server("Santa Clara", "boromir", "10.15.168.22") ::
    new Server("NZ", "shelob", "10.16.9.179") ::
    new Server("RTP", "ugluk", "10.15.32.210") ::
    new Server("UK", "balrog", "10.11.65.21") ::
    new Server("Singapore", "treebeard", "10.11.48.28") ::
    new Server("Slovenia", "beregond", "10.15.201.30" /* was "192.168.180.30" */) ::
    new Server("India", "eomer", "192.168.250.51") :: Nil
  }
}