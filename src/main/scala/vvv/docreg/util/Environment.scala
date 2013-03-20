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

package vvv.docreg.util

import vvv.docreg.backend._
import vvv.docreg.agent._
import akka.actor.{PoisonPill, Props, ActorSystem}
import com.typesafe.config.{ConfigFactory}

trait Environment extends BackendComponent with DocumentStreamComponent with DirectoryComponent with DaemonAgentComponent
{
  def start()

  def exit()
}

trait EnvironmentImpl extends Environment with BackendComponent with DirectoryComponentImpl with DaemonAgentComponent
{
  val akkaConfig = ConfigFactory.load("akka.conf")
  val system = ActorSystem("DocRegWebSystem", akkaConfig.getConfig("docreg-web").withFallback(akkaConfig))
  import system._
  val daemonAgent = actorOf(Props[DaemonAgentImpl], "DaemonAgent")
  val documentStream = actorOf(Props[DocumentStream], "DocumentStream")
  val backend = actorOf(Props(new Backend(directory, daemonAgent, documentStream)), "Backend")
  val poller = actorOf(Props(new ChangePoller(AgentVendor.server, backend, daemonAgent)))
  val userStorage = actorOf(Props[UserStorage], "UserStorage")

  def start()
  {
    poller ! 'Reset
  }

  def exit()
  {
    backend ! 'Die
    poller ! 'Die
    daemonAgent ! 'Die
    system.shutdown()
  }
}

object Environment
{
  var env: Environment = _
}
