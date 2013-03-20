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

package vvv.docreg.backend

import net.liftweb.common.{Full, Empty, Failure, Box}
import vvv.docreg.util.StringUtil.ValidEmail

object UserMigration {

  def migrateEmail(input: String): Box[String] = {
    input match {
      case ValidEmail(user, "aviatnet.com") =>
        migrateUser(user) match {
          case Full(changed) => Full(changed + "@aviatnet.com")
          case _ => Empty
        }

      case ValidEmail(user, "hstx.com") =>
        Full(migrateUser(user).openOr(user) + "@aviatnet.com")

      case ValidEmail(user, "stratexnet.com") =>
        Full(migrateUser(user).openOr(user) + "@aviatnet.com")
      
      case ValidEmail(_, _) =>
        Failure("Migration for '" + input + "' is not supported")
      case _ =>
        Failure("Email '" + input + "' is not valid")
    }
  }

  def migrateUser(input: String): Box[String] = {
    var x = input.replaceAll("_", ".")
    x match {
      case "prashanth.shitikond" => Full("prashanth.sitikond")
      case "martim.asprey" => Full("martin.asprey")
      case "dantliff" => Full("david.antliff")
      case "swwang" => Full("shiwen.wang")
      case "sreddy" => Full("srinivasa.reddy")
      case "bnishida" => Full("brad.nishida")
      case "gmcilroy" => Full("guy.mcilroy")
      case "rmatian" => Full("roland.matian")
      case "dlangdale-hunt" => Full("dean.langdale-hunt")
      case "svarin" => Full("stephane.varin")
      case "aelola" => Full("arthur.elola")
      case "nkrishnamurthy" => Full("narayana.krishnamurthy")
      case "dhunt" => Full("darran.hunt")
      case "rwidjajakusuma" => Full("ricardo.widjajakusuma")
      case "hbui" => Full("hoang.bui")
      case unchanged if unchanged == input => Empty
      case changed => Full(changed)
    }
  }
}