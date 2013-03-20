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

import net.liftweb._
import http._
import actor._
import common._
import mapper._
import util.Helpers._
import java.util.Properties
import scala.collection.JavaConversions._

object ProjectProps {
  def get(key: String): Box[String] = Box.legacyNullTest(props.getProperty(key))
  lazy val props: Properties = {
    val p = new Properties
    p.setProperty("project.name", "DocReg+Web")
    p.setProperty("project.version", "0.8.3")
    p
  }
}
