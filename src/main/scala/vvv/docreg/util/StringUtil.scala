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

import java.util.regex._
import util.matching.Regex

object StringUtil {
  val DomainUsername: Regex = """([a-zA-Z0-9.-]+)[\\/]([a-zA-Z0-9._%+-]+)""".r
  val ValidEmail: Regex = """([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+\.[a-zA-Z]{2,4})""".r
  val FileName: Regex = """.*\.(.+)""".r

  def nameFromEmail(email: String): String = {
    email indexOf '@' match {
      case -1 => ""
      case i =>
        val namePart = email substring (0, i) replaceAll ("[._%\\-+]"," ") replaceAll ("[0-9]","") replaceAll ("  "," ")
        titleCase(namePart)
    }
  }

  val titleCasePattern = Pattern.compile("(^|\\W)([a-z])")
  def titleCase(s: String): String = {
    val m = titleCasePattern.matcher(s.toLowerCase())
    val sb = new StringBuffer(s.length())
    while (m.find()) {
      m.appendReplacement(sb, m.group(1) + m.group(2).toUpperCase() )
    }
    m.appendTail(sb)
    return sb.toString()
  }

  def prePadTo(input: String, len: Int, pad: Char): String = input.reverse.padTo(len, pad).reverse
    /* var x = key
    List.range(x.size, 4).foreach((i) => x = "0" + x)
    x */

  def fileExtension(fileName: String): Option[String] = fileName match {
    case FileName(extension) => Some(extension)
    case _ => None
  }

  def retitleFile(newTitle: String, fileName: String): Option[String] =
  {
    newTitle match {
      case s: String if (s.length() > 0) =>
      {
        Some(newTitle + fileExtension(fileName).map("." + _).getOrElse(""))
      }
      case _ =>
      {
        None
      }
    }
  }

  def pluralise(count: Long, description: String): String = pluralise(count, description, "s")

  def pluralise(count: Long, description: String, pluraliser: String): String =
  {
    if (count == 1) count + " " + description else count + " " + description + pluraliser
  }
}
