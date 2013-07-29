/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

import java.util.regex._
import util.matching.Regex

object StringUtil {
  val DomainUsername: Regex = """([a-zA-Z0-9.-]+)[\\/]([a-zA-Z0-9._%+-]+)""".r
  val ValidEmail: Regex = """([a-zA-Z0-9._%+-]+)@([a-zA-Z0-9.-]+\.[a-zA-Z]{2,4})""".r
  val FileName: Regex = """.*\.(.+)""".r
  val FileNameAndExt: Regex = """(.*)(\.[^.]+)""".r
  val ValidDocumentFileName: Regex = """^([0-9]+-[0-9]+-)(.+)$""".r

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

  def truncateFilename(filename: String, maxSize: Int, maxSizeBase: Int): String = {
    filename match {
      case ValidDocumentFileName(prefix, FileNameAndExt(name, ext)) => {
        prefix + name.take(scala.math.min(maxSize - (prefix.size + ext.size), maxSizeBase)) + ext
      }
      case ValidDocumentFileName(prefix, text) => {
        prefix + text.take(scala.math.min(maxSize - prefix.size, maxSizeBase))
      }
      case FileNameAndExt(name, ext) => {
        name.take(scala.math.min(maxSize - ext.size, maxSizeBase)) + ext
      }
      case text => {
        text.take(scala.math.min(maxSize, maxSizeBase))
      }
    }
  }

  def pluralise(count: Long, description: String): String = pluralise(count, description, "s")

  def pluralise(count: Long, description: String, pluraliser: String): String =
  {
    if (count == 1) count + " " + description else count + " " + description + pluraliser
  }
}
