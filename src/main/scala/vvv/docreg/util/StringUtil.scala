package vvv.docreg.util

import java.util.regex._

object StringUtil {
  def nameFromEmail(email: String): String = {
    val namePart = email substring (0, email indexOf '@') replaceAll ("[._%\\-+]"," ") replaceAll ("[0-9]","") replaceAll ("  "," ")
    titleCase(namePart)
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
}
