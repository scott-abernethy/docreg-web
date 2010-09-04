package vvv.docreg.util

import java.util.Date

class Ago(private val date: Date) {
  def now = new Date
  override def toString = {
    val mins = (now.getTime - date.getTime) / (1000 * 60)
    if (mins < 60) pluralise(mins, "min") else pluralise (mins / 60, "hour")
  }
  def pluralise(count: Long, description: String) =
    if (count == 1) count + " " + description else count + " " + description + "s"
}
