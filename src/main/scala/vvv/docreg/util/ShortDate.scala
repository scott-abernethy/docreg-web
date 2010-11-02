package vvv.docreg.util

import java.text._
import java.util.Date
import java.util.TimeZone

object ShortDate {
  val dayF = new SimpleDateFormat("dd-MMM-yyyy")
  dayF.setTimeZone(TimeZone.getDefault)
  val timeF = new SimpleDateFormat("h:mm a")
  timeF.setTimeZone(TimeZone.getDefault)
  val dateF = new SimpleDateFormat("d MMM '('E')'")
  dateF.setTimeZone(TimeZone.getDefault)
}

class ShortDate(private val date: Date) {
  def now = new Date
  override def toString = {
    if (ShortDate.dayF.format(now) == ShortDate.dayF.format(date)) {
      "Today " + ShortDate.timeF.format(date)
    } else {
      ShortDate.dateF.format(date) 
    }
  }
}
