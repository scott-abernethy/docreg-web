package vvv.docreg.util

import java.sql.Timestamp
import java.util.Date

object T {
  def at(date: Date) = {
    new Timestamp(date.getTime)
  }

  def ago(duration: Long) = {
    new Timestamp(System.currentTimeMillis() - duration)
  }

  def now() = {
    new Timestamp(System.currentTimeMillis())
  }
}
