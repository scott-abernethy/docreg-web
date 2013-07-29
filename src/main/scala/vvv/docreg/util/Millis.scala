/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

object Millis
{
  def now() = new Millis().mark()
  def zero() = new Millis().zero()
}

class Millis
{
  var time: Long = _

  def elapsed_?(period: Long): Boolean =
  {
    System.currentTimeMillis() > (time + period)
  }

  def zero(): Millis =
  {
    time = 0
    this
  }

  def mark(): Millis =
  {
    time = System.currentTimeMillis()
    this
  }
}