/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.util

/**
 * Created by IntelliJ IDEA.
 * User: sabernethy
 * Date: 23/08/11
 * Time: 4:23 PM
 * To change this template use File | Settings | File Templates.
 */

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