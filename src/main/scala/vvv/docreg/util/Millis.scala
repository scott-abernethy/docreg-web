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