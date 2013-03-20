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
