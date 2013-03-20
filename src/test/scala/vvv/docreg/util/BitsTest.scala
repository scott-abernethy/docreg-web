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

import org.specs.Specification
import xml.{PrettyPrinter, NodeSeq}

object BitsTest extends Specification {
  "Bits" should {
    "restrictedNotice" >> {
      xmlCheck(Bits.restrictedNotice(0), <p><span class="badge badge-warning">0</span> Restricted documents not shown</p>)
      xmlCheck(Bits.restrictedNotice(1), <p><span class="badge badge-warning">1</span> Restricted document not shown</p>)
      xmlCheck(Bits.restrictedNotice(2), <p><span class="badge badge-warning">2</span> Restricted documents not shown</p>)
    }
  }

  def xmlCheck(expected: NodeSeq, detected: NodeSeq)
  {
    val printer: PrettyPrinter = new PrettyPrinter(80, 2)
    printer.formatNodes(expected) must be_==( printer.formatNodes(detected) )
  }

}
