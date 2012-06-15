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
