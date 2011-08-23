package vvv.docreg.agent

import org.specs.Specification

object DaemonProtocolTest extends Specification
{
  "DaemonProtocol" should
  {
    "have valid transaction ids" >>
    {
      DaemonProtocol.transactionId = 0
      DaemonProtocol.nextTransactionId() must be_==(1)

      DaemonProtocol.transactionId = 98765
      DaemonProtocol.nextTransactionId() must be_==(98766)
      DaemonProtocol.nextTransactionId() must be_==(98767)

      DaemonProtocol.transactionId = Int.MaxValue - 2
      DaemonProtocol.nextTransactionId() must be_==(Int.MaxValue - 1)
      DaemonProtocol.nextTransactionId() must be_==(0)
      DaemonProtocol.nextTransactionId() must be_==(1)
    }
  }
}