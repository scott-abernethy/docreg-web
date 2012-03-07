package vvv.docreg.agent

import actors.Actor
import org.jboss.netty.buffer.ChannelBuffer

trait DaemonAgent extends Actor

// Encoder ... create DownstreamMessage to write to channel
//  hook up replyTo in DownstreamMessage?
//  who handles message and transaction id? low level.

// Decoder ... decode function matched to each Downstream message OR each message type. someone needs to detail who it goes back to.

/*
if we get a message where we can't find the requester, reject. thus we can assign a processing method to the requester, thus we can assign a processing method to the request / DownstreamMessage.
 */

case class RequestPackage(replyTo: Actor, target: String, request: Request)
case class ReplyPackage(header: Header, reply: Reply)

class DaemonAgentImpl extends DaemonAgent with DaemonProtocol
{
  var previousTransaction: Int = 0
  val consumers = List(this)
  var outstandingTransactions: Map[Int, Actor] = Map.empty

  // todo close protocol on exit

  def nextTransaction(): Int =
  {
    if (previousTransaction < (Int.MaxValue - 1))
    {
      previousTransaction = previousTransaction + 1
    }
    else
    {
      previousTransaction = 0
    }
    previousTransaction
  }

  def act()
  {
    loop
    {
      // todo change to react when DaemonProtocol doesn't block
      receive
      {
        case RequestPackage(replyTo, target, request) =>
        {
          val encoding = request match {
            // todo yuck, these should be encode stacks in the protocol.
            case x: NextChangeRequest =>
            {
              Some((buffer: ChannelBuffer) => new NextChangeRequestEncoder{}.encode(x, buffer))
            }
            case x: RegisterRequest =>
            {
              Some((buffer: ChannelBuffer) => new RegisterRequestEncoder{}.encode(x, buffer))
            }
            case x: SubmitRequest =>
            {
              Some((buffer: ChannelBuffer) => new SubmitRequestEncoder{}.encode(x, buffer))
            }
            case _ =>
            {
              None
            }
          }
          encoding.foreach{ e =>
            val id = nextTransaction()
            outstandingTransactions += (id -> replyTo)
            transmit(target, DownstreamMessage(
              Header(DaemonProtocol.protocolVersion, MessageType.nextChangeRequest, id, 1),
              e
            ))
          }
        }
        case ReplyPackage(Header(_, _, transaction, _), reply) =>
        {
          outstandingTransactions.get(transaction).foreach{ replyTo =>
            outstandingTransactions -= transaction
            replyTo ! reply
          }
        }
        case _ =>
        {
          // Ignored message.
        }
      }
    }
  }


}