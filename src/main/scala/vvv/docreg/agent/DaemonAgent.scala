/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.agent

import org.jboss.netty.buffer.ChannelBuffer
import net.liftweb.common.Loggable
import akka.actor.{PoisonPill, ActorRef, Actor}

trait DaemonAgent extends Actor

case class RequestPackage(replyTo: ActorRef, target: String, request: Request)
case class ReplyPackage(header: Header, reply: Reply)

class DaemonAgentImpl extends DaemonAgent with DaemonProtocol with Loggable
{
  var previousTransaction: Int = 0
  val consumers = List(self)
  var outstandingTransactions: Map[Int, ActorRef] = Map.empty

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


  def receive = {
        case RequestPackage(replyTo, target, request) =>
        {
          val encoding = request match {
            // todo yuck, these should be encode stacks in the protocol.
            case x: NextChangeRequest =>
            {
              val e = new NextChangeRequestEncoder{}
              Some((e.messageType, (buffer: ChannelBuffer) => e.encode(x, buffer)))
            }
            case x: RegisterRequest =>
            {
              val e = new RegisterRequestEncoder{}
              Some((e.messageType, (buffer: ChannelBuffer) => e.encode(x, buffer)))
            }
            case x: SubmitRequest =>
            {
              val e = new SubmitRequestEncoder{}
              Some((e.messageType, (buffer: ChannelBuffer) => e.encode(x, buffer)))
            }
            case x: EditRequest =>
            {
              val e = new EditRequestEncoder{}
              Some((e.messageType, (buffer: ChannelBuffer) => e.encode(x, buffer)))
            }
            case x: UneditRequest =>
            {
              val e = new UneditRequestEncoder{}
              Some((e.messageType, (buffer: ChannelBuffer) => e.encode(x, buffer)))
            }
            case x: SubscribeRequest =>
            {
              val e = new SubscribeRequestEncoder{}
              Some((e.messageType, (buffer: ChannelBuffer) => e.encode(x, buffer)))
            }
            case x: UnsubscribeRequest =>
            {
              val e = new UnsubscribeRequestEncoder{}
              Some((e.messageType, (buffer: ChannelBuffer) => e.encode(x, buffer)))
            }
            case x: ApprovalRequest =>
            {
              val e = new ApprovalRequestEncoder{}
              Some((e.messageType, (buffer: ChannelBuffer) => e.encode(x, buffer)))
            }
            case _ =>
            {
              None
            }
          }
          // Todo, unedit doesn't have a reply, so no outstanding transaction!!!!!!!!
          encoding.foreach{ x =>
            val id = nextTransaction()
            outstandingTransactions += (id -> replyTo)
            transmit(target, DownstreamMessage(
              Header(DaemonProtocol.protocolVersion, x._1, id, 1),
              x._2
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
        case 'Die =>
        {
          logger.info("DaemonAgent killed")
          close()
          self ! PoisonPill
        }
        case _ =>
        {
          // Ignored message.
        }
  }


}

trait DaemonAgentComponent
{
  val daemonAgent: ActorRef
}

//trait DaemonAgentComponentImpl extends DaemonAgentComponent
//{
//  val daemonAgent = new DaemonAgentImpl()
//}