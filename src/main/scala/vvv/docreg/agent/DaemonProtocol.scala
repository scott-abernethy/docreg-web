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

package vvv.docreg.agent

import org.jboss.netty.channel.socket.nio.NioDatagramChannelFactory
import java.util.concurrent.Executors
import org.jboss.netty.bootstrap.ConnectionlessBootstrap
import org.jboss.netty.channel.socket.DatagramChannel
import java.net.InetSocketAddress
import org.jboss.netty.channel._
import org.jboss.netty.buffer.{ChannelBuffers, ChannelBuffer}
import org.jboss.netty.handler.codec.oneone.{OneToOneDecoder, OneToOneEncoder}
import net.liftweb.common.Loggable
import java.text.DecimalFormat
import actors.{TIMEOUT, Actor}
import akka.actor.ActorRef

trait DaemonProtocol extends Loggable
{
  val factory = new NioDatagramChannelFactory(Executors.newCachedThreadPool)

  val consumers: List[ActorRef]

  val bootstrap = new ConnectionlessBootstrap(factory)
  bootstrap.setPipelineFactory(new ChannelPipelineFactory {
    def getPipeline = Channels.pipeline(
      new DaemonProtocolEncoder,
      new DaemonProtocolDecoder(
        Map(
          MessageType.nextChangeReply -> NextChangeReplyDecoder,
          MessageType.registerReply -> RegisterReplyDecoder,
          MessageType.submitReply -> SubmitReplyDecoder,
          MessageType.editReply -> EditReplyDecoder,
          MessageType.subscribeReply -> SubscribeReplyDecoder,
          MessageType.unsubscribeReply -> UnsubscribeReplyDecoder,
          MessageType.approvalReply -> ApprovalReplyDecoder
        )
      ),
      new DaemonProtocolHandler(m => consumers.foreach(_ ! m))
    )
  })
  bootstrap.setOption("receiveBufferSizePredictorFactory", new FixedReceiveBufferSizePredictorFactory(1522))

  val channel: DatagramChannel = bootstrap.bind(new InetSocketAddress(0)).asInstanceOf[DatagramChannel]
  
  def transmit(hostname: String, message: DownstreamMessage)
  {
    channel.write(message, new InetSocketAddress(hostname, 5436))
  }

  def close()
  {
    channel.close.awaitUninterruptibly(5000)
    factory.releaseExternalResources()
  }
}

class DaemonProtocolEncoder extends OneToOneEncoder with Loggable
{
  def encode(ctx: ChannelHandlerContext, channel: Channel, msg: AnyRef) =
  {
    msg match {
      case DownstreamMessage(header, body) =>
      {
        //println("Protocol downstream " + header)

        val buffer = ChannelBuffers.dynamicBuffer();

        // header
        buffer.writeInt(header.version)
        buffer.writeInt(header.message.id)
        buffer.writeInt(header.transactionId)
        buffer.writeInt(header.sequence)

        // body
        body(buffer)

        buffer
      }
      case _ =>
      {
        // Can't encode this, ignore
        msg
      }
    }
  }
}

class DaemonProtocolDecoder(decoders: Map[MessageType.Type, ReplyDecoder]) extends OneToOneDecoder with Loggable
{
  def decode(ctx: ChannelHandlerContext, channel: Channel, msg: AnyRef): AnyRef =
  {
    msg match
    {
      case buffer: ChannelBuffer =>
      {
        // header
        val version = buffer.readInt()
        val messageId = buffer.readInt()
        val transactionId = buffer.readInt()
        val sequenceId = buffer.readInt()
        val header: Header = Header(version, MessageType(messageId), transactionId, sequenceId)

        val message = decodeMessage(header, buffer)
        //println("Upstream message received " + header + " and decoded as " + message)
        message
      }
      case _ =>
      {
        // Can't decode this, ignore
        msg
      }
    }
  }

  def decodeMessage(header: Header, buffer: ChannelBuffer): AnyRef =
  {
    header match
    {
      case Header(v, messageType, transaction, sequence) if (DaemonProtocol.protocolVersion == v && decoders.contains(messageType)) =>
      {
        ReplyPackage(header, decoders(messageType).decode(header, buffer))
      }
      case _ =>
      {
        null
      }
    }
  }
}

class DaemonProtocolHandler(consume: (Any) => Unit) extends SimpleChannelUpstreamHandler with Loggable
{
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent)
  {
    consume(e.getMessage)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent)
  {
    logger.warn("Protocol exception " + e.getCause, e.getCause)
  }
}

case class DownstreamMessage(header: Header, body: (ChannelBuffer) => Unit)

object DaemonProtocol
{
  val protocolVersion: Int = 4

  def main(args: Array[String])
  {
    println("disabled")
//    //getNextChange(Actor.self, "shelob", -1)
//
////    val request = RegisterRequest(
////      "New Document Test 3",
////      "DocReg",
////      "Testing document addition with docregbeta",
////      "Everyone",
////      "sabernethy",
////      "sabernethy",
////      "docregweb",
////      "0.7.0"
////    )
////    val encoder = new RegisterRequestEncoder{}
//
//    val request = NextChangeRequest(-1)
//    val encoder = new NextChangeRequestEncoder{}
//
//    val msg = new DownstreamMessage(
//      Header(3, encoder.messageType, 1, 1),
//      buffer => encoder.encode(request, buffer)
//    )
//
//    val x = new DaemonProtocol{
//      val consumers = List(self)
//    }
//    x.transmit("shelob", msg)
//    Actor.receiveWithin(5000) {
//      case in => println("XX " + in)
//    }
  }
}