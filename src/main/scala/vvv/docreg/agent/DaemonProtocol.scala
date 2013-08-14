/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
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

trait DaemonProtocol extends Loggable {

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
  
  def transmit(hostname: String, message: DownstreamMessage) {
    channel.write(message, new InetSocketAddress(hostname, 5436))
  }

  def close() {
    channel.close.awaitUninterruptibly(5000)
    factory.releaseExternalResources()
  }
}

class DaemonProtocolEncoder extends OneToOneEncoder with Loggable {

  def encode(ctx: ChannelHandlerContext, channel: Channel, msg: AnyRef) = {
    msg match {
      case DownstreamMessage(header, body) => {
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
      case _ => {
        // Can't encode this, ignore
        msg
      }
    }
  }
}

class DaemonProtocolDecoder(decoders: Map[MessageType.Type, ReplyDecoder]) extends OneToOneDecoder with Loggable {

  def decode(ctx: ChannelHandlerContext, channel: Channel, msg: AnyRef): AnyRef = {
    msg match {
      case buffer: ChannelBuffer => {
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
      case _ => {
        // Can't decode this, ignore
        msg
      }
    }
  }

  def decodeMessage(header: Header, buffer: ChannelBuffer): AnyRef = {
    header match {
      case Header(v, messageType, transaction, sequence) if (DaemonProtocol.protocolVersion == v && decoders.contains(messageType)) => {
        ReplyPackage(header, decoders(messageType).decode(header, buffer))
      }
      case _ => {
        null
      }
    }
  }
}

class DaemonProtocolHandler(consume: (Any) => Unit) extends SimpleChannelUpstreamHandler with Loggable {

  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent) {
    consume(e.getMessage)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent) {
    logger.warn("Protocol exception " + e.getCause, e.getCause)
  }
}

case class DownstreamMessage(header: Header, body: (ChannelBuffer) => Unit)

object DaemonProtocol {

  val protocolVersion: Int = 4

}

object DaemonProtocolRig {
  
  def main(args: Array[String]) {
    val request = NextChangeRequest(-1)
    val encoder = new NextChangeRequestEncoder{}
    val version = 4

    val msg = new DownstreamMessage(
      Header(version, encoder.messageType, 1, 1),
      buffer => encoder.encode(request, buffer)
    )

    import akka.actor.ActorDSL._
    import akka.actor._
    implicit val system = ActorSystem("temp")
    val dummy = actor(new Act {
      become {
        case in => println("Received... " + in)
      }
    })

    val x = new DaemonProtocol{
      val consumers = List(dummy)
    }
    println("Transmitting...")
    x.transmit("shelob", msg)
  }
}
