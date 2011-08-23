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
import java.nio.charset.Charset
import actors.Actor

object DaemonProtocol extends Loggable
{
  var transactionId: Int = 0

  def nextTransactionId(): Int =
  {
    transactionId = transactionId + 1
    transactionId
  }

  def main(args: Array[String])
  {
    getNextChange(Actor.self, "shelob", -1)
    Actor.receiveWithin(5000) {
      case msg => println("XX " + msg)
    }
  }

  def getNextChange(consumer: Actor, hostname: String, changeNumber: Int)
  {
    val factory = new NioDatagramChannelFactory(Executors.newCachedThreadPool)
    val bootstrap = new ConnectionlessBootstrap(factory)
    bootstrap.setPipelineFactory(new ChannelPipelineFactory {
      def getPipeline = Channels.pipeline(
        new DaemonProtocolEncoder,
        new DaemonProtocolDecoder,
        new DaemonProtocolHandler(consumer)
      )
    })
    bootstrap.setOption("receiveBufferSizePredictorFactory", new FixedReceiveBufferSizePredictorFactory(1522))

    val channel: DatagramChannel = bootstrap.bind(new InetSocketAddress(0)).asInstanceOf[DatagramChannel]

    channel.write(new DownstreamMessage(Messages.nextChange, buffer => buffer.writeInt(changeNumber)), new InetSocketAddress(hostname, 5436))

    if (!channel.getCloseFuture.awaitUninterruptibly(5000))
    {
      logger.error("Protocol reply not received in a timely fashion")
      channel.close.awaitUninterruptibly(5000)
    }

    factory.releaseExternalResources
  }
}

class DaemonProtocolEncoder extends OneToOneEncoder with Loggable
{
  def encode(ctx: ChannelHandlerContext, channel: Channel, msg: AnyRef) =
  {
    msg match {
      case DownstreamMessage(message, body) =>
        logger.debug("Protocol downstream " + message + " " + message.id)
        val transactionId: Int = DaemonProtocol.nextTransactionId
        val buffer = ChannelBuffers.dynamicBuffer();

        // header
        buffer.writeInt(3) // version
        buffer.writeInt(message.id) // message id
        buffer.writeInt(transactionId) // transaction id
        buffer.writeInt(transactionId) // sequence id

        // body
        body(buffer)

        buffer
      case _ =>
        // Can't encode this, ignore
        msg
    }
  }
}

class DaemonProtocolDecoder extends OneToOneDecoder
{
  def decode(ctx: ChannelHandlerContext, channel: Channel, msg: AnyRef): AnyRef =
  {
    msg match
    {
      case buffer: ChannelBuffer =>

        // header
        val version = buffer.readInt()
        val messageId = buffer.readInt()
        val transactionId = buffer.readInt()
        val sequenceId = buffer.readInt()
        val header: Header = Header(version, Messages(messageId), transactionId, sequenceId)

        decodeMessage(header, buffer)
      case _ =>
        // Can't decode this, ignore
        msg
    }
  }

  def decodeMessage(header: Header, buffer: ChannelBuffer): AnyRef =
  {
    header match
    {
      case Header(3, Messages.nextChangeResponse, t, s) =>
        decodeChangeReply(header, buffer)
      case _ =>
        null
    }
  }

  def decodeChangeReply(header: Header, buffer: ChannelBuffer): AnyRef =
  {
    val changeNumber = buffer.readInt()
    val key = buffer.readInt()
    val version = buffer.readInt()

    val fileName = decodeString(buffer, 128)
    val projectName = decodeString(buffer, 64)
    val title = decodeString(buffer, 64)
    val description = decodeString(buffer, 512)
    val access = decodeString(buffer, 128)
    val author = decodeString(buffer, 64)
    val date = decodeString(buffer, 32)
    val server = decodeString(buffer, 32)
    val client = decodeString(buffer, 32)
    val editor = decodeString(buffer, 64)
    val editorStart = decodeString(buffer, 32)
    
    ChangeReply(header, changeNumber, DocumentInfo(key, version, fileName, projectName, title, description, access, author, date, server, client, editor, editorStart))
  }

  def decodeString(b: ChannelBuffer, length: Int) =
  {
    val string: String = b.readBytes(length).toString(Charset.forName("UTF-8"))
    string.substring(0, string.indexOf('\u0000'))
  }
}

class DaemonProtocolHandler(consumer: Actor) extends SimpleChannelUpstreamHandler with Loggable
{
  override def messageReceived(ctx: ChannelHandlerContext, e: MessageEvent)
  {
    consumer ! e.getMessage
    e.getChannel.close
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, e: ExceptionEvent)
  {
    logger.warn("Protocol exception " + e.getCause)
    e.getChannel.close
  }
}

case class DownstreamMessage(message: Messages.Type, body: (ChannelBuffer) => Unit)

case class Header(version: Int, message: Messages.Type, transactionId: Int, sequence: Int)

case class DocumentInfo(key: Int, version: Int, fileName: String, projectName: String, title: String, description: String, access: String, author: String, date: String, server: String, client: String, editor: String, editorStart: String)

case class ChangeReply(header: Header, changeNumber: Int, documentInfo: DocumentInfo)

object Messages extends Enumeration
{
  type Type = Value
  val nextChange = Value(21)
  val nextChangeResponse = Value(22)
}