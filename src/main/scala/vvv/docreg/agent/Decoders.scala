package vvv.docreg.agent

import org.jboss.netty.buffer.ChannelBuffer
import java.nio.charset.Charset

trait ReplyDecoder {
  def decode(header: Header, buffer: ChannelBuffer): Reply

  def readString(b: ChannelBuffer, length: Int) =
  {
    val string: String = b.readBytes(length).toString(Charset.forName("UTF-8"))
    string.substring(0, string.indexOf('\u0000'))
  }
}

object NextChangeReplyDecoder extends ReplyDecoder
{
  def decode(header: Header, buffer: ChannelBuffer) =
  {
    val changeNumber = buffer.readInt()
    val key = buffer.readInt()
    val version = buffer.readInt()
    val fileName = readString(buffer, 128)
    val projectName = readString(buffer, 64)
    val title = readString(buffer, 64)
    val description = readString(buffer, 512)
    val access = readString(buffer, 128)
    val author = readString(buffer, 64)
    val date = readString(buffer, 32)
    val server = readString(buffer, 32)
    val client = readString(buffer, 32)
    val editor = readString(buffer, 64)
    val editorStart = readString(buffer, 32)

    NextChangeReply(
      changeNumber,
      DocumentInfo(
        key,
        version,
        fileName,
        projectName,
        title,
        description,
        access,
        author,
        date,
        server,
        client,
        editor,
        editorStart)
    )
  }
}

object RegisterReplyDecoder extends ReplyDecoder
{
  def decode(header: Header, buffer: ChannelBuffer) =
  {
    val response: String = readString(buffer, 128)
    val suggestedFileName: String = readString(buffer, 128)

    RegisterReply(
      response,
      suggestedFileName
    )
  }
}

object SubmitReplyDecoder extends ReplyDecoder
{
  def decode(header: Header, buffer: ChannelBuffer) =
  {
    val response: String = readString(buffer, 128)
    val suggestedFileName: String = readString(buffer, 128)

    SubmitReply(
      response,
      suggestedFileName
    )
  }
}