package vvv.docreg.agent

import org.jboss.netty.buffer.ChannelBuffer

trait RequestEncoder[T <: Request]
{
  def messageType: MessageType.Type

  def encode(request: T, buffer: ChannelBuffer)

  def writeString(b: ChannelBuffer, string: String, length: Int)
  {
    val bit = if (string.size > length)
    {
      println("Truncated string during encoding, " + string + " must be size " + length)
      string.substring(0, length - 1)
    }
    else
    {
      string
    }

    b.writeBytes(bit.toCharArray.map(_.toByte))
    b.writeZero(length - bit.size)
  }
}

trait RegisterRequestEncoder extends RequestEncoder[RegisterRequest]
{
  def messageType = MessageType.registerRequest

  def encode(request: RegisterRequest, buffer: ChannelBuffer)
  {
//     char acName[128];          // file name
//    char acProject[64];        // project name
//    char acDescription[512];   // document description
//    char acAccess[128];        // document access rights
//    char acAuthor[64];         // author name as displayed
//    char acClientName[64];     // name of the client PC as resolved by client
//    char acUserName[64];       // user name client is running under
//    char acClientVersion[16];  // client software version
    writeString(buffer, request.fileName, 128)
    writeString(buffer, request.project, 64)
    writeString(buffer, request.comment, 512)
    writeString(buffer, request.access, 128)
    writeString(buffer, request.author, 64)
    writeString(buffer, request.clientName, 64)
    writeString(buffer, request.userName, 64)
    writeString(buffer, request.clientVersion, 16)
  }
}

trait SubmitRequestEncoder extends RequestEncoder[SubmitRequest]
{
  def messageType = MessageType.submitRequest

  def encode(request: SubmitRequest, buffer: ChannelBuffer)
  {
//        char acName[128];
//    int iFileSize;                       // size of the file or -1 if unknown
    writeString(buffer, request.fileName, 128)
    buffer.writeInt(request.size)
  }
}

trait NextChangeRequestEncoder extends RequestEncoder[NextChangeRequest]
{
  def messageType = MessageType.nextChangeRequest

  def encode(request: NextChangeRequest, buffer: ChannelBuffer)
  {
    buffer.writeInt(request.lastChange)
  }
}

class RequestAndEncoder[T <: Request](request: T, encoder: RequestEncoder[T])
{

}