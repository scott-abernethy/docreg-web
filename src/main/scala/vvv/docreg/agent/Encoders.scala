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

import org.jboss.netty.buffer.ChannelBuffer
import net.liftweb.common.Loggable

trait RequestEncoder[T <: Request] extends Loggable
{
  def messageType: MessageType.Type

  def encode(request: T, buffer: ChannelBuffer)

  def writeString(b: ChannelBuffer, string: String, length: Int)
  {
    val bit = if (string.length() > length)
    {
      logger.warn("Truncated string during encoding, " + string + " must be size " + length)
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
    writeString(buffer, request.clientHost, 64)
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

trait EditRequestEncoder extends RequestEncoder[EditRequest]
{
  def messageType = MessageType.editRequest

  def encode(request: EditRequest, buffer: ChannelBuffer)
  {
//    char acFileName[128];
//    char acAuthor[64];
    writeString(buffer, request.fileName, 128)
    writeString(buffer, request.userName, 64)
  }
}

trait UneditRequestEncoder extends RequestEncoder[UneditRequest]
{
  def messageType = MessageType.uneditRequest

  def encode(request: UneditRequest, buffer: ChannelBuffer)
  {
//    char acFileName[128];
//    char acAuthor[64];
    writeString(buffer, request.fileName, 128)
    writeString(buffer, request.userName, 64)
  }
}

trait SubscribeRequestEncoder extends RequestEncoder[SubscribeRequest]
{
  def messageType = MessageType.subscribeRequest

  def encode(request: SubscribeRequest, buffer: ChannelBuffer)
  {
//    char acName[128];          // file name
//    char acAuthor[64];         // author name as displayed
//    char acEmailAddress[64];   // e-mail address
//    char acOptions[128];       // space separated options
    writeString(buffer, request.fileName, 128)
    writeString(buffer, request.userName, 64)
    writeString(buffer, request.email, 64)
    writeString(buffer, request.options, 128)
  }
}

trait UnsubscribeRequestEncoder extends RequestEncoder[UnsubscribeRequest]
{
  def messageType = MessageType.unsubscribeRequest

  def encode(request: UnsubscribeRequest, buffer: ChannelBuffer)
  {
//    char acName[128];          // file name
//    char acAuthor[64];         // author name as displayed
//    char acEmailAddress[64];   // e-mail address
    writeString(buffer, request.fileName, 128)
    writeString(buffer, request.userName, 64)
    writeString(buffer, request.email, 64)
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

trait ApprovalRequestEncoder extends RequestEncoder[ApprovalRequest]
{
  def messageType = MessageType.approvalRequest

  def encode(request: ApprovalRequest, buffer: ChannelBuffer)
  {
//    char acFileName[128];      // file name
//    char acApprover[64];       // approver name as displayed
//    char acEmailAddress[64];   // e-mail address
//    char acStatus[32];         // "Approved", "Not Approved" or "Pending"
//    char acComment[128];       // optional comment on approval
//    char acClientName[64];        // name of the client PC as resolved by client
//    char acUserName[64];          // user name client is running under
    writeString(buffer, request.fileName, 128)
    writeString(buffer, request.approverUserName, 64)
    writeString(buffer, request.approverEmail, 64)
    writeString(buffer, request.status, 32)
    writeString(buffer, request.comment, 128)
    writeString(buffer, request.clientHost, 64)
    writeString(buffer, request.userName, 64)
  }
}
