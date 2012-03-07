package vvv.docreg.agent

import actors.Actor

// todo message type part of request case class? register handler for each type, gives case class encode and decode.
object MessageType extends Enumeration
{
  type Type = Value
  val registerRequest = Value(0)
  val registerReply = Value(1)
  val submitRequest = Value(4)
  val submitReply = Value(5)
  val nextChangeRequest = Value(21)
  val nextChangeReply = Value(22)
}

case class Header(version: Int, message: MessageType.Type, transactionId: Int, sequence: Int)

case class DocumentInfo(key: Int, version: Int, fileName: String, projectName: String, title: String, description: String, access: String, author: String, date: String, server: String, client: String, editor: String, editorStart: String)

sealed abstract class Request
sealed abstract class Reply

case class RegisterRequest(fileName: String, project: String, comment: String, access: String, author: String, userName: String, clientName: String, clientVersion: String) extends Request
case class RegisterReply(response: String, suggestedFileName: String) extends Reply

case class SubmitRequest(fileName: String, size: Int = -1) extends Request
case class SubmitReply(response: String, suggestedFileName: String) extends Reply

case class NextChangeRequest(lastChange: Int) extends Request
case class NextChangeReply(changeNumber: Int, documentInfo: DocumentInfo) extends Reply

