/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.agent

import akka.actor.Actor
import scala.util.control.Exception._
import scalax.file.NotFileException
import java.util.concurrent.TimeUnit
import scalax.io.managed.SeekableByteChannelResource
import java.text.SimpleDateFormat
import java.util.{TimeZone, Date}
import util.matching.Regex
import java.io.{File, FileInputStream, FileNotFoundException}
import net.liftweb.common.Loggable
import scalax.io.{LongTraversable, Resource, SeekableByteChannel}
import scala.concurrent.Future
import scala.util.Success
import scala.concurrent.duration._

// docreg/docreg.txt
// docreg/editlog.txt
// docreg/Server.txt
// docreg/log/XXXX.log
// docreg/mail/XXXX.mail
// docreg/approval/XXXX.approval

case object GetRegister
case object GetEditLog
case class GetLog(key: String, access: String)
case class GetMail(key: String)
case class GetApproval(key: String)

case class ResponseRegister(items: List[DocumentInfo])
case class ResponseLog(key: String, items: List[RevisionInfo])
case class ResponseMail(key: String, items: List[SubscriberInfo])
case class ResponseApproval(key: String, items: List[ApprovalInfo])
case class ResponseFailure(request: AnyRef)

class FileDatabase extends Actor
{
  import scala.concurrent.ExecutionContext.Implicits.global

  def receive =
  {
    case m @ GetRegister => {
      val to = sender
      FileDatabaseHelper.loadRegister().onComplete{
        case Success(items) => to ! ResponseRegister(items)
        case _ => to ! ResponseFailure(m)
      }
    }

    case m @ GetLog(key, access) => {
      val to = sender
      FileDatabaseHelper.loadLog(key, access).onComplete{
        case Success(items) => to ! ResponseLog(key, items)
        case _ => to ! ResponseFailure(m)
      }
    }

    case m @ GetMail(key) => {
      val to = sender
      FileDatabaseHelper.loadMail(key).onComplete{
        case Success(items) => to ! ResponseMail(key, items)
        case _ => to ! ResponseFailure(m)
      }
    }

    case m @ GetApproval(key) => {
      val to = sender
      FileDatabaseHelper.loadApproval(key).onComplete{
        case Success(items) => to ! ResponseApproval(key, items)
        case _ => to ! ResponseFailure(m)
      }
    }
  }
}

object FileDatabaseHelper {
//  private static final Pattern FILENAME_FORMAT = Pattern.compile("^([a-zA-Z0-9]+)-([0-9]+)-(.*)");

  def parseDate(dateString: String): Date = {
    if ("".equals(dateString)) {
      return null
    }
    try {
      // Date formats are not synchronized, so create this every time.
      val x = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss 'Z'");
      x.setTimeZone(TimeZone.getTimeZone("UTC"))
      x.parse(dateString);
    }
    catch {
      case x: Exception => {
        println("Failed to parse '" + dateString + "' - " + x);
        null;
      }
    }
  }

  def loadRegister(): Future[List[DocumentInfo]] = {
    val register = if (AgentVendor.secure) "secure/docreg-secure.txt" else "docreg/docreg.txt"
    loadAsync(register, createDocumentInfo _)
  }

  val ValidNumber: Regex = """^([0-9]+)$""".r

  def createDocumentInfo(data: Array[String]): Option[DocumentInfo] = {
    data.toList match {
      case List(ValidNumber(key), ValidNumber(version), fileName, projectName, title, description, access, author, date, server, client, editor, editorStart) => {
        Some(DocumentInfo(key.toInt, version.toInt, fileName, projectName, title, description, access, author, parseDate(date), server, client, editor, parseDate(editorStart)))
      }
      case List(ValidNumber(key), ValidNumber(version), fileName, projectName, title, description, access, author, date, server, client) => {
        Some(DocumentInfo(key.toInt, version.toInt, fileName, projectName, title, description, access, author, parseDate(date), server, client, "", null))
      }
      case _ => {
        None
      }
    }
  }

  def loadMail(key: String): Future[List[SubscriberInfo]] = {
    loadAsync("docreg/mail/" + key + ".mail", createMailInfo _)
  }

  def createMailInfo(data: Array[String]): Option[SubscriberInfo] = {
    data.toList match {
      case List(userName, email, options) => {
        Some(SubscriberInfo(userName, email, options))
      }
      case _ => {
        None
      }
    }
  }

  def loadLog(key: String, access: String): Future[List[RevisionInfo]] = {
    def pathFor = access match {
      case "Secure" if (AgentVendor.secure) => "secure/log/" + key + ".log"
      case _ => "docreg/log/" + key + ".log"
    }
    loadAsync(pathFor, createRevisionInfo _)
  }

  def createRevisionInfo(data: Array[String]): Option[RevisionInfo] = {
    data.toList match {
      case List(fileName, project, comment, access, author, date, server, clientIp, clientHost, clientUserName, clientVersion, crc) => {
        Some(RevisionInfo(fileName, project, comment, access, author, parseDate(date), server, clientIp, clientHost, clientUserName, clientVersion, crc))
      }
      case _ => {
        None
      }
    }
  }

  def loadApproval(key: String): Future[List[ApprovalInfo]] = {
    loadAsync("docreg/approval/" + key + ".approval", createApprovalInfo _)
  }

  def createApprovalInfo(data: Array[String]): Option[ApprovalInfo] = {
    data.toList match {
      case List(fileName, approverName, approverEmail, status, comment, date, _, clientIp, clientHost, clientUserName) => {
        Some(ApprovalInfo(fileName, approverName, approverEmail, status, comment, parseDate(date), clientIp, clientHost, clientUserName))
      }
      case x => {
        println(x)
        None
      }
    }
  }

  def loadAsync[X](path: String, encoder: (Array[String]) => Option[X]): Future[List[X]] = {
    implicit val codec = scalax.io.Codec.ISO8859
    val fullPath: String = AgentVendor.home + "/" + path
    val resource = Resource.fromInputStream(new FileInputStream(fullPath))

    val items = resource.lines().dropWhile(_.isEmpty).map(_ split '\t')

    val processor = for {
      in <- items.processor
      _ <- in.repeatUntilEmpty()
      next <- in.next.timeout(15.seconds)
    } yield encoder(next)

    processor.traversable[Option[X]].async.foldLeft(List.empty[X]) {
      (list, x) =>
        x match {
          case Some(encoded) =>  list ::: (encoded :: Nil)
          case _ => list
        }
    }
  }
}

object SubmitBin {
  def copyTo(sourceFile: File, submitFileName: String) {
    val local = Resource.fromFile(sourceFile)
    val remote = Resource.fromFile(AgentVendor.home + "/submit/" + submitFileName)
    // todo error handling?
    local.copyDataTo(remote)
  }
}