package vvv.docreg.agent

import akka.actor.Actor
import scala.util.control.Exception._
import scalax.file.NotFileException
import akka.util.Duration
import java.util.concurrent.TimeUnit
import akka.dispatch.Await
import java.io.{FileInputStream, FileNotFoundException}
import scalax.io.managed.SeekableByteChannelResource
import scalax.io.{SeekableByteChannel, Resource}
import java.text.SimpleDateFormat
import java.util.{TimeZone, Date}
import util.matching.Regex

// docreg/docreg.txt
// docreg/editlog.txt
// docreg/Server.txt
// docreg/log/XXXX.log
// docreg/mail/XXXX.mail
// docreg/approval/XXXX.approval

case object GetRegister
case object GetEditLog
case class GetLog(key: String)
case class GetMail(key: String)
case class GetApproval(key: String)

case class ResponseRegister(items: List[DocumentInfo])
case class ResponseLog(key: String, items: List[RevisionInfo])
case class ResponseMail(key: String, items: List[SubscriberInfo])
case class ResponseApproval(key: String, items: List[ApprovalInfo])
case class ResponseFailure(request: AnyRef)

class FileDatabase extends Actor
{
  def receive =
  {
    case m @ GetRegister => {
      FileDatabaseHelper.loadRegister() match {
        case Some(items) => sender ! ResponseRegister(items)
        case _ => sender ! ResponseFailure(m)
      }
    }

    case m @ GetLog(key) => {
      FileDatabaseHelper.loadLog(key) match {
        case Some(items) => sender ! ResponseLog(key, items)
        case _ => sender ! ResponseFailure(m)
      }
    }

    case m @ GetMail(key) => {
      FileDatabaseHelper.loadMail(key) match {
        case Some(items) => sender ! ResponseMail(key, items)
        case _ => sender ! ResponseFailure(m)
      }
    }

    case m @ GetApproval(key) => {
      FileDatabaseHelper.loadApproval(key) match {
        case Some(items) => sender ! ResponseApproval(key, items)
        case _ => sender ! ResponseFailure(m)
      }
    }
  }
}

object FileDatabaseHelper {
  val DATE_FORMAT = {
    val x = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss 'Z'");
    x.setTimeZone(TimeZone.getTimeZone("UTC"))
    x
  }
//  private static final Pattern FILENAME_FORMAT = Pattern.compile("^([a-zA-Z0-9]+)-([0-9]+)-(.*)");

  def parseDate(dateString: String): Date = {
    try {
      DATE_FORMAT.parse(dateString);
    }
    catch {
      case x => {
        println("Failed to parse " + dateString + " - " + x.getMessage);
        null;
      }
    }
  }

  def loadRegister(): Option[List[DocumentInfo]] = {
    // TODO
//    load("secure/docreg-secure.txt", createDocumentInfo _)
    load("docreg/docreg.txt", createDocumentInfo _)
  }

  val ValidNumber: Regex = """^([0-9])+$""".r

  def createDocumentInfo(data: Array[String]): Option[DocumentInfo] = {
    data.toList match {
      case List(ValidNumber(key), ValidNumber(version), fileName, projectName, title, description, access, author, date, server, client, editor, editorStart) => {
        Some(DocumentInfo(key.toInt, version.toInt, fileName, projectName, title, description, access, author, parseDate(date), server, client, editor, parseDate(editorStart)))
      }
      case _ => {
        None
      }
    }
  }

  def loadMail(key: String): Option[List[SubscriberInfo]] = {
    load("docreg/mail/" + key + ".mail", createMailInfo _)
  }

  def createMailInfo(data: Array[String]): Option[SubscriberInfo] = {
    data.toList match {
      case List(userName, email, emailEvent) => {
        Some(SubscriberInfo(userName, email, emailEvent))
      }
      case _ => {
        None
      }
    }
  }

  def loadLog(key: String): Option[List[RevisionInfo]] = {
    load("docreg/log/" + key + ".log", createRevisionInfo _)
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

  def loadApproval(key: String): Option[List[ApprovalInfo]] = {
    load("docreg/approval/" + key + ".approval", createApprovalInfo _)
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

  def load[X](path: String, encoder: (Array[String]) => Option[X]): Option[List[X]] = {
    implicit val codec = scalax.io.Codec.ISO8859

    val fullPath: String = "/srv/docreg-fs/" + path

    val resource = Resource.fromInputStream(new FileInputStream(fullPath))
//    val resource = Resource.fromFile(filePath)

    // TODO filter on regex
    val items = resource.lines().dropWhile(_.isEmpty)/*.filter(_ => true)*/.map(_ split '\t')

//    val processor = for {
//      in <- items.processor
//      _ <- in.repeatUntilEmpty()
//      next <- in.next
//    } yield new TabLine(next)

//    processor.traversable.async.mkString("aaa"))

    //    Await.result(processor.traversable.async.foldmkString("AAA"), Duration.create(60L, TimeUnit.SECONDS)) match {
    //      case s: String => println(s)
    //      case _ => println("Fail")
    //    }

    val result = catching(classOf[NotFileException], classOf[FileNotFoundException]) either {
      items.foldLeft(List.empty[X]) {
        (list, line) =>
          encoder(line) match {
            case Some(encoded) => encoded :: list
            case _ => list
          }
      }
    }

    result match {
      case Left(error) => None
      case Right(data) => Some(data.reverse) // reverse needed due to foldLeft
    }
  }
}