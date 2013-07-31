package vvv.docreg.agent.faux

import vvv.docreg.agent.FileDatabaseApi._
import akka.actor.Actor
import vvv.docreg.agent.DocumentInfo
import java.util.Date
import vvv.docreg.agent.RevisionInfo
import vvv.docreg.util.T
import net.liftweb.json._
import net.liftweb.util.ControlHelpers._
import net.liftweb.common.Box
import vvv.docreg.agent.FileDatabaseHelper
import vvv.docreg.agent.ApprovalInfo

trait FauxData {
  def loadData(): Box[JValue] = tryo {
    val is = getClass().getClassLoader().getResourceAsStream("faux/external-db.json")
    val str = scala.io.Source.fromInputStream(is).mkString  
    parse(str)
  }
}

class FauxFileDatabase extends Actor with FauxData {
  
  lazy val db = loadData().openOr(JNothing)
  
  def receive = {
    case GetRegister => {
      val register: List[DocumentInfo] = documentParser(db)
      sender ! ResponseRegister(register)
    }
    case m @ GetLog(key, access) => {
      sender ! ResponseLog(key, revisionParser(db, key))
    }
    case GetMail(key) => {
      sender ! ResponseMail(key, Nil)
    }
    case GetApproval(key) => {
      sender ! ResponseApproval(key, approvalParser(db, key))
    }
    case other => {
      unhandled(other)
    }
  }
  
  def documentParser(json: JValue): List[DocumentInfo] = {
    for {
      JObject(document) <- json \ "documents"
      JField("number", JString(number)) <- document
      JField("version", JInt(version)) <- document
      JField("filename", JString(filename)) <- document
      JField("project", JString(project)) <- document
      JField("title", JString(title)) <- document
      JField("comment", JString(comment)) <- document
      JField("server", JString(server)) <- document
      JField("access", JString(access)) <- document
      JField("date", JString(date)) <- document
      JField("author-name", JString(name)) <- document
      JField("author-ip", JString(ip)) <- document
    } 
    yield DocumentInfo(number.toInt, version.toInt, filename, project, title, comment, access, name, FileDatabaseHelper.parseDate(date), server, ip, null, null)  
  }
  
  def revisionParser(json: JValue, key: String): List[RevisionInfo] = {
    for {
      JObject(document) <- json \ "documents"
      JField("number", JString(number)) <- document
      if (number == key)
      JField("project", JString(project)) <- document
      JField("access", JString(access)) <- document
      JField("server", JString(server)) <- document
      JField("revisions", JArray(revisions)) <- document
      JObject(revision) <- revisions
      JField("filename", JString(filename)) <- revision
      JField("comment", JString(comment)) <- revision
      JField("date", JString(date)) <- revision
      JField("author-name", JString(name)) <- revision
      JField("author-ip", JString(ip)) <- revision
      JField("author-host", JString(host)) <- revision
      JField("author-username", JString(username)) <- revision
      JField("author-version", JString(v)) <- revision
      JField("crc", JString(crc)) <- revision
    }
    yield RevisionInfo(filename, project, comment, access, name, 
        FileDatabaseHelper.parseDate(date), server, ip, host, username, v, crc)
  }
  
  def approvalParser(json: JValue, key: String): List[ApprovalInfo] = {
    for {
      JObject(document) <- json \ "documents"
      JField("number", JString(number)) <- document
      if (number == key)
      JField("approvals", JArray(approvals)) <- document
      JObject(approval) <- approvals
      JField("filename", JString(filename)) <- approval
      JField("comment", JString(comment)) <- approval
      JField("status", JString(status)) <- approval
      JField("date", JString(date)) <- approval
      JField("author-ip", JString(ip)) <- approval
      JField("author-host", JString(host)) <- approval
      JField("author-email", JString(email)) <- approval
      JField("author-username", JString(username)) <- approval
      JField("client-username", JString(actioner)) <- approval
    }
    yield ApprovalInfo(filename, username, email, status, comment, 
        FileDatabaseHelper.parseDate(date), ip, host, actioner)
  }
}