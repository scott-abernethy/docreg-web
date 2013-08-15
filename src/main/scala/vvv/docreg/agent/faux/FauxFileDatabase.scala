/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.agent.faux

import scala.util.Random
import akka.actor.Actor
import java.util.Date
import vvv.docreg.util.T
import vvv.docreg.util.StringUtil.prePadTo
import vvv.docreg.agent._
import vvv.docreg.agent.FileDatabaseApi._
import net.liftweb.json._
import net.liftweb.util.ControlHelpers._
import net.liftweb.common.Box
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

trait FauxData {

  def loadData(): Box[JValue] = tryo {
    // Load db from JSON file
    val is = getClass().getClassLoader().getResourceAsStream("faux/external-db.json")
    val str = scala.io.Source.fromInputStream(is).mkString  
    parse(str)
  }
  
  def documentParser(json: JValue): List[DocumentInfo] = {
    import vvv.docreg.util.JValueWithFilter._
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
    yield DocumentInfo(number.toInt, version.toInt, filename, project, title, comment, access, name, parseAgentDate(date), server, ip, null, null)  
  }
  
  def revisionParser(json: JValue, key: String): List[RevisionInfo] = {
    import vvv.docreg.util.JValueWithFilter._
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
        parseAgentDate(date), server, ip, host, username, v, crc)
  }
  
  def approvalParser(json: JValue, key: String): List[ApprovalInfo] = {
    import vvv.docreg.util.JValueWithFilter._
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
        parseAgentDate(date), ip, host, actioner)
  }

  def subscriberParser(json: JValue, key: String): List[SubscriberInfo] = {
    import vvv.docreg.util.JValueWithFilter._
    for {
      JObject(document) <- json \ "documents"
      JField("number", JString(number)) <- document
      if (number == key)
      JField("subscriptions", JArray(subscriptions)) <- document
      JObject(subscription) <- subscriptions
      JField("username", JString(username)) <- subscription
      JField("email", JString(email)) <- subscription
      JField("options", JString(options)) <- subscription
    }
    yield SubscriberInfo(username, email, options)
  }

  def usedNumbers(json: JValue): Set[String] = {
    import vvv.docreg.util.JValueWithFilter._
    (
      for {
        JString(number) <- json \ "documents" \ "number"
      } 
      yield number
    ).toSet
  }

  def findFreeNumber(json: JValue): Int = {
    // Pick a random starting 'number' then find the first unused after that...
    Stream.from(Random.nextInt(9999)).map(_ % 9999).filterNot(x => usedNumbers(json).contains(prePadTo(x.toString, 4, '0'))).head
  }

  def addDocumentChange(info: DocumentInfo, username: String, json: JValue): (DocumentInfo, JValue) = {
    val num = findFreeNumber(json)
    val numStr = documentNumberFormat.format(num)
    val filename = "%s-001-%s".format(numStr, info.fileName)
    val dateStr = formatAgentDate(info.date)

    // Update DocumentInfo with new num etc
    val info2 = info.copy(number = num, version = 1, fileName = filename)

    val toAdd: JValue = ("documents" -> List(
        ("number" -> numStr) ~
        ("version" -> info2.version) ~
        ("filename" -> info2.fileName) ~
        ("project" -> info2.projectName) ~
        ("title" -> info2.title) ~
        ("comment" -> info2.description) ~
        ("server" -> info2.server) ~
        ("access" -> info2.access) ~
        ("author-name" -> info2.author) ~
        ("author-ip" -> info2.client) ~
        ("date" -> dateStr) ~
        ("revisions" -> List(
          ("filename" -> info2.fileName) ~
          ("comment" -> info2.description) ~
          ("date" -> dateStr) ~
          ("author-name" -> info2.author) ~
          ("author-ip" -> info2.client) ~
          ("author-host" -> info2.client) ~
          ("author-username" -> username) ~
          ("author-version" -> "3") ~
          ("crc" -> "asdf")
          ))
      ))

    (info2, json.merge(toAdd))
  }
  
  def updateDocumentChange(info: DocumentInfo, username: String, json: JValue): (DocumentInfo, JValue) = {
    val dateStr = formatAgentDate(info.date)

    // This JSON merge is more tricky / messy. Below is a transformer...
    def mergeDb(): JValue = {
      def mergeRevs(revs: JValue): JValue = {
        val latest =
          ("filename" -> info.fileName) ~
          ("comment" -> info.description) ~
          ("date" -> dateStr) ~
          ("author-name" -> info.author) ~
          ("author-ip" -> info.client) ~
          ("author-host" -> info.client) ~
          ("author-username" -> username) ~
          ("author-version" -> "3") ~
          ("crc" -> "asdf")
        revs match {
          case JArray(rs) => JArray(latest :: rs)
          case _ => JArray(latest :: Nil)
        }
      }

      def mergeDoc(doc: JObject): JValue = {
        ("number" -> info.getKey) ~
        ("version" -> info.version) ~
        ("filename" -> info.fileName) ~
        ("project" -> info.projectName) ~
        ("title" -> info.title) ~
        ("comment" -> info.description) ~
        ("server" -> info.server) ~
        ("access" -> info.access) ~
        ("author-name" -> info.author) ~
        ("author-ip" -> info.client) ~
        ("date" -> dateStr) ~
        ("revisions" -> mergeRevs(doc \ "revisions"))
      }

      def mergeDocs(docs: JValue): JValue = {
        docs.transform { 
          case doc @ JObject(JField("number", JString(num)) :: fs) if (num == info.getKey) => {
            mergeDoc(doc)
          }
        }
      }

      // Now apply the transforms and return the result...
      json.transform { 
        case JField("documents", docs) => JField("documents", mergeDocs(docs))
      }
    }

    (info, mergeDb())
  }
}

class FauxFileDatabase extends Actor with FauxData {

  var db: JValue = JNothing

  def receive = {
    case GetRegister => {
      db = loadData().openOr(JNothing)
      val register: List[DocumentInfo] = documentParser(db)
      sender ! ResponseRegister(register)
    }
    case m @ GetLog(key, access) => {
      sender ! ResponseLog(key, revisionParser(db, key))
    }
    case GetMail(key) => {
      sender ! ResponseMail(key, subscriberParser(db, key))
    }
    case GetApproval(key) => {
      sender ! ResponseApproval(key, approvalParser(db, key))
    }
    case AddDocument(info, username, notifyTo) => {
      println("add doc, existing = " + usedNumbers(db) + ", this = " + info.getKey())
      val (change, db2) = 
        if (usedNumbers(db).contains(info.getKey())) updateDocumentChange(info, username, db)
        else addDocumentChange(info, username, db)
      db = db2
      notifyTo ! AddDocumentChange(change)
    }
    case other => {
      unhandled(other)
    }
  }
}
