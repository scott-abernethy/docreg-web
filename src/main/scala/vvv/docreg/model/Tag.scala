/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.model

import vvv.docreg.db.{DbObject}
import vvv.docreg.db.DbSchema.{tags, documents}
import org.squeryl.PrimitiveTypeMode._
import scala.xml._

class Tag extends DbObject[Tag] {
  def dbTable = tags
  var documentId: Long = 0
  var name: String = ""
}

object Tag extends Tag {
  def ensure(name: String, documentId: Long) {
    inTransaction{
      if (find(name, documentId).isEmpty) {
        val tag = new Tag
        tag.documentId = documentId
        tag.name = name
        tags.insert(tag);
      }
    }
  }

  def remove(name: String, documentId: Long) {
    inTransaction{
      tags.deleteWhere(t => t.name === name and documentId === documentId)
    }
  }

  def find(name: String, documentId: Long): Option[Tag] = {
    inTransaction(
      from(tags)(t => where(t.name === name and t.documentId === documentId) select(t)).headOption
    )
  }

  def documentsWithTag(name: String): List[Document] = {
    inTransaction(
      join(tags, documents)( (t, d) =>
        where(t.name === name)
        select(d)
        orderBy(d.title asc, d.id asc)
        on(t.documentId === d.id)
      ).toList
    )
  }

  def names(): List[String] = {
    inTransaction(
      from(tags)(t => select(t.name) orderBy(t.name asc)).distinct.toList
    )
  }

  def namesForDocument(documentId: Long): List[String] = {
    inTransaction(
      from(tags)(t => where(t.documentId === documentId) select(t.name) orderBy(t.name asc)).distinct.toList
    )
  }

  def url(name: String): String = {
    if (name.startsWith("#")) {
      "/tag/" + name.substring(1) // there is nothing to escape in a tag name
    }
    else {
      "/tag"
    }
  }

  def addLinks(text: String, tags: List[String]): Node = {
    var out = text
    for (tag <- tags) {
      out = out.replaceAll(tag, "<a href='" + Tag.url(tag) + "'>" + tag + "</a>")
    }
    Unparsed(out)
  }
}
