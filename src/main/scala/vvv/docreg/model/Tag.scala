package vvv.docreg.model

import vvv.docreg.db.{DbObject}
import vvv.docreg.db.DbSchema.{tags, documents}
import org.squeryl.PrimitiveTypeMode._

class Tag extends DbObject[Tag] {
  def dbTable = tags
  var documentId: Long = 0
  var name: String = ""
  var ignored: Boolean = false
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

  def all(): List[Tag] = {
    inTransaction(
      from(tags)(t => select(t)).toList
    )
  }

  def forDocument(documentId: Long): List[Tag] = {
    inTransaction(
      from(tags)(t => where(t.documentId === documentId) select(t)).toList
    )
  }
}
