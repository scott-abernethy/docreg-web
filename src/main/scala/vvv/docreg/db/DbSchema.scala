package vvv.docreg.db

import vvv.docreg.model._
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Table, KeyedEntity}

trait DbObject[T <: KeyedEntity[Long]] extends KeyedEntity[Long] {
  var id: Long = 0
  def dbTable: Table[T]

  def reload(): Option[T] = {
    dbTable.lookup(id)
  }

  def lookup(id: Long): Option[T] = {
    dbTable.lookup(id)
  }
}

object DbSchema extends org.squeryl.Schema {
  val users = table[User]
  val userLookups = table[UserLookup]
  val projects = table[Project]
  val userProjects = table[UserProject]
  val documents = table[Document]
  val revisions = table[Revision]
  val pendings = table[Pending]
  val approvals = table[Approval]
  val subscriptions = table[Subscription]

  on(users)(u => declare(
    u.username is(indexed, unique, dbType("varchar(64)")),
    u.name is(indexed, dbType("varchar(64)")),
    u.email is(indexed, dbType("varchar(64)")),
    u.host is(dbType("varchar(64)")),
    u.localServer is(dbType("varchar(64)")),
    u.timeZone is(dbType("varchar(32)"))
  ))

  on(userLookups)(u => declare(
    u.userId is (indexed),
    u.username is(indexed, dbType("varchar(64)")),
    u.name is(indexed, dbType("varchar(64)")),
    u.email is(indexed, dbType("varchar(64)")),
    columns(u.username, u.name, u.email) are(indexed, unique)
  ))

  on(projects)(p => declare(
    p.name is(indexed, unique, dbType("varchar(64)"))
  ))

  on(userProjects)(u => declare(
    u.userId is (indexed),
    u.projectId is(indexed),
    columns(u.userId, u.projectId) are(indexed, unique)
  ))

  on(documents)(d => declare(
    d.key is(indexed, unique, dbType("varchar(10)")),
    d.title is(dbType("varchar(128)")),
    d.access is(dbType("varchar(128)"))
  ))

  on(revisions)(r => declare(
    r.documentId is(indexed),
    columns(r.documentId, r.version) are (unique, indexed),
    r.filename is(dbType("varchar(128)")),
    r.comment is(dbType("varchar(512)"))
  ))

  on(pendings)(p => declare(
    p.userId is(indexed),
    p.documentId is(indexed),
    columns(p.userId, p.documentId, p.action) are (unique, indexed)
  ))

  on(approvals)(a => declare(
    a.revisionId is(indexed),
    a.userId is(indexed),
    a.comment is(dbType("varchar(128)")),
    columns(a.revisionId, a.userId) are (unique, indexed)
  ))

  on(subscriptions)(s => declare(
    s.documentId is(indexed),
    s.userId is(indexed),
    columns(s.documentId, s.userId) are (unique, indexed)
  ))

  val projectsToDocuments = oneToManyRelation(projects, documents).via( (p,d) => p.id === d.projectId)
  val documentsToRevisions = oneToManyRelation(documents, revisions).via( (d,r) => d.id === r.documentId)
  val documentsToPendings = oneToManyRelation(documents, pendings).via( (d,p) => d.id === p.documentId)
  val usersToRevisions = oneToManyRelation(users, revisions).via( (u,r) => u.id === r.authorId)
  val usersToApprovals = oneToManyRelation(users, approvals).via( (u,a) => u.id === a.userId)
  val usersToPendings = oneToManyRelation(users, pendings).via( (u,p) => u.id === p.userId)
  /*
   on(cultists)(c => declare(
     c.email is(indexed, unique)
   ))
   on(artifacts)(a => declare(
     a.path is(indexed, dbType("varchar(255)")),
     columns(a.gatewayId, a.path) are(unique, indexed)
   ))
   on(clones)(c => declare(
     c.artifactId is(indexed),
     c.forCultistId is(indexed),
     columns(c.artifactId, c.forCultistId) are(unique, indexed)
   ))
   on(presences)(p => declare(
     p.artifactId is(indexed, unique)
   ))

   val cultistToGateways = oneToManyRelation(cultists, gateways).via((c,g) => c.id === g.cultistId)
   val gatewayToArtifacts = oneToManyRelation(gateways, artifacts).via((g,a) => g.id === a.gatewayId)
   val artifactToClones = oneToManyRelation(artifacts, clones).via((a,cl) => a.id === cl.artifactId)
   val cultistToClones = oneToManyRelation(cultists, clones).via((c,cl) => c.id === cl.forCultistId)
   val artifactToPresences = oneToManyRelation(artifacts, presences).via((a,p) => a.id === p.artifactId)

   override def drop = super.drop
  */
}
