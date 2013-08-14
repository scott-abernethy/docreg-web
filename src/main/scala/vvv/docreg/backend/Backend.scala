/*
 * Copyright (c) 2013 Aviat Networks.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import vvv.docreg.model.ApprovalState._
import vvv.docreg.util._
import net.liftweb.common._
import java.util.Date
import vvv.docreg.agent._
import vvv.docreg.agent.FileDatabaseApi._
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.model._
import akka.actor._
import scala.concurrent.duration._
import vvv.docreg.db.DbSchema

object BackendApi {
  case class Reload(d: Document)

  case class Loaded(ds: List[DocumentInfo])

  case class Reconcile(document: DocumentInfo, revisions: List[RevisionInfo], approvals: List[ApprovalInfo], subscriptions: List[SubscriberInfo])

  case class ApprovalApproved(document: Document, revision: Revision, user: User, state: ApprovalState, comment: String, actionedBy: User)

  case class ApprovalRequested(document: Document, revision: Revision, users: Iterable[User], actionedBy: User)

  case class SubscribeRequested(document: Document, user: User, options: String)

  case class UnsubscribeRequested(document: Document, user: User)

  case class Edit(document: Document, user: User)

  case class Unedit(document: Document, user: User)

  case class Submit(document: Document, projectName: String, localFile: () => java.io.File, userFileName: String, comment: String, user: User)

  case class Create(projectName: String, localFile: () => java.io.File, fileName: String, comment: String, user: User)
}

trait BackendComponent {
  val backend: ActorRef
}

class Backend(directory: Directory, daemonAgent: ActorRef, documentStream: ActorRef, fileDatabase: ActorRef) 
  extends Actor with Loggable with RevisionReconcile with ApprovalReconcile with SubscriptionReconcile with TagReconcile {

  import BackendApi._

  val product = ProjectProps.get("project.name") openOr "drw"
  val version = ProjectProps.get("project.version") openOr "0.0"
  val clientVersion = "dr+w " + version
  val target = AgentVendor.server
  val clerk = context.actorOf(Props(new Clerk(self)), "Clerk")
  val priorityClerk = context.actorOf(Props(new Clerk(self)), "PriorityClerk")
  val userLookup = new UserLookupProvider {
    def lookup(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], why: String) = UserLookup.lookup(usernameOption, emailOption, nameOption, directory, why)
  }

  override def preStart() {
    val system = Map(
      "java.version" -> System.getProperty("java.version"),
      "java.vendor" -> System.getProperty("java.vendor"),
      "timezone" -> java.util.TimeZone.getDefault.getDisplayName)
    logger.info("System is " + system)
    logger.info("Backend up for " + product + " v" + version + " connected to " + target)
    context.system.scheduler.scheduleOnce(10.seconds, self, 'Resync)(context.dispatcher)
    clerk ! Filing(fileDatabase)
    priorityClerk ! Filing(fileDatabase)
    super.preStart()
  }

  def receive = {
    case 'Resync => {
      logger.info("Backend resync against register, starting...")
      fileDatabase ! GetRegister
    }
    case ResponseRegister(ds) => {
      logger.debug("Parsing register for changes to reconcile, started...")
      self ! Loaded(ds)
    }
    case ResponseFailure(GetRegister) => {
      logger.warn("Failed to load register! Will try again in 6 hours.");
      context.system.scheduler.scheduleOnce(6.hours, self, 'Resync)(context.dispatcher)
    }
    case Loaded(d :: ds) => {
      Document.forKey(d.number) match {
        case Full(document) if (documentUpToDate(document, d)) => // No need to update
        case _ => clerk ! Prepare(d)
      }
      // Slow down the resync by delaying the remaining loads...
      // With 10K documents, one per ~100 msec, resync takes ~16 mins
      // With 1.8M douments, one per ~100 msec, resync takes ~50 hours
      context.system.scheduler.scheduleOnce(100.milliseconds, self, Loaded(ds))(context.dispatcher)
    }
    case Loaded(Nil) => {
      logger.debug("Backend resync against register, complete. Next in 24 hours")
      context.system.scheduler.scheduleOnce(24.hours, self, 'Resync)(context.dispatcher)
    }
    case Changed(d) => {
      // Todo: Apply what we know of the change now, then reconcile. Though the reconcile typically takes <1 second.
      logger.debug("Change received, sending to clerk " + d.number)
      priorityClerk ! Prepare(d)
    }
    case Some(msg @ Reconcile(d, revisions, approvals, subscriptions)) => {
      logger.debug("Reconcile " + d.number + " : " + (revisions.size, approvals.size, subscriptions.size))
      Document.forKey(d.number) match {
        case Full(document) => updateDocument(document, msg)
        case _ => createDocument(msg)
      }
    }
    case ApprovalApproved(d, r, user, state, comment, actionedBy) => {
      daemonAgent ! RequestPackage(self, target,
        ApprovalRequest(
          r.filename,
          user.shortUsername(),
          user.email,
          state match {
            case ApprovalState.approved => "Approved"
            case ApprovalState.notApproved => "Not Approved"
            case _ => "Pending"
          },
          comment,
          product, // todo is this consistent?
          actionedBy.shortUsername()
        ))
    }
    case ApprovalRequested(d, r, users, actionedBy) => {
      users.foreach(self ! ApprovalApproved(d, r, _, ApprovalState.pending, "", actionedBy))
    }
    case ApprovalReply(response) => {
      logger.info("Approval reply, " + response)
    }
    case SubscribeRequested(d, user, options) => {
      logger.info("Subscribe request, " + List(d.latest.filename, user.shortUsername(), user.email, options))
      daemonAgent ! RequestPackage(self, target, SubscribeRequest(d.latest.filename, user.shortUsername(), user.email, options))
    }
    case SubscribeReply(response, fileName, userName) => {
      logger.info("Subscribe reply, " + List(response, fileName, userName))
    }
    case UnsubscribeRequested(d, user) => {
      daemonAgent ! RequestPackage(self, target, UnsubscribeRequest(d.latest.filename, user.shortUsername(), user.email))
    }
    case UnsubscribeReply(response, fileName, userName) => {
      logger.info("Unsubscribe reply, " + List(response, fileName, userName))
    }
    case Edit(d, user) => {
      logger.info("Edit request, " + List(d.latest.filename, user.shortUsername()))
      daemonAgent ! RequestPackage(self, target, EditRequest(d.latest.filename, user.shortUsername()))
    }
    case EditReply(user) => {
      logger.info("Edit reply, editor is " + user)
    }
    case Unedit(d, user) => {
      logger.info("Unedit request, " + List(d.latest.filename, user.shortUsername()))
      daemonAgent ! RequestPackage(self, target, UneditRequest(d.latest.filename, user.shortUsername()))
    }
    case msg @ Submit(d, projectName, localFile, userFileName, comment, user) => {
      // todo check revision is latest?
      val engine = context.actorOf(Props(new SubmitEngine(daemonAgent, target, user.host, clientVersion)))
      engine ! msg
    }
    case msg @ Create(projectName, localFile, userFileName, comment, user) => {
      val engine = context.actorOf(Props(new SubmitNewEngine(daemonAgent, target, user.host, clientVersion)))
      engine ! msg
    }
    case 'Die => {
      logger.info("Backend killed")
      context.stop(self)
    }
  }

  private def projectWithName(name: String): Project = {
    Project.forName(name) match {
      case Some(p) => p
      case _ => {
        val project = new Project
        project.name = name
        Project.dbTable.insert(project)
        project
      }
    }
  }

  private def createDocument(reconcile: Reconcile) {
    try {
      val msgs = transaction {
        val document = new Document
        assignDocument(document, reconcile.document)
        Document.dbTable.insert(document)

        assignEditor(document, reconcile.document)

        val update = reconcileRevisions(document, reconcile.revisions)
        if (!update.contains(ReconcileDocumentRemoved)) {
          reconcileApprovals(document, reconcile.approvals)
          reconcileSubscriptions(document, reconcile.subscriptions)
          reconcileTags(document, reconcile.revisions.map(_.comment))
          List(DocumentAdded(document))
        }
        else {
          Nil
        }
      }
      msgs.foreach(m => documentStream ! m)
    } catch {
      case e: java.lang.NullPointerException => logger.error("Exception " + e + " with " + reconcile.document.number); e.printStackTrace
    }
  }

  private def updateDocument(document: Document, reconcile: Reconcile) {
    var msgs = List.empty[Any]
    transaction {
      DbSchema.documents.update(d => where(d.id === document.id) set(d.reconciled := T.now))
      val update = reconcileRevisions(document, reconcile.revisions)
      if (update.contains(ReconcileDocumentRemoved)) {
        clerk ! PrepareAlt(document.number)
        return
      }
      reconcileSubscriptions(document, reconcile.subscriptions)
      reconcileApprovals(document, reconcile.approvals)
      reconcileTags(document, reconcile.revisions.map(_.comment))

      val docChanged = assignDocument(document, reconcile.document)
      val editorChanged = assignEditor(document, reconcile.document)
      if (docChanged) {
        Document.dbTable.update(document)
      }
      // send DocumentChanged before DocumentRevised for optimal ui updates
      if (docChanged || update.contains(ReconcileRevisionUpdated)) {
        // we don't care about editorChanged here, as it is not reflected in current listeners
        msgs = msgs ::: DocumentChanged(document) :: Nil
      }
      update.collect {
        case ReconcileRevisionAdded(r) => r
      }.foreach {
        id =>
          document.revision(id).foreach(revision => msgs = msgs ::: DocumentRevised(document, revision) :: Nil)
      }
      if (update.contains(ReconcileRevisionPurged)) {
        clerk ! PrepareAlt(document.number)
      }
    }
    msgs.foreach(m => documentStream ! m)
  }

  private def assignDocument(document: Document, d: DocumentInfo): Boolean = {
    var dirty = false
    val number = d.number
    val projectId = projectWithName(d.projectName).id
    val title = d.title
    val access = d.access
    if (document.number != number) {
      document.number = number
      dirty = true
    }
    if (document.projectId != projectId) {
      document.projectId = projectId
      dirty = true
    }
    if (document.title != title) {
      document.title = title
      dirty = true
    }
    if (document.access != access) {
      document.access = access
      dirty = true
    }
    dirty
  }

  private def assignEditor(document: Document, d: DocumentInfo): Boolean = {
    if (d.editor != null && d.editor.length > 0) {
      UserLookup.lookup(Some(d.editor), None, None, directory, "editor on " + document + " with " + d) match {
        case Full(u) => {
          Pending.assignEditor(u, document, d.editorStart)
        }
        case _ => {
          logger.warn("Editor not resolved for '" + d.editor + "' on " + document)
          false
        }
      }
    }
    else {
      Pending.unassignEditor(document)
    }
  }

  def documentUpToDate(document: Document, d: DocumentInfo): Boolean = {
    if (net.liftweb.util.Props.devMode) false
    else {
      val weekAgo = T.daysAgo(7)
      val recentEditor = d.editor != null && d.editorStart != null && d.editorStart.after(weekAgo)
      val reconcileOutOfDate = document.reconciled == null || document.reconciled.before(weekAgo)
      document.latest_?(d.version) && !recentEditor && document.access.equalsIgnoreCase(d.access) && !reconcileOutOfDate
    }
  }
}
