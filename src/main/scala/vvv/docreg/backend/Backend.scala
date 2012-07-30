package vvv.docreg.backend

import vvv.docreg.model.ApprovalState._
import vvv.docreg.util._

import _root_.net.liftweb.common._
import java.util.Date
import vvv.docreg.agent._
import org.squeryl.PrimitiveTypeMode._
import vvv.docreg.model._
import akka.actor._

case class Connect()

case class Reload(d: Document)

case class Loaded(ds: List[DocumentInfo])

case class Reconcile(document: DocumentInfo, revisions: List[RevisionInfo], approvals: List[ApprovalInfo], subscriptions: List[SubscriberInfo])

case class ApprovalApproved(document: Document, revision: Revision, user: User, state: ApprovalState, comment: String, actionedBy: User)

case class ApprovalRequested(document: Document, revision: Revision, users: Iterable[User], actionedBy: User)

case class SubscribeRequested(document: Document, user: User)

case class UnsubscribeRequested(document: Document, user: User)

case class Edit(document: Document, user: User)

case class Unedit(document: Document, user: User)

case class Submit(document: Document, projectName: String, localFile: () => java.io.File, userFileName: String, comment: String, user: User)

case class Create(projectName: String, localFile: () => java.io.File, fileName: String, comment: String, user: User)

trait BackendComponent {
  val backend: ActorRef
}

class Backend(directory: Directory, daemonAgent: ActorRef, documentServer: scala.actors.Actor) extends Actor with Loggable with RevisionReconcile with ApprovalReconcile {
  val product = ProjectProps.get("project.name") openOr "drw"
  val version = ProjectProps.get("project.version") openOr "0.0"
  val clientVersion = "dr+w " + version
  val target = AgentVendor.server
  val reconciler = context.actorOf(Props(new Reconciler(self)), "Reconciler")
  val priorityReconciler = context.actorOf(Props(new Reconciler(self)), "PriorityReconciler")
  val userLookup = new UserLookupProvider {
    def lookup(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], why: String) = UserLookup.lookup(usernameOption, emailOption, nameOption, directory, why)
  }

  val fileDatabase = context.actorOf(Props[FileDatabase], name = "FileDatabase")

  protected def receive = {
    case Connect() => {
      logger.info("Starting " + product + " v" + version + " " + java.util.TimeZone.getDefault.getDisplayName)
      logger.info("Connecting to " + target)
      fileDatabase ! GetRegister
    }
    case ResponseRegister(ds) => {
      logger.debug("Parsing register for changes to reconcile, started...")
      self ! Loaded(ds)
    }
    case ResponseFailure(GetRegister) => {
      logger.warn("Failed to load register");
    }
    case Loaded(d :: ds) => {
      Document.forKey(d.getKey) match {
        case Full(document) => {
          // reconcile if
          // 1. not latest version
          // 2. editor (recently)
          val recentEditor = d.editor != null && d.editorStart != null && d.editorStart.after(new Date(System.currentTimeMillis() - (1000 * 60 * 60 * 24 * 7)))
          if (!document.latest_?(d.version) || recentEditor) {
            reconciler ! Prepare(d, fileDatabase)
          }
        }
        case _ => {
          reconciler ! Prepare(d, fileDatabase)
        }
      }
      self ! Loaded(ds)
    }
    case Loaded(Nil) => {
      logger.debug("Parsing register for changes to reconcile, complete.")
    }
        case Changed(d) => {
          // Todo: Apply what we know of the change now, then reconcile. Though the reconcile typically takes <1 second.
          logger.debug("Change received, sending to reconcile " + d.getKey)
          priorityReconciler ! Prepare(d, fileDatabase)
        }
        case Some(msg @ Reconcile(d, revisions, approvals, subscriptions)) => {
          logger.debug("Reconcile " + d.getKey() + " : " + (revisions.size, approvals.size, subscriptions.size))
          Document.forKey(d.getKey) match {
            case Full(document) => updateDocument(document, msg)
            case _ => createDocument(msg)
          }
        }
        case ApprovalApproved(d, r, user, state, comment, actionedBy) => {
          daemonAgent ! RequestPackage(self, target,
            ApprovalRequest(
              r.filename,
              user.shortUsername(), // todo this was user.displayName?!
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
          users foreach (self ! ApprovalApproved(d, r, _, ApprovalState.pending, "", actionedBy))
        }
        case ApprovalReply(response) => {
          logger.info("Approval reply, " + response)
        }
        case SubscribeRequested(d, user) => {
          daemonAgent ! RequestPackage(self, target, SubscribeRequest(d.latest.filename, user.shortUsername(), user.email, "always"))
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
          // TODO Kill off reconciler?
          self ! PoisonPill
        }
        case other => {
          logger.warn("Unrecognised message " + other)
        }
  }

  // TODO akka version of this!
//  override def exceptionHandler = {
//    case e: Exception => {
//      logger.error("Backend exception " + e.getMessage, e)
//    }
//  }

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
      transaction {
        val document = new Document
        assignDocument(document, reconcile.document)
        Document.dbTable.insert(document)

        assignEditor(document, reconcile.document)

        val update = reconcileRevisions(document, reconcile.revisions)
        if (!update.contains(ReconcileDocumentRemoved)) {
          reconcileApprovals(document, reconcile.approvals)
          updateSubscriptions(document, reconcile.subscriptions)
          documentServer ! DocumentAdded(document)
        }
      }
    } catch {
      case e: java.lang.NullPointerException => logger.error("Exception " + e + " with " + reconcile.document.getKey()); e.printStackTrace
    }
  }

  private def updateDocument(document: Document, reconcile: Reconcile) {
    transaction {
      val update = reconcileRevisions(document, reconcile.revisions)
      if (update.contains(ReconcileDocumentRemoved)) {
        return
      }
      updateSubscriptions(document, reconcile.subscriptions)
      reconcileApprovals(document, reconcile.approvals)

      val docChanged = assignDocument(document, reconcile.document)
      val editorChanged = assignEditor(document, reconcile.document)
      if (docChanged) {
        Document.dbTable.update(document)
      }
      update.collect {
        case ReconcileRevisionAdded(r) => r
      }.foreach {
        id =>
          document.revision(id).foreach(revision => documentServer ! DocumentRevised(document, revision))
      }
      if (docChanged || editorChanged || update.contains(ReconcileRevisionUpdated)) {
        documentServer ! DocumentChanged(document)
      }
    }
  }

  private def assignDocument(document: Document, d: DocumentInfo): Boolean = {
    document.number = d.getKey()
    document.projectId = projectWithName(d.projectName).id
    document.title = d.title
    document.access = d.access
    true
//    document.dirty_?
    // todo check that revision based info here, such as access, is correct in the AgentDocument object.
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

  def updateSubscriptions(document: Document, subscriptions: List[SubscriberInfo])
  {
    // TODO
    val subscribers: List[User] = for {
      s <- subscriptions
      u <- UserLookup.lookup(Some(s.userName), Some(s.email), None, directory, "subscription on " + document + " for " + s)
    } yield u

    val listed = subscribers.distinct.toSet
    val existing = Subscription.usersFor(document).toSet
    val remove = existing.diff(listed)
    val create = listed.diff(existing)

    for (u <- create)
    {
      // TODO
      val subscription = new Subscription
      subscription.documentId = document.id
      subscription.userId = u.id
      Subscription.dbTable.insert(subscription)
    }

    if (remove.size > 0) {
      Subscription.dbTable.deleteWhere(s => (s.documentId === document.id) and (s.userId in remove.map(_.id)))
    }
  }
}
