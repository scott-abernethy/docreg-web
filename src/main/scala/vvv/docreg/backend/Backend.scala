package vvv.docreg.backend

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.JavaConversions._
import vvv.docreg.model._
import vvv.docreg.model.ApprovalState._
import vvv.docreg.util._

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import java.io.IOException
import com.hstx.docregsx.{Document => AgentDocument, Revision => AgentRevision, Approval => AgentApproval, Subscriber => AgentSubscriber, ApprovalStatus => AgentApprovalState}
import vvv.docreg.db.DbVendor
import java.util.Date
import vvv.docreg.agent._
import org.squeryl.PrimitiveTypeMode._

case class Connect()
case class Reload(d: Document)
case class Reconcile(document: AgentDocument, revisions: List[AgentRevision], approvals: List[AgentApproval], subscriptions: List[AgentSubscriber])
case class ApprovalApproved(document: Document, revision: Revision, user: User, state: ApprovalState, comment: String)
case class ApprovalRequested(document: Document, revision: Revision, users: Iterable[User])
case class SubscribeRequested(document: Document, user: User)
case class UnsubscribeRequested(document: Document, user: User)
case class Edit(document: Document, user: User)
case class Unedit(document: Document, user: User)
case class Submit(document: Document, projectName: String, localFile: () => java.io.File, userFileName: String, comment: String, user: User)
case class Create(projectName: String, localFile: () => java.io.File, fileName: String, comment: String, user: User)

trait Backend extends Actor

trait BackendComponent {
  val backend: Backend
}

trait BackendComponentImpl extends BackendComponent
{
  this: DocumentServerComponent with AgentComponent with DaemonAgentComponent with DirectoryComponent =>

  val backend = new Backend with Loggable with RevisionReconcile with ApprovalReconcile
  {
  val product = ProjectProps.get("project.name") openOr "drw"
  val version = ProjectProps.get("project.version") openOr "0.0"
  val clientVersion = "dr+w " + version
  val target = Backend.server
  val reconciler = new Reconciler(this).start()
  val priorityReconciler = new Reconciler(this).start()
  var agent: vvv.docreg.backend.Agent = _
  val userLookup = new UserLookupProvider {
    def lookup(usernameOption: Option[String], emailOption: Option[String], nameOption: Option[String], why: String) = UserLookup.lookup(usernameOption, emailOption, nameOption, directory, why)
  }

    def act()
  {
    loop
    {
      react
      {
        case Connect() =>
        {
          logger.info("Starting " + product + " v" + version + " " + java.util.TimeZone.getDefault.getDisplayName)
          agent = createAgent(clientVersion, target, product, self)
        }
        case Loaded(d :: ds) => {
          Document.forKey(d.getKey) match {
            case Full(document) => {
              // reconcile if
              // 1. not latest version
              // 2. editor (recently)
              val v: Int = Integer.parseInt(d.getVersion)
              val recentEditor = d.getEditor != null && d.getEditorStart != null && d.getEditorStart.after(new Date(System.currentTimeMillis() - (1000 * 60 * 60 * 24 * 7)))
              if (!document.latest_?(v) || recentEditor) {
                reconciler ! Prepare(d, agent)
              }
            }
            case _ => {
              reconciler ! Prepare(d, agent)
            }
          }
          this ! Loaded(ds)
        }
        case Loaded(Nil) => {
          logger.debug("Parsing docreg.txt for changes to reconcile complete")
        }
        case Changed(d) =>
        {
          // Todo: Apply what we know of the change now, then reconcile. Though the reconcile typically takes <1 second.
          logger.debug("Change received, sending to reconcile " + d.key)
          priorityReconciler ! Prepare(DaemonProtocol.documentInfoToAgentDocument(d), agent)
        }
        case msg @ Reconcile(d, revisions, approvals, subscriptions) =>
        {
          logger.debug("Reconcile " + d.getKey)
          Document.forKey(d.getKey) match
          {
            case Full(document) => updateDocument(document, msg)
            case _ => createDocument(msg)
          }
        }
        case ApprovalApproved(d, r, user, state, comment) =>
        {
          daemonAgent ! RequestPackage(Actor.self, target,
            ApprovalRequest(
              r.filename,
              user.shortUsername(), // todo this was user.displayName?!
              user.email,
              state match {
                case ApprovalState.approved => AgentApprovalState.Approved.toString()
                case ApprovalState.notApproved => AgentApprovalState.NotApproved.toString()
                case _ => AgentApprovalState.Pending.toString()
              },
              comment,
              product, // todo is this consistent?
              user.shortUsername()
            ))
        }
        case ApprovalRequested(d, r, users) =>
        {
          users foreach (this ! ApprovalApproved(d, r, _, ApprovalState.pending, ""))
        }
        case ApprovalReply(response) =>
        {
          logger.info("Approval reply, " + response)
        }
        case SubscribeRequested(d, user) =>
        {
          daemonAgent ! RequestPackage(Actor.self, target, SubscribeRequest(d.latest.filename, user.shortUsername(), user.email, "always"))
        }
        case SubscribeReply(response, fileName, userName) =>
        {
          logger.info("Subscribe reply, " + List(response, fileName, userName))
        }
        case UnsubscribeRequested(d, user) =>
        {
          daemonAgent ! RequestPackage(Actor.self, target, UnsubscribeRequest(d.latest.filename, user.shortUsername(), user.email))
        }
        case UnsubscribeReply(response, fileName, userName) =>
        {
           logger.info("Unsubscribe reply, " + List(response, fileName, userName))
        }
        case Edit(d, user) =>
        {
          logger.info("Edit request, " + List(d.latest.filename, user.shortUsername()))
          daemonAgent ! RequestPackage(Actor.self, target, EditRequest(d.latest.filename, user.shortUsername()))
        }
        case EditReply(user) =>
        {
          logger.info("Edit reply, editor is " + user)
        }
        case Unedit(d, user) =>
        {
          logger.info("Unedit request, " + List(d.latest.filename, user.shortUsername()))
          daemonAgent ! RequestPackage(Actor.self, target, UneditRequest(d.latest.filename, user.shortUsername()))
        }
        case msg @ Submit(d, projectName, localFile, userFileName, comment, user) =>
        {
          // todo check revision is latest?
          val engine = new SubmitEngine(daemonAgent, target, user.host, clientVersion)
          engine.start()
          engine ! msg
        }
        case msg @ Create(projectName, localFile, userFileName, comment, user) =>
        {
          val engine = new SubmitNewEngine(daemonAgent, target, user.host, clientVersion)
          engine.start()
          engine ! msg
        }
        case 'Die =>
        {
          logger.info("Backend killed")
          exit()
        }
        case other =>
        {
          logger.warn("Unrecognised message " + other)
        }
      }
    }
  }

    override def exceptionHandler =
    {
      case e: Exception =>
      {
        logger.error("Backend exception " + e.getMessage, e)
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
      transaction {
      val document = new Document
      assignDocument(document, reconcile.document)
      Document.dbTable.insert(document)

      assignEditor(document, reconcile.document)

      val update = reconcileRevisions(document, reconcile.revisions.map(x => RevisionReconcile.agentToInfo(x)))
      if (!update.contains(ReconcileDocumentRemoved))
      {
        reconcileApprovals(document, reconcile.approvals.map(x => ApprovalReconcile.agentToInfo(x)))
        updateSubscriptions(document, reconcile.subscriptions)
        documentServer ! DocumentAdded(document)
      }
      }
    } catch {
      case e: java.lang.NullPointerException => logger.error("Exception " + e + " with " + reconcile.document.getKey); e.printStackTrace
    }
  }

  private def updateDocument(document: Document, reconcile: Reconcile) {
    transaction {
    val update = reconcileRevisions(document, reconcile.revisions.map(x => RevisionReconcile.agentToInfo(x)))
    if (update.contains(ReconcileDocumentRemoved))
    {
      return
    }
    updateSubscriptions(document, reconcile.subscriptions)
    reconcileApprovals(document, reconcile.approvals.map(x => ApprovalReconcile.agentToInfo(x)))
    
    val docChanged = assignDocument(document, reconcile.document)
    val editorChanged = assignEditor(document, reconcile.document)
    if (docChanged || editorChanged) {
      Document.dbTable.update(document)
    }
    update.collect{case ReconcileRevisionAdded(r) => r}.foreach{ id =>
      document.revision(id).foreach(revision => documentServer ! DocumentRevised(document, revision))
    }
    if (docChanged || editorChanged || update.contains(ReconcileRevisionUpdated)) {
      documentServer ! DocumentChanged(document)
    }
    }
  }

  private def assignDocument(document: Document, d: AgentDocument): Boolean = {
    document.number = d.getKey
    document.projectId = projectWithName(d.getProject).id
    document.title = d.getTitle
    document.access = d.getAccess
    true
//    document.dirty_?
    // todo check that revision based info here, such as access, is correct in the AgentDocument object.
  }

  private def assignEditor(document: Document, d: AgentDocument): Boolean = {
      if (d.getEditor != null && d.getEditor.length > 0) {
        UserLookup.lookup(Some(d.getEditor()), None, None, directory, "editor on " + document + " with " + d) match {
          case Full(u) => {
            Pending.assignEditor(u, document, d.getEditorStart)
          }
          case _ => {
            logger.warn("Editor not resolved for '" + d.getEditor + "' on " + document)
            false
          }
        }
      }
      else {
        Pending.unassignEditor(document)
      }
  }

  def updateSubscriptions(document: Document, subscriptions: List[AgentSubscriber])
  {
    val subscribers: List[User] = for {
      s <- subscriptions
      u <- UserLookup.lookup(Some(s.getSubscriberUserName), Some(s.getSubscriberEmail), None, directory, "subscription on " + document + " for " + s)
    } yield u

    val listed = subscribers.distinct.toSet
    val existing = Subscription.usersFor(document).toSet
    val remove = existing.diff(listed)
    val create = listed.diff(existing)

    for (u <- create)
    {
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
}

object Backend {
  val server: String = Props.get("backend.server") openOr "shelob" // shelob.gnet.global.vpn?
}
