package vvv.docreg.backend

import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.JavaConversions._
import vvv.docreg.model._
import vvv.docreg.model.ApprovalState._
import vvv.docreg.util._

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import java.io.IOException
import com.hstx.docregsx.{Document => AgentDocument, Revision => AgentRevision, Approval => AgentApproval, Subscriber => AgentSubscriber, ApprovalStatus => AgentApprovalState}
import vvv.docreg.db.DbVendor
import java.util.Date
import vvv.docreg.agent._

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

  val backend = new Backend with Loggable
  {
  val product = ProjectProps.get("project.name") openOr "drw"
  val version = ProjectProps.get("project.version") openOr "0.0"
  val clientVersion = "dr+w " + version
  val target = Backend.server
  val reconciler = new Reconciler(this).start()
  val priorityReconciler = new Reconciler(this).start()
  var agent: vvv.docreg.backend.Agent = _

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
          logger.info("Parsing docreg.txt for changes to reconcile complete")
        }
        case Changed(d) =>
        {
          // Todo: Apply what we know of the change now, then reconcile. Though the reconcile typically takes <1 second.
          logger.info("Change received, sending to reconcile " + d.key)
          priorityReconciler ! Prepare(DaemonProtocol.documentInfoToAgentDocument(d), agent)
        }
        case msg @ Reconcile(d, revisions, approvals, subscriptions) =>
        {
          logger.info("Reconcile " + d.getKey)
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
              user.email.is,
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
          daemonAgent ! RequestPackage(Actor.self, target, SubscribeRequest(d.latest.filename, user.shortUsername(), user.email.is, "always"))
        }
        case SubscribeReply(response, fileName, userName) =>
        {
          logger.info("Subscribe reply, " + List(response, fileName, userName))
        }
        case UnsubscribeRequested(d, user) =>
        {
          daemonAgent ! RequestPackage(Actor.self, target, UnsubscribeRequest(d.latest.filename, user.shortUsername(), user.email.is))
        }
        case UnsubscribeReply(response, fileName, userName) =>
        {
           logger.info("Unsubscribe reply, " + List(response, fileName, userName))
        }
        case Edit(d, user) =>
        {
          daemonAgent ! RequestPackage(Actor.self, target, EditRequest(d.latest.filename, user.shortUsername()))
        }
        case EditReply(user) =>
        {
          logger.info("Edit reply, editor is " + user)
        }
        case Unedit(d, user) =>
        {
          daemonAgent ! RequestPackage(Actor.self, target, UneditRequest(d.latest.filename, user.shortUsername()))
        }
        case msg @ Submit(d, projectName, localFile, userFileName, comment, user) =>
        {
          // todo check revision is latest?
          val engine = new SubmitEngine(daemonAgent, target, user.host.is, clientVersion)
          engine.start()
          engine ! msg
        }
        case msg @ Create(projectName, localFile, userFileName, comment, user) =>
        {
          val engine = new SubmitNewEngine(daemonAgent, target, user.host.is, clientVersion)
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

    private def projectWithName(name: String) = {
    val existing = Project.forName(name) 
    if (existing == null) {
      val project = Project.create
      project.name(name)
      project.save
      project
      // TODO notify new project? Or do it as notification on save.
    } else {
      existing
    }
  }

  private def createDocument(reconcile: Reconcile) {
    try {
      DB.use(DefaultConnectionIdentifier) { c =>
      val document = Document.create
      assignDocument(document, reconcile.document)
      document.save

      assignEditor(document, reconcile.document)

      reconcile.revisions.foreach{createRevision(document, _)}
      applyApprovals(document, reconcile.approvals)
      updateSubscriptions(document, reconcile.subscriptions)
      
      documentServer ! DocumentAdded(document)
      }
    } catch {
      case e: java.lang.NullPointerException => logger.error("Exception " + e + " with " + reconcile.document.getKey); e.printStackTrace
    }
  }

  private def createRevision(document: Document, r: AgentRevision): Revision = {
    val revision = Revision.create
    revision.document(document)
    assignRevision(revision, r)
    revision.save
    revision
  }

  private def applyApprovals(document: Document, approvals: Iterable[AgentApproval]) = approvals foreach { a =>
    // The agent returns a single approval item per revision/user pair, so no need to cull.
    Revision.forDocument(document, a.getVersion) match {
      case Full(revision) =>
        val user = UserLookup.lookup(Some(a.getUsername()), Some(a.getApproverEmail), Some(a.getApproverName), directory, "approval " + a + " on " + document) openOr null
        val approval = Approval.forRevisionBy(revision, user) match {
          case Full(a) => a
          case _ => Approval.create.revision(revision).by(user)
        } 
        approval.state(ApprovalState.parse(a.getStatus.toString))
        approval.date(a.getDate)
        approval.comment(a.getComment)
        approval.save
      case _ => 
        logger.warn("Approval found with no matching revision: " + a)
    }
  }

  private def updateDocument(document: Document, reconcile: Reconcile) {
    DB.use(DefaultConnectionIdentifier) { c =>
    updateRevisions(document, reconcile.revisions)
    updateSubscriptions(document, reconcile.subscriptions)
    applyApprovals(document, reconcile.approvals)
    
    val docChanged = assignDocument(document, reconcile.document)
    val editorChanged = assignEditor(document, reconcile.document)
    if (docChanged || editorChanged) {
      document.save
      documentServer ! DocumentChanged(document)
    }
    }
  }

  private def assignDocument(document: Document, d: AgentDocument): Boolean = {
    document.key(d.getKey)
    document.project(projectWithName(d.getProject))
    document.title(d.getTitle)
    document.access(d.getAccess)
    document.dirty_?
    // todo check that revision based info here, such as access, is correct in the AgentDocument object.
  }

  private def assignEditor(document: Document, d: AgentDocument): Boolean = {
    DB.use(DefaultConnectionIdentifier) { c =>
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
  }

  private def assignRevision(revision: Revision, r: AgentRevision) {
    val author = UserLookup.lookup(Some(r.getUsername), None, Some(r.getAuthor), directory, "revision on " + revision + " for " + r)
    revision.version(r.getVersion)
    revision.filename(r.getFilename)
    revision.author(author)
    revision.date(r.getDate)
    revision.comment(r.getComment)
  }

  private def updateRevisions(document: Document, revisions: List[AgentRevision]) {
    revisions.foreach { r =>
      document.revision(r.getVersion) match {
        case Full(revision) =>
          assignRevision(revision, r)
          if (revision.dirty_?) {
            revision.save
            documentServer ! DocumentChanged(document)
          }
        case _ => 
          val latest = createRevision(document, r)
          documentServer ! DocumentRevised(document, latest)
      }
    }
  }

  def updateSubscriptions(document: Document, subscriptions: List[AgentSubscriber])
  {
    val subscribers: List[User] = for {
      s <- subscriptions
      u <- UserLookup.lookup(Some(s.getSubscriberUserName), Some(s.getSubscriberEmail), None, directory, "subscription on " + document + " for " + s)
    } yield u

    val listed = subscribers.distinct.toSet
    val existing = document.subscribers.toSet
    val remove = existing.diff(listed)
    val create = listed.diff(existing)

    for (u <- create)
    {
      val subscription = Subscription.create
      subscription.document(document)
      subscription.user(u)
      subscription.save
    }

    for (u <- remove; s <- Subscription.find(By(Subscription.document, document), By(Subscription.user, u)))
    {
      s.delete_!
    }
  }

}
}

object Backend {
  val server: String = Props.get("backend.server") openOr "shelob" // shelob.gnet.global.vpn?
}
