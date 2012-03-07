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
import vvv.docreg.agent.{DaemonAgentComponent, Changed, DaemonProtocol}

case class Connect()
case class Reload(d: Document)
case class Reconcile(document: AgentDocument, revisions: List[AgentRevision], approvals: List[AgentApproval], subscriptions: List[AgentSubscriber])
case class ApprovalApproved(document: Document, revision: Revision, user: User, state: ApprovalState, comment: String)
case class ApprovalRequested(document: Document, revision: Revision, users: Iterable[User])
case class SubscribeRequested(document: Document, user: User)
case class UnsubscribeRequested(document: Document, user: User)
case class Edit(document: Document, user: User)
case class Unedit(document: Document, user: User)
case class Submit(document: Document, projectName: String, localFile: java.io.File, userFileName: String, comment: String, user: User)
case class SubmitNew(projectName: String, localFile: java.io.File, userFileName: String, comment: String, user: User)

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
          agent = createAgent("dr+w " + version, Backend.server, product, self)
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
        case Updated(d) =>
        {
          priorityReconciler ! Prepare(d, agent)
        }
        case Changed(d) =>
        {
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
          val done = agent.approval(r.filename, 
            user.displayName, 
            user.email.is,
            state match {
              case ApprovalState.approved => AgentApprovalState.Approved
              case ApprovalState.notApproved => AgentApprovalState.NotApproved
              case _ => AgentApprovalState.Pending
            },
            comment,
            product,
            user.shortUsername())
          if (done) logger.info("Approval processed") else logger.warn("Approval rejected for " + r + " by " + user + " to " + state)
        }
        case ApprovalRequested(d, r, users) =>
        {
          users foreach (this ! ApprovalApproved(d, r, _, ApprovalState.pending, ""))
        }
        case SubscribeRequested(d, user) =>
        {
          // todo subscribe needs to pass username
          if(agent.subscribe(d.latest.filename, user.email))
            logger.info(user + " has subscribed to " + d)
          else
            logger.warn("Subscribe request rejected for " + d + " by " + user)
        }
        case UnsubscribeRequested(d, user) =>
        {
          // todo subscribe needs to pass username
          if(agent.unsubscribe(d.latest.filename, user.email))
            logger.info(user + " has unsubscribed from " + d)
          else
            logger.warn("Unsubscribe request rejected for " + d + " by " + user)
        }
        case Edit(d, user) =>
        {
          agent.edit(d.latest.filename, user.shortUsername())
          logger.info("Edit request sent")
        }
        case Unedit(d, user) =>
        {
          try {
            agent.unedit(d.latest.filename, user.shortUsername())
          } catch {
            // An IOException means there was no reply from the sent request. This is what we want since there is no reply from an UNEDIT_RQST.
            case e: IOException => logger.info("Unedit request sent")
          }
        }
        case Submit(d, projectName, localFile, userFileName, comment, user) =>
        {
          // todo check revision is latest?
          try {
            agent.registerCopySubmit(localFile, d.nextFileName(userFileName), projectName, d.access.is, user.shortUsername(), user.host.is, comment)
          } catch {
            case a => logger.warn("Submit failed " + a)
            a.printStackTrace()
          }
        }
        /*
        case SubmitNew() => {
          //check document name doesn't have 0000-000 format.
          // register, and check reply message for document and revision
          //Register status: Accepted
          //Suggested file name: 6116-001-Testing document addition.txt
          // submit
          // success.
          //
        }
         */
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
