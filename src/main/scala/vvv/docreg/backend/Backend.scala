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

case class Connect()
case class Reload(d: Document)
case class Reconcile(document: AgentDocument, revisions: List[AgentRevision], approvals: List[AgentApproval], subscriptions: List[AgentSubscriber])
case class ApprovalApproved(document: Document, revision: Revision, user: User, state: ApprovalState, comment: String)
case class ApprovalRequested(document: Document, revision: Revision, users: Iterable[User])
case class SubscribeRequested(document: Document, user: User)
case class UnsubscribeRequested(document: Document, user: User)
case class Edit(document: Document, user: User)
case class Unedit(document: Document, user: User)
case class Submit(document: Document, localFile: java.io.File, userFileName: String, comment: String, user: User)

trait Backend extends Actor

trait BackendComponent {
  val backend: Backend
}

trait BackendComponentImpl extends BackendComponent {
  this: DocumentServerComponent with AgentComponent with DirectoryComponent =>
  val backend = new Backend with Loggable {

  val product = ProjectProps.get("project.name") openOr "drw"
  val version = ProjectProps.get("project.version") openOr "0.0"
  val reconciler = new Reconciler(this).start()
  val priorityReconciler = new Reconciler(this).start()
  var agent: vvv.docreg.backend.Agent = _

  def act() {
    loop {
      react {
        case Connect() => 
          logger.info("Starting " + product + " v" + version + " " + java.util.TimeZone.getDefault.getDisplayName)
          agent = createAgent("dr+w " + version, Backend.server, product, self)

        case Loaded(ds) =>
          ds.foreach(reconciler ! Prepare(_, agent))

        case Updated(d) =>
          priorityReconciler ! Prepare(d, agent)

        case msg @ Reconcile(d, revisions, approvals, subscriptions) =>
          Document.forKey(d.getKey) match {
            case Full(document) => updateDocument(document, msg)
            case _ => createDocument(msg)
          }

        case ApprovalApproved(d, r, user, state, comment) =>
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
            user.username.is)
          if (done) logger.info("Approval processed") else logger.warn("Approval rejected for " + r + " by " + user + " to " + state)

        case ApprovalRequested(d, r, users) =>
          users foreach (this ! ApprovalApproved(d, r, _, ApprovalState.pending, ""))

        case SubscribeRequested(d, user) =>
          // todo subscribe needs to pass username
          if(agent.subscribe(d.latest.filename, user.email))
            logger.info(user + " has subscribed to " + d)
          else
            logger.warn("Subscribe request rejected for " + d + " by " + user)

        case UnsubscribeRequested(d, user) =>
          // todo subscribe needs to pass username
          if(agent.unsubscribe(d.latest.filename, user.email))
            logger.info(user + " has unsubscribed from " + d)
          else
            logger.warn("Unsubscribe request rejected for " + d + " by " + user)

        case Edit(d, user) =>
          // todo should this be username?
          agent.edit(d.latest.filename, user.displayName)
          logger.info("Edit request sent")

        case Unedit(d, user) =>
          // todo should this be username?
          try {
            agent.unedit(d.latest.filename, user.displayName)
          } catch { /* An IOException means there was no reply from the sent request.
                       This is what we want since there is no reply from an UNEDIT_RQST. */
            case e: IOException => logger.info("Unedit request sent")
          }

        case Submit(d, localFile, userFileName, comment, user) =>
          // todo check revision is latest?
          try {
            agent.registerCopySubmit(localFile, d.nextFileName(userFileName), d.projectName, d.access.is, user.username, user.host.is, comment)
          } catch {
            case a => logger.warn("Submit failed " + a)
            a.printStackTrace()
          }

        case m @ _ => logger.warn("Unrecognised message " + m)
      }
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

      reconcile.revisions.foreach{createRevision(document, _)}
      applyApprovals(document, reconcile.approvals)
      reconcile.subscriptions.foreach{createSubscription(document, _)}
      
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
        val user = UserLookup.lookup(Some(a.getUsername()), Some(a.getApproverEmail), Some(a.getApproverName), directory) openOr null
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

  private def createSubscription(document: Document, s: AgentSubscriber): Subscription = {
    val subscription = Subscription.create
    subscription.document(document)
    assignSubscription(subscription, s)
    subscription.save
    subscription
  }

  private def updateDocument(document: Document, reconcile: Reconcile) {
    DB.use(DefaultConnectionIdentifier) { c =>
    updateRevisions(document, reconcile.revisions)
    updateSubscriptions(document, reconcile.subscriptions)
    applyApprovals(document, reconcile.approvals)
    
    assignDocument(document, reconcile.document)
    if (document.dirty_?) {
      document.save
      documentServer ! DocumentChanged(document)
    }
    }
  }

  private def assignDocument(document: Document, d: AgentDocument) {
    document.key(d.getKey)
    document.project(projectWithName(d.getProject))
    document.title(d.getTitle)
    document.editor(d.getEditor)
    document.access(d.getAccess)
    // todo check that revision based info here, such as access, is correct in the AgentDocument object.
  }

  private def assignRevision(revision: Revision, r: AgentRevision) {
    val author = UserLookup.lookup(Some(r.getUsername), None, Some(r.getAuthor), directory)
    revision.version(r.getVersion)
    revision.filename(r.getFilename)
    revision.author(author)
    revision.date(r.getDate)
    revision.comment(r.getComment)
  }

  private def assignSubscription(subscription: Subscription, s: AgentSubscriber) {
    val emailOption: Option[String] = Option(s.getSubscriberEmail).flatMap(e => UserMigration.migrateEmail(e.toLowerCase))

    UserLookup.lookup(Some(s.getSubscriberUserName()), emailOption, None, directory) match {
      case Full(u) => subscription.user(u)
      case _ => logger.error("Failed to assign subscription for " + s)
    }
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

  private def updateSubscriptions(document: Document, subscriptions: List[AgentSubscriber]) {
    subscriptions.foreach { s =>
      val subscriber = for {
        u <- UserLookup.lookup(Some(s.getSubscriberUserName), Some(s.getSubscriberEmail), None, directory)
        s <- document.subscriber(u)
      } yield s 
      subscriber match {
        case Full(subscription) =>
          assignSubscription(subscription, s)
          if (subscription.dirty_?) {
            subscription.save
          }
        case _ =>
          val latest = createSubscription(document, s)
      }
    }
  }

}
}

object Backend {
  val server: String = Props.get("backend.server") openOr "shelob" // shelob.gnet.global.vpn?
}
