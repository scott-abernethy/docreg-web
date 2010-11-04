package vvv.docreg.backend

import com.hstx.docregsx.{Document => AgentDocument, Revision => AgentRevision, Approval => AgentApproval, ApprovalStatus => AgentApprovalState, _}
import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.JavaConversions._
import vvv.docreg.model._
import vvv.docreg.model.ApprovalState._
import vvv.docreg.util._

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._

case class Connect()
case class Updated(d: AgentDocument)
case class Reload(d: Document)
case class ApprovalApproved(document: Document, revision: Revision, user: User, state: ApprovalState, comment: String)

class Backend extends Actor with Logger {
  val product = ProjectProps.get("project.name") openOr "drw"
  val version = ProjectProps.get("project.version") openOr "0.0"
  val reconciler = new Reconciler(this)
  var agent: Agent = _
  def act() {
    loop {
      react {
        case Connect() => 
          info("Starting " + product + " v" + version + " " + java.util.TimeZone.getDefault.getDisplayName)
          agent = new Agent(version, Backend.server, product)
          val library = new FileList(Backend.server, agent)
          library.addUpdateListener(new UpdateListener() {
            def updated(ds: java.util.List[AgentDocument]) = ds.foreach(Backend.this ! Updated(_))
            def updated(d: AgentDocument) = Backend.this ! Updated(d)
          })
        case Updated(d) => 
          Document.forKey(d.getKey) match {
            case Full(document) => updateDocument(document, d)
            case _ => createDocument(d)
          }
        case Reload(d) =>
          updateRevisions(d)
          updateApprovals(d)
        case ApprovalApproved(d, r, user, state, comment) =>
          val done = agent.approval(r.filename, 
            user.displayName, 
            user.email.is,
            state match {
              case ApprovalState.approved => AgentApprovalState.Approved
              case _ => AgentApprovalState.NotApproved
            },
            comment,
            product,
            user.email.is)
        case _ => println("?")
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

  private def createDocument(d: AgentDocument) {
    try {
      val document = Document.create
      assignDocument(document, d)
      document.save

      agent.loadRevisions(d).foreach{createRevision(document, _)}
      agent.loadApprovals(d).foreach{createApproval(document, _)}
      
      DocumentServer ! DocumentAdded(document)
    } catch {
      case e: java.lang.NullPointerException => println("Exception " + e + " with " + d.getKey); e.printStackTrace
    }
  }

  private def createRevision(document: Document, r: AgentRevision): Revision = {
    val revision = Revision.create
    revision.document(document)
    assignRevision(revision, r)
    revision.save
    revision
  }
  
  private def createApproval(document: Document, a: AgentApproval) {
    Revision.forDocument(document, a.getVersion) match {
      case Full(revision) => createApproval(revision, User.forEmailOrCreate(a.getApproverEmail) openOr null, a)
      case _ => warn("Approval found with no matching revision: " + a)
    }
  }

  private def createApproval(revision: Revision, user: User, a: AgentApproval): Approval = {
    val approval = Approval.create
    approval.revision(revision)
    approval.by(user)
    assignApproval(approval, a)
    approval.save
    approval
  }
  
  private def updateDocument(document: Document, d: AgentDocument) {
    if (document.latest_?(d.getVersion.toLong)) {
      reconciler ! PriorityReconcile(document)
    } else {
      updateRevisions(document)
    }
    
    assignDocument(document, d)
    if (document.dirty_?) { 
      document.save
      DocumentServer ! DocumentChanged(document)
    }
  }

  private def assignDocument(document: Document, d: AgentDocument) {
    document.key(d.getKey)
    document.project(projectWithName(d.getProject))
    document.title(d.getTitle)
    document.editor(d.getEditor)
  }

  private def assignRevision(revision: Revision, r: AgentRevision) {
    revision.version(r.getVersion)
    revision.filename(r.getFilename)
    revision.author(r.getAuthor)
    revision.date(r.getDate)
    revision.comment(r.getComment)
  }
  
  private def assignApproval(approval: Approval, a: AgentApproval) {
    approval.state(ApprovalState.parse(a.getStatus.toString))
    approval.date(a.getDate)
    approval.comment(a.getComment)
  }

  private def updateRevisions(document: Document) {
    agent.loadRevisions(document.key).foreach { r =>
      document.revision(r.getVersion) match {
        case Full(revision) =>
          assignRevision(revision, r)
          if (revision.dirty_?) {
            revision.save
            DocumentServer ! DocumentChanged(document)
          }
        case _ => 
          val latest = createRevision(document, r)
          DocumentServer ! DocumentRevised(document, latest)
      }
    }
  }

  private def updateApprovals(document: Document) {
    agent.loadApprovals(document.key).foreach { a =>
      Revision.forDocument(document, a.getVersion) match {
        case Full(revision) =>
          val user = User.forEmailOrCreate(a.getApproverEmail)
          val state = ApprovalState.parse(a.getStatus.toString)
          val approval = Approval.findAll(By(Approval.revision, revision), By(Approval.by, user), By(Approval.state, state)) match {
            case Nil => Empty
            case x :: Nil => Full(x)
            case x :: xs => warn("Found duplicate approvals for " + a); Full(x)
          } 
          approval match {
            case Full(a) => //update
            case _ => createApproval(revision, user openOr null, a)

          }
        case _ => warn("Approval found with no matching revision: " + a)
      } 
    }
  }
}

object Backend extends Backend {
  val server: String = Props.get("backend.server") openOr "shelob" // shelob.gnet.global.vpn?
}
