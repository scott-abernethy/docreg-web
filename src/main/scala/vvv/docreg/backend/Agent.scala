package vvv.docreg.backend

import actors.Actor
import com.hstx.docregsx.{Document, UpdateListener, FileList}
import scala.collection.JavaConversions._
import java.io.File
import vvv.docreg.agent.ChangePoller

case class Loaded(ds: List[Document])
case class Updated(d: Document)

trait Agent {
//    private void notifyMessageListener(com.hstx.docregsx.SendMessage sendMessage, com.hstx.docregsx.ReceiveMessage receiveMessage) { /* compiled code */ }
//
//    public java.lang.String edit(com.hstx.docregsx.Document document, java.lang.String s) throws java.io.IOException { /* compiled code */ }
//
    def edit(s: String, s1: String): String
//
//    public java.lang.String register(com.hstx.docregsx.Document document, java.lang.String s) throws java.io.IOException { /* compiled code */ }
//
//    public java.lang.String submit(com.hstx.docregsx.Document document) throws java.io.IOException { /* compiled code */ }
//
//    public void unedit(com.hstx.docregsx.Document document, java.lang.String s) throws java.io.IOException { /* compiled code */ }
//
    def unedit(s: String, s1: String): Unit

    def subscribe(s: String, s1: String): Boolean

    def unsubscribe(s: String, s1: String): Boolean

//    public boolean approval(String filename, String approverName, String approverEmail, ApprovalStatus status, String comment, String clientHost, String clientUsername)

    def approval(filename: String, approverName: String, approverEmail: String, approvalStatus: com.hstx.docregsx.ApprovalStatus, comment: String, clientHost: String, clientUsername: String): Boolean

//    public com.hstx.docregsx.ChangeReply getNextChangeRequest(int i) throws java.io.IOException { /* compiled code */ }
//
    def loadRevisions(document: com.hstx.docregsx.Document): java.util.List[com.hstx.docregsx.Revision]
    def loadRevisions(s: String): java.util.List[com.hstx.docregsx.Revision]
//
    def loadApprovals(document: com.hstx.docregsx.Document): java.util.List[com.hstx.docregsx.Approval]

    def loadApprovals(s: String): java.util.List[com.hstx.docregsx.Approval]

    def loadSubscribers(document: com.hstx.docregsx.Document): java.util.List[com.hstx.docregsx.Subscriber]

    def loadSubscribers(s: String): java.util.List[com.hstx.docregsx.Subscriber]

//    private void addToSendQueue(com.hstx.docregsx.SendMessage sendMessage) { /* compiled code */ }
//
//    private void processResponse(com.hstx.docregsx.Message message) { /* compiled code */ }
//
//    private com.hstx.docregsx.ReceiveMessage blockingSend(com.hstx.docregsx.SendMessage sendMessage) throws java.io.IOException { /* compiled code */ }

    def registerCopySubmit(file: File, fileName: String, project: String, access: String, user: String, clientHost: String, comment: String)
}

trait AgentComponent {
  def createAgent(version: String, server: String, user: String, backend: Actor): Agent
}

class WrappedAgent(version: String, val server: String, user: String, val backend: Actor) extends com.hstx.docregsx.Agent(version, server, user) with Agent
{
  val updateListener = new UpdateListener()
  {
    def updated(ds: java.util.List[com.hstx.docregsx.Document])
    {
      backend ! Loaded(ds.toList)
    }
    def updated(d: com.hstx.docregsx.Document)
    {
      // Do nothing, as change poller takes care of this now.
    }
  }
  new ChangePoller(server, backend).start() ! 'Reset
  val library = new FileList(server, this)
  library.addUpdateListener(updateListener)
}

trait AgentComponentImpl extends AgentComponent {
  def createAgent(version: String, server: String, user: String, backend: Actor) = new WrappedAgent(version, server, user, backend)
}