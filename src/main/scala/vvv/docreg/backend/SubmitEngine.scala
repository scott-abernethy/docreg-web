package vvv.docreg.backend

import vvv.docreg.agent._
import net.liftweb.common.Loggable
import java.io.File
import com.hstx.docregsx.ScpClient
import vvv.docreg.model.Document.ValidDocumentFileName
import akka.actor.{PoisonPill, Actor, ActorRef}

class SubmitEngine(agent: ActorRef, target: String, clientHost: String, clientVersion: String) extends Actor with Loggable
{
  var cachedRequest: Submit = null


  protected def receive = {
        case msg @ Submit(document, projectName, localFile, userFileName, comment, user) =>
        {
          // todo check revision is latest?
          // todo pass in new user title rather than setting d's title. similar to Create below
          // todo check the fields, including comment which should default to "[no description]"? Check default for approval etc also.
          cachedRequest = msg
          val request = RegisterRequest(userFileName, projectName, if (comment.length() < 1) "[no description]" else comment, document.access, user.displayName, user.shortUsername(), clientHost, clientVersion)
          logger.info("SubmitEngine register " + request)
          agent ! RequestPackage(self, target, request)
        }
        case RegisterReply(response, suggestedFileName) if (response.startsWith("Rejected")) =>
        {
          (suggestedFileName, cachedRequest) match
          {
            case (ValidDocumentFileName(suggestedKey, _, suggestedSuffix), Submit(document, projectName, localFile, ValidDocumentFileName(key, _, suffix), comment, user)) if (suggestedKey == key && suggestedSuffix == suffix) =>
            {
              logger.warn("SubmitEngine rejected, so RETRY register using " + suggestedFileName)
              self ! Submit(document, projectName, localFile, suggestedFileName, comment, user)
            }
            case _ =>
            {
              logger.warn("SubmitEngine rejected, would have accepted " + suggestedFileName + " but that doesn't make sense.")
              self ! 'Die
            }
          }
        }
        case RegisterReply(response, suggestedFileName) =>
        {
          // todo reply package could include request, thus no need to store it.

          // Sending multiple times with the same file name gives the same doc key suggestion.
          // "Accepted - file name already contained correct version
          if (response != null && response.startsWith("Accepted"))
          {
            logger.info("Register reply " + response + " with suggested filename of " + suggestedFileName)
              // Submit with suggested file name assuming, as long as it is version 001 and has same fileName part.
            // todo for edit submit, filename should be same.
            val submittedFileName = suggestedFileName
            var scpClient = new ScpClient(target)
            logger.debug("Copying file")
            scpClient.copy(cachedRequest.localFile.apply().toString(), submittedFileName);
            logger.debug("Copying file, done")
            // todo check file size
            agent ! RequestPackage(self, target, SubmitRequest(submittedFileName, -1))
            // todo delete local file?
          }
          else
          {
            logger.warn("Failed to register new document " + response + " -> " + suggestedFileName)
            // todo warn user!
          }
        }
        case SubmitReply(response, suggestedFileName) =>
        {
          logger.info("Submit reply " + response + " with suggested filename of " + suggestedFileName)
          // todo, notify user
          self ! 'Die
        }
        case 'Die =>
        {
          self ! PoisonPill
        }
        case other =>
        {
          logger.warn("Submit engine got unexpected " + other)
        }
        // todo timeout
  }

  // TODO actor equivalent
//  override def exceptionHandler =
//  {
//    case e: Exception =>
//    {
//      logger.error("SubmitEngine exception " + e.getMessage, e)
//      self ! 'Die
//    }
//  }
}

//SubmitEngine register RegisterRequest(0021-439-Document Register Test - SC.doc,DocReg,Fix title, submit via new async protocol client.,Everyone,Scott Abernethy,sabernethy,0:0:0:0:0:0:0:1,dr+w 0.7.0)
//Protocol downstream Header(3,registerRequest,17,1)
//Upstream message received Header(3,registerReply,17,1) and decoded as ReplyPackage(Header(3,registerReply,17,1),RegisterReply(Accepted - file name already contained correct version,0021-439-Document Register Test - SC.doc))
//[INFO] Register reply Accepted - file name already contained correct version with suggested filename of 0021-439-Document Register Test - SC.doc
//[INFO] Copying file
//
//
//vvv.docreg.backend.SubmitEngine@7d02c3c6: caught java.io.IOException: Error during SCP transfer.
//java.io.IOException: Error during SCP transfer.
//	at ch.ethz.ssh2.SCPClient.put(SCPClient.java:575)
//	at ch.ethz.ssh2.SCPClient.put(SCPClient.java:448)
//	at com.hstx.docregsx.ScpClient.copy(ScpClient.java:58)
//	at vvv.docreg.backend.SubmitEngine$$anonfun$act$1$$anonfun$apply$1.apply(SubmitEngine.scala:43)
//	at vvv.docreg.backend.SubmitEngine$$anonfun$act$1$$anonfun$apply$1.apply(SubmitEngine.scala:18)
//	at scala.actors.ReactorTask.run(ReactorTask.scala:31)
//	at scala.actors.Reactor$class.resumeReceiver(Reactor.scala:129)
//	at vvv.docreg.backend.SubmitEngine.scala$actors$ReplyReactor$$super$resumeReceiver(SubmitEngine.scala:9)
//	at scala.actors.ReplyReactor$class.resumeReceiver(ReplyReactor.scala:68)
//	at vvv.docreg.backend.SubmitEngine.resumeReceiver(SubmitEngine.scala:9)
//	at scala.actors.Actor$class.searchMailbox(Actor.scala:500)
//	at vvv.docreg.backend.SubmitEngine.searchMailbox(SubmitEngine.scala:9)
//	at scala.actors.Reactor$$anonfun$startSearch$1$$anonfun$apply$mcV$sp$1.apply$mcV$sp(Reactor.scala:117)
//	at scala.actors.Reactor$$anonfun$startSearch$1$$anonfun$apply$mcV$sp$1.apply(Reactor.scala:114)
//	at scala.actors.Reactor$$anonfun$startSearch$1$$anonfun$apply$mcV$sp$1.apply(Reactor.scala:114)
//	at scala.actors.ReactorTask.run(ReactorTask.scala:33)
//	at scala.concurrent.forkjoin.ForkJoinPool$AdaptedRunnable.exec(ForkJoinPool.java:611)
//	at scala.concurrent.forkjoin.ForkJoinTask.quietlyExec(ForkJoinTask.java:422)
//	at scala.concurrent.forkjoin.ForkJoinWorkerThread.mainLoop(ForkJoinWorkerThread.java:340)
//	at scala.concurrent.forkjoin.ForkJoinWorkerThread.run(ForkJoinWorkerThread.java:325)
//Caused by: java.io.FileNotFoundException: /tmp/lift_mime6067894564014843743upload (No such file or directory)
//	at java.io.FileInputStream.open(Native Method)
//	at java.io.FileInputStream.<init>(FileInputStream.java:120)
//	at ch.ethz.ssh2.SCPClient.sendFiles(SCPClient.java:188)
//	at ch.ethz.ssh2.SCPClient.put(SCPClient.java:571)
//	... 19 more
