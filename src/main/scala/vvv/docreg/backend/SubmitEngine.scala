/*
 * Copyright (c) 2013 Scott Abernethy.
 * This file is part of DocReg+Web. Please refer to the NOTICE.txt file for license details.
 */

package vvv.docreg.backend

import vvv.docreg.agent._
import net.liftweb.common.Loggable
import vvv.docreg.model.Document.ValidDocumentFileName
import akka.actor.{PoisonPill, Actor, ActorRef}
import vvv.docreg.model.{Document, User}
import vvv.docreg.util.StringUtil._
import vvv.docreg.agent.SubmitRequest
import vvv.docreg.agent.RegisterRequest
import vvv.docreg.agent.SubmitReply
import vvv.docreg.agent.RegisterReply
import vvv.docreg.agent.RequestPackage

class SubmitEngine(agent: ActorRef, target: String, clientHost: String, clientVersion: String) extends Actor with Loggable
{
  var cachedRequest: Submit = null

  def receive = {
        case msg @ Submit(document, projectName, localFile, userFileName, comment, user) =>
        {
          // todo check revision is latest?
          // todo pass in new user title rather than setting d's title. similar to Create below
          // todo check the fields, including comment which should default to "[no description]"? Check default for approval etc also.
          cachedRequest = msg
          val request = SubmitEngine.registerRequest(projectName, document.access, userFileName, comment, user, clientHost, clientVersion)
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
            logger.debug("Copying file")
            // todo error handling? or just akka fail handler?
            SubmitBin.copyTo(cachedRequest.localFile.apply(), submittedFileName)
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
        // todo supervision
  }
}

object SubmitEngine {
   def registerRequest(projectName: String, access: String, userFileName: String, comment: String, user: User, clientHost: String, clientVersion: String): RegisterRequest = {
      RegisterRequest(
         truncateFilename(userFileName.replaceAll("[\t\n\r:*?<>|/\"'\\\\]", " ").replaceAll("[ ]+", " "), 80, 64),
         projectName,
         if (comment.length() < 1) "[no description]" else comment,
         access,
         user.displayName,
         user.shortUsername(),
         clientHost,
         clientVersion
      )
   }
}