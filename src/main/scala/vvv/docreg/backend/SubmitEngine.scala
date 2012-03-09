package vvv.docreg.backend

import actors.Actor
import vvv.docreg.agent._
import net.liftweb.common.Loggable
import java.io.File
import com.hstx.docregsx.ScpClient

class SubmitEngine(agent: DaemonAgent, target: String, clientHost: String, clientVersion: String) extends Actor with Loggable
{
  var cachedLocalFile: File = null

  def act()
  {
    loop
    {
      react
      {
        case Submit(document, projectName, localFile, userFileName, comment, user) =>
        {
          // todo check revision is latest?
          // todo pass in new user title rather than setting d's title. similar to Create below
          // todo check the fields, including comment which should default to "[no description]"? Check default for approval etc also.
          cachedLocalFile = localFile
          agent ! RequestPackage(Actor.self, target, RegisterRequest(userFileName, projectName, if (comment.length() < 1) "[no description]" else comment, "Everyone", user.displayName, user.shortUsername(), clientHost, clientVersion))
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
            val submittedFileName = suggestedFileName
            var scpClient = new ScpClient(target)
            logger.info("Copying file")
            scpClient.copy(cachedLocalFile.toString(), submittedFileName);
            logger.info("Copying file, done")
            // todo check file size
            agent ! RequestPackage(Actor.self, target, SubmitRequest(submittedFileName, -1))
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
          Actor.self ! 'Die
        }
        case 'Die =>
        {
          exit()
        }
        case other =>
        {
          logger.warn("Submit engine got unexpected " + other)
        }
        // todo timeout
      }
    }
  }
}