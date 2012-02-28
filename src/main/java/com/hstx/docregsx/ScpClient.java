package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


import java.io.IOException;
import ch.ethz.ssh2.*;


/**
 * com.hstx.docregsx.ScpClient.
 * @author Portal team
 */
public class ScpClient
{
   private final String server;


   public ScpClient(String server)
   {
      this.server = server;
   }


   public void copy(String localFile, String remoteFile)
      throws IOException
   {
      Connection connection = new Connection(server);
      connection.connect();
      try
      {
         InteractiveCallback interactiveCallback = new InteractiveCallback()
         {
            public String[] replyToChallenge(String name, String instruction, int numprompts, String[] prompts, boolean[] echo)
               throws Exception
            {
               String[] responses = new String[prompts.length];
               for (int i = 0; i < prompts.length; i++)
               {
                  String prompt = prompts[i];
                  if (prompt.equals("Password: "))
                  {
                     responses[i] = "***REMOVED***";
                  }
                  else
                  {
                     responses[i] = "";
                  }
               }
               return responses;
            }
         };
         connection.authenticateWithKeyboardInteractive("docreg", interactiveCallback);
         SCPClient client = new SCPClient(connection);

         client.put(localFile, remoteFile, "submit/", "0664");
      }
      finally
      {
         connection.close();
      }
   }


}
