package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


import java.io.File;
import java.io.IOException;


/**
 * com.hstx.docregsx.SubmitMachine.
 * @author Portal team
 */
public class SubmitMachine
{
   public void submit(String server, Agent agent, File file, String fileName, String project, String access, String user, String clientHost, String description)
      throws IOException
   {
       ScpClient scpClient = new ScpClient(server);
       try
       {
           setStatus("Registering " + fileName + " " + project + " " + access + " " + user + " " + clientHost + " " + description);
           String text = description.trim();
           String register = agent.register(fileName, project, access, user, clientHost, text.length() == 0 ? "[no description]" : text);
           if (!register.startsWith("Accepted"))
           {
               throw new RuntimeException(register);
           }
           setStatus("Sending file");
           scpClient.copy(file.toString(), fileName);
           setStatus("Submitting");
           String submit = agent.submit(fileName);
           if (!submit.equals("Accepted"))
           {
               throw new RuntimeException(submit);
           }
           setStatus("Submit successful");
       }
       catch (Exception e)
       {
           setStatus("Submit failed - " + e.getMessage());
           throw new IOException("Submit failed - " + e.getMessage(), e);
       }
   }

    private void setStatus(String s) {
        System.out.println("Submit>> " + s);
    }

}
