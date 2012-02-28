package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


import java.awt.*;
import java.io.*;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.List;


/**
 * com.hstx.docregsx.FileList.
 * @author Portal team
 */
public class FileList
{
   private final URL url;
   private List<Document> docs = new ArrayList<Document>();
   private final List<UpdateListener> listeners = new ArrayList<UpdateListener>();
   private boolean listFetched;


   public FileList(String server, final Agent agent)
      throws IOException
   {
      url = new URL("http://" + server + "/docreg/docreg.txt");
      //new Thread(new RefreshProcess(agent)).start();
      refresh();
   }


   public void addUpdateListener(UpdateListener listener)
   {
      listeners.add(listener);
   }


   public void removeUpdateListener(UpdateListener listener)
   {
      listeners.remove(listener);
   }


   private List fetchList()
      throws IOException
   {
      List docs = new ArrayList();
      URLConnection connection = url.openConnection();
      connection.setReadTimeout(10000);
      BufferedReader bufferedeReader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
      try
      {
         String line;
         line = bufferedeReader.readLine();
         if (line == null)
         {
            throw new RuntimeException("Empty file: " + url);
         }
         while ((line = bufferedeReader.readLine()) != null)
         {
            docs.add(new Document(line));
         }
      }
      finally
      {
         bufferedeReader.close();
      }
      return docs;
   }


   public void refresh()
   {
      new Thread(new Runnable()
      {
         public void run()
         {
            try
            {
               final List docs = fetchList();
               listFetched = true;
               FileList.this.docs = docs;
               notifyListeners(docs);
            }
            catch (IOException e)
            {
               e.printStackTrace();
            }
         }
      }).start();
   }


   private void update(Document doc)
   {
      for (int i = 0; i < docs.size(); i++)
      {
         Document oldDoc = docs.get(i);
         if (oldDoc.getKey().equals(doc.getKey()))
         {
            docs.set(i, doc);
            return;
         }
      }
      docs.add(doc);
   }


   private void notifyListeners(List docs)
   {
      for (UpdateListener updateListener : new ArrayList<UpdateListener>(listeners))
      {
         updateListener.updated(docs);
      }
   }

   private void notifyListeners(Document doc)
   {
      for (UpdateListener updateListener : new ArrayList<UpdateListener>(listeners))
      {
         updateListener.updated(doc);
      }
   }


   public List<Document> getList()
   {
      return docs;
   }


   public boolean isListFetched()
   {
      return listFetched;
   }


   private class RefreshProcess
      implements Runnable
   {
      private final Agent agent;


      public RefreshProcess(Agent agent)
      {
         this.agent = agent;
      }


      public void run()
      {
         try
         {
            int changeRequest = getNextChangeRequest(-1).getChangeNumber();
//            System.out.println("changeRequest = " + changeRequest);
            refresh();
            while (true)
            {
               final ChangeReply reply = getNextChangeRequest(changeRequest);
               int nextChangeRequest = reply.getChangeNumber();
//               System.out.println("nextChangeRequest = " + nextChangeRequest);
               if (nextChangeRequest == changeRequest)
               {
                  Thread.sleep(1000);
               }
               else
               {
                  //System.out.println("Updated: " + reply.getDoc().getTitle());
                  changeRequest = nextChangeRequest;
                       Document doc = reply.getDoc();
                       update(doc);
                       notifyListeners(doc);
               }
            }
         }
         catch (InterruptedException e)
         {
            e.printStackTrace();
         }
      }


      private ChangeReply getNextChangeRequest(int value)
      {
         while (true)
         {
            try
            {
               return agent.getNextChangeRequest(value);
            }
            catch (IOException e)
            {
               e.printStackTrace();
            }

            try
            {
               Thread.sleep(10000);
            }
            catch (InterruptedException e)
            {
               e.printStackTrace();
            }
         }
      }
   }
}
