package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/

import java.io.*;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.List;


/**
 * com.hstx.docregsx.FileList.
 * @author Portal team
 */
public class TabFile
{
   private final List<TabLine> docs = new ArrayList<TabLine>();
   private final List<UpdateListener> listeners = new ArrayList<UpdateListener>();
   private int columnCount;


   public TabFile()
   {
   }


   public TabFile(URL url)
      throws IOException
   {
      URLConnection connection = url.openConnection();
      connection.setReadTimeout(10000);
      BufferedReader bufferedeReader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
      try
      {
         String line;
         while ((line = bufferedeReader.readLine()) != null)
         {
            //System.out.println("line = " + line);
            TabLine tabLine = new TabLine(line);
            docs.add(tabLine);
            columnCount = Math.max(columnCount, tabLine.size());
         }
      }
      finally
      {
         bufferedeReader.close();
      }
   }


   public void addUpdateListener(UpdateListener listener)
   {
      listeners.add(listener);
   }


   public List<TabLine> getList()
   {
      return docs;
   }


   public int getColumnCount()
   {
      return columnCount;
   }
}
