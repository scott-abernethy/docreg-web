package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.DecimalFormat;
import java.util.Date;
import java.util.StringTokenizer;

public class Document
{
//   int iDocNum;                         // number of document that changed
//   int iDocVer;
//   char acFileName[128];         // registered file name
//   char acProject[64];           // project name
//   char acTitle[64];                // document title
//   char acDescription[512];      // document description
//   char acAccess[128];           // access rights
//   char acAuthor[64];            // author's name
//   char acDate[32];              // date of submission
//   char acServer[32];            // name of authoritative server
//   char acClient[32];            // IP address of client that submitted document
//   char acEditor[64];                 // editor's name

   //   char acEditStart[32];             // date editing started



   private String key;
   private String version;
   private String document;
   private String project;
   private String title;
   private String description;
   private String access;
   private String author;
   private String date;
   private String server;
   private String client;
   private String editor;
   private String start;


   public Document(String line)
   {
      StringTokenizer tokenizer = new StringTokenizer(line, "\t");
      key = tokenizer.nextToken();
      version = tokenizer.nextToken();
      document = tokenizer.nextToken();
      project = tokenizer.nextToken();
      title = tokenizer.nextToken();
      description = tokenizer.nextToken();
      access = tokenizer.nextToken();
      author = tokenizer.nextToken();
      date = tokenizer.nextToken();
      server = tokenizer.nextToken();
      client = tokenizer.nextToken();
      editor = tokenIfAvailable(tokenizer);
      start = tokenIfAvailable(tokenizer);
   }


   private String tokenIfAvailable(StringTokenizer tokenizer)
   {
      if (!tokenizer.hasMoreTokens())
      {
         return null;
      }
      return tokenizer.nextToken();
   }


   public String getKey()
   {
      return key;
   }


   public String getVersion()
   {
      return version;
   }


   public String getDocument()
   {
      return document;
   }


   public String getEditor()
   {
      return editor;
   }

   public Date getEditorStart()
   {
      return Util.parseDate(start);
   }

   public URL getUrl()
      throws MalformedURLException
   {
      return new URL("http://docreg/docreg/release/" + getCodedName(document));
   }


   public URL getLatestUrl()
      throws MalformedURLException
   {
      return new URL("http://docreg/docreg/current/" + getSelectedVersion("CUR"));
   }


   public File getLocalFile(File storeDir)
   {
      String child = getSelectedVersion("XXX");
      return new File(storeDir, child);
   }


   private String getSelectedVersion(String version)
   {
      String codedName = getCodedName(document);
      return codedName.substring(0, 5) + version + codedName.substring(8);
   }


   public static String getCodedName(String name)
   {
      StringBuffer sb = new StringBuffer();
      for (int i = 0; i < name.length(); i++)
      {
         char c = name.charAt(i);
         if (c == 32 || c == 35)
         {
            sb.append("%" + Integer.toHexString(c));
         }
         else
         {
            sb.append(c);
         }
      }
      return sb.toString();
   }


   public String getSubmitFileName()
   {
      StringBuilder sb = new StringBuilder();
      sb.append(document.substring(0, 5));
      String revision = document.substring(5, 8);
      sb.append(new DecimalFormat("000").format(Integer.parseInt(revision) + 1));
      sb.append(document.substring(8));
      return sb.toString();
   }


   public Date getDate()
   {
      return Util.parseDate(date);
   }


   public URL getSubscriptionUrl(String server)
      throws MalformedURLException
   {
      return new URL("http://"+server+"/docreg/mail/" + document.substring(0, 4) + ".mail");
   }


   public URL getLogUrl(String server)
      throws MalformedURLException
   {
      return Util.logFile(server, key);
   }


   public String getTitle()
   {
      return title;
   }


   public String getProject()
   {
      return project;
   }


   public String getDescription()
   {
      return description;
   }


   public String getAccess()
   {
      return access;
   }


   public String getAuthor()
   {
      return author;
   }
}
