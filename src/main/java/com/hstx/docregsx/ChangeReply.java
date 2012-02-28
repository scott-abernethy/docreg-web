package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


import java.io.IOException;
import java.text.DecimalFormat;


/**
 * com.hstx.docregsx.ChangeReply.
 * @author Portal team
 */
public class ChangeReply
{
   private int changeNumber;
   private Document doc;
//   int iChangeNumber;              // unique ID for this change
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


   public ChangeReply(ReceiveMessage message)
      throws IOException
   {
      changeNumber = message.getInt();
      StringBuilder sb = new StringBuilder();
      sb.append(new DecimalFormat("0000").format(message.getInt()));
      sb.append('\t');
      sb.append(new DecimalFormat("000").format(message.getInt()));
      int[] sizes = new int[]{128, 64, 64, 512, 128, 64, 32, 32, 32, 64, 32};
      for (int i = 0; i < sizes.length; i++)
      {
         String s = message.getString(sizes[i]);
//         if (i != 2)
//         {
         sb.append('\t');
         sb.append(s);
//         }
//         if (i == 3)
//         {
//            sb.append("\t<no description>");
//         }
      }
      doc = new Document(sb.toString());
   }


   public int getChangeNumber()
   {
      return changeNumber;
   }


   public Document getDoc()
   {
      return doc;
   }
}
