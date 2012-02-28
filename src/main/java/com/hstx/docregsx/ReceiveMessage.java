package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


import java.io.*;


/**
 * com.hstx.docregsx.ReceiveMessage.
 * @author Portal team
 */
public class ReceiveMessage
   implements Message
{
   private final int version;
   private final int messageId;
   private final int transactionId;
   private final int sequence;
   private final DataInputStream dis;


   public ReceiveMessage(byte[] data)
      throws IOException
   {
      dis = new DataInputStream(new ByteArrayInputStream(data));
      // message header
      //typedef struct
      //{
      //    int iVersion;
      //    tsMSG_ID eMsgId;
      //    int iTransactionId;
      //    int iSequence;
      //} tsMSG_HDR;
      version = dis.readInt();
      assert version == 3;
      messageId = dis.readInt();
      transactionId = dis.readInt();
      sequence = dis.readInt();
   }


   public int getMessageId()
   {
      return messageId;
   }


   public int getTransactionId()
   {
      return transactionId;
   }


   public int getSequence()
   {
      return sequence;
   }


   public String getString(int length)
      throws IOException
   {
      byte[] buf = new byte[length];
      dis.readFully(buf);
      StringBuffer sb = new StringBuffer();
      for (int i = 0; i < buf.length; i++)
      {
         char c = (char)buf[i];
         if (c == 0)
         {
            break;
         }
         sb.append(c);
      }
      return sb.toString();
   }


   public int getInt()
      throws IOException
   {
      return dis.readInt();
//      byte[] buf = new byte[4];
//      dis.readFully(buf);
//      int value = 0;
//      for (int i = 0; i < buf.length; i++)
//      {
//         value |= ((int)buf[i] & 0xff) << i * 8;
//      }
//      return value;
   }
}
