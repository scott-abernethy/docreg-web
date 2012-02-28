package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


import java.io.*;


/**
 * com.hstx.docregsx.SendMessage.
 * @author Portal team
 */
public class SendMessage
   implements Message
{
   public static final int REGISTER_RQST = 0;  // client to server
   public static final int REGISTER_CNFM = 1;  // server to client
   public static final int REGISTER_RLY_RQST = 2;  // server to server
   public static final int REGISTER_RLY_CNFM = 3;  // server to server
   public static final int SUBMIT_RQST = 4;  // client to server
   public static final int SUBMIT_CNFM = 5;  // server to client
   public static final int EDIT_RQST = 6;  // client to server
   public static final int EDIT_CNFM = 7;  // server to client
   public static final int EDIT_IND = 8;  // server to server
   public static final int UNEDIT_IND = 9;  // server to server
   public static final int UNEDIT_RQST = 10; // client to server
   public static final int SYNC_IND = 11; // server to server
   public static final int REFRESH_RQST = 12; // client to server
   public static final int REFRESH_CNFM = 13; // server to client
   public static final int EDIT_QUERY_IND = 14; // server to server
   public static final int EDIT_QUERY_RSP = 15; // server to server
   public static final int SUBSCRIBE_RQST = 16; // client to server
   public static final int SUBSCRIBE_CNFM = 17; // server to client
   public static final int UNSUBSCRIBE_RQST = 18; // client to server
   public static final int UNSUBSCRIBE_CNFM = 19; // server to client
   public static final int NEXT_CHANGE_RQST = 21;
   public static final int APPROVAL_RQST = 35; // client to server
   public static final int APPROVAL_CNFM = 36; // server to client

   private static int nextTransaction;

   private ByteArrayOutputStream os;
   private DataOutputStream dos;
   private int transactionId;
   private int retries;
   private MessageListner listner = new MessageListner()
   {
      public void success(ReceiveMessage reply)
      {
      }


      public void failure()
      {
      }
   };


   public SendMessage(int messageId)
      throws IOException
   {
      os = new ByteArrayOutputStream();
      dos = new DataOutputStream(os);
      transactionId = getNextTransaction();
      writeHeader(dos, messageId, transactionId, 0);
   }


   public int getTransactionId()
   {
      return transactionId;
   }


   public int getSequence()
   {
      return transactionId;
   }


   private int getNextTransaction()
   {
      return nextTransaction++;
   }


   private void writeHeader(DataOutputStream dos, int messageId, int transactionId, int sequence)
      throws IOException
   {
      // message header
      //typedef struct
      //{
      //    int iVersion;
      //    tsMSG_ID eMsgId;
      //    int iTransactionId;
      //    int iSequence;
      //} tsMSG_HDR;

      dos.writeInt(3);
      dos.writeInt(messageId);
      dos.writeInt(transactionId);
      dos.writeInt(sequence);
   }


   public byte[] getBytes()
   {
      return os.toByteArray();
   }


   public void writeString(String s, int length)
      throws IOException
   {
      for (int i = 0; i < length; i++)
      {
         if (i < s.length())
         {
            dos.writeByte(s.charAt(i));
         }
         else
         {
            dos.writeByte(0);
         }
      }
   }


   public boolean countRetries()
   {
      return ++retries == 10;
   }


   public void writeInt(int value)
      throws IOException
   {
      dos.writeInt(value);
   }


   public MessageListner getListener()
   {
      return listner;
   }


   public void setListener(MessageListner listner)
   {
      this.listner = listner;
   }
}
