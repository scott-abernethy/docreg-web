package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.net.*;
import java.text.*;
import java.util.*;
import java.util.List;


/**
 * com.hstx.docregsx.Agent.
 * @author Portal team
 */
public class Agent
{
   private static final int DOCREG_PORT = 5436;
   private final DatagramSocket socket;
   private final String version;
   private final String server;
   private final String user;
   private final List<SendMessage> sendList = new ArrayList<SendMessage>();


   public Agent(String version, String server, String user)
      throws UnknownHostException, SocketException
   {
      this.version = version;
      this.server = server;
      this.user = user;

      socket = new DatagramSocket();
      new Thread(new Runnable()
      {
         public void run()
         {
            try
            {
               while (!socket.isClosed())
               {
                  byte[] buf = new byte[1522];
                  DatagramPacket packet = new DatagramPacket(buf, buf.length);
                  socket.receive(packet);
                  byte[] data = new byte[packet.getLength()];
                  System.arraycopy(packet.getData(), packet.getOffset(), data, 0, data.length);
                  processResponse(new ReceiveMessage(data));
               }
            }
            catch (IOException e)
            {
               e.printStackTrace();
            }
         }
      }).start();
      new Thread(new Runnable()
      {
         public void run()
         {
            try
            {
               while (!socket.isClosed())
               {
                  List<SendMessage> messages;
                  synchronized (sendList)
                  {
                     sendList.wait(500);
                     messages = new ArrayList<SendMessage>(sendList);
                  }
//                  send(new com.hstx.docregsx.SendMessage(com.hstx.docregsx.SendMessage.REFRESH_RQST));
                  for (SendMessage message : messages)
                  {
                     send(message);
                     if (message.countRetries())
                     {
                        processResponse(message);
                     }
                  }
               }
            }
            catch (IOException e)
            {
               e.printStackTrace();
            }
            catch (InterruptedException e)
            {
               e.printStackTrace();
            }
         }


         private void send(SendMessage message)
            throws IOException
         {
            byte[] bytes = message.getBytes();
            socket.send(new DatagramPacket(bytes, bytes.length, new InetSocketAddress(Inet4Address.getByName(Agent.this.server), DOCREG_PORT)));
         }
      }).start();
   }


   private void notifyMessageListener(SendMessage message, final ReceiveMessage reply)
   {
      final MessageListner listener = message.getListener();
      EventQueue.invokeLater(new Runnable()
      {
         public void run()
         {
            if (reply == null)
            {
               listener.failure();
            }
            else
            {
               listener.success(reply);
            }
         }
      });
   }


   /* returns the user who is editing the document */
   public String edit(Document doc, String user) 
      throws IOException
   {
      return edit(doc.getDocument(), user);
   }
   
   
   public String edit(String filename, String user)
      throws IOException
   {
      SendMessage message = new SendMessage(SendMessage.EDIT_RQST);
      message.writeString(filename, 128);
      message.writeString(user, 64);
      ReceiveMessage reply = blockingSend(message);
      return reply.getString(64);
   }


//    // message sent to initiate registration
//typedef struct
//{
//    tsMSG_HDR sHeader;
//    char acName[128];          // file name
//    char acProject[64];        // project name
//    char acDescription[512];   // document description
//    char acAccess[128];        // document access rights
//    char acAuthor[64];         // author name as displayed
//    char acClientName[64];     // name of the client PC as resolved by client
//    char acUserName[64];       // user name client is running under
//    char acClientVersion[16];  // client software version
//} tsREGISTER_RQST;
   public String register(String submitFileName, String submitProject, String access, String user, String clientHost, String description)
      throws IOException
   {
      SendMessage message = new SendMessage(SendMessage.REGISTER_RQST);
      message.writeString(submitFileName, 128);
      message.writeString(submitProject, 64);
      message.writeString(description, 512);
      message.writeString(access, 128);
      message.writeString(user, 64);
      message.writeString(clientHost, 64);
      message.writeString(user, 64);
      message.writeString(version, 16);
      ReceiveMessage reply = blockingSend(message);
      String response = reply.getString(128);
      String suggestedName = reply.getString(128);
      System.out.println("Register status: " + response);
      if (suggestedName.length() != 0)
      {
         System.out.println("Suggested file name: " + suggestedName);
      }
      return response;
   }

   public void registerCopySubmit(File file, String fileName, String project, String access, String user, String clientHost, String comment)
           throws IOException
   {
      new SubmitMachine().submit(server, this, file, fileName, project, access, user, clientHost, comment);
   }

   public String submit(Document doc)
      throws IOException
   {
      return submit(doc.getSubmitFileName());
   }

   public String submit(String submitFileName)
      throws IOException
   {
      SendMessage message = new SendMessage(SendMessage.SUBMIT_RQST);
      message.writeString(submitFileName, 128);
      message.writeInt(-1);
      ReceiveMessage reply = blockingSend(message);
      String response = reply.getString(128);
      String suggestedName = reply.getString(128);
      System.out.println("Submit status: " + response);
      if (suggestedName.length() != 0)
      {
         System.out.println("Suggested file name: " + suggestedName);
      }
      return response;
   }

   public void unedit(Document doc, String email)
      throws IOException
   {
      unedit(doc.getDocument(), email);
   }
   

   public void unedit(String filename, String email)
      throws IOException
   {
      SendMessage message = new SendMessage(SendMessage.UNEDIT_RQST);
      message.writeString(filename, 128);
      message.writeString(email, 64);
      blockingSend(message);
   }


   public boolean subscribe(String filename, String email)
      throws IOException
   {
      SendMessage message = new SendMessage(SendMessage.SUBSCRIBE_RQST);
      message.writeString(filename, 128);
      message.writeString(email, 64);//Just use email as user
      message.writeString(email, 64);
      String options = "always";
      message.writeString(options, 128);
      ReceiveMessage reply = blockingSend(message);
      return "Accepted".equalsIgnoreCase(reply.getString(128));
   }


   public boolean unsubscribe(String filename, String email)
      throws IOException
   {
      SendMessage message = new SendMessage(SendMessage.UNSUBSCRIBE_RQST);
      message.writeString(filename, 128);
      message.writeString(email, 64);//Just use email as user
      message.writeString(email, 64);
      ReceiveMessage reply = blockingSend(message);
      return "Accepted".equalsIgnoreCase(reply.getString(128));
   }

//typedef struct
//{
//    char acFileName[128];         // file name
//    char acApprover[64];          // author name as displayed
//    char acEmailAddress[64];      // e-mail address
//    char acStatus[32];            // "Approved", "Not Approved" or "Pending"
//    char acComment[128];          // optional comment on approval
//    char acDate[32];              // date of approval
//    char acServer[32];            // name of authoritative server
//    char acClientIp[32];          // IP address of client that submitted approval
//    char acClientName[64];        // name of the client PC as resolved by client
//    char acUserName[64];          // user name client is running under
//} tsAPPROVAL_RECORD;
  public boolean approval(String filename, String approverName, String approverEmail, ApprovalStatus status, String comment, String clientHost, String clientUsername)
    throws IOException
  {
//     typedef struct
//{
//    tsMSG_HDR sHeader;
//    char acFileName[128];      // file name
//    char acApprover[64];       // approver name as displayed
//    char acEmailAddress[64];   // e-mail address
//    char acStatus[32];         // "Approved", "Not Approved" or "Pending"
//    char acComment[128];       // optional comment on approval
//    char acClientName[64];     // name of the client PC as resolved by client
//    char acUserName[64];       // user name client is running under
//} tsAPPROVAL_RQST;
    SendMessage message = new SendMessage(SendMessage.APPROVAL_RQST);
    message.writeString(filename, 128);    
    message.writeString(approverName, 64);    
    message.writeString(approverEmail, 64);    
    message.writeString(status.toString(), 32);    
    message.writeString(comment, 128);    
    message.writeString(clientHost, 64);    
    message.writeString(clientUsername, 64);    
    ReceiveMessage reply = blockingSend(message);
//    typedef struct
//{
//    tsMSG_HDR sHeader;
//    char acResponse[128];      // "Accepted" or "Rejected"
//} tsAPPROVAL_CNFM;
    return "Accepted".equalsIgnoreCase(reply.getString(128));
  }


   public ChangeReply getNextChangeRequest(int value)
      throws IOException
   {
      SendMessage message = new SendMessage(SendMessage.NEXT_CHANGE_RQST);
      message.writeInt(value);
      return new ChangeReply(blockingSend(message));
   }

  public List<Revision> loadRevisions(Document doc) {
    return loadRevisions(doc.getKey());  
  }

  public List<Revision> loadRevisions(String key) {
    try {
      TabFile file = new TabFile(Util.logFile(server, key));
      List<TabLine> lines = file.getList();
      List<Revision> revisions = new LinkedList<Revision>();
      Map<Long, Revision> versions = new HashMap<Long, Revision>();
      for (int i = 0; i < lines.size(); i++)
      {
        Revision rev = new Revision(lines.get(i));
        if (versions.containsKey(rev.getVersion())) {
          //System.out.println("Duplicate revision found for " + key + " at " + rev.getVersion());
          revisions.remove(versions.get(rev.getVersion()));
        }
        revisions.add(rev);
        versions.put(rev.getVersion(), rev);
      }
      return revisions;
    } catch (IOException e) {
      return Collections.emptyList();
    }
  }

  public List<Approval> loadApprovals(Document doc) {
    return loadApprovals(doc.getKey());  
  }

  public List<Approval> loadApprovals(String key) {
    try {
      TabFile file = new TabFile(Util.approvalFile(server, key));
      List<TabLine> lines = file.getList();
      List<Approval> approvals = new LinkedList<Approval>();
      for (int i = 0; i < lines.size(); i++)
      {
        try {
          approvals.add(new Approval(lines.get(i)));
        } catch (ParseException e) {
          e.printStackTrace();
        }
      }
      return approvals;
    } catch (IOException e) {
      return Collections.emptyList();
    }
  }
  
  public List<Subscriber> loadSubscribers(Document doc) {
    return loadSubscribers(doc.getKey());
  }
  
  public List<Subscriber> loadSubscribers(String key) {
    try {
      TabFile file = new TabFile(Util.mailFile(server, key));
      List<TabLine> lines = file.getList();
      List<Subscriber> subscribers = new LinkedList<Subscriber>();
      for (int i = 0; i < lines.size(); i++) 
      {
        try {
          subscribers.add(new Subscriber(lines.get(i)));
        } catch (ParseException e) {
          e.printStackTrace();
        }
      }
      return subscribers;
    } catch (IOException e) {
      return Collections.emptyList();
    }
  }
         

  
   private void addToSendQueue(SendMessage message)
   {
      synchronized (sendList)
      {
         sendList.add(message);
         sendList.notifyAll();
      }
   }


   private void processResponse(Message message)
   {
      synchronized (sendList)
      {
         for (Iterator<SendMessage> iterator = sendList.iterator(); iterator.hasNext();)
         {
            SendMessage sendMessage = iterator.next();
            if (sendMessage.getTransactionId() == message.getTransactionId())
            {
               iterator.remove();
               if (message instanceof ReceiveMessage)
               {
                  notifyMessageListener(sendMessage, (ReceiveMessage)message);
               }
               else
               {
                  notifyMessageListener(sendMessage, null);
               }
            }
         }
      }
   }


   private ReceiveMessage blockingSend(SendMessage message)
      throws IOException
   {
      final Object lock = new Object();
      final ReceiveMessage[] result = new ReceiveMessage[1];
      message.setListener(new MessageListner()
      {

         public void success(ReceiveMessage reply)
         {
            result[0] = reply;
            synchronized (lock)
            {
               lock.notifyAll();
            }
         }


         public void failure()
         {
            synchronized (lock)
            {
               lock.notifyAll();
            }
         }
      });
      try
      {
         synchronized (lock)
         {
//            System.out.println("Sending: " + message.getTransactionId() + " " + message.getSequence());
            addToSendQueue(message);
            lock.wait();
         }
      }
      catch (InterruptedException e)
      {
         e.printStackTrace();
      }
      if (result[0] == null)
      {
         throw new IOException("No reply");
      }
      return result[0];
   }

   public static void main(String[] args) throws IOException {
      Agent x = new Agent("x.y.z", "shelob", "sabernethy");
      x.registerCopySubmit(new File("/tmp", "test.txt"), "6118-001-XXXX-001-Another test document.txt", "DocReg", "Everyone", "sabernethy", "10.16.2.0", "Initial test");
   }
}
