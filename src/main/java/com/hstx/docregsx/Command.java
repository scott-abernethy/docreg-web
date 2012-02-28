/**
 * Copyright (c) 2010 Aviat Networks Inc.
 */
package com.hstx.docregsx;

import java.io.IOException;
import java.net.SocketException;
import java.util.List;

public class Command
{
  public static void main(String[] args) throws IOException, SocketException {
    final String server = "faramir.gnet.global.vpn";
    final Agent a = new Agent("x", server, "test");

    List<Approval> as = a.loadApprovals("0018");
    for (Approval approval : as) {
      System.out.println(approval);
    }
    /*final FileList l = new FileList(server, a);
    l.addUpdateListener(new UpdateListener() {
      public void updated(List<Document> docs) {
        for (Document d : docs)
        System.out.println(d.getTitle());
        System.out.println();
      }

      public void updated(Document doc) {
        System.out.println("doc + " + doc.getTitle());

        List<Revision> list = a.loadRevisions(doc);
        for (Revision r : list) {
          System.out.println(" > " + r.getVersion() + " " + r.getDate());          
        }
      }
    });*/
  }
}
