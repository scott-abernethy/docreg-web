package com.hstx.docregsx;

import java.text.*;
import java.util.*;

public class Approval {
/*
 * Tab file contents:
 * 0 filename
 * 1 approver name
 * 2 approver email
 * 3 status
 * 4 comment (or "No Comment")
 * 5 date
 * 6 authoritative server
 * 7 client ip
 * 8 client pc
 * 9 client user name
 */
  private final TabLine tabline;
  private final long version;
  private final Date date;
  private final ApprovalStatus status;

  public Approval(TabLine tabline) throws ParseException {
    this.tabline = tabline;
    version = Util.parseVersion(getFilename());
    date = Util.parseDate(this.tabline.get(5));
    status = ApprovalStatus.factory.parse(this.tabline.get(3));
  }

  public long getVersion() {
    return version;
  }

  public String getFilename() {
    return tabline.get(0);
  }

  public String getApproverName() {
    return tabline.get(1);
  }

  public String getApproverEmail() {
    return tabline.get(2);
  }

  public ApprovalStatus getStatus() {
    return status;
  }

  public String getComment() {
    return tabline.get(4);
  }

  public Date getDate() {
    return date;
  }

  public String getServer() {
    return tabline.get(6);
  }

  public String getClientIp() {
    return tabline.get(7);
  }

  public String getClientPc() {
    return tabline.get(8);
  }

  public String getUsername() {
    return tabline.get(9);
  }

  public String toString() {
    return "Approval" + tabline; 
  }
}
