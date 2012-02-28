/**
 * Copyright (c) 2010 Aviat Networks Inc.
 */
package com.hstx.docregsx;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Revision {
  private final TabLine tabline;
  private final long version;
  private final Date date;

  public Revision(TabLine tabline) {
    this.tabline = tabline;
    version = Util.parseVersion(getFilename());
    date = Util.parseDate(this.tabline.get(5));
  }

  public long getVersion() {
    return version;
  }

  public String getFilename() {
    return tabline.get(0);
  }

  public String getProject() {
    return tabline.get(1);
  }

  public String getComment() {
    return tabline.get(2);
  }

  public String getAccess() {
    return tabline.get(3);
  }

  public String getAuthor() {
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

  public String getClientVersion() {
    return tabline.get(10);
  }

  public String getCrc() {
    return tabline.get(11);
  }

  public String toString() {
    return "Rev" + tabline; 
  }
}
