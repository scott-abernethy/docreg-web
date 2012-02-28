package com.hstx.docregsx;

import java.text.*;
import java.util.*;

public enum ApprovalStatus {
  Approved("Approved"), NotApproved("Not Approved"), Pending("Pending");

  public static final Factory factory = new Factory();
  private final String toString;
  private ApprovalStatus(String toString) {
    this.toString = toString;
  }
  public String toString() {
    return toString;
  }

  public static class Factory {
    private final Map<String, ApprovalStatus> map = new HashMap<String, ApprovalStatus>();
    private Factory() {
      for (ApprovalStatus a : ApprovalStatus.values()) {
        map.put(a.toString(), a);
      }
    }
    public ApprovalStatus parse(String statusText) throws ParseException {
      ApprovalStatus s = map.get(statusText);
      if (s == null) {
        throw new ParseException("Failed to parse approval status '" + statusText + "'", -1);
      } 
      return s;
    }

  }
}
