package com.hstx.docregsx;

import java.text.*;

public class Subscriber {
/*
 * Tab file contents:
 * 0 subscriber username
 * 1 subscriber email
 * 2 email event
 */
    
    private final TabLine tabline;
    
    public Subscriber(TabLine tabline) throws ParseException {
      this.tabline = tabline;
    }
    
    public String getSubscriberUserName() {
      return tabline.get(0);
    }
    
    public String getSubscriberEmail() {
      return tabline.get(1);
    }
    
    public String getEmailEvent() {
      return tabline.get(2);
    }
}
    