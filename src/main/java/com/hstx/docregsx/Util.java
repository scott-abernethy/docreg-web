/**
 * Copyright (c) 2010 Aviat Networks Inc.
 */
package com.hstx.docregsx;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Util {
  // Date format is '2002-06-13 21:38:53 Z'
  private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss 'Z'");
  private static final Pattern FILENAME_FORMAT = Pattern.compile("^([a-zA-Z0-9]+)-([0-9]+)-(.*)");
  private static final int VERSION_GROUP = 2;

  static {
    DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("UTC"));
  }

  public static Date parseDate(String dateString) {
    try {
      return DATE_FORMAT.parse(dateString);
    } catch (Exception e) {
      System.out.println("Failed to parse " + dateString);
      return null;
    }
  }

  public static URL logFile(String server, String key) throws MalformedURLException {
    return new URL("http://"+server+"/docreg/log/" + key + ".log");    
  }

  public static URL approvalFile(String server, String key) throws MalformedURLException {
    return new URL("http://"+server+"/docreg/approval/" + key + ".approval");    
  }
  
  public static URL mailFile(String server, String key) throws MalformedURLException {
    return new URL("http://"+server+"/docreg/mail/" + key + ".mail");
  }

  public static long parseVersion(String filename) {
    try {
      Matcher matcher = FILENAME_FORMAT.matcher(filename);
      matcher.matches();
      String s = matcher.group(VERSION_GROUP);
      return Long.parseLong(s);
    } catch (Exception e) {
      System.out.println("Failed to parse " + filename);
      return -1;
    }
  }

  public static void main(String[] args) {
    String text = "doc0003-059-Document Blah blah asdfsdfasd.doc";
    Matcher matcher = FILENAME_FORMAT.matcher(text);
    System.out.println("matcher.matches() = " + matcher.matches());
    System.out.println(matcher.group(0));
    System.out.println(matcher.group(1));
    System.out.println(matcher.group(VERSION_GROUP));
    System.out.println(matcher.group(3));
    System.out.println(parseVersion(text));
  }
}
