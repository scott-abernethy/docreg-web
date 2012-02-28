package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


import java.util.ArrayList;
import java.util.List;


/**
 * com.hstx.docregsx.TabLine.
 * @author Portal team
 */
public class TabLine
{
   private List<String> items = new ArrayList<String>();

   public TabLine(String line)
   {
      int lastIndex = 0;
      while (true)
      {
         int index = line.indexOf('\t', lastIndex);
         if (index == -1)
         {
            break;
         }
         items.add(line.substring(lastIndex, index));
         lastIndex = index + 1;
      }
      items.add(line.substring(lastIndex));
   }


   public String get(int index)
   {
      if (index >= items.size())
      {
         return null;
      }
      return items.get(index);
   }


   public int size()
   {
      return items.size();
   }

  public String toString() {
    return items.toString();
  }
}
