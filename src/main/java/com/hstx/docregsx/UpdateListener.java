package com.hstx.docregsx;

import java.util.List; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


/**
 * com.hstx.docregsx.UpdateListener.
 * @author Portal team
 */
public interface UpdateListener
{
   void updated(List<Document> docs);

  void updated(Document doc);
}
