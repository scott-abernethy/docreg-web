package com.hstx.docregsx; /***************************************************
 *
 * Copyright (c) 2001-2005 Stratex Networks Inc.
 *
 ***************************************************/


/**
 * com.hstx.docregsx.MessageListner.
 * @author Portal team
 */
public interface MessageListner
{
   void success(ReceiveMessage reply);


   void failure();
}
