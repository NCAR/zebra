/*
 * The new event logger.
 */
static char *rcsid = "$Id: EventLogger.c,v 2.5 1991-11-21 22:29:34 kris Exp $";
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/Xaw/Form.h>
# include <X11/Shell.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/SmeLine.h>
# include <X11/Xaw/Cardinals.h>
# include "defs.h"
# include "../include/message.h"
# include "../include/dm.h"
# include "../include/config.h"
# include "copyright.h"



/*
 * Event mask info.
 */
int Emask = EF_EMERGENCY | EF_PROBLEM | EF_INFO;

struct EMMap
{
	int	em_flag;
	char	*em_name;
	char	em_code;
	Widget	em_w;
} EMap[] =
{
	{ EF_EMERGENCY,		"Emergencies",	'E', 0	},
	{ EF_PROBLEM,		"Problems",	'P', 0	},
	{ EF_CLIENT,		"Client events", 'C', 0	},
	{ EF_INFO,		"Informational", 'I', 0	},
	{ EF_DEBUG,		"Debugging",	'D', 0	},
	{ 0, 0, 0, 0}
};


/*
 * Keep things from getting too big.
 */
# define MAXCHAR	5000	/* This is too big	*/
# define TRIMCHAR	3000	/* Trim back to this	*/

/*
 * Text info.
 */
static int Buflen = 0;
static char *Initmsg = "$Id: EventLogger.c,v 2.5 1991-11-21 22:29:34 kris Exp $\n\
Copyright (C) 1991 UCAR, All rights reserved.\n";

/*
 * Our widgets.
 */
Widget Top, Text, Shell, Form, Wm;
XtAppContext Appc;
bool Visible = FALSE;
bool Override = TRUE;



static String Resources[] = 
{
	"	*input:		True",
	"	*Label*font:	-*-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*",
	"	*Toggle*font:	-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
	"	*Text*font:	-*-times-medium-r-*-*-*-120-*-*-*-*-*-*",
	0
};

/*
 * Bitmap for our check mark in the menus
 */
#define check_width ((unsigned int) 9)
#define check_height ((unsigned int) 8)
char Check_bits[] = {
	0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
	0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
};
Pixmap Check;

FILE *Log_file = (FILE *) 0;

char *Mother = 0;

# ifdef __STDC__
	void	SendToMother (struct msg_elog *, char *);
# else
	void	SendToMother ();
# endif


main (argc, argv)
int argc;
char **argv;
{
	Arg args[20];
	Widget w, label;
	int xevent (), msg_event (), clearbutton (), wm ();
	char *fname;
/*
 * Hook into the message system.
 */
	if (! msg_connect (msg_event, "Event logger"))
	{
		printf ("Unable to connect to message handler\n");
		exit (1);
	}
/*
 * Open our log file.
 */
	if (fname = (char *) getenv ("ZEB_LOGFILE"))
	{
		Log_file = fopen (fname, "w");

		if (! Log_file)
			printf ("Error %d opening log file '%s'\n", fname);
	}
/*
 * Get the event mask, if any
 */
	if (getenv ("EVENT_MASK"))
		Emask = atoi (getenv ("EVENT_MASK"));
/*
 * Get set up with the toolkit.
 */
	Top = XtAppInitialize (&Appc, "EventLogger", NULL, 0, &argc, argv,
		Resources, NULL, 0);
	Check = XCreateBitmapFromData (XtDisplay (Top),
		RootWindowOfScreen (XtScreen (Top)), Check_bits,
		check_width, check_height);
/*
 * If there is an argument left, it's somebody to send problems to.
 */
	if (argc > 1)
		Mother = argv[1];
/*
 * Create our shell.
 */
	XtSetArg (args[0], XtNinput, True);
	XtSetArg (args[1], XtNoverrideRedirect, True);
	Shell = XtCreatePopupShell ("Event Logger", topLevelShellWidgetClass,
		Top, args, 2);
/*
 * Put a form inside it.
 */
	Form = XtCreateManagedWidget ("form", formWidgetClass, Shell, args, 0);
/*
 * The label.
 */
	XtSetArg (args[0], XtNfromHoriz, NULL);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNlabel, "Event Logger: ");
	XtSetArg (args[3], XtNborderWidth, 0);
	XtSetArg (args[4], XtNtop, XtChainTop);
	XtSetArg (args[5], XtNbottom, XtChainTop);
	XtSetArg (args[6], XtNleft, XtChainLeft);
	XtSetArg (args[7], XtNright, XtChainLeft);
	label = XtCreateManagedWidget ("label", labelWidgetClass, Form,args,8);
/*
 * Add the clear button.
 */
	XtSetArg (args[0], XtNfromHoriz, label);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNtop, XtChainTop);
	XtSetArg (args[3], XtNbottom, XtChainTop);
	XtSetArg (args[4], XtNleft, XtChainLeft);
	XtSetArg (args[5], XtNright, XtChainLeft);
	w = XtCreateManagedWidget ("Clear", commandWidgetClass, Form, args, 6);
	XtAddCallback (w, XtNcallback, (XtCallbackProc) clearbutton, 
		(XtPointer) 0);
/*
 * The window manager button.
 */
	XtSetArg (args[0], XtNfromHoriz, w);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNtop, XtChainTop);
	XtSetArg (args[3], XtNbottom, XtChainTop);
	XtSetArg (args[4], XtNleft, XtChainLeft);
	XtSetArg (args[5], XtNright, XtChainLeft);
	XtSetArg (args[6], XtNlabel, "Ctl: DM");
	w = Wm = XtCreateManagedWidget("wm",commandWidgetClass, Form, args, 7);
	XtAddCallback (Wm, XtNcallback, (XtCallbackProc) wm, (XtPointer) 0);
/*
 * The filter button.
 */
	XtSetArg (args[0], XtNfromHoriz, w);
	XtSetArg (args[1], XtNfromVert, NULL);
	XtSetArg (args[2], XtNtop, XtChainTop);
	XtSetArg (args[3], XtNbottom, XtChainTop);
	XtSetArg (args[4], XtNleft, XtChainLeft);
	XtSetArg (args[5], XtNright, XtChainLeft);
	XtSetArg (args[6], XtNmenuName, "EventMasks");
	w = XtCreateManagedWidget ("Events ->", menuButtonWidgetClass,
		Form, args, 7);
	add_popup (w);
/*
 * Now the big, hairy, text widget.
 */
	XtSetArg (args[0], XtNresize, XawtextResizeNever);
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNfromVert, label);
	XtSetArg (args[3], XtNtype, XawAsciiString);
	XtSetArg (args[4], XtNeditType, XawtextRead);
	XtSetArg (args[5], XtNscrollVertical, XawtextScrollAlways);
	XtSetArg (args[6], XtNtop, XtChainTop);
	XtSetArg (args[7], XtNstring, Initmsg);
	XtSetArg (args[8], XtNleft, XtChainLeft);
	XtSetArg (args[9], XtNbottom, XtChainBottom);
	XtSetArg (args[10], XtNright, XtChainRight);
	Text = XtCreateManagedWidget ("text", asciiTextWidgetClass, Form,
		args, 11);
	Buflen = strlen (Initmsg);
/*
 * Join the client event and event logger groups.
 */
	msg_join ("Client events");
	msg_join ("Event logger");
/*
 * Tell msglib about our X connection.
 */
	msg_add_fd (XConnectionNumber (XtDisplay (Shell)), xevent);
/*
 * Now we just wait.
 */
	reconfig (625, 600, 500, 150);
	sync ();
	xevent ();
	msg_await ();
}






add_popup (w)
Widget w;
/*
 * Add the event popup to this widget.
 */
{
	Widget pulldown, sme;
	int i, ev_popup_cb ();
	Arg args[2];
/*
 * Make the pulldown.
 */
	pulldown = XtCreatePopupShell ("EventMasks", simpleMenuWidgetClass,
		Top, NULL, ZERO);
/*
 * Add the entries.  In Ardent land, we can not set the bitmap in at 
 * creation time, for whatever reason.
 */
	XtSetArg (args[0], XtNleftMargin, check_width + 7);
	for (i = 0; EMap[i].em_flag; i++)
	{
		XtSetArg (args[1], XtNleftBitmap,
			EMap[i].em_flag & Emask ? Check : None);
		EMap[i].em_w = w = XtCreateManagedWidget (EMap[i].em_name,
			smeBSBObjectClass, pulldown, args, 2);
		XtAddCallback (w, XtNcallback, (XtCallbackProc) ev_popup_cb, 
			(XtPointer) i);
	}
}





ev_popup_cb (w, index, junk)
Widget w;
int index, junk;
/*
 * Deal with the event popup.
 */
{
	Arg args[2];
/*
 * Tweak the event mask bit.
 */
	Emask ^= EMap[index].em_flag;
/*
 * Modify the menu accordingly.
 */
	XtSetArg (args[0], XtNleftBitmap,
			EMap[index].em_flag & Emask ? Check : None);
	XtSetValues (EMap[index].em_w, args, 1);
/*
 * Broadcast the new mask.
 */
	SendEMask ();
}





xevent (fd)
int fd;
/*
 * Deal with an Xt event.
 */
{
	XEvent event;
/*
 * Deal with events as long as they keep coming.
 */
 	while (XtAppPending (Appc))
	{
		XtAppNextEvent (Appc, &event);
		XtDispatchEvent (&event);
	}
	return (0);
}






msg_event (msg)
struct message *msg;
/*
 * Log a client event.
 */
{
	struct mh_client *client = (struct mh_client *) msg->m_data;
	char mb[200];
/*
 * Check for log messages.
 */
	if (msg->m_proto == MT_LOG)
	{
		do_log ('-', msg->m_from, msg->m_data);
		return (0);
	}
/*
 * Maybe it's a display manager message.
 */
	else if (msg->m_proto == MT_DISPLAYMGR)
	{
		dm_msg (msg->m_data);
		return (0);
	}
/*
 * If it's an extended message, do something with it.
 */
	else if (msg->m_proto == MT_ELOG)
	{
		struct msg_elog *el = (struct msg_elog *) msg->m_data;
		int i;
		char code = '?';
	/*
	 * If somebody's tweaking the event mask, we'll ignore it.
	 */
	 	if (Emask & EF_SETMASK)
			return (0);
	/*
	 * If this is in our mask, we log it.
	 */
		else if (Emask & el->el_flag || Emask == 0)
		{
			for (i = 0; EMap[i].em_flag; i++)
				if (EMap[i].em_flag & el->el_flag)
					code = EMap[i].em_code;
			do_log (code, msg->m_from, el->el_text);
		}
	/*
	 * Otherwise assume that somebody is out of sync with the
	 * event mask, and we send it out to the world.
	 */
		else
		{
			static int nsend = 0;
			if ((nsend++ % 15) == 0)
				SendEMask ();
		}
	/*
	 * Things in the PROBLEM and EMERGENCY classes get sent back to 
	 * mother.
	 */
		if (Mother && el->el_flag & (EF_PROBLEM | EF_EMERGENCY))
			SendToMother (el, msg->m_from);
		return (0);
	}
/*
 * Everything else is assumed to be a message handler event.
 */	
	switch (client->mh_type)
	{
	   case MH_CLIENT:
	   	switch (client->mh_evtype)
		{
		   case MH_CE_CONNECT:
		   	sprintf (mb,"Connect on %d", msg->m_seq);
			break;
		   case MH_CE_DISCONNECT:
		   	sprintf (mb,"Disconnect on %d", msg->m_seq);
			break;
		   case MH_CE_JOIN:
		   	sprintf (mb,"Group %s joined on %d",
				client->mh_group, msg->m_seq);
			break;
		}
		if (Emask & EF_CLIENT || Emask == 0)
			do_log ('C', client->mh_client, mb);
		break;
	   case MH_SHUTDOWN:
		exit (0);
		break;
	   default:
	   	sprintf (mb, "Strange message type: %d %d", msg->m_proto,
			client->mh_type);
	   	do_log ('P', "EventLogger", mb);
		break;
	}
	return (0);
}





do_log (code, from, msg)
char code, *from, *msg;
{
	Arg args[10];
	XawTextBlock tb;
	static char fmtbuf[300];
/*
 * Format the message to be logged.
 */
	sprintf (fmtbuf, "%c %-14s%s\n", code, from, msg);

	if (Log_file)
	{
		fprintf (Log_file, fmtbuf);
		fflush (Log_file);
	}

	tb.firstPos = 0;
	tb.length = strlen (fmtbuf);
	tb.ptr = fmtbuf;
	tb.format = FMT8BIT;
/*
 * Add it to the buffer.  Turn on editing only for long enough to do this
 * operation.
 */
	XtSetArg (args[0], XtNeditType, XawtextAppend);
	XtSetValues (Text, args, 1);
	XawTextReplace (Text, Buflen, Buflen, &tb);
/*
 * If this is getting too big, trim it back.
 */
	if ((Buflen += strlen (fmtbuf)) > MAXCHAR)
	{
		tb.firstPos = tb.length = 0;
		XawTextReplace (Text, 0, Buflen - TRIMCHAR, &tb);
		Buflen = TRIMCHAR;
	}
/*
 * Update.
 */
	XtSetArg (args[0], XtNeditType, XawtextRead);
	XtSetValues (Text, args, 1);
	XawTextDisplay (Text);
	sync ();
	XawTextSetInsertionPoint (Text, Buflen);
}




clearbutton ()
/*
 * Clear the window.
 */
{
	Arg args[2];

	XtSetArg (args[0], XtNstring, "");
	XtSetValues (Text, args, 1);
	XawTextDisplay (Text);
	Buflen = 0;
}




sync ()
/*
 * Synchronize with the window system.
 */
{
	XSync (XtDisplay (Top), False);
}






dm_msg (dmsg)
struct dm_msg *dmsg;
/*
 * Deal with a DM message.
 */
{
	struct dm_hello reply;
/*
 * See what we got.
 */
	switch (dmsg->dmm_type)
	{
	/*
	 * Maybe it's a DM scoping us out.
	 */
	   case DM_HELLO:
	   	reply.dmm_type = DM_HELLO;
		msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &reply, 
			sizeof (reply));
		break;
	/*
	 * Maybe it's a reconfig.
	 */
	   case DM_RECONFIG:
		if (Override)
		   	reconfig (dmsg->dmm_x, dmsg->dmm_y, dmsg->dmm_dx,
				dmsg->dmm_dy);
		break;
	/*
	 * They might want us to go away entirely.
	 */
	   case DM_SUSPEND:
	   	if (Visible)
		{
			Visible = FALSE;
			XtPopdown (Shell);
		}
		break;
 
	   default:
	   	do_log ('P', "DM", "Funky DM message");
	}
}







reconfig (x, y, w, h)
int x, y, w, h;
/*
 * Reconfigure the window.
 */
{
	Arg args[5];

	XtSetArg (args[0], XtNx, x);
	XtSetArg (args[1], XtNy, y);
	XtSetArg (args[2], XtNwidth, w);
	XtSetArg (args[3], XtNheight, h);
	XtSetValues (Shell, args, 4);
/* 
 * If they can't see us yet, make it so now.
 */
	if (! Visible)
	{
		XtPopup (Shell, XtGrabNone);
		Visible = TRUE;
	}
	sync ();
}






wm ()
/*
 * Try to change override redirect.
 */
{
	Arg args[2];
/*
 * If the window is up, take it down.
 */
	if (Visible)
		XtPopdown (Shell);
/*
 * Set the parameter.
 */
	Override = ! Override;
	XtSetArg (args[0], XtNoverrideRedirect, Override);
	XtSetValues (Shell, args, 1);
/*
 * Set the label on the command widget too.
 */
	if (Override)
		XtSetArg (args[0], XtNlabel, "Ctl: DM");
	else
		XtSetArg (args[0], XtNlabel, "Ctl: WM");
	XtSetValues (Wm, args, 1);
/*
 * Put the window back if it was before.
 */
	if (Visible)
		XtPopup (Shell, XtGrabNone);
}





SendEMask ()
/*
 * Broadcast the event mask to the world.
 */
{
	int flag;

	flag = Emask | EF_SETMASK;
	msg_send ("Everybody", MT_ELOG, TRUE, &flag, sizeof (flag));
}





void
SendToMother (el, from)
struct msg_elog *el;
char *from;
/*
 * Problem messages go back to the data store.
 */
{
	int i;
	char code;
	char line[1000];
/*
 * Get a code.
 */
	for (i = 0; EMap[i].em_flag; i++)
		if (EMap[i].em_flag & el->el_flag)
			code = EMap[i].em_code;
/*
 * Put together a message.
 */
	sprintf (line, " %c %s %s", code, from, el->el_text);
	msg_send ("Event Logger@data-store", MT_LOG, FALSE, line,
			strlen (line) + 1);
}
