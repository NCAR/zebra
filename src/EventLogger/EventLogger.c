/*
 * The new event logger.
 */
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
# include <errno.h>
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
# include "message.h"
# include "timer.h"
# include "dm.h"
# include "config.h"
# include "copyright.h"
# ifndef lint
MAKE_RCSID ("$Id: EventLogger.c,v 2.15 1993-05-13 09:48:13 granger Exp $")
# endif

# define EL_NAME "EventLogger"

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
static char *Initmsg = "$Id: EventLogger.c,v 2.15 1993-05-13 09:48:13 granger Exp $\n\
Copyright (C) 1991 UCAR, All rights reserved.\n";

/*
 * Special info to enable debugging logging on selected clients only.
 */
stbl ProcTable;		/* Keep track of all processes */
typedef struct _ProcInfo
{
	char	pi_name[40];	/* Process name		*/
	Widget	pi_menu;	/* The menu entry	*/
	bool	pi_enabled;	/* Is debugging enabled? */
} ProcInfo;

/*
 * Our widgets.
 */
Widget Top, Text, Shell, Form, Wm, ProcMenu;
XtAppContext Appc;
bool Visible = FALSE;
bool Override = TRUE;

/*
 * Currently active entry from timestamp periods menu
 */
# define DEFAULT_PERIOD 	300 	/* 5 minutes */
Widget TimestampEntry = NULL;

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
const char Check_bits[] = {
	0x00, 0x01, 0x80, 0x01, 0xc0, 0x00, 0x60, 0x00,
	0x31, 0x00, 0x1b, 0x00, 0x0e, 0x00, 0x04, 0x00
};
Pixmap Check;

FILE *Log_file = (FILE *) 0;

/*
 * Is there somebody out there interested in our problems?  If so, this
 * is their name.
 */
char *Mother = 0;

/*
 * Forwards.
 */
void	SendToMother FP((struct msg_elog *, char *));
Widget	MakeDbgButton FP ((Widget));
void	NewProc FP ((char *));
void	DeadProc FP ((char *));
void	ToggleProc FP ((Widget, XtPointer, XtPointer));
int	SendDbgMask FP ((char *, int, SValue *, long));
int	PassDebug FP ((int, char *));
static void AppendToLog FP ((char *buf));
static Widget MakeTimestampButton FP ((Widget left, int default_period));
static void TimestampSetup FP ((int period));
static void Timestamp FP ((ZebTime *t, void *param));
static void TimestampCallback FP ((Widget w, XtPointer client_data, 
				   XtPointer call_data));



main (argc, argv)
int argc;
char **argv;
{
	Arg args[20];
	Widget w, label;
	int xevent (), msg_event (), clearbutton (), wm (), n;
	int ts_secs = -1;
	char *fname;
/*
 * Initialize.
 */
	usy_init ();
	ProcTable = usy_c_stbl ("ProcTable");
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
			printf ("Error %d opening log file '%s'\n", errno, 
				fname);
	}
/*
 * Get the event mask, if any, and the timestamp interval, if any
 */
	if (getenv ("EVENT_MASK"))
		Emask = atoi (getenv ("EVENT_MASK"));
	if (getenv ("EVENT_TIMESTAMP"))
		ts_secs = atoi (getenv ("EVENT_TIMESTAMP"));
	else
		ts_secs = DEFAULT_PERIOD;
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
	n = 0;
	XtSetArg (args[n], XtNinput, True);		n++;
	XtSetArg (args[n], XtNoverrideRedirect, True);	n++;
	XtSetArg (args[n], XtNallowShellResize, True);	n++;
	Shell = XtCreatePopupShell ("Event Logger", topLevelShellWidgetClass,
		Top, args, n);
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
 * The debug button.
 */
	w = MakeDbgButton (w);
/*
 * The timestamp interval button
 */
	w = MakeTimestampButton (w, ts_secs);
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
	reconfig (625, 600, 500, 150);
/*
 * Now we start our timestamp callbacks and wait.
 */
	if (! TimestampEntry)
		TimestampSetup (ts_secs);
	else
		XtCallCallbacks (TimestampEntry, XtNcallback, 0);
	sync ();
	xevent ();
	msg_await ();
}





Widget
MakeDbgButton (left)
Widget left;
/*
 * Create the menu button to control debugging.
 */
{
	Widget button;
	Arg args[10];
	int n;
/*
 * Create the menu button to start with.
 */
	n = 0;
	XtSetArg (args[0], XtNfromHoriz, left);		n++;
	XtSetArg (args[1], XtNfromVert, NULL);		n++;
	XtSetArg (args[2], XtNtop, XtChainTop);		n++;
	XtSetArg (args[3], XtNbottom, XtChainTop);	n++;
	XtSetArg (args[4], XtNleft, XtChainLeft);	n++;
	XtSetArg (args[5], XtNright, XtChainLeft);	n++;
	XtSetArg (args[6], XtNmenuName, "Processes");	n++;
	button = XtCreateManagedWidget ("Debug ->", menuButtonWidgetClass,
		Form, args, n);
/*
 * The menu that goes with it.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Debugging");	n++;
	ProcMenu = XtCreatePopupShell ("Processes", simpleMenuWidgetClass,
		Top, args, n);
/*
 * The actual entries will be created as the connect notifications arrive.
 */
	return (button);
}



static Widget
MakeTimestampButton (left, deflt)
Widget left;
int deflt;
/*
 * Create the menu button to control the timestamp period.
 */
{
	Widget button, menu, entry;
	Arg args[10];
	int n, i;
	static int periods[10] = { 0, 5, 10, 30, 60, 300, 600, 1800 };
	static char *pnames[10] = { "None", "5 seconds", "10 seconds",
				   "30 seconds", "1 minute",
				   "5 minutes", "10 minutes", "30 minutes" };
/*
 * Create the menu button to start with.
 */
	n = 0;
	XtSetArg (args[0], XtNfromHoriz, left);		n++;
	XtSetArg (args[1], XtNfromVert, NULL);		n++;
	XtSetArg (args[2], XtNtop, XtChainTop);		n++;
	XtSetArg (args[3], XtNbottom, XtChainTop);	n++;
	XtSetArg (args[4], XtNleft, XtChainLeft);	n++;
	XtSetArg (args[5], XtNright, XtChainLeft);	n++;
	XtSetArg (args[6], XtNmenuName, "Timestamps");	n++;
	button = XtCreateManagedWidget ("Timestamps ->", menuButtonWidgetClass,
					Form, args, n);
/*
 * The menu that goes with it.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Period");	n++;
	menu = XtCreatePopupShell ("Timestamps", simpleMenuWidgetClass,
				   Top, args, n);
	XtCreateManagedWidget ("line", smeLineObjectClass, menu, NULL, 0);
	XtSetArg (args[0], XtNleftMargin, check_width + 7);
	for (i = 0; i < 8; ++i)
	{
		n = 1;
		XtSetArg (args[n], XtNlabel, pnames[i]); ++n;
		XtSetArg (args[n], XtNleftBitmap, None); ++n;
		entry = XtCreateManagedWidget ("entry", smeBSBObjectClass,
					       menu, args, n);
		if (periods[i] == deflt)
			TimestampEntry = entry;
		XtAddCallback (entry, XtNcallback, 
			       TimestampCallback, (XtPointer)periods[i]);
	}
	return (button);
}




add_popup (w)
Widget w;
/*
 * Add the event popup to this widget.
 */
{
	Widget pulldown;
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





/*ARGSUSED*/
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





xevent ()
/*
 * Deal with an Xt event.
 */
{
	XEvent event;
/*
 * Deal with events as long as they keep coming.
 */
	sync ();
 	while (XtAppPending (Appc))
	{
		XtAppNextEvent (Appc, &event);
		XtDispatchEvent (&event);
		sync ();
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
		else if (Emask & el->el_flag || Emask == 0 ||
				PassDebug (el->el_flag, msg->m_from))
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
			NewProc (client->mh_client);
		   	sprintf (mb,"Connect on %d", msg->m_seq);
			break;
		   case MH_CE_DISCONNECT:
			DeadProc (client->mh_client);
		   	sprintf (mb,"Disconnect on %d", msg->m_seq);
			break;
		   case MH_CE_JOIN:
		   	sprintf (mb,"Group %s joined on %d",
				client->mh_group, msg->m_seq);
			break;
		   case MH_CE_QUIT:
		   	sprintf (mb,"Group %s quit on %d",
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
	static char fmtbuf[300];
	static char last_msg[300];
	static int repeat_count = 0;
/*
 * Format the message to be logged.
 */
	sprintf (fmtbuf, "%c %-14s%s\n", code, from, msg);

/*
 * See if we've seen this message before: if so, print the repeat count
 * at regular intervals; otherwise reset repeat count and proceed as usual.
 * Note that repeat_count will always be at least one except for the first
 * time this function is entered.
 */
	if ((repeat_count > 0) && !strcmp (fmtbuf, last_msg))
	{
		++repeat_count;
		if ((repeat_count == 5) ||
		    ((repeat_count < 50) && !(repeat_count % 10)) ||
		    (!(repeat_count % 50)))
		{
			sprintf (fmtbuf,"%c %-14s%s, repeated %d times\n",
				 'R', from, msg, repeat_count);
		}
		else if (repeat_count > 5)
		{
			return;
		}
		/*
		 * Else leave fmtbuf unchanged
		 */
	}
	else
	{
		strcpy (last_msg, fmtbuf);
	/*
	 * If messages were previously being skipped, make a note about the
	 * final number of repeats.
	 */
		if (repeat_count > 5)
			sprintf (fmtbuf, 
				 "R %-14sLast message repeated %d times\n%s",
				 EL_NAME, repeat_count, last_msg);
		repeat_count = 1;
	}

	AppendToLog (fmtbuf);
}



static void
AppendToLog (fmtbuf)
char *fmtbuf;
/*
 * Actually append the text to the log file and to the text widget buffer
 */
{
	Arg args[10];
	XawTextBlock tb;

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
 * Clear log message buffer, then clear text window.
 */	
{
	Arg args[2];

	do_log (' ',"","");
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
	   	reply.dmm_win = 0;
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
	   	if (Visible && Override)
		{
			Visible = FALSE;
			XtPopdown (Shell);
		}
		break;
 	/*
	 * DM currently sends a defaults table, which does us very 
	 * little good.
	 */
	   case DM_DEFAULTS:
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
	int n;
/*
 * For positioning, move the shell.
 */
	n = 0;
	XtSetArg (args[n], XtNx, x);	n++;
	XtSetArg (args[n], XtNy, y);	n++;
	XtSetValues (Shell, args, n);
/*
 * For sizing, change the form inside the shell.
 */
	n = 0;
	XtSetArg (args[n], XtNwidth, w);	n++;
	XtSetArg (args[n], XtNheight, h);	n++;
	XtSetValues (Form, args, n);
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
/*
 * Broadcast out the default mask to everybody.
 */
	flag = Emask | EF_SETMASK;
	msg_send ("Everybody", MT_ELOG, TRUE, &flag, sizeof (flag));
/*
 * Now go through and fix up any processes for which debugging has been
 * requested.
 */
	usy_search (ProcTable, SendDbgMask, 0, FALSE, 0);
}





/*ARGSUSED*/
int 
SendDbgMask (proc, type, v, junk)
char *proc;
int type;
SValue *v;
long junk;
/*
 * Maybe send out a debug-enabled event mask to this process.
 */
{
	int flag = Emask | EF_SETMASK | EF_DEBUG;
	ProcInfo *pinfo = (ProcInfo *) v->us_v_ptr;

	if (pinfo->pi_enabled)
		msg_send (proc, MT_ELOG, FALSE, &flag, sizeof (flag));
	return (TRUE);
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
	char line[1000], mom[80];
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
	sprintf (mom, "Event Logger@%s", Mother);
	msg_send (mom, MT_LOG, FALSE, line, strlen (line) + 1);
}





void
NewProc (name)
char *name;
/*
 * Deal with a new process.
 */
{
	ProcInfo *pinfo = ALLOC (ProcInfo);
	Arg args[4];
	int n;
	SValue v;
/*
 * Create the widget for this process.
 */
	n = 0;
	XtSetArg (args[n], XtNleftBitmap, None);		n++;
	XtSetArg (args[n], XtNleftMargin, check_width + 7);	n++;
	pinfo->pi_menu = XtCreateManagedWidget (name, smeBSBObjectClass,
		ProcMenu, args, n);
	pinfo->pi_enabled = FALSE;
	strcpy (pinfo->pi_name, name);
	XtAddCallback (pinfo->pi_menu, XtNcallback, ToggleProc,
		(XtPointer) pinfo->pi_name);
/*
 * Add the symbol table entry, and we're done.
 */
	v.us_v_ptr = (char *) pinfo;
	usy_s_symbol (ProcTable, name, SYMT_POINTER, &v);
}




void
DeadProc (name)
char *name;
/*
 * This process is gone.
 */
{
	int type;
	SValue v;
	ProcInfo *pinfo;
/*
 * Look up this process.
 */
	if (! usy_g_symbol (ProcTable, name, &type, &v))
		return;
/*
 * Free up everything.
 */
	pinfo = (ProcInfo *) v.us_v_ptr;
	XtDestroyWidget (pinfo->pi_menu);
	free (v.us_v_ptr);
	usy_z_symbol (ProcTable, name);
}





/*ARGSUSED*/
void
ToggleProc (w, proc, xpinfo)
Widget w;
XtPointer proc, xpinfo;
/*
 * Toggle logging for this process.
 */
{
	Arg args[2];
	ProcInfo *pinfo;
	SValue v;
	int type;
/*
 * Look up this process.
 */
	if (! usy_g_symbol (ProcTable, (char *) proc, &type, &v))
	{
		printf ("Proc '%s' not in ProcInfo!\n", proc);
		return;
	}
/*
 * Toggle the debug flag and tweak the widget.
 */
	pinfo = (ProcInfo *) v.us_v_ptr;
	pinfo->pi_enabled = ! pinfo->pi_enabled;
	XtSetArg (args[0], XtNleftBitmap, pinfo->pi_enabled ? Check : None);
	XtSetValues (w, args, 1);
/*
 * Tweak this process's event mask.
 */
	SendEMask ();
}





int
PassDebug (flag, name)
int flag;
char *name;
/*
 * Return TRUE if this is a debug message that should be logged even though
 * they are disabled in general.
 */
{
	SValue v;
	int type;
	ProcInfo *pinfo;
/*
 * If this is not a debug message, or we have not heard of this process,
 * then we say no.
 */
	if ((flag & EF_DEBUG) == 0 || ! usy_g_symbol (ProcTable,name,&type,&v))
		return (FALSE);
/*
 * Otherwise we need to see what the data structure says.
 */
	pinfo = (ProcInfo *) v.us_v_ptr;
	return (pinfo->pi_enabled);
}



static void
TimestampSetup(period)
int period;		/* timestamp interval, in seconds, 0 to disable */
{
	static slot = -1;
	ZebTime t;

	if (slot >= 0)
	{
		tl_Cancel (slot);
		slot = -1;
	}
/*
 * Set up our timestamp, start timestamp on multiple of the period
 */
	if (period > 0)
	{
		tl_Time (&t);
		t.zt_MicroSec = 0;
		t.zt_Sec = ((t.zt_Sec / period) + 1) * period;
		slot = tl_AbsoluteReq (Timestamp, 0, &t, period*INCFRAC);
	}
}



/*ARGSUSED*/
static void
Timestamp (t, param)
ZebTime *t;
void *param;
/*
 * Append a timestamp message to the log
 */
{
	char buf[100];

	sprintf (buf, "T %-14sTimestamp: ", EL_NAME);
#ifdef notdef
	TC_EncodeTime (t, (t->zt_Sec % 3600 == 0) ? TC_Full : TC_TimeOnly,
		       buf + strlen(buf));
#endif
	TC_EncodeTime (t, TC_Full, buf + strlen(buf));
	strcat(buf,"\n");
	AppendToLog (buf);
}



/*ARGSUSED*/
static void
TimestampCallback (entry, client_data, call_data)
Widget entry;
XtPointer client_data;
XtPointer call_data;
{
	int period = (int)client_data;
	Arg args[2];

	TimestampSetup (period);
	XtSetArg (args[0], XtNleftBitmap, None);
	if (TimestampEntry)
		XtSetValues (TimestampEntry, args, 1);
	XtSetArg (args[0], XtNleftBitmap, Check);
	XtSetValues (entry, args, 1);
	TimestampEntry = entry;
}
