/*
 * Data store dump widget.
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

static char *rcsid = "$ID$";

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Box.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Shell.h>

# include <ui_param.h>
# include <ui_date.h>
# include <defs.h>
# include <message.h>
# include <copyright.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"


# define MAXPLAT	64

/*
 * Data structures for the platforms we know.
 */
char	*Names[MAXPLAT];
Widget	Entry[MAXPLAT];

/*
 * Data structures for widgets and their contents.
 */
XtAppContext Appc;
Widget	Top, Form, Viewport, Box;
Widget	WDisplay, DispText;
bool	DisplayUp = False;

/*
 * Data file variables.
 */
FILE	*Fptr;
char	Fname[30];
static struct fname
{
	int	flag;
	char	*name;
} Flags[] =
	{
		{	DPF_MOBILE,	"mobile"	},
		{	DPF_COMPOSITE,	"composite"	},
		{	DPF_DISCRETE,	"discrete"	},
		{	DPF_REGULAR,	"regular"	},
		{	DPF_SUBPLATFORM, "subplatform"	},
		{	DPF_REMOTE,	"remote-dir"	},
	};

# define NFLAG (sizeof (Flags)/sizeof (struct fname))

# ifdef __STDC__
        static void	AddPlatforms ();
        static void	CreateDSDWidget (void);
        static void	Die (void);
	static void	DumpPlatform (Platform *);
	static void	DumpChains (char *, int);
	static void	GetTimes (Platform *, ZebTime *, ZebTime *);
        static void	SetEntry (int, ZebTime *, ZebTime *);
        static int	MsgHandler (Message *);
        static void	MsgInput ();
	static int	PopupDisplay (Widget, XtPointer, XtPointer);
	static void	Rescan ();
        static Widget	CreateDisplayWidget (void);
        static void	PopdownDisplay (Widget, XtPointer, XtPointer);
# else
        static void	AddPlatforms ();
        static void	CreateDSDWidget ();
        static void	Die ();
	static void	DumpPlatform ();
	static void	DumpChains ();
	static void	GetTimes ();
        static void	SetEntry ();
        static int	MsgHandler ();
        static void	MsgInput ();
	static int	PopupDisplay ();
	static void	Rescan ();
        static Widget	CreateDisplayWidget ();
        static void	PopdownDisplay ();
# endif


main (argc, argv)
int	argc;
char	**argv;
{
/*
 * Hook into the message system and initialize data store.
 */
	usy_init ();
	if (! msg_connect (MsgHandler, "dsdwidget"))
	{
		printf ("Unable to connect to message handler.\n");
		exit (1);
	}
	if (! ds_Initialize ())
	{
		printf ("Unable to initialize data store.\n");
		exit (1);
	}
/*
 * Hook into window system.
 */
	Top = XtAppInitialize (&Appc, "dsdwidget", NULL, 0, &argc, argv,
                NULL, NULL, 0);
/*
 * Create the data file.
 */
	sprintf (Fname, "/tmp/dsdwidget%d", getpid ());

	Fptr = fopen (Fname, "w");
	fprintf (Fptr, "data file");
	fclose (Fptr);
/*
 * Make our widgets.
 */
	CreateDSDWidget ();
	CreateDisplayWidget ();
/*
 * Add platform labels and times to our widget.
 */
	AddPlatforms ();
/*
 * Display the widget and wait for something to happen.
 */
        XtRealizeWidget (Top);
        XtAppAddInput (Appc, msg_get_fd (), (XtPointer) XtInputReadMask, 
		(XtInputCallbackProc) MsgInput, (XtPointer) 0);
        XtAppMainLoop (Appc);
}


static int
MsgHandler (msg)
Message	*msg;
/*
 * Log a client event.
 */
{
/*
 * Everything is assumed to be a message handler event.
 */
        if (msg->m_proto == MT_MESSAGE)
        {
                struct mh_template *tmpl = (struct mh_template *) msg->m_data;
                if (tmpl->mh_type == MH_SHUTDOWN)
                        Die ();
        }
        return (0);
}


static void
MsgInput (junk, fd, morejunk)
XtPointer junk;
int *fd;
XtInputId *morejunk;
/*
 * Input is happening.
 */
{
	static int nexit = 0;

        if (msg_incoming (*fd))
        {
		if (++nexit > 50)
			Die ();
        }
}


static void
Die ()
/*
 * Exit gracefully.
 */
{
        msg_ELog (EF_INFO, "Exiting.");
	unlink (Fname);
        exit (0);
}



static void
DumpPlatform (p)
Platform *p;
{
	int i;
/*
 * Make sure this isn't a subplatform.
 */
	if (p->dp_flags & DPF_SUBPLATFORM)
		return;
/*
 * Open the data file.
 */	
	Fptr = fopen (Fname, "w");
/*
 * Write platform name and info to the file.
 */
	fprintf (Fptr, "Platform '%s', dir '%s'\n\tFlags 0x%x ( ", p->dp_name,
		p->dp_dir, p->dp_flags);
	for (i = 0; i < NFLAG; i++)
		if (p->dp_flags & Flags[i].flag)
			fprintf (Fptr, "%s ", Flags[i].name);
	fprintf (Fptr, " )\n");
/*
 * Write data times to the file.
 */
	if (! (p->dp_flags & DPF_SUBPLATFORM))
	{
		DumpChain ("L", p->dp_LocalData);
		if (p->dp_flags & DPF_REMOTE)
			DumpChain ("R", p->dp_RemoteData);
	}
/*
 * Close the data file.
 */
	fclose (Fptr);
}


static void
GetTimes (p, begin, end)
Platform *p;
ZebTime *begin, *end;
/*
 * Get the begin and end data times for a platform.
 */
{
	int start = p->dp_LocalData;
	DataFile *dp;
	
	if (start == NULL)
	{
		end->zt_Sec = end->zt_MicroSec = 0;	
		begin->zt_Sec = begin->zt_MicroSec = 0;	
	}
	else
	{
		dp = DFTable + start;
		*end = dp->df_end;
		while (start)
		{
			dp = DFTable + start;
			start = dp->df_FLink;
		}
		*begin = dp->df_begin;		
	}
}


DumpChain (which, start)
char *which;
int start;
/*
 * Dump out a datafile chain.
 */
{
	DataFile *dp;
	char abegin[30], aend[30];

	while (start)
	{
		dp = DFTable + start;
		TC_EncodeTime (&dp->df_begin, TC_Full, abegin);
		TC_EncodeTime (&dp->df_end, TC_TimeOnly, aend);
		fprintf (Fptr, "  %s %2d/%d '%s' %s -> %s [%d]\n", which,
			start, dp->df_use, dp->df_name,
			abegin, aend, dp->df_nsample);
		start = dp->df_FLink;
	}
}





static void
CreateDSDWidget ()
/*
 * Create the widget.
 */
{
        Arg args[5];
        int n;
        Widget button, die;
/*
 * Start with a form, of course.
 */
        Form = XtCreateManagedWidget ("form", formWidgetClass, Top, NULL, 0);
/*
 * Check the disk again button.
 */
        n = 0;
        XtSetArg (args[n], XtNlabel, "New Data?");		n++;
        XtSetArg (args[n], XtNfromHoriz, NULL);                 n++;
        XtSetArg (args[n], XtNfromVert, NULL);                  n++;
        button = XtCreateManagedWidget ("rsdisk", commandWidgetClass,
                        Form, args, n);
        XtAddCallback (button, XtNcallback, Rescan, 0);
/*
 * Also the die button.
 */
        n = 0;
        XtSetArg (args[n], XtNlabel, "Exit");                   n++;
        XtSetArg (args[n], XtNfromHoriz, button);               n++;
        XtSetArg (args[n], XtNfromVert, NULL);                  n++;
        die = XtCreateManagedWidget ("die", commandWidgetClass, Form, args, n);
        XtAddCallback (die, XtNcallback, (XtCallbackProc) Die, 
		(XtPointer) 0);
/*
 * A big viewport.
 */
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);         n++;
        XtSetArg (args[n], XtNfromVert, button);	n++;
        Viewport = XtCreateManagedWidget ("viewport", viewportWidgetClass,
                        Form, args, n);
/*
 * ...with a box in it.
 */
        n = 0;
        Box = XtCreateManagedWidget ("box", boxWidgetClass, Viewport, args, n);
}


static void
Rescan ()
/*
 * Rescan the disk for new data and display the results.
 */
{
	int i, nplat;
	ZebTime begin, end;
	Platform *p;

	msg_ELog (EF_INFO, "Rescanning disk.");	
	dsm_ShmLock ();
	for (i = 0; i < SHeader->sm_nPlatform; i++)
	{
		p = PTable + i;
		if (p->dp_flags & DPF_SUBPLATFORM)
			continue;

		GetTimes (p, &begin, &end);
		msg_ELog (EF_DEBUG, "Setting entry %d", i);
		SetEntry (nplat, &begin, &end);
		nplat++;
	}
	dsm_ShmUnlock ();
}


static void
SetEntry (index, begin, end)
int index;
ZebTime *begin, *end;
/*
 * Make a label for this platform.
 */
{
        char label[80], end_date[40], begin_date[40];
        Arg arg;
/*
 * Make the label.
 */
	msg_ELog (EF_DEBUG, "Setting entry for %s", Names[index]);
	if ((end->zt_Sec == 0) && (begin->zt_Sec == 0))
	{
		sprintf (label, "%-17s     -- None --", Names[index]);
	}
	else 
	{
# ifdef notdef
		ud_format_date (end_date, (date *)end, UDF_FULL);
		ud_format_date (begin_date, (date *)begin, UDF_FULL);
# endif
		TC_EncodeTime (end, TC_Full, end_date);
		TC_EncodeTime (begin, TC_Full, begin_date);
		sprintf (label, "%-17s     %20s -> %20s", Names[index],
			begin_date, end_date);
# ifdef notdef
	    sprintf (label, "%-17s     %d %2d:%02d:%02d -> %d %2d:%02d:%02d",
		Names[index], begin->ds_yymmdd, begin->ds_hhmmss/10000,
                (begin->ds_hhmmss/100) % 100, begin->ds_hhmmss % 100,
		end->ds_yymmdd, end->ds_hhmmss/10000,
                (end->ds_hhmmss/100) % 100, end->ds_hhmmss % 100);
# endif
	}
	msg_ELog (EF_DEBUG, "Entry %s", label);
/*
 * Stash it into the widget.
 */
        XtSetArg (arg, XtNlabel, label);
        XtSetValues (Entry[index], &arg, 1);
}


static void
AddPlatforms ()
/*
 * Add platform names and data times to dsdwidget.
 */
{
	int i, nplat = 0;
	Platform *p;
	time begin, end;

	dsm_ShmLock ();
	for (i = 0; i < SHeader->sm_nPlatform; i++)
	{
		p = PTable + i;
		if (p->dp_flags & DPF_SUBPLATFORM)
			continue;

	        Names[nplat] = usy_pstring (p->dp_name);
	/*
	 * Create a command button for a platform.
	 */
		GetTimes (p, &begin, &end);
        	Entry[nplat] = XtCreateManagedWidget (p->dp_name, 
			commandWidgetClass, Box, NULL, 0);
		XtAddCallback (Entry[nplat], XtNcallback, 
			(XtCallbackProc) PopupDisplay, (XtPointer) i);

		SetEntry (nplat, &begin, &end);
		nplat++;
	}
	dsm_ShmUnlock ();
}


static int 
PopupDisplay (w, val, junk)
Widget w;
XtPointer val, junk;
/*
 * Popup the window which display the full data for a platform.
 */
{
	int index = (int) val;
	Platform *p;
	Arg arg;

	if (! DisplayUp)
	{
	/*
 	 * Fill in the data file.
	 */
		dsm_ShmLock ();
		p = PTable + index;
		DumpPlatform (p);
		dsm_ShmUnlock ();
	/*
	 * Tell the widget about the data file.
	 */
		XtSetArg (arg, XtNstring, Fname);
		XtSetValues (DispText, &arg, 1);
	/*
	 * Force an update of the DispText widget.
	 */
		XawTextDisplay (DispText);
	/*
	 * Popup the window.
	 */
		XtPopup (WDisplay, XtGrabNone);	
		DisplayUp = True;
	}
}


static Widget
CreateDisplayWidget ()
/*
 * Build a widget for displaying the full data for a platform.
 */
{
        int     n;
        Arg     args[15];
        Widget  form, w;
/*
 * Create the popup widget shell and the form widget within it that holds
 * everything
 */
        n = 0;
        XtSetArg (args[n], XtNresize, True); 	n++;
        WDisplay = XtCreatePopupShell ("platform_data",
                topLevelShellWidgetClass, Form, args, n);

        n = 0;
        XtSetArg (args[n], XtNborderWidth, 3); n++;
        form = XtCreateManagedWidget ("displayForm", formWidgetClass, WDisplay,
                args, n);
/*
 * Button to remove widget
 */
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
        XtSetArg (args[n], XtNfromVert, NULL);		n++;
        XtSetArg (args[n], XtNlabel, "Done");		n++;
        w = XtCreateManagedWidget ("remove", commandWidgetClass, form,
                args, n);
        XtAddCallback (w, XtNcallback, PopdownDisplay, 0);
/*
 * AsciiText widget to hold the full data display. 
 */
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
        XtSetArg (args[n], XtNfromVert, w);		n++;
        XtSetArg (args[n], XtNdisplayCaret, False);	n++;
        XtSetArg (args[n], XtNscrollHorizontal, XawtextScrollWhenNeeded); n++;
        XtSetArg (args[n], XtNscrollVertical, XawtextScrollWhenNeeded); n++;
        XtSetArg (args[n], XtNwidth, 600);		n++;
        XtSetArg (args[n], XtNheight, 200);		n++;
        XtSetArg (args[n], XtNtype, XawAsciiFile);	n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit);	n++;
        XtSetArg (args[n], XtNstring, Fname);		n++;
        DispText = XtCreateManagedWidget ("display", asciiTextWidgetClass,
                form, args, n);
}


static void
PopdownDisplay (w, junk1, junk2)
Widget          w;
XtPointer       junk1, junk2;
/*
 * Remove (pop down) the full display widget
 */
{
        if (DisplayUp)
        {
                XtPopdown (WDisplay);
                DisplayUp = False;
        }
}

