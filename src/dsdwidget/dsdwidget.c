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

static char *rcsid = "$Id: dsdwidget.c,v 1.13 1993-07-01 20:13:15 granger Exp $";

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


# define MAXPLAT	1024

/*
 * Data structures for the platforms we know.
 */
char	*Names[MAXPLAT];
Widget	Entry[MAXPLAT];

char *RemoteName;
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

static void	AddPlatforms FP((void));
static void	CreateDSDWidget FP((void));
static void	Die FP((void));
static void	DumpPlatform FP((int, PlatformInfo *));
static void	DumpChains FP((char *, int));
static void	GetTimes FP((int, PlatformInfo *, ZebTime *, ZebTime *));
static void	SetEntry FP((int, ZebTime *, ZebTime *));
static int	MsgHandler FP((Message *));
static void	MsgInput ();
static int	PopupDisplay FP((Widget, XtPointer, XtPointer));
static void	Rescan FP((void));
static Widget	CreateDisplayWidget FP((void));
static void	PopdownDisplay FP((Widget, XtPointer, XtPointer));


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
	if (! (RemoteName = getenv ("REMOTE_NAME")))
		RemoteName = "Remote";
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
DumpPlatform (pid, pi)
int pid;
PlatformInfo *pi;
{
	int i;
	DataSrcInfo dsi;
/*
 * Make sure this isn't a subplatform.
 */
	if (pi->pl_SubPlatform)
		return;
/*
 * Open the data file.
 */	
	Fptr = fopen (Fname, "w");
/*
 * Write platform name and info to the file.
 */
	fprintf (Fptr, "Platform '%s'\n", pi->pl_Name);
/*
 * Write data times to the file.
 */
	ds_GetDataSource (pid, 0, &dsi);
	DumpChain ("Local", dsi.dsrc_FFile);
	if (pi->pl_NDataSrc >= 2)
	{
		ds_GetDataSource (pid, 1, &dsi);
		DumpChain (RemoteName, dsi.dsrc_FFile);
	}
/*
 * Close the data file.
 */
	fclose (Fptr);
}


static void
GetTimes (index, pi, begin, end)
int index;
PlatformInfo *pi;
ZebTime *begin, *end;
/*
 * Get the begin and end data times for a platform.
 */
{
	int start;
	DataSrcInfo dsi;
	DataFileInfo dfi;
/*
 * Go through the local data list.
 */
	ds_GetDataSource (index, 0, &dsi);
	start = dsi.dsrc_FFile;
	if (start == NULL)
	{
		end->zt_Sec = end->zt_MicroSec = 0;	
		begin->zt_Sec = begin->zt_MicroSec = 0;	
	}
	else
	{
		ds_GetFileInfo (start, &dfi);
		*end = dfi.dfi_End;
		start = dfi.dfi_Next;
		while (start)
		{
			ds_GetFileInfo (start, &dfi);
			start = dfi.dfi_Next;
		}
		*begin = dfi.dfi_Begin;
	}
/*
 * If there is no remote data, quit.
 */
	if (pi->pl_NDataSrc < 2)
		return;
	ds_GetDataSource (index, 1, &dsi);
	start = dsi.dsrc_FFile;
/*
 * See if there is remote data on a wider scale.
 */
	ds_GetFileInfo (start, &dfi);
	if (end->zt_Sec == 0 || TC_Less (*end, dfi.dfi_End))
		*end = dfi.dfi_End;
	start = dfi.dfi_Next;
	while (start)
	{
		ds_GetFileInfo (start, &dfi);
		start = dfi.dfi_Next;
	}
	if (begin->zt_Sec == 0 || TC_Less (dfi.dfi_Begin, *begin))
		*begin = dfi.dfi_Begin;
}


DumpChain (which, start)
char *which;
int start;
/*
 * Dump out a datafile chain.
 */
{
	DataFileInfo dfi;
	char abegin[30], aend[30];

	while (start)
	{
		ds_GetFileInfo (start, &dfi);
		TC_EncodeTime (&dfi.dfi_Begin, TC_Full, abegin);
		fprintf (Fptr, "%-8s '%s' %s", which, dfi.dfi_Name, abegin);
		if (dfi.dfi_NSample > 1)
		{
			TC_EncodeTime (&dfi.dfi_End, TC_TimeOnly, aend);
			fprintf (Fptr, " -> %s [%hu]", aend, dfi.dfi_NSample);
		}
		fprintf (Fptr, "\n");
		start = dfi.dfi_Next;
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
        XtAddCallback (button, XtNcallback, (XtCallbackProc) Rescan, 0);
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
	int i, nplat = 0, np;
	ZebTime begin, end;
	PlatformInfo pi;

	msg_ELog (EF_INFO, "Rescanning disk.");	
	np = ds_GetNPlat ();
	for (i = 0; i < np; i++)
	{
		ds_GetPlatInfo (i, &pi);
		if (pi.pl_SubPlatform)
			continue;

		GetTimes (i, &pi, &begin, &end);
		msg_ELog (EF_DEBUG, "Setting entry %d", i);
		SetEntry (nplat, &begin, &end);
		nplat++;
	}
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
		sprintf (label, "%-17s     -- None --         ", Names[index]);
	}
	else 
	{
		TC_EncodeTime (end, TC_Full, end_date);
		TC_EncodeTime (begin, TC_Full, begin_date);
		sprintf (label, "%-17s     %20s -> %20s", Names[index],
			begin_date, end_date);
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
	int i, nplat = 0, np;
	PlatformInfo pi;
	UItime begin, end;

	np = ds_GetNPlat ();
	for (i = 0; i < np; i++)
	{
	/*
	 * Get the platform info.  Don't bother with subplatforms, since
	 * there is no info of interest there.
	 */
		ds_GetPlatInfo (i, &pi);
		if (pi.pl_SubPlatform)
			continue;
	        Names[nplat] = usy_pstring (pi.pl_Name);
	/*
	 * Create a command button for a platform.
	 */
		GetTimes (i, &pi, (ZebTime *)&begin, (ZebTime *)&end);
        	Entry[nplat] = XtCreateManagedWidget (pi.pl_Name,
			commandWidgetClass, Box, NULL, 0);
		XtAddCallback (Entry[nplat], XtNcallback, 
			(XtCallbackProc) PopupDisplay, (XtPointer) i);

		SetEntry (nplat, (ZebTime *)&begin, (ZebTime *)&end);
		nplat++;
	}
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
	PlatformInfo pi;
	Arg arg;

	if (! DisplayUp)
	{
	/*
 	 * Fill in the data file.
	 */
		ds_GetPlatInfo (index, &pi);
		DumpPlatform (index, &pi);
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
        XtSetArg (args[n], XtNwidth, 550);		n++;
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

