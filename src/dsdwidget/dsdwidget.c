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

static char *rcsid = 
   "$Id: dsdwidget.c,v 1.17 1994-01-20 03:47:38 granger Exp $";

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
PlatformId PlatIds[MAXPLAT];
Widget	Entry[MAXPLAT];
int	NPlat = 0;

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

static void	AddPlatforms FP((char *re, bool sort));
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


static void
usage (prog)
char *prog;
{
	printf("Usage: %s [-h] [-a] [-u] [-t <title>] [regexp ...] \n",
	       prog);
	printf("If a regular expression is present, only those platforms\n");
	printf("whose names match the expression are displayed.  Any\n");
	printf("number or combination of expressions may be given.  If\n");
	printf("there are no strings to match, all of the platforms will\n");
	printf("be displayed, by default alphabetically\n");
	printf("   -h\tPrint this usage message.\n");
	printf("   -a\tAlphabetize the platforms for each matching string.\n");
	printf("   -u\tDon't alphabetize the platform names.\n");
	printf("   -t\tSpecify a title for the window.\n");
}


main (argc, argv)
int	argc;
char	**argv;
{
	char name[256];
	bool sort;
	char *title;
	int c;
	int optind, i;

	sort = TRUE;
	title = name;
	optind = 1;
	while ((optind < argc) && (strlen(argv[optind]) == 2) &&
	       (argv[optind][0] == '-'))
	{
		c = argv[optind][1];
		switch (c)
		{
		   case 'h':
			usage(argv[0]);
			exit(0);
			break;
		   case 'a':
			sort = TRUE;
			break;
		   case 'u':
			sort = FALSE;
			break;
		   case 't':
			if (optind + 1 < argc)
				title = argv[++optind];
			else
			{
				printf ("-t option needs argument\n");
				usage (argv[0]);
				exit (2);
			}
			break;
		}
		for (i = 1; i < argc - optind; ++i)
			argv[i] = argv[i + optind];
		argc -= optind;
		optind = 1;
	}
/*
 * Hook into the message system and initialize data store.  We use our pid
 * in the name since it is very possible someone will want to run different
 * displays for different sets of platforms.
 */
	usy_init ();
	sprintf (name, "dsdwidget-%d", getpid());
	if (! msg_connect (MsgHandler, name))
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
 * Hook into the window system.
 */
	Top = XtVaAppInitialize (&Appc, "dsdwidget", NULL, 0, &argc, argv,
                NULL, XtNtitle, title, NULL);
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
	if (argc < 2)
		AddPlatforms (NULL, sort);
	for (optind = 1; optind < argc; ++optind)
		AddPlatforms (argv[optind], sort);
	if (NPlat == 0)
	{
		printf ("%s: No matches found!\n", argv[0]);
		exit (1);
	}
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
	DataSrcInfo dsi;
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
	if (start == 0)
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
	char abegin[20], aend[20];

	while (start)
	{
		ds_GetFileInfo (start, &dfi);
		TC_EncodeTime (&dfi.dfi_Begin, TC_Full, abegin);
		fprintf (Fptr, "%-8s '%s' %s", which, dfi.dfi_Name, abegin);
		if (dfi.dfi_NSample > 1)
		{
			TC_EncodeTime (&dfi.dfi_End, TC_Full, aend);
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
 * In theory, our platform cache is being updated by messages broadcast
 * from the DataStore.  So all we need to do is retrieve the latest
 * platform info and reset the widget entries.
 */
{
	int i;
	ZebTime begin, end;
	PlatformInfo pi;

	msg_ELog (EF_INFO, "Updating platform info.");
	for (i = 0; i < NPlat; i++)
	{
		ds_GetPlatInfo (PlatIds[i], &pi);
		GetTimes (PlatIds[i], &pi, &begin, &end);
		msg_ELog (EF_DEBUG, "Setting entry %d", i);
		SetEntry (i, &begin, &end);
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
        char label[80], end_date[20], begin_date[20];
        Arg arg;
/*
 * Make the label.
 */
	msg_ELog (EF_DEBUG, "Setting entry for %s", Names[index]);
	if ((end->zt_Sec == 0) && (begin->zt_Sec == 0))
	{
		sprintf (label, "%-17s  -- None --         ", Names[index]);
	}
	else 
	{
		TC_EncodeTime (end, TC_Full, end_date);
		TC_EncodeTime (begin, TC_Full, begin_date);
		sprintf (label, "%-17s  %20s -> %20s", Names[index],
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
AddPlatforms (re, sort)
char *re;	/* NULL implies get them all 		*/
bool sort;	/* true if we want them alphabetized 	*/
/*
 * Add platform names and data times to dsdwidget.
 */
{
	int i, nplat;
	PlatformInfo pi;
	UItime begin, end;
	PlatformId *platforms;

	platforms = ds_GatherPlatforms (re, &nplat, sort, FALSE);
	for (i = 0; i < nplat; i++)
	{
	/*
	 * Make sure we have room for another platform
	 */
		if (NPlat >= MAXPLAT)
		{
			msg_ELog (EF_PROBLEM, "Too many platforms: %d", NPlat);
			break;
		}
	/*
	 * Get the platform info.  Don't bother with subplatforms, since
	 * there is no info of interest there.
	 */
		ds_GetPlatInfo (platforms[i], &pi);
	        Names[NPlat] = usy_pstring (pi.pl_Name);
		PlatIds[NPlat] = platforms[i];
	/*
	 * Create a command button for a platform.
	 */
		GetTimes (platforms[i], &pi, 
			  (ZebTime *)&begin, (ZebTime *)&end);
        	Entry[NPlat] = XtCreateManagedWidget (pi.pl_Name,
			commandWidgetClass, Box, NULL, 0);
		XtAddCallback (Entry[NPlat], XtNcallback, 
			       (XtCallbackProc) PopupDisplay, 
			       (XtPointer) platforms[i]);

		SetEntry (NPlat, (ZebTime *)&begin, (ZebTime *)&end);
		NPlat++;
	}
	if (platforms)
		free (platforms);
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
        XtSetArg (args[n], XtNwidth, 650);		n++;
        XtSetArg (args[n], XtNheight, 200);		n++;
        XtSetArg (args[n], XtNtype, XawAsciiFile);	n++;
        XtSetArg (args[n], XtNeditType, XawtextRead);	n++;
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

