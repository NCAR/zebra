/*
 * Last data arrived display.
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

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Box.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Xaw/Label.h>

# include <defs.h>
# include <zl_symbol.h>		/* for usy_pstring */
# include <message.h>
# include <timer.h>
# include <config.h>
# include <copyright.h>
# include <DataStore.h>

RCSID ("$Id: LastData.c,v 2.14 1998-10-28 21:22:55 corbet Exp $")


/*
 * Data structures for the platforms we know.
 */
# define MaximumPlat 64
char *Names[MaximumPlat];
Widget Labels[MaximumPlat];
Widget MEntries[MaximumPlat];
zbool Enabled[MaximumPlat];
int Counters[MaximumPlat];
int YThresh[MaximumPlat], RThresh[MaximumPlat];
int NPlat = 0, Border = 0;

/*
 * Color stuff.
 */
Pixel Black, Yellow, Red;
# define YellowThresh	4
# define RedThresh	15


Widget Top, Form, Menu, Viewport, Box;
XtAppContext Appc;


static void CreateWidget FP ((void));
static void Die FP ((void));
static int MsgHandler FP ((Message *));
static void DoPlatforms FP ((char *));
static void AddPlatform FP ((char *, int, int, ZebTime *));
static void TogglePlat FP ((Widget, int, int));
static void GetColors FP ((void));
static void MakeLabel FP ((int, ZebTime *, ZebTime *));
static void SetColor FP ((int));
static void Arrival FP ((PlatformId, int, ZebTime *, int, UpdCode));
static void TimeOut FP ((ZebTime *, int));
static void MsgInput FP ((XtPointer, int *, XtInputId *));




main (argc, argv)
int argc;
char **argv;
{
/*
 * Hook into the message system.
 */
	if (! msg_connect (MsgHandler, "LastData"))
	{
		printf ("Unable to connect to message handler\n");
		exit (1);
	}
	ds_Initialize ();
/*
 * Get hooked into the window system.
 */
	Top = XtAppInitialize (&Appc, "LastData", NULL, 0, &argc, argv,
		NULL, NULL, 0);
/*
 * Make our widgets.
 */
	CreateWidget ();
	GetColors ();
/*
 * Add our platforms.
 */
	DoPlatforms (argv[1]);
/*
 * Our polling interrupt.
 */
	tl_RelativeReq (TimeOut, 0, 60*INCFRAC, 60*INCFRAC);
	XtRealizeWidget (Top);
	XtAppAddInput (Appc, msg_get_fd (), (XtPointer)XtInputReadMask, 
		       MsgInput, 0);
	XtAppMainLoop (Appc);
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
		msg_ELog (EF_INFO, "Exiting.");
		if (++nexit > 50)
			exit (1);
	}
}





static void
Die ()
{
	exit (0);
}



static void
CreateWidget ()
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
 * Within the form lives a menubutton.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Platforms ->");		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
	XtSetArg (args[n], XtNfromVert, NULL);			n++;
	XtSetArg (args[n], XtNmenuName, "PlatformMenu");	n++;
	button = XtCreateManagedWidget ("platforms", menuButtonWidgetClass,
			Form, args, n);
/*
 * Also the die button.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Exit");			n++;
	XtSetArg (args[n], XtNfromHoriz, button);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);			n++;
	die = XtCreateManagedWidget ("die", commandWidgetClass, Form, args, n);
	XtAddCallback (die, XtNcallback, (XtCallbackProc) Die, (XtPointer) 0);
/*
 * A menu for the menubutton.
 */
	Menu = XtCreatePopupShell ("PlatformMenu", simpleMenuWidgetClass,
			Top, NULL,0);
/*
 * A big viewport.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, button);			n++;
	Viewport = XtCreateManagedWidget ("viewport", viewportWidgetClass,
			Form, args, n);
/*
 * ...with a box in it.
 */
	n = 0;
	Box = XtCreateManagedWidget ("box", boxWidgetClass, Viewport, args, n);
}




static int
MsgHandler (msg)
Message *msg;
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
DoPlatforms (config_file)
char *config_file;
/*
 * Add all the platforms.
 */
{
	ZebTime now;
	FILE *cfile; 
	char line[80], *cp;
	int yellow, red;
/*
 * Make sure we have a config.
 */
	cfile = fopen (config_file, "r");
	if (cfile == NULL)
	{
		msg_ELog (EF_PROBLEM, "No config file");
		exit (1);
	}
/*
 * When are we?
 */
	tl_Time (&now);
/*
 * Go through and read in lines.
 */
	while (fgets (line, 80, cfile) != NULL)
	{
		if ((cp = (char *) strchr (line, ' ')) == NULL ||
			sscanf (cp + 1, "%d %d", &yellow, &red) < 2)
		{
			msg_ELog (EF_PROBLEM, "Bad config line: %s", line);
			continue;
		}
		*cp = '\0';
		AddPlatform (line, yellow, red, &now);
	}		
	fclose (cfile);
}




static void
AddPlatform (name, yellow, red, now)
char *name;
int yellow, red;
ZebTime *now;
/*
 * Add a platform by this name.
 */
{
	PlatformId pid;
	ZebTime t;
	Arg args[5];
	int n, ntime;
/*
 * Basic stuff.
 */
	Names[NPlat] = usy_pstring (name);
	Enabled[NPlat] = TRUE;
/*
 * Look up this platform and find out when data last appeared.
 */
	pid = ds_LookupPlatform (name);
	/* ignore unknown platform */
	if ( pid < 0 ) return;
	ntime = ds_DataTimes (pid, now, 1, DsBefore, &t);
	YThresh[NPlat] = yellow;
	RThresh[NPlat] = red;
/*
 * Create a menu entry.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, name);	n++;
	MEntries[NPlat] = XtCreateManagedWidget (name, smeBSBObjectClass,
		Menu, args, n);
	XtAddCallback (MEntries[NPlat], XtNcallback, 
		       (XtCallbackProc)TogglePlat, (XtPointer)NPlat);
/*
 * Also create a label.
 */
	Labels[NPlat] = XtCreateManagedWidget (name, labelWidgetClass,
				Box, NULL, 0);
	MakeLabel (NPlat, ntime > 0 ? &t : 0, now);
/*
 * Arrange for notifications on this one.
 */
	ds_RequestNotify (pid, NPlat, Arrival);
	NPlat++;
}





static void
TogglePlat (w, which, junk)
Widget w;
int which, junk;
/*
 * Toggle a platform.
 */
{
	if (Enabled[which])
	{
		Enabled[which] = FALSE;
		XtUnmanageChild (Labels[which]);
	}
	else
	{
		Enabled[which] = TRUE;
		XtManageChild (Labels[which]);
	}
}





static void
GetColors ()
/*
 * Find our colors.
 */
{
	XColor xc, exact;
	Display *disp = XtDisplay (Top);

	XAllocNamedColor (disp, DefaultColormap (disp, 0), "black",&xc,&exact);
	Black = xc.pixel;
	XAllocNamedColor (disp, DefaultColormap (disp, 0), "red", &xc, &exact);
	Red = xc.pixel;
	XAllocNamedColor (disp, DefaultColormap (disp, 0),"orange",&xc,&exact);
	Yellow = xc.pixel;
}




static void
MakeLabel (index, t, now)
int index;
ZebTime *t, *now;
/*
 * Make a label for this platform.
 */
{
	char label[80];
	Arg args[2];
	int days;
/*
 * Make the label.
 */
	if (t)
	{
		char atime[30];
		TC_EncodeTime (t, TC_TimeOnly, atime);
		if ((days = (TC_ZtToSys (now)-TC_ZtToSys (t))/(24*3600)) > 0)
			sprintf (label, "%-14s +%d %s",
				Names[index], days, atime);
		else
			sprintf (label, "%-17s %s", Names[index], atime);
	}
	else
		sprintf (label, "%-12s    -- Never --", Names[index]);
/*
 * Stash it into the widget.
 */
	XtSetArg (args[0], XtNlabel, label);
	XtSetValues (Labels[index], args, 1);
/*
 * Set the counter.
 */
	Counters[index] = (t && ! days) ? 0 : 999;
	SetColor (index);
}





static void
SetColor (index)
int index;
/*
 * Set the color on this widget.
 */
{
	Arg args[2];
	Pixel color;

	if (Counters[index] < YThresh[index])
		color = Black;
	else
		color = (Counters[index] < RThresh[index]) ? Yellow : Red;

	XtSetArg (args[0], XtNforeground, color);
	XtSetValues (Labels[index], args, 1);
}





static void
Arrival (pid, index, t, ns, code)
PlatformId pid;
int index, ns;
ZebTime *t;
UpdCode code;
/*
 * Data's here for this one.
 */
{
	ZebTime now;
	Arg args[2];

	tl_Time (&now);
	MakeLabel (index, t, &now);
}





static void
TimeOut (t, junk)
ZebTime *t;
int junk;
/*
 * Check things.
 */
{
	int i;

	for (i = 0; i < NPlat; i++)
		if (++Counters[i] == YThresh[i] || Counters[i] == RThresh[i])
			SetColor (i);
}
