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
static char 	*rcsid = "$ID$";

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
# include <message.h>
# include <timer.h>
# include <config.h>
# include <copyright.h>
# include "DataStore.h"
# include "dsPrivate.h"




/*
 * Data structures for the platforms we know.
 */
# define MaximumPlat 64
char *Names[MaximumPlat];
Widget Labels[MaximumPlat];
Widget MEntries[MaximumPlat];
bool Enabled[MaximumPlat];
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


# ifdef __STDC__
	static void CreateWidget (void);
	static void Die (void);
	static int MsgHandler (Message *);
	static void DoPlatforms (char *);
	static void AddPlatform (char *, int, int, time *);
	static void TogglePlat (Widget, int, int);
	static void GetColors (void);
	static void MakeLabel (int, time *, time *);
	static void SetColor (int);
	static void Arrival (PlatformId, int, time *, int);
	static void TimeOut (time *, int);
	static void MsgInput ();
# else
	static void CreateWidget ();
	static void Die ();
	static int MsgHandler ();
	static void DoPlatforms ();
	static void AddPlatform ();
	static void TogglePlat ();
	static void GetColors ();
	static void MakeLabel ();
	static void SetColor ();
	static void Arrival ();
	static void TimeOut ();
	static void MsgInput ();
# endif




main (argc, argv)
int argc;
char **argv;
{
/*
 * Hook into the message system.
 */
	usy_init ();
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
	tl_AddRelativeEvent (TimeOut, 0, 60*INCFRAC, 60*INCFRAC);
	XtRealizeWidget (Top);
	XtAppAddInput (Appc, msg_get_fd (), XtInputReadMask, MsgInput, 0);
	XtAppMainLoop (Appc);
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
	XtAddCallback (die, XtNcallback, Die, 0);
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
	time now;
	FILE *cfile; 
	char line[80], *cp, *strchr ();
	int yellow, red;
/*
 * Make sure we have a config.
 */
# ifdef notdef
	sprintf (config_file, "%s/LastData.config", LIBDIR);
# endif
	cfile = fopen (config_file, "r");
	if (cfile == NULL)
	{
		msg_ELog (EF_PROBLEM, "No config file");
		exit (1);
	}
/*
 * When are we?
 */
	tl_GetTime (&now);
/*
 * Go through and read in lines.
 */
	while (fgets (line, 80, cfile) != NULL)
	{
		if ((cp = strchr (line, ' ')) == NULL ||
			sscanf (cp + 1, "%d %d", &yellow, &red) < 2)
		{
			msg_ELog (EF_PROBLEM, "Bad config line: %s", line);
			continue;
		}
		*cp = '\0';
		AddPlatform (line, yellow, red, &now);
	}		
	fclose (cfile);
# ifdef notdef
	AddPlatform ("mesonet", &now);
	AddPlatform ("cp4", &now);
	AddPlatform ("lightning", &now);
	AddPlatform ("cp2-az-limits", &now);
	AddPlatform ("spotlight", &now);
	AddPlatform ("boundary", &now);
	AddPlatform ("n510mh", &now);
	AddPlatform ("n566na", &now);
	AddPlatform ("field-mill", &now);
# endif
}




static void
AddPlatform (name, yellow, red, now)
char *name;
int yellow, red;
time *now;
/*
 * Add a platform by this name.
 */
{
	PlatformId pid;
	time t;
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
	XtAddCallback (MEntries[NPlat], XtNcallback, TogglePlat, NPlat);
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
time *t, *now;
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
		if ((days = (TC_FccToSys (now)-TC_FccToSys (t))/(24*3600)) > 0)
			sprintf (label, "%-14s +%d %2d:%02d:%02d",
				Names[index], days, t->ds_hhmmss/10000,
				(t->ds_hhmmss/100) % 100, t->ds_hhmmss % 100);
		else
			sprintf (label, "%-17s %2d:%02d:%02d", Names[index], 
				t->ds_hhmmss/10000, (t->ds_hhmmss/100) % 100,
				t->ds_hhmmss % 100);
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
Arrival (pid, index, t, ns)
PlatformId pid;
int index, ns;
time *t;
/*
 * Data's here for this one.
 */
{
	time now;
	Arg args[2];

	tl_GetTime (&now);
	MakeLabel (index, t, &now);
# ifdef notdef
/*
 * Mark the most recent data.
 */
	XtSetArg (args[0], XtNborderWidth, 0);
	XtSetValues (Labels[Border], args, 1);
	XtSetArg (args[0], XtNborderWidth, 1);
	XtSetValues (Labels[index], args, 1);
	Border = index;
# endif
}





static void
TimeOut (t, junk)
time *t;
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
