/*
 * Quick and dirty radar status display.
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

# include <copyright.h>
# include <math.h>
# include <X11/Intrinsic.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Shell.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Cardinals.h>
# include <GraphicsW.h>
# include "defs.h"
# include "../include/message.h"
# include "../include/dm.h"
# include "../include/timer.h"
# include "RadarInfo.h"

static char *rcsid = "$Id: RadarStatus.c,v 2.3 1992-04-02 21:47:11 burghart Exp $";

# define PI		3.1415927
# define DEG_TO_RAD(x)	(((x) * 0.017453293))

/*
 * Our widgets.
 */
Widget Top, Second, Shell, Form, Wm, DateLabel, Graphics;
XtAppContext Appc;
bool Visible = FALSE;
bool Override = TRUE;

bool DataSeen = FALSE;

/*
 * Default width and height for the graphical display
 */
# define WIDTH	320
# define HEIGHT 100

# ifdef __STDC__
	static int	GetStatus (int, char *, int);
	static void	MsgInput (XtPointer, int *, XtInputId *);
	static void	MsgBCInput (XtPointer, int *, XtInputId *);
	static void	Timeout (void);
	static void	GrUpdate (RadarInfo *);
# else
	static int	GetStatus ();
	static void	MsgInput ();
	static void	MsgBCInput ();
	static void	Timeout ();
	static void	GrUpdate ();
# endif


main (argc, argv)
int argc;
char **argv;
{
	Arg args[20];
	Widget w, label;
	int xevent (), msg_event (), clearbutton (), wm (), n, bcs;
/*
 * Hook into the message system.
 */
	if (! msg_connect (msg_event, "RadarStatus"))
	{
		printf ("Unable to connect to message handler\n");
		exit (1);
	}
	if ((bcs = msg_BCSetup (0, RadarInfoPort, GetStatus)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "Unable to get radar status port");
		exit (1);
	}
/*
 * Get set up with the toolkit.
 */
	Top = XtAppInitialize (&Appc, "RadarStatus", NULL, 0, &argc, argv,
		NULL, NULL, 0);
/*
 * Create our shell.
 */
	n = 0;
	XtSetArg (args[n], XtNinput, True);	n++;
	XtSetArg (args[n], XtNoverrideRedirect, True);	n++;
	Shell = XtCreatePopupShell ("RadarStatus", topLevelShellWidgetClass,
		Top, args, n);
/*
 * Put a form inside it.
 */
	Form = XtCreateManagedWidget ("form", formWidgetClass, Shell, args, 0);
/*
 * The label.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNlabel, "Radar time: --:--:--");	n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	DateLabel = label = XtCreateManagedWidget ("datelabel", 
			labelWidgetClass, Form, args, n);
/*
 * The window manager button.
 */
	n = 0;	
	XtSetArg (args[n], XtNfromHoriz, label);	n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	XtSetArg (args[n], XtNlabel, "Ctl: DM");	n++;
	w = Wm = XtCreateManagedWidget("wm",commandWidgetClass, Form, args, n);
	XtAddCallback (Wm, XtNcallback, wm, 0);
/*
 * The second line line label.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);	n++;
	XtSetArg (args[n], XtNfromVert, label);	n++;
	XtSetArg (args[n], XtNwidth, WIDTH);	n++;
	XtSetArg (args[n], XtNlabel, "Radar offline");	n++;
	XtSetArg (args[n], XtNborderWidth, 0);		n++;
	Second = XtCreateManagedWidget ("second", labelWidgetClass,
			Form, args, n);
/*
 * The graphics portion
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, Second); n++;
	XtSetArg (args[n], XtNwidth, WIDTH); n++;
	XtSetArg (args[n], XtNheight, HEIGHT); n++;
	XtSetArg (args[n], XtNframeCount, 2); n++;
	Graphics = XtCreateManagedWidget ("graphics", graphicsWidgetClass,
			Form, args, n);
# ifdef notdef
/*
 * Tell msglib about our X connection.
 */
	msg_add_fd (XConnectionNumber (XtDisplay (Shell)), xevent);
# endif
/*
 * Instead, tell X about the msg connection.
 */
	XtAppAddInput (Appc, msg_get_fd (), XtInputReadMask, MsgInput, 0);
	XtAppAddInput (Appc, bcs, XtInputReadMask, MsgBCInput, 0);
/*
 * A timer event to turn back to "offline" if data drops out.
 */
	tl_AddRelativeEvent (Timeout, 0, 30*INCFRAC, 30*INCFRAC);
/*
 * Now we just wait.  If we were given a command line argument, assume
 * they want to see us now; otherwise we await something from DM.
 */
	if (argc > 1)
	{
		Visible = Override = TRUE;
		wm ();
	}
	XtAppMainLoop (Appc);
# ifdef notdef
	sync ();
	xevent ();
	msg_await ();
# endif
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
	if (msg_incoming (*fd))
		exit (1);
}





static void
MsgBCInput (junk, fd, morejunk)
XtPointer junk;
int *fd;
XtInputId *morejunk;
/*
 * Input is happening.
 */
{
	msg_BCHandler (*fd);
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
/*
 * Maybe it's a display manager message.
 */
	if (msg->m_proto == MT_DISPLAYMGR)
		dm_msg (msg->m_data);
/*
 * Everything else is assumed to be a message handler event.
 */	
	else if (msg->m_proto == MT_MESSAGE)
	{
		struct mh_template *tmpl = (struct mh_template *) msg->m_data;
		if (tmpl->mh_type == MH_SHUTDOWN)
			exit (0);
	}
	return (0);
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
	   	if (Visible)
		{
			Visible = FALSE;
			XtPopdown (Shell);
		}
		break;
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




static int
GetStatus (port, data, len)
int port, len;
char *data;
/*
 * Deal with a broadcast message.
 */
{
	RadarInfo *info = (RadarInfo *) data;
	char string[100];
	Arg args[2];
	int xcenter0, xcenter1, ycenter;
	static char *modes[] = { "CAL", "PPI", "COP", "RHI",
				 "??4", "??5", "??6", "??7", "SUR" };
/*
 * Update the text
 */
	sprintf (string, "Radar time: %2d:%02d:%02d",
		info->ri_last.ds_hhmmss/10000,
		(info->ri_last.ds_hhmmss/100) % 100,
		info->ri_last.ds_hhmmss % 100);
	XtSetArg (args[0], XtNlabel, string);
	XtSetValues (DateLabel, args, 1);

	sprintf (string, "Az:%-5.1f El:%-4.1f Fix:%-4.1f Mode:%s", info->ri_az,
		info->ri_el, info->ri_fixed, modes[info->ri_mode]);
	XtSetArg (args[0], XtNlabel, string);
	XtSetValues (Second, args, 1);
/*
 * Update the graphics
 */
	GrUpdate (info);

	DataSeen = TRUE;
	sync ();
}





static void
Timeout ()
/*
 * See if there's been data.
 */
{
	Arg args[2];

	if (! DataSeen)
	{
		XtSetArg (args[0], XtNlabel, "-- Radar offline --");
		XtSetValues (Second, args, 1);

		GrUpdate ((RadarInfo *) 0);
	}
	DataSeen = FALSE;
}




static void
GrUpdate (info)
RadarInfo *info;
{
	int	i, len, xcenter, ycenter, xend, yend;
	XColor	black, red, dummy;
	XGCValues	gcvals;
	static GC	fg_gc, bg_gc;
	static int	drawframe = 0;
/*
 * Quick bailout
 */
	if (! Visible)
		return;
/*
 * Get graphics contexts if we haven't already
 */
	XSynchronize (XtDisplay (Graphics), True);
	if (! fg_gc)
	{
		XAllocNamedColor (XtDisplay (Graphics), 
			DefaultColormap (XtDisplay (Graphics), 0), "black",
			&black, &dummy);
		XAllocNamedColor (XtDisplay (Graphics), 
			DefaultColormap (XtDisplay (Graphics), 0), "red",
			&red, &dummy);

		gcvals.foreground = black.pixel;
		bg_gc = XCreateGC (XtDisplay (Graphics), GWFrame (Graphics), 
			GCForeground, &gcvals);

		gcvals.foreground = red.pixel;
		gcvals.line_width = 2;
		fg_gc = XCreateGC (XtDisplay (Graphics), GWFrame (Graphics), 
			GCForeground | GCLineWidth, &gcvals);
	}
/*
 * Alternate frames each time
 */
	drawframe = (drawframe == 0) ? 1 : 0;
	GWDrawInFrame (Graphics, drawframe);
	GWClearFrame (Graphics, drawframe);
/*
 * Azimuth background
 */
	len = GWWidth (Graphics) / 4 - 5;
	xcenter = GWWidth (Graphics) / 4;
	ycenter = GWHeight (Graphics) / 2;

	for (i = 0; i < 360; i += 45)
	{
		xend = xcenter + (int)(len * cos (DEG_TO_RAD (i)));
		yend = ycenter - (int)(len * sin (DEG_TO_RAD (i)));
	
		XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), 
			bg_gc, xcenter, ycenter, xend, yend);
	}

	XDrawArc (XtDisplay (Graphics), GWFrame (Graphics), bg_gc, 
		xcenter - len, ycenter - len, 2 * len, 2 * len, 0, 64 * 360);
/*
 * Azimuth line
 */
	if (info)
	{
		xend = xcenter + (int)(len * 
				cos (0.5 * PI - DEG_TO_RAD (info->ri_az)) *
				cos (DEG_TO_RAD (info->ri_el)));
		yend = ycenter - (int)(len * 
				sin (0.5 * PI - DEG_TO_RAD (info->ri_az)) *
				cos (DEG_TO_RAD (info->ri_el)));
		XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), fg_gc, 
			xcenter, ycenter, xend, yend);
	}
/*
 * Elevation background
 */
	len = GWWidth (Graphics) / 2 - 5;
	xcenter = GWWidth (Graphics) / 2;
	ycenter = GWHeight (Graphics) - 5;

	
	for (i = 0; i <= 90; i += 30)
	{
		xend = xcenter + (int)(len * cos (DEG_TO_RAD (i)));
		yend = ycenter - (int)(len * sin (DEG_TO_RAD (i)));
	
		XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), 
			bg_gc, xcenter, ycenter, xend, yend);
	}

	XDrawArc (XtDisplay (Graphics), GWFrame (Graphics), bg_gc, 
		xcenter - len, ycenter - len, 2 * len, 2 * len, 0, 64 * 90);
/*
 * Elevation line
 */
	if (info)
	{
		xend = xcenter + (int)(len * cos (DEG_TO_RAD (info->ri_el)));
		yend = ycenter - (int)(len * sin (DEG_TO_RAD (info->ri_el)));
		XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), fg_gc, 
			xcenter, ycenter, xend, yend);
	}
/*
 * Display the frame we just drew
 */
	GWDisplayFrame (Graphics, drawframe);
}
