/*
 * The FCC clock program.
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
# include <X11/X.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/Label.h>

# include <defs.h>
# include <message.h>
# include <dm.h>
# include <GraphicsW.h>
# include <copyright.h>
# include <timer.h>
# include <pd.h>

RCSID ("$Id: fccclock.c,v 2.14 1998-10-28 21:21:27 corbet Exp $")

/*
 * Default resources.
 */
static String Resources[] = {
	"	*Label*font:	-*-new century schoolbook-bold-r-*-*-25-*-*-*-*-*-*-*",
	0,
};

# define FONTNAME "-*-times-bold-r-*-*-*-240-*-*-*-*-*-*"

static Widget Top;			/* The top level widget		*/
static Widget Graphics, GrShell;	/* The graphics widget		*/
static XtAppContext Actx;		/* The application context	*/
static GC Gc = 0;
static Font Tfont;
/*
 * The state of the window.
 */
enum wstate { UP, DOWN };
enum wstate WindowState = DOWN;

static int Height;
static int WindowSent = 0;	/* True once we've sent our window ID to dm */
static unsigned long Fg = 0, Bg;
static Colormap Cm;

/*
 * Our plot description.
 */
static plot_description Pd = 0, Defaults = 0;

/*
 * Forward routine definitions.
 */
int msg_handler ();
int xtEvent ();
void NewTime ();
static void SendGeometry FP((struct dm_msg *dmm));
static void dmgr_message FP((struct dm_msg *dmsg));
static void reconfig FP((struct dm_msg *dmsg));
static void StartUpdate FP((void));
static void SyncWindow FP((void));
static void ParamChange FP((struct dm_parchange *dmp));
static void NewPD FP((struct dm_pdchange *dmp));
static void ChangeState FP((enum wstate new));
static void inspectPD FP((void));


void
main (argc, argv)
int argc;
char **argv;
{
	Arg args[10];
/*
 * Connect to the message handler immediately -- Ardent weirdness
 * requires this.
 */
	usy_init ();
	dm_Setup (&argc, argv, NULL);
	msg_connect (msg_handler, dm_MessageName());
	msg_join ("TimeChange");
	msg_join (dm_GroupName());
/*
 * Get the toolkit going.
 */
	Top = XtAppInitialize (&Actx, "FccClock", NULL, ZERO, &argc,
		argv, Resources, NULL, ZERO);
/*
 * Now create a popup shell to hold the graphics widget that holds
 * our output.
 */
	XtSetArg (args[0], XtNallowShellResize, True);
	XtSetArg (args[1], XtNwinGravity, StaticGravity);
	GrShell = XtCreatePopupShell ("fccclock", applicationShellWidgetClass,
				      Top, args, 2);
/*
 * Inside this shell goes the label widget to hold the time.
 */
	XtSetArg (args[0], XtNinput, True);
	XtSetArg (args[1], XtNframeCount, 1);
	Graphics = XtCreateManagedWidget ("clock", graphicsWidgetClass,
					  GrShell, args, 2);
/*
 * Tell DM that we're here, but we don't have a window yet
 */
	dm_Greet ();
	msg_add_fd (XConnectionNumber (XtDisplay (Top)), xtEvent);
	tl_ChangeHandler (NewTime);
/*
 * Now wait for things.
 */
	msg_await ();
	exit (0);
}






int
msg_handler (msg)
struct message *msg;
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;
/*
 * Just branch out on the message type.
 */
	switch (msg->m_proto)
	{
	/*
	 * Display manager messages.
	 */
	   case MT_DISPLAYMGR:
	   	dmgr_message ((struct dm_msg *) msg->m_data);
		break;

	/*
	 * Message handler stuff.  The only thing we know how to deal
	 * with now is SHUTDOWN.
	 */
	   case MT_MESSAGE:
	   	if (tm->mh_type == MH_SHUTDOWN)
			exit (0);
		msg_log ("Unknown MESSAGE proto type: %d", tm->mh_type);
		break;
	}
	return (0);
}



int
xtEvent (fd)
int fd;
/*
 * Deal with an Xt event.
 */
{
	XEvent event;
/*
 * Deal with events as long as they keep coming.
 */
 	while (XtAppPending (Actx))
	{
		XtAppNextEvent (Actx, &event);
		XtDispatchEvent (&event);
	}
	return (0);
}






static void
dmgr_message (dmsg)
struct dm_msg *dmsg;
/*
 * Deal with a display manager message.
 */
{
	switch (dmsg->dmm_type)
	{
	/*
	 * Reconfigure.
	 */
	   case DM_RECONFIG:
		dm_Reconfig ((struct dm_reconfig *) dmsg);
	   	reconfig (dmsg);
		break;
	/*
	 * Ribbit.
	 */
	   case DM_DIE:
		exit (0);
	/*
	 * Suspend.
	 */
	   case DM_SUSPEND:
		ChangeState (DOWN);
		break;
	/*
	 * Some stuff we just ignore, but we'll get these because the
	 * DM thinks we're a graphics process (for now).
	 */
	   case DM_PDCHANGE:
	   	NewPD ((struct dm_pdchange *) dmsg);
		break;

	   case DM_PARCHANGE:
	   	ParamChange ((struct dm_parchange *) dmsg);
		break;

	   case DM_GEOMETRY:
		SendGeometry ((struct dm_msg *) dmsg);
		break;

	   case DM_DEFAULTS:
	   case DM_REALTIME:
	   case DM_HISTORY:
	   case DM_EVBIND:
	   	break;
	/*
	 * ???
	 */
	   default:
	   	msg_log ("Unknown DM Message type: %d", dmsg->dmm_type);
		break;
	}
}




static void
SendGeometry (dmm)
struct dm_msg *dmm;
{
	struct dm_msg reply;
	Dimension width, height;
	Position x, y;
	Arg args[5];
	int n;

	reply.dmm_type = DM_R_GEOMETRY;

/*
 * Generate an expose event, which should set
 * our current geometry in the shell widget.
 */
	XMapRaised (XtDisplay (Top), XtWindow (GrShell));
	SyncWindow ();
	n = 0;
	XtSetArg (args[n], XtNx, (XtArgVal)&x);	n++;
	XtSetArg (args[n], XtNy, (XtArgVal)&y);	n++;
	XtSetArg (args[n], XtNwidth, &width);	n++;
	XtSetArg (args[n], XtNheight, &height);	n++;
	XtGetValues (GrShell, args, n);
	reply.dmm_x = x;
	reply.dmm_y = y;
	reply.dmm_dx = width;
	reply.dmm_dy = height;

	msg_send (DISPLAY_MANAGER, MT_DISPLAYMGR, FALSE, &reply, sizeof (reply));
}






static void
ConfigureWindow (display, win, x, y, width, height)
Display *display;
Window win;
int x, y;
int width, height;
/*
 * Configure this window with the given geometry.  Rely on the window
 * manager to send the ConfigureNotify to the client so that the 
 * window's children re-arrange themselves as necessary.
 */
{
	unsigned int mask;
	XWindowChanges changes;

	mask = CWX | CWY | CWWidth | CWHeight;
	changes.x = x;
	changes.y = y;
	changes.width = width;
	changes.height = height;
	XConfigureWindow (display, win, mask, &changes);
	XSync (display, True);
}




static void
reconfig (dmsg)
struct dm_msg *dmsg;
/*
 * Reconfigure the window.
 */
{
/*
 * If we are not currently on-screen, put it there.
 */
	if (WindowState == DOWN)
	{
		ChangeState (UP);
		StartUpdate ();
	}
/*
 * Then reconfigure our shell geometry.
 */
	ConfigureWindow (XtDisplay(GrShell), XtWindow(GrShell), 
			 dmsg->dmm_x, dmsg->dmm_y,
			 dmsg->dmm_dx, dmsg->dmm_dy);
	XClearWindow (XtDisplay (Top), XtWindow (Graphics));
/*
 * The raise window is to get us an expose event to the graphics widget
 */
	XMapRaised (XtDisplay (Top), XtWindow (GrShell));
	SyncWindow();
}



static void
ChangeState (new)
enum wstate new;
/*
 * Set the window to this state.
 */
{
        XGCValues               gcvals;
/*
 * Change the window state.
 */
	if (new == WindowState)
		return;
	else if (new == UP)
	{
		XtPopup (GrShell, XtGrabNone);
	/*
	 * If this is the first pop-up, let dm know our window id
	 */
		if (! WindowSent)
		{
			WindowSent = 1;
			dm_SendWindowID (XtWindow(GrShell));
		}
	}
	else
	{
		XtPopdown (GrShell);
		tl_AllCancel ();
	}
	WindowState = new;
	SyncWindow ();
/*
 * If the graphics context does not exist, create it now.
 */
	if (! Gc)
	{
		XColor screen, exact;
	/*
	 * Load the default font.
	 */
		Tfont = XLoadFont (XtDisplay (Top), FONTNAME);
	/*
	 * Get our colors.
	 */
	 	Cm = DefaultColormap (XtDisplay (Top), 0);
		XAllocNamedColor (XtDisplay (Top), Cm, "white",&screen,&exact);
		Fg = screen.pixel;
		XAllocNamedColor (XtDisplay (Top), Cm, "black",&screen,&exact);
		Bg = screen.pixel;
	/*
	 * Now get the GC.
	 */
		gcvals.font = Tfont;
		gcvals.foreground = Fg;
		gcvals.background = Bg;
		Gc = XCreateGC (XtDisplay (Top), XtWindow (Graphics), 
			GCFont | GCForeground | GCBackground, &gcvals);
	}
}



static void
SyncWindow ()
/*
 * Synchronize with the window system.
 */
{
	XSync (XtDisplay (Top), False);
}







void
NewTime (t)
ZebTime *t;
/*
 * Deal with a change in ``current'' time
 */
{
/*
 * Reestablish triggers and do the plot
 */
	if (WindowState == UP)
		StartUpdate ();
}



void
UpdateClock (t, junk)
ZebTime *t;
int junk;
/*
 * Actually update the clock.
 */
{
	char dbuf[40];
/*
 * A resize may have given us a new height which we should be aware of
 */
	Height = GWHeight (Graphics);
	TC_EncodeTime (t, TC_Full, dbuf);
	strcat (dbuf, "  ");
	XDrawImageString (XtDisplay (Top), XtWindow (Graphics), Gc,
		5, Height - 5, dbuf, strlen (dbuf));
	SyncWindow ();
}



static void
StartUpdate ()
/*
 * Start Updating the time.
 */
{
/*
 * Cancel everything just to be sure.
 */
	tl_AllCancel ();
/*
 * Queue up our timer request.
 */
	tl_RelativeReq (UpdateClock, 0, 10, 10);
}



static void
NewPD (dmp)
struct dm_pdchange *dmp;
/*
 * A new plot description has arrived.
 */
{
	raw_plot_description rpd;
/*
 * If we have an old plot description, get rid of it.
 */
	if (Pd)
		pd_Release (Pd);
/*
 * Go ahead and recompile the PD now.
 */
	rpd.rp_len = dmp->dmm_pdlen;
	rpd.rp_data = dmp->dmm_pdesc;
	Pd = pd_Load (&rpd);
/*
 * Pull out anything of interest.
 */
	inspectPD ();
}



static void
ParamChange (dmp)
struct dm_parchange *dmp;
/*
 * Deal with a DM parameter change.
 */
{
	if (! Pd)
	{
		msg_log ("Param change with no PD");
		return;
	}
	pd_Store (Pd, dmp->dmm_comp, dmp->dmm_param, dmp->dmm_value,
		SYMT_STRING);
	inspectPD ();
}




static void
inspectPD ()
/*
 * Look over this PD to see if there is anything useful to us.
 */
{
	Font tmp;
	char fontname[200];
        XGCValues               gcvals;
	XColor screen, exact;
	Arg args[2];
/*
 * If there is a font stored here for us, pull it out.
 */
	if (pd_Retrieve (Pd, "clock", "font", fontname, SYMT_STRING))
	{
		if ((tmp = XLoadFont (XtDisplay (Top), fontname)) == 0)
			msg_log ("Bad font name '%s'", fontname);
		else
			Tfont = tmp;
	}
/*
 * Look for a foreground color.
 */
	if (pd_Retrieve (Pd, "clock", "foreground", fontname, SYMT_STRING))
	{
		if (! XAllocNamedColor (XtDisplay (Top), Cm, fontname,
			&screen, &exact))
			msg_log ("Bad foreground color: '%s'", fontname);
		else
		{
			if (screen.pixel != Fg)
				XFreeColors (XtDisplay (Top), Cm, &Fg, 1, 0);
			Fg = screen.pixel;
			XtSetArg (args[0], XtNforeground, Fg);
			XtSetValues (Graphics, args, 1);
		}
	}
/*
 * Look for a background color.
 */
	if (pd_Retrieve (Pd, "clock", "background", fontname, SYMT_STRING))
	{
		if (! XAllocNamedColor (XtDisplay (Top), Cm, fontname,
			&screen, &exact))
			msg_log ("Bad background color: '%s'", fontname);
		else
		{
			if (screen.pixel != Fg && screen.pixel != Bg)
				XFreeColors (XtDisplay (Top), Cm, &Bg, 1, 0);
			Bg = screen.pixel;
			XtSetArg (args[0], XtNbackground, Bg);
			XtSetValues (Graphics, args, 1);
/*
			XSetWindowBackground (XtDisplay (Top),
				XtWindow (Graphics), Bg);
 */
		}
	}
/*
 * Change our GC accordingly.
 */
	gcvals.font = Tfont;
	gcvals.foreground = Fg;
	gcvals.background = Bg;
	XChangeGC (XtDisplay (Top), Gc, GCFont|GCForeground|GCBackground,
		&gcvals);
	XClearWindow (XtDisplay (Top), XtWindow (Graphics));
}

