/*
 * The zeb graphics process.
 */
static char *rcsid = "$Id: GraphProc.c,v 2.10 1991-11-13 22:03:24 corbet Exp $";
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
# include <X11/cursorfont.h>
# include <ui.h>
# include <fcntl.h>

# include "../include/config.h"
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/dm.h"
# include "../include/pd.h"
# include "../include/GraphicsW.h"
# include "../include/timer.h"
# include "../include/DataStore.h"
# include "../include/copyright.h"

# include "gp_cmds.h"
# include "EventQueue.h"
# include "GC.h"
# include "GraphProc.h"

/*
 * Default resources.
 */
static String Resources[] = {
	"	*input:		True",
	"	*Label*font:	-*-helvetica-bold-r-*-*-*-120-*-*-*-*-*-*",
	"	*Toggle*font:	-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
	"	*Text*font:	-*-helvetica-medium-r-*-*-*-120-*-*-*-*-*-*",
	"	*title*font:	-*-times-bold-r-*-*-17-120-*-*-*-*-*-*",
	"	*arrow*font:	-*-symbol-*-*-*-*-*-120-*-*-*-*-*-*",
	"	*Text*height:	20",
	"	*title*borderWidth: 0",
/*	"	*Command*font:	-*-times-medium-i-*-*-*-120-*-*-*-*-*-*", */
	0,
};

/*
 * Globals.
 */
char Ourname[40];	/* What is our process name?	*/

/*
 * Definition of globals referenced in GraphProc.h
 */
Widget	Top;				/* The top level widget		*/
Widget	Graphics, GrShell;		/* The graphics widget		*/
Display	*Disp;				/* Our display			*/
char 	FrameFilePath[40];		/* Path to FrameFile		*/
int	FrameFileFlag = 0;		/* True when file should be opened */
int	FrameCount = 1;			/* Number of frames		*/
int	MaxFrames;			/* Maximum number of frames	*/	
int	DisplayFrame = 0;		/* Frame being displayed	*/
int	DrawFrame = 0;			/* Frame to draw next		*/
XtAppContext	Actx;			/* The application context	*/
bool	Abort = FALSE;			/* Has the current plot been stopped?*/
bool	HoldProcess = FALSE;		/* Plotting on hold?		*/
stbl	Vtable;				/* The variable table		*/
plot_description	Pd = 0;		/* Current plot description	*/
plot_description	Defaults = 0;	/* Plot description info	*/
time	PlotTime;			/* The current plot time.	*/
int	Event_X, Event_Y;		/* Button event locations	*/
enum pmode	PlotMode = NoMode;
enum wstate	WindowState = DOWN;
bool	MovieMode = FALSE;
Cursor	BusyCursor, NormalCursor;	/* Our cursors			*/
float	Xlo, Xhi, Ylo, Yhi;
float	Alt;
/*
 * Post processing mode stuff.
 */
int	PostProcMode = FALSE;
time	PostProcTime;

/*
 * Definition of the global graphics context in GC.h
 */
GC	Gcontext;

/*
 * Saved versions of the "command line" parameters.
 */
static int Argc;
static char **Argv;

/*
 * Forward routine definitions.
 */
int	msg_handler (), dispatcher (), xtEvent ();
void	SendEndpoints ();

static void DMButton (), UiErrorReport (), UiPfHandler ();

extern void Ue_PointerEvent (), Ue_ButtonUp (), Ue_KeyEvent ();
extern void Ue_MotionEvent ();

/*
 * Routines called through the event queue mechanism.
 */
void eq_reconfig (), eq_sync ();

# ifdef __STDC__
	static void NewTime (time *);
# else
	static void NewTime ();
# endif




GPShutDown ()
/*
 * Finish up and quit.
 */
{
	int i;
	char filename[100];
	
	ui_finish ();
# ifdef SHM
	RP_ZapSHMImage (Graphics);
	if(GWShmPossible(Graphics))
		for(i = 0; i < FrameCount; i++)
			GWZapShmPixmap(Graphics, i);
# endif
	sprintf(filename, "%s/%s%dFrameFile", FrameFilePath, Ourname, getpid());
	unlink(filename);
	exit (0);
}






main (argc, argv)
int argc;
char **argv;
{
	char loadfile[200];
/*
 * The first argument is always supposed to be our process name.
 */
	strcpy (Ourname, argv[0]);
/*
 * Connect to the message handler immediately -- Ardent weirdness
 * requires this.
 */
	msg_connect (msg_handler, Ourname);
	msg_join ("Graphproc");
	msg_join ("TimeChange");
	msg_DeathHandler (GPShutDown);
/*
 * Hand off our information to the UI, and initialize things.
 */
	fixdir ("GP_LOAD_FILE", LIBDIR, "graphproc.lf", loadfile);
	ui_init (loadfile, FALSE, TRUE);
	Argc = argc;  Argv = argv;
	ui_setup ("Graphproc", &Argc, Argv, (char *) Resources);
/*
 * Initialize the data store.
 */
	if (! ds_Initialize ())
	{
		msg_ELog (EF_EMERGENCY, "Data store initialize failed");
		exit (1);
	}
/*
 * Now we have to go into the UI, and finish our setup later.  This is
 * essentially a kludge designed to keep UI from trying to open tty
 * sources to a terminal that doesn't exist (or which should not be
 * mucked with).
 */
	ui_get_command ("initial", Ourname, dispatcher, 0);
	GPShutDown ();
}




finish_setup ()
/*
 * Finish the rest of the setup, now that the UI has been initialized and
 * ui$init run.
 */
{
	Arg args[10];
	void Ue_el ();
	static XtActionsRec actions[] =
	{
		{ "ue_pointer_event",	Ue_PointerEvent	},
		{ "ue_key_event",	Ue_KeyEvent	},
		{ "ue_button_up",	Ue_ButtonUp	},
		{ "ue_el",		Ue_el		},
		{ "ue_motion",		Ue_MotionEvent	},
	};
	int type[5], pd_defined (), pd_param (), pd_paramsearch();
	int pd_removeparam (), substr_remove(), strlength ();
	char *initfile, perf[80];
/*
 * Force a shift into window mode, so we can start with the fun stuff.
 */
	uw_ForceWindowMode ((char *) 0, &Top, &Actx);
	XtAppAddActions (Actx, actions, FIVE);
# ifdef notdef
	XtRegisterGrabAction (Ue_PointerEvent, True,
	      ButtonPressMask|ButtonReleaseMask, GrabModeAsync, GrabModeAsync);
# endif
/*
 * If there is a parameter left on the "command line", it will be the name
 * of an initialization file to read.
 */
	if (Argc > 1)
		initfile = Argv[1];
	else
		initfile = "../gp/Widgets";
	Vtable = usy_g_stbl ("ui$variable_table");
/*
 * Now create a popup shell to hold the graphics widget that holds
 * our output.
 */
	XtSetArg (args[0], XtNinput, True);
	XtSetArg (args[1], XtNoverrideRedirect, True);
	GrShell = XtCreatePopupShell ("grshell", topLevelShellWidgetClass,
		Top, args, 2);
/*
 * Inside this shell goes the graphics widget itself.
 */
	Graphics = XtCreateManagedWidget ("graphics", graphicsWidgetClass,
		GrShell, NULL, 0);
/*
 * Cursors.
 */
	Disp = XtDisplay (Top);
	NormalCursor = XCreateFontCursor (Disp, XC_draft_small);
	BusyCursor = XCreateFontCursor (Disp, XC_watch);
/*
 * Module initializations.
 */
	ct_Init ();		/* Color tables		*/
	Ue_Init ();		/* User event handling	*/
	I_init ();		/* Icons		*/
	lw_InitWidgets ();	/* Limit widgets	*/
	InitDataMenu ();	/* Data available menu	*/
	pw_InitPos ();		/* Position Widget	*/
/*
 * Tell DM that we're here.
 */
	greet_dm ();
/*
 * Graphics context
 */
	Gcontext = XCreateGC (XtDisplay (Graphics), XtWindow (Graphics), 
		0, NULL);
/*
 * Set up our event handlers.
 */
	lle_AddFD (msg_get_fd (), msg_incoming);
	lle_AddFD (XConnectionNumber (Disp), xtEvent);
/*
 * Command line functions.
 */
	type[0] = type[1] = type[2] = type[3] = type[4] = SYMT_STRING;
	uf_def_function ("pd_param", 3, type, pd_param);
	uf_def_function ("pd_paramsearch", 4, type, pd_paramsearch);
	uf_def_function ("pd_defined", 2, type, pd_defined);
	uf_def_function ("pd_removeparam", 2, type, pd_removeparam);
	uf_def_function ("substr_remove", 2, type, substr_remove);
	uf_def_function ("strlength", 1, type, strlength);
/*
 * Redirect UI output.
 */
	ui_ErrorOutputRoutine (UiErrorReport);
	ui_OutputRoutine (UiPfHandler, UiPfHandler);
/*
 * More initialization
 */
	mc_DefMovieWidget ();		/* Movie control	*/
	fc_InvalidateCache ();		/* Clear frame cache	*/
	tl_ChangeHandler (NewTime);
/*
 * Indirect variables.
 */
	usy_c_indirect (Vtable, "ourname", Ourname, SYMT_STRING, 40);
	usy_c_indirect (Vtable, "holdprocess", &HoldProcess, SYMT_BOOL, 0);
/*
 * Pull in the widget definition file.
 */
	sprintf (perf, "read %s", initfile);
	ui_perform (perf);
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
	   	dm_message ((struct dm_msg *) msg->m_data);
		break;

	/*
	 * Message handler stuff.  The only thing we know how to deal
	 * with now is SHUTDOWN.
	 */
	   case MT_MESSAGE:
	   	if (tm->mh_type == MH_SHUTDOWN)
			GPShutDown ();
		msg_ELog (EF_PROBLEM, "Unknown MESSAGE proto type: %d",
			tm->mh_type);
		break;
	 /*
	  * Data store.
	  */
	    case MT_DATASTORE:
	    	ds_DSMessage (msg);
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




greet_dm ()
/*
 * Send the greeting to the display manager.
 */
{
	struct dm_hello dmh;
	Arg args[10];
	int i = 0;
/*
 * MAJOR KLUDGERY: Realize our graphics widget quickly, so that we will
 *		   have a window to send back to the display manager.  Then
 *		   take it off the screen again.  We give it huge coords so
 *		   that the user hopefully never actually sees it.
 */
	XtSetArg (args[i], XtNx, 5000);  i++;
	XtSetArg (args[i], XtNy, 5000);  i++;
	XtSetArg (args[i], XtNwidth, 50);  i++;
	XtSetArg (args[i], XtNheight, 50);  i++;
	XtSetArg (args[i], XtNallowShellResize, True); i++;
	XtSetValues (GrShell, args, i);
	ChangeState (UP);
	ChangeState (DOWN);
/*
 * Now send the message.
 */
	dmh.dmm_type = DM_HELLO;
	dmh.dmm_win = XtWindow (GrShell);
	msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &dmh, sizeof (dmh));
}




dispatcher (junk, cmds)
int junk;
struct ui_command *cmds;
/*
 * The command dispatcher.  Assume it is RUN for now.
 */
{
	static bool first = TRUE;
	extern void mc_MovieRun ();
	struct dm_event dme;

	switch (UKEY (*cmds))
	{
	/*
	 * RUN is our hook through UI to get things going.  This should only
	 * happen once, but let's guard just to be sure.
	 */
	   case GPC_RUN:
	   	if (first++)
			finish_setup ();
		else
			msg_ELog (EF_PROBLEM, "Somebody typed RUN again");
		lle_MainLoop ();
		break;
	/*
	 * DM for sending literal commands back to the display manager.
	 */
	   case GPC_DM:
		dme.dmm_type = DM_EVENT;
		if (cmds[1].uc_vptype != SYMT_STRING)
		{
			msg_ELog (EF_PROBLEM, "Non-string DM cmd: '%s'",
				cmds[1].uc_text);
			break;
		}
		strcpy (dme.dmm_data, UPTR (cmds[1]));
		msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &dme,
			sizeof (dme));
		break;
	/*
	 * Change a PD parameter.
	 */
	   case GPC_PARAMETER:
		parameter (UPTR (cmds[1]), UPTR (cmds[2]), UPTR (cmds[3]));
		break;

	/*
	 * Altitude stepping.
	 */
	   case GPC_ALTSTEP:
	   	alt_Step (UINT (cmds[1]));
		break;
# if C_CAP_OVERLAY
	/*
	 * Predefined features.
	 */
	   case GPC_FEATURE:
	   	ov_Feature (cmds + 1);
		break;
# endif
	/*
	 * Movie control.
	 */
	   case GPC_MOVIE:
	   	if (UKEY (cmds[1]))
			Eq_AddEvent (PDisplay, mc_MovieRun, 0, 0, Bounce);
		else
			mc_MovieStop ();
		break;
	/*
	 * Box drawing.
	 */
	   case GPC_DRAWBOX:
	   	rb_Box (cmds + 1);
		break;
	/*
	 * Push a new set of coords.
	 */
	   case GPC_PUSHCOORDS:
	   	pc_PushCoords (cmds + 1);
		break;
	/*
	 * Pop them off again.
	 */
	   case GPC_POPCOORDS:
	   	pc_PopCoords ();
		break;
	/*
	 * Rubberband a line
	 */
	   case GPC_DRAWLINE:
		rb_Line (cmds + 1);
		break;
	/*
	 * Ship endpoints off to the associated cross-section graphics
	 * process
	 */
	   case GPC_SENDENDPOINTS:
		SendEndpoints (cmds + 1);
		break;
	/*
	 * Activate a limit widget.
	 */
	   case GPC_ACTIVATE:
	   	lw_ActivateWidget (UINT (cmds[1]), cmds + 2);
		break;
	/*
	 * Draw a polyline.
	 */
	   case GPC_POLYLINE:
	   	rb_PolyLine (cmds + 1);
		break;
	/*
	 * Move a component to a new position.
	 */
	   case GPC_MOVECOMP:
		if (Pd)
		{
		   	pd_MoveComponent (Pd, UPTR (cmds[1]), UINT (cmds[2]));
			fc_InvalidateCache ();
			I_DoIcons ();
			if (MovieMode)
				mc_ParamChange (); /* Hope this works */
			else
				pc_PlotHandler ();
			Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
		}
		break;
	/*
	 * Get a position and display it in its widget.
	 */
	   case GPC_GETPOSITION:
		pw_PosStatus ();
		break;
	/*
	 * "Should never happen"
	 */
	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown kw %d", UKEY (*cmds));
		break;
	}
	return (TRUE);
}





dm_message (dmsg)
struct dm_msg *dmsg;
/*
 * Deal with a display manager message.
 */
{
	struct dm_dial *dmd;
	struct dm_history *dmh;

	switch (dmsg->dmm_type)
	{
	/*
	 * Reconfigure.
	 */
	   case DM_RECONFIG:
	   	Eq_AddEvent (PUrgent, eq_reconfig, dmsg, sizeof (*dmsg),
			Override);
		break;
	/*
	 * Ribbit.
	 */
	   case DM_DIE:
	   	msg_ELog (EF_DEBUG, "DM decreed shutdown");
		GPShutDown ();
	/*
	 * Suspend.
	 */
	   case DM_SUSPEND:
		ChangeState (DOWN);
		pc_CancelPlot ();
		break;
	/*
	 * Load a new plot description.
	 */
	   case DM_PDCHANGE:
	   	ChangePD ((struct dm_pdchange *) dmsg);
		break;
	   case DM_PARCHANGE:
	   	ChangeParam ((struct dm_parchange *) dmsg);
		break;
	/*
	 * History mode.
	 */
	   case DM_HISTORY:
	   	dmh = (struct dm_history *) dmsg;
	   	HistoryMode (&dmh->dmm_time);
		break;
	/*
	 * Real time mode.
	 */
	   case DM_REALTIME:
	   	RealTimeMode ();
		break;
	/*
	 * Default table.
	 */
	   case DM_DEFAULTS:
		ChangeDefaults ((struct dm_pdchange *) dmsg);
		break;
	/*
	 * Change of event bindings.
	 */
	   case DM_EVBIND:
	   	Ue_NewBinding ((struct dm_ebchange *) dmsg);
		break;
	/*
	 * Dial events.
	 */
	   case DM_DIAL:
		dmd = (struct dm_dial *) dmsg;
		DialEvent (dmd);
		break;
	/*
	 * ???
	 */
	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown DM Message type: %d",
			dmsg->dmm_type);
		break;
	}
}





void
eq_reconfig (dmsg, len)
struct dm_msg *dmsg;
int len;
/*
 * Reconfigure the window.
 */
{
	Arg args[10];
	bool schanged, wchanged;
/*
 * Figure out if anything really important has changed.
 */
	schanged = (dmsg->dmm_dx != GWWidth (Graphics)) ||
			(dmsg->dmm_dy != GWHeight (Graphics));
	wchanged = (WindowState == DOWN);
/*
 * Go through and set the new values for the shell.
 */
 	XtSetArg (args[0], XtNx, dmsg->dmm_x);
	XtSetArg (args[1], XtNy, dmsg->dmm_y);
	XtSetValues (GrShell, args, TWO);

	XtSetArg (args[0], XtNwidth, dmsg->dmm_dx);
	XtSetArg (args[1], XtNheight, dmsg->dmm_dy);
	XtSetValues (Graphics, args, TWO);
/*
 * If we are not currently on-screen, put it there.  Also set the cursor 
 * to our normal value.
 */
	if (WindowState == DOWN)
		ChangeState (UP);
	XDefineCursor (Disp, XtWindow (Graphics), NormalCursor);
/*
 * If nothing drastic has changed, we can quit now and not redraw everything.
 */
	if (! schanged && ! wchanged)
		return;
/*
 * Invalidate the frame cache if the window size has changed.
 */
	if (schanged)
		fc_InvalidateCache ();
/*
 * Force the window to the bottom.
 */
# ifdef notdef
	XLowerWindow (XtDisplay (Top), XtWindow (GrShell));
# endif
/*
 * Force a redisplay.
 */
	if (Pd)
	{
		Eq_AddEvent (PDisplay, I_DoIcons, NULL, 0, Bounce);
		Eq_AddEvent (PDisplay, pc_PlotHandler, NULL, 0, Override);
	}
	/* ...... */
}




ChangeState (new)
enum wstate new;
/*
 * Set the window to this state.
 */
{
/*
 * Change the window state.
 */
	if (new == WindowState)
		return;
	else if (new == UP)
		XtPopup (GrShell, XtGrabNone);
	else
		XtPopdown (GrShell);
	WindowState = new;
	sync ();
}




void
eq_sync ()
/*
 * Synchronize with the window system.
 */
{
	XSync (Disp, False);
}




void eq_ReturnPD ()
/*
 * Send our PD back to the display manager -- it has changed locally.
 */
{
	struct dm_pdchange *dmp;
	raw_plot_description *rpd = pd_Unload (Pd);
	int len = sizeof (struct dm_pdchange) + rpd->rp_len;
/*
 * Allocate a sufficiently big pdchange structure.
 */
	dmp = (struct dm_pdchange *) malloc (len);
/*
 * Move over the stuff.
 */
	dmp->dmm_type = DM_PDCHANGE;
	dmp->dmm_pdlen = rpd->rp_len;
	memcpy (dmp->dmm_pdesc, rpd->rp_data, rpd->rp_len);
	msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, dmp, len);
/*
 * (1/29/91 jc -- sigh) Free the memory we used.
 */
	pd_RPDRelease (rpd);
	free (dmp);
}





void
sync ()
/*
 * Arrange for a synchronize to happen.
 */
{
	Eq_AddEvent (PWhenever, eq_sync, 0, 0, Bounce);
}



void
eq_ResetAbort ()
/*
 * Reset the abort flag.
 */
{
	Abort = FALSE;
}




ChangePD (dmp)
struct dm_pdchange *dmp;
/*
 * A new plot description has arrived.
 */
{
	raw_plot_description rpd;
/*
 * If we have an old plot description, get rid of it.  Also cancel any
 * pending plot activity and free the colors we were using.
 */
	if (Pd)
	{
		pd_Release (Pd);
		pc_CancelPlot ();
		ct_FreeColors ();
	}
/*
 * Go ahead and recompile the PD now.
 */
	rpd.rp_len = dmp->dmm_pdlen;
	rpd.rp_data = dmp->dmm_pdesc;
	Pd = pd_Load (&rpd);
/*
 * Invalidate the frame cache.
 */
	fc_InvalidateCache ();
/*
 * Now we need to set up to display the new PD, and also to redo icons.
 */
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
	Eq_AddEvent (PDisplay, I_DoIcons, 0, 0, Bounce);
}





ChangeParam (dmp)
struct dm_parchange *dmp;
/*
 * Change one parameter in our plot description.
 */
{
	char	*par;
/*
 * Make sure this makes sense.
 */
	if (! Pd)
	{
		msg_ELog (EF_PROBLEM, "Param change (%s/%s/%s) with no PD",
			dmp->dmm_comp, dmp->dmm_param, dmp->dmm_value);
		return;
	}
/*
 * Store the new parameter.
 */
	pd_Store (Pd, dmp->dmm_comp, dmp->dmm_param, dmp->dmm_value, 
		SYMT_STRING);
/*
 * Now reset things.
 */
	par = dmp->dmm_param;
	Eq_AddEvent (PDisplay, pc_ParamChange, par, 1 + strlen(par), Augment);
}





parameter (comp, param, value)
char *comp, *param, *value;
/*
 * Change a parameter on a window.
 */
{
/*
 * Sanity check.
 */
	if (! Pd)
	{
		msg_ELog (EF_PROBLEM, "Param change (%s/%s/%s) with no PD",
			comp, param, value);
		return;
	}
/*
 * Do the change.
 */
	pd_Store (Pd, comp, param, value, SYMT_STRING);
/*
 * Now reset things.
 */
	Eq_AddEvent (PDisplay, pc_ParamChange, param, strlen (param) + 1,
			Augment);
/*
 * We'll also eventually want to ship the PD back to DM.
 */
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
}


ChangeDefaults (dmp)
struct dm_pdchange *dmp;
/*
 * A new defaults table has arrived.
 */
{
	raw_plot_description rpd;
	plot_description pd;
	int xxx;
/*
 * Go ahead and recompile the PD now.
 */
	rpd.rp_len = dmp->dmm_pdlen;
	rpd.rp_data = dmp->dmm_pdesc;
	pd = pd_Load (&rpd);
	pda_StorePD (pd, "defaults");
/*
 * Redisplay with reinitialization of timer stuff, since anything could 
 * have changed.  (Maybe this can be made smarter in the future?)
 */
	if (Pd)
		Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
}







HistoryMode (when)
time *when;
/*
 * Go into history mode.
 */
{
/*
 * Sanity checking time.
 */
	if (! Pd)
	{
		msg_ELog (EF_PROBLEM,
			"History mode requested with no plot description!");
		return;
	}
/*
 * Store the new plot-mode
 */
	pd_Store (Pd, "global", "plot-mode", "history", SYMT_STRING);
/*
 * Stash the plot time into the PD.
 */
	pd_Store (Pd, "global", "plot-time", (char*) when, SYMT_DATE);
	PlotTime = *when;
/*
 * Save post processing time if necessary.
 */
	if (PostProcMode)
		PostProcTime = *when;
/*
 * Now reset things.
 */
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
/*
 * Send the new pd back to the display manager.
 */
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
}





DialEvent (dmd)
struct dm_dial *dmd;
/*
 * Deal with a dial event.
 */
{
/*
 * Figure out what they want.
 */
	if (! strcmp (dmd->dmm_param, "time"))
		DialTime (dmd->dmm_motion);
	else if (! strcmp (dmd->dmm_param, "movieframe"))
		mc_Dial (dmd->dmm_motion);
	else if (! strcmp (dmd->dmm_param, "altitude"))
		alt_Step (dmd->dmm_motion);
	else
	{
		msg_ELog (EF_PROBLEM, "Unknown dial param: %s",dmd->dmm_param);
		return;
	}
}





DialTime (motion)
int motion;
/*
 * Simple (for now) time control through dials.
 */
{
	if (motion > 0)
		pmu_dadd (&PlotTime.ds_yymmdd, &PlotTime.ds_hhmmss, 100);
	else
		pmu_dsub (&PlotTime.ds_yymmdd, &PlotTime.ds_hhmmss, 100);
	HistoryMode (&PlotTime);
}









RealTimeMode ()
/*
 * Go into real time mode.
 */
{
/*
 * Sanity checking time.
 */
	if (! Pd)
	{
		msg_ELog (EF_PROBLEM,
			"Real-time mode requested with no plot description!");
		return;
	}
/*
 * If we're already in real time mode, do nothing.
 */
	if (PlotMode == RealTime)
		return;
/*
 * Store the new plot-mode
 */
	pd_Store (Pd, "global", "plot-mode", "real-time", SYMT_STRING);
/*
 * Now reset things.
 */
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
/*
 * Send the new pd back to the display manager.
 */
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
}




static void
NewTime (t)
time *t;
/*
 * Deal with a change in ``current'' time
 */
{
/*
 * If we're not in real-time mode, we don't care
 */
	if (PlotMode != RealTime)
		return;
/*
 * Redo the plot
 */
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
}






pd_defined (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * the pd_defined CLF.
 *
 *	pd_defined (comp, param)
 */
{
	char junk[200];

	*rett = SYMT_BOOL;
	if (! Pd)
		retv->us_v_int = FALSE;
	else 
		retv->us_v_int = pd_Retrieve (Pd, argv[0].us_v_ptr,
			argv[1].us_v_ptr, junk, SYMT_STRING);
}


pd_removeparam (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * the pd_removeparam CLF.
 *
 *	pd_removeparam (comp, param)
 */
{
	*rett = SYMT_BOOL;
	pd_RemoveParam (Pd, argv[0].us_v_ptr, argv[1].us_v_ptr);
	retv->us_v_int = TRUE;
}






pd_param (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * The pd_param command line function.
 *
 *	pd_param (pd, comp, param, type)
 */
{
	char tmp[500];
	int type = uit_int_type (argv[2].us_v_ptr);

	*rett = SYMT_STRING;
	if (! Pd)
		retv->us_v_ptr = usy_string ("No PD");
	else if (type == SYMT_UNDEFINED)
		retv->us_v_ptr = usy_string ("Bad type");
	else if (! pd_Retrieve (Pd, argv[0].us_v_ptr, argv[1].us_v_ptr, 
			tmp, type))
		retv->us_v_ptr = usy_string ("(Undefined)");
	else if (type == SYMT_STRING)
		retv->us_v_ptr = usy_string (tmp);
	else
	{
		*rett = type;
		memcpy (retv, tmp, sizeof (date));	/* XXX */
	}
}
	
pd_paramsearch (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * The pd_paramsearch command line function.
 *
 *	pd_paramsearch (comp, param, qual, type)
 */
{
	char tmp[500];
	int type = uit_int_type (argv[3].us_v_ptr);

	*rett = SYMT_STRING;
	if (! Pd)
		retv->us_v_ptr = usy_string ("No PD");
	else if (type == SYMT_UNDEFINED)
		retv->us_v_ptr = usy_string ("Bad type");
	else if (! pda_Search (Pd, argv[0].us_v_ptr, argv[1].us_v_ptr,
			argv[2].us_v_ptr, tmp, type))
		retv->us_v_ptr = usy_string ("(Undefined)");
	else if (type == SYMT_STRING)
		retv->us_v_ptr = usy_string (tmp);
	else
	{
		*rett = type;
		memcpy (retv, tmp, sizeof (date));	/* XXX */
	}
}


substr_remove (narg, argv, argt, retv, rett)
int 	narg, *argt, *rett;
union usy_value	*argv, *retv;
/*
 * Command line function to remove a substring from a string.
 *
 *	substr_remove (string, substring)
 *
 *	where the string is a list of items separated by commas.
 */
{
	int	nstr, i;
	char	*string[30], *tmp;
	
	tmp = (char *) malloc (500 * sizeof (char));
	nstr = CommaParse (argv[0].us_v_ptr, string);
	tmp[0] = '\0';
	for (i = 0; i < nstr; i ++)
	{
		if (strcmp (string[i], argv[1].us_v_ptr) == 0)
			continue;
		if (strlen (tmp) > 0)
			tmp = strcat (tmp, ",");
		tmp = strcat (tmp, string[i]);
	}
	*rett = SYMT_STRING;
	retv->us_v_ptr = usy_string (tmp);
	free (tmp);
}
	

strlength (narg, argv, argt, retv, rett)
int 	narg, *argt, *rett;
union usy_value	*argv, *retv;
/*
 * Command line function to return the length of a string.
 *
 *	strlength (string)
 */
{
	int	i;
	
	i = strlen (argv[0].us_v_ptr);
	*rett = SYMT_INT;
	retv->us_v_int = i;
}
	




static void
UiErrorReport (line)
char *line;
/*
 * Report errors generated in UI.
 */
{
	msg_ELog (EF_PROBLEM, "UI ERROR: %s", line);
}




static void
UiPfHandler (line)
char *line;
/*
 * Handle ui_printf'd stuff.
 */
{
	char *nl, *strchr ();
	char tbuf[400];
/*
 * Clean out NL's.
 */
	strcpy (tbuf, line);
	while (nl = strchr (tbuf, '\n'))
		*nl = ' ';
	msg_ELog (EF_INFO, "ui_printf('%s')", tbuf);
}




void
SendEndpoints (cmds)
struct ui_command *cmds;
/*
 * Send new endpoints to the associated cross-section graphics process
 */
{
	SValue	v;
	int	type;
	float	x0, y0, x1, y1;
	char	win[30], comp[30];
	struct dm_event	dme;
/*
 * If the coords are given explicitly, use them.
 */
	if (cmds->uc_ctype != UTT_END)
	{
		x0 = UFLOAT (cmds[0]);
		y0 = UFLOAT (cmds[1]);
		x1 = UFLOAT (cmds[2]);
		y1 = UFLOAT (cmds[3]);
	}
/*
 * Otherwise assume that a drawline has been run, and use those values.
 */
	else
	{
		usy_g_symbol (Vtable, "linex0", &type, &v); x0 = v.us_v_float;
		usy_g_symbol (Vtable, "liney0", &type, &v); y0 = v.us_v_float;
		usy_g_symbol (Vtable, "linex1", &type, &v); x1 = v.us_v_float;
		usy_g_symbol (Vtable, "liney1", &type, &v); y1 = v.us_v_float;
	}
/*
 * Get the window and component to which the endpoints are going
 */
	if (! pda_Search (Pd, "global", "xsect-window", NULL, win, 
		SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "No associated cross-section window");
		return;
	}

	if (! pda_Search (Pd, "global", "xsect-component", NULL, comp, 
		SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "No associated cross-section component");
		return;
	}
/*
 * Send the endpoints
 */
	dme.dmm_type = DM_EVENT;

	sprintf (dme.dmm_data, "param %s %s left-endpoint %.3f,%.3f", win, 
		comp, x0, y0);
	msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &dme, sizeof (dme));

	sprintf (dme.dmm_data, "param %s %s right-endpoint %.3f,%.3f", win, 
		comp, x1, y1);
	msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &dme, sizeof (dme));

	msg_ELog (EF_DEBUG, 
		"Sent endpoints (%.2f,%.2f) (%.2f,%.2f) to '%s/%s'", x0, y0,
		x1, y1, win, comp);
}
