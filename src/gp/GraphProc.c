/*
 * My version of the graphics process, for now -- upper level control
 * and timing stuff.
 */
static char *rcsid = "$Id: GraphProc.c,v 1.3 1990-06-04 14:25:46 burghart Exp $";

# include <X11/X.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Cardinals.h>
# include <ui.h>

# include "../include/defs.h"
# include "EventQueue.h"
# include "../include/message.h"
# include "../include/dm.h"
# include "../include/pd.h"
# include "../include/GraphicsW.h"
# include "../include/timer.h"

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
	"	*Command*font:	-*-times-medium-i-*-*-*-120-*-*-*-*-*-*",
	0,
};

/*
 * Globals.
 */
char Ourname[40];	/* What is our process name?	*/


/*
 * Definition of globals referenced in GraphProc.h
 */
Widget Top;				/* The top level widget		*/
Widget Graphics, GrShell;		/* The graphics widget		*/
int FrameCount = 1;			/* Number of frames		*/
int DisplayFrame = 0;			/* Frame being displayed	*/
int DrawFrame = 0;			/* Frame to draw next		*/
XtAppContext Actx;			/* The application context	*/
bool Abort = FALSE;			/* Has the current plot been stopped*/
plot_description Pd = 0, Defaults = 0;	/* Plot description info	*/
time PlotTime;				/* The current plot time.	*/
enum pmode PlotMode = RealTime;
enum wstate WindowState = DOWN;
bool NewPD = FALSE;

/*
 * Forward routine definitions.
 */
int msg_handler ();
int dispatcher ();
int xtEvent ();
static void DMButton ();

extern void Ue_PointerEvent (), Ue_ButtonUp (), Ue_KeyEvent ();
/*
 * Routines called through the event queue mechanism.
 */
void eq_reconfig (), eq_sync ();



main (argc, argv)
int argc;
char **argv;
{
	Arg args[10];
	static XtActionsRec actions[] =
	{
		{ "ue_pointer_event",	Ue_PointerEvent	},
		{ "ue_key_event",	Ue_KeyEvent	},
		{ "ue_button_up",	Ue_ButtonUp	},
	};
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
/*
 * Get the toolkit going.
 */
	Top = XtAppInitialize (&Actx, "Graphproc", NULL, ZERO, &argc,
		argv, Resources, NULL, ZERO);
	XtAppAddActions (Actx, actions, THREE);
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
 * Initialize the UI.
 */
	ui_setup ("GraphProc", argc, argv, (char *) 0);
	ui_init ("/fcc/lib/graphproc.lf", FALSE, TRUE);
/*
 * Initialize color table and user event stuff
 */
	ct_Init ();
	Ue_Init ();
/*
 * Tell DM that we're here.
 */
	greet_dm ();
/*
 * Set up our event handlers.
 */
	lle_AddFD (msg_get_fd (), msg_incoming);
	lle_AddFD (XConnectionNumber (XtDisplay (Top)), xtEvent);
/*
 * Go into the UI, which should throw us immediately into run mode.
 */
	ui_get_command ("initial", "you shouldn't see this", dispatcher, 0);
	shutdown ();
}




shutdown ()
/*
 * Finish up and quit.
 */
{
	ui_finish ();
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
	   	dm_message ((struct dm_msg *) msg->m_data);
		break;

	/*
	 * Message handler stuff.  The only thing we know how to deal
	 * with now is SHUTDOWN.
	 */
	   case MT_MESSAGE:
	   	if (tm->mh_type == MH_SHUTDOWN)
			shutdown ();
		msg_log ("Unknown MESSAGE proto type: %d", tm->mh_type);
		break;
	 /*
	  * Timer.
	  */
	    case MT_TIMER:
		tm_message ((struct tm_time *) msg->m_data);
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

	dmh.dmm_type = DM_HELLO;
	msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &dmh, sizeof (dmh));
}




dispatcher (cmds)
struct ui_command *cmds;
/*
 * The command dispatcher.  Assume it is RUN for now.
 */
{
	lle_MainLoop ();
	return (0);
}





dm_message (dmsg)
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
	   	Eq_AddEvent (PUrgent, eq_reconfig, dmsg, sizeof (*dmsg),
			Override);
		break;
	/*
	 * Ribbit.
	 */
	   case DM_DIE:
	   	msg_log ("DM decreed shutdown");
		shutdown ();
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
	/*
	 * History mode.
	 */
	   case DM_HISTORY:
	   	HistoryMode ((struct dm_history *) dmsg);
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
	 * ???
	 */
	   default:
	   	msg_log ("Unknown DM Message type: %d", dmsg->dmm_type);
		break;
	}
}




tm_message (te)
struct tm_time	*te;
/*
 * Deal with a timer message
 */
{
	switch (te->tm_type)
	{
	/*
	 * We deal with a time change here
	 */
	    case TRR_TCHANGE:
	   	NewTime ((struct tm_tchange *) te);
		break;
	/*
	 * Other messages can be handled by the timer lib event dispatcher
	 */
	    default:
	    	tl_DispatchEvent (te);
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
/*
 * Go through and set the new values for the shell.
 */
 	XtSetArg (args[0], XtNx, dmsg->dmm_x);
	XtSetArg (args[1], XtNy, dmsg->dmm_y);
	XtSetArg (args[2], XtNwidth, dmsg->dmm_dx);
	XtSetArg (args[3], XtNheight, dmsg->dmm_dy);
	XtSetValues (GrShell, args, FOUR);
/*
 * If we are not currently on-screen, put it there.
 */
	if (WindowState == DOWN)
		ChangeState (UP);
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
		Eq_AddEvent (PDisplay, px_PlotExec, "global", 7, Override);
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
	XSync (XtDisplay (Top), False);
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
 * We need to get the plot mode when we next execute the plot handler
 */
	NewPD = TRUE;
/*
 * Now we need to set up to display the new PD.
 */
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
}





ChangeDefaults (dmp)
struct dm_pdchange *dmp;
/*
 * A new defaults table has arrived.
 */
{
	raw_plot_description rpd;
	plot_description pd;
/*
 * Go ahead and recompile the PD now.
 */
	rpd.rp_len = dmp->dmm_pdlen;
	rpd.rp_data = dmp->dmm_pdesc;
	pd = pd_Load (&rpd);
	pda_StorePD (pd, "defaults");
/*
 * We need to get the plot mode when we next execute the plot handler
 */
	NewPD = TRUE;
/*
 * Redisplay with reinitialization of timer stuff, since anything could 
 * have changed.  (Maybe this can be made smarter in the future?)
 */
	if (Pd)
		Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
}







HistoryMode (dmh)
struct dm_history *dmh;
/*
 * Go into history mode.
 */
{
/*
 * Sanity checking time.
 */
	if (! Pd)
	{
		msg_log ("History mode requested with no plot description!");
		return;
	}
	else
		msg_log ("History mode: %d %d", dmh->dmm_time.ds_yymmdd,
			dmh->dmm_time.ds_hhmmss);
/*
 * Cancel anything that might be going now.
 */
	pc_CancelPlot ();
/*
 * Set the time and do a full display.
 */
	PlotTime = dmh->dmm_time;
	PlotMode = History;
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
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
		msg_log ("Real-time mode requested with no plot description!");
		return;
	}

	msg_log ("Real-time mode");
/*
 * If we're already in real time mode, do nothing.
 */
	if (PlotMode == RealTime)
		return;
/*
 * Cancel anything going now
 */
	pc_CancelPlot ();
/*
 * Switch modes and start plotting.
 */
	PlotMode = RealTime;
	if (WindowState == UP)
		pc_SetUpTriggers ();
	tl_GetTime (&PlotTime);
	
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
}




NewTime (tch)
struct tm_change	*tch;
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
 * Cancel anything going now
 */
	pc_CancelPlot ();
/*
 * Reestablish triggers and do the plot
 */
	if (WindowState == UP)
		pc_SetUpTriggers ();
	tl_GetTime (&PlotTime);
	
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
}
