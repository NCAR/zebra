/*
 * The zeb graphics process.
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

# include <unistd.h>
# include <X11/X.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/cursorfont.h>
# include <ui.h>
# include <fcntl.h>

# include <config.h>
# include <defs.h>
# include <message.h>
# include <dm.h>
# include <pd.h>
# include <GraphicsW.h>
# include <timer.h>
# include <DataStore.h>
# include <copyright.h>

# include "gp_cmds.h"
# include "EventQueue.h"
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "LayoutControl.h"
# include "LLEvent.h"

MAKE_RCSID ("$Id: GraphProc.c,v 2.54 1995-06-29 23:44:38 granger Exp $")

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
	"	*title*borderWidth: 0",
/*	"	*Command*font:	-*-times-medium-i-*-*-*-120-*-*-*-*-*-*", */
	0,
};

/*
 * Definition of globals referenced in GraphProc.h
 */
Widget	Top;				/* The top level widget		*/
Widget	Graphics, GrShell;		/* The graphics widget		*/
Display	*Disp;				/* Our display			*/
char 	FrameFilePath[PathLen];		/* Path to FrameFile		*/
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
ZebTime	PlotTime;			/* The current plot time.	*/
long	ForecastOffset;			/* Forecast offset time (models)*/
bool	ValidationMode;			/* Validation mode for models?	*/
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
bool	PostProcMode = FALSE;
ZebTime	PostProcTime;

static int ListPosition (), RmElement (), ReplElement ();

/*
 * Definition of the global graphics context in GC.h
 */
GC	Gcontext;

/*
 * The icon and map paths.
 */
char IconPath[PathLen];
char MapPath[PathLen];

/*
 * Saved versions of the "command line" parameters.
 */
static int Argc;
static char **Argv;

/*
 * A symbol table for require's and the path to search them out in.
 */
static stbl RequireTable = 0;
static char RequirePath[PathLen];

/*
 * Forward routine definitions.
 */
int	msg_handler ();
void	SendEndpoints ();
static void WMResize ();
static void PopdownWidgets ();

static void UiErrorReport (), UiPfHandler ();

extern void Ue_PointerEvent (), Ue_ButtonUp (), Ue_KeyEvent ();
extern void Ue_MotionEvent ();

/*
 * Routines called through the event queue mechanism.
 */
void eq_reconfig (), eq_sync ();

static void finish_setup FP ((void));
static void NewTime FP ((ZebTime *));
static int AnswerQuery FP ((char *));
static void SendGeometry FP((struct dm_msg *dmm));
static int RealPlatform FP ((int, SValue *, int *, SValue *, int *));
static void Enqueue FP ((EQpriority, char *));
static int dmgr_message ();
static void gp_sync FP ((void));
static void ChangeState FP ((enum wstate new));
static void ChangeParam FP ((struct dm_parchange *dmp));
static void ChangeDefaults FP ((struct dm_pdchange *dmp));
static void HistoryMode FP ((ZebTime *when));
static void RealTimeMode FP ((void));
static void DialEvent FP ((struct dm_dial *dmd));
static void DialTime FP ((int motion));



void
GPShutDown ()
/*
 * Finish up and quit.
 */
{
	int i;
	char filename[100];
	
	ui_finish ();
	pdm_Finish ();
# ifdef SHM
	RP_ZapSHMImage (Graphics);
	if (GWShmPossible (Graphics))
		for(i = 0; i < FrameCount; i++)
			if (GWFrameShared (Graphics, i))
				GWZapShmPixmap(Graphics, i);
# endif
/*
 * Is this necessary?
 */
	sprintf(filename, "%s/%s%dFrameFile",FrameFilePath, 
		msg_myname(), getpid());
	unlink(filename);

	exit (0);
}



void
main (argc, argv)
int argc;
char **argv;
{
	char loadfile[200];
/*
 * Get any dm options from our command line.
 */
	dm_Setup (&argc, argv, NULL /* no default handle */);
/*
 * Connect to the message handler immediately -- Ardent weirdness
 * requires this.
 */
	if (! msg_connect (msg_handler, dm_MessageName()))
	{
		fprintf (stderr, 
			 "%s (%s): unable to connect to message handler\n",
			 dm_MessageName(), argv[0]);
		exit (1);
	}
	msg_join (dm_GroupName());
	msg_join ("TimeChange");
	msg_DeathHandler ((int (*)()) GPShutDown);
	msg_SetQueryHandler (AnswerQuery);
/*
 * Hand off our information to the UI, and initialize things.
 */
	fixdir ("GP_LOAD_FILE", GetLibDir (), "graphproc.lf", loadfile);
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
	ui_get_command ("initial", (char *)msg_myname(), dispatcher, 0);
	GPShutDown ();
}



static void
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
	int pd_removeparam (), substr_remove(), ReplString ();
	char initfile[128], perf[80];
/*
 * Force a shift into window mode, so we can start with the fun stuff.
 */
	uw_ForceWindowMode ((char *) 0, &Top, &Actx);
	XtAppAddActions (Actx, actions, FIVE);
	XtRegisterGrabAction (Ue_PointerEvent, True,
	      ButtonPressMask|ButtonReleaseMask, GrabModeAsync, GrabModeAsync);
/*
 * If there is a parameter left on the "command line", it will be the name
 * of an initialization file to read.
 */
	if ((Argc > 1) && (Argv[1][0] != '\0'))
	{
		if (! access (Argv[1], F_OK))
			strcpy (initfile, Argv[1]);
		else
			sprintf (initfile, "%s/%s", GetProjDir (), Argv[1]);
	}
	else
		initfile[0] = '\0';
	Vtable = usy_g_stbl ("ui$variable_table");
/*
 * Now create a popup shell to hold the graphics widget that holds
 * our output.
 */
	XtSetArg (args[0], XtNinput, True);
	XtSetArg (args[1], XtNwidthInc, 4);
	XtSetArg (args[2], XtNallowShellResize, True);
	XtSetArg (args[3], XtNwinGravity, StaticGravity);
	XtSetArg (args[4], XtNtitle, dm_WindowName());
	GrShell = XtCreatePopupShell (msg_myname(), 
			      applicationShellWidgetClass, Top, args, 5);
/*
 * Inside this shell goes the graphics widget itself.
 */
	Graphics = XtCreateManagedWidget ("graphics", graphicsWidgetClass,
		GrShell, NULL, 0);
/*
 * Add the resize callback procedure.
 */
	XtAddCallback (Graphics, XtNresizeCallback, (XtCallbackProc) WMResize, 
		(XtPointer) NULL);
/*
 * Cursors.
 */
	Disp = XtDisplay (Top);
	NormalCursor = XCreateFontCursor (Disp, XC_draft_small);
	BusyCursor = XCreateFontCursor (Disp, XC_watch);
#ifdef XDEBUG
	XSynchronize (Disp, True);
#endif
/*
 * Get the require table set up just in case anybody needs it.
 */
	RequireTable = usy_c_stbl ("RequireTable");
/*
 * Module initializations.
 */
	SetupConfigVariables ();/* Configuration info		*/
	F_Init ();		/* Data store fields module 	*/
	ct_Init ();		/* Color tables			*/
	Ue_Init ();		/* User event handling		*/
	I_init ();		/* Icons			*/
	lw_InitWidgets ();	/* Limit widgets		*/
	ot_Init ();		/* Overlay times widget		*/
	InitDataMenu ();	/* Data available menu		*/
	InitFieldMenu ();	/* Field selection		*/
	pw_InitPos ();		/* Position Widget		*/
	iw_Initialize ();	/* Data insertion widget	*/
	aw_InitAnnot ();	/* Annotation widget		*/
	lc_Init ();		/* Layout control		*/
	pdm_Init ();		/* Plot description monitoring	*/
	dm_SetupVariables ();	/* dm indirect variables	*/
/*
 * Tell DM that we're here.
 */
#ifdef notdef
	greet_dm ();
#endif
	dm_Greet ();

#ifdef notdef	/* moved to happen after we have a window */
/*
 * Graphics context
 */
	Gcontext = XCreateGC (XtDisplay (Graphics), XtWindow (Graphics), 
		0, NULL);
#endif
/*
 * Set up our event handlers.
 */
	lle_AddFD (msg_get_fd (), (void (*)()) msg_incoming);
	lle_AddFD (XConnectionNumber (Disp), (void (*)()) xtEvent);
/*
 * Command line functions.
 */
	type[0] = type[1] = type[2] = type[3] = type[4] = SYMT_STRING;
	uf_def_function ("pd_param", 3, type, pd_param);
	uf_def_function ("pdparam", 2, type, pd_param);
	uf_def_function ("pd_paramsearch", 4, type, pd_paramsearch);
	uf_def_function ("pdsearch", 3, type, pd_paramsearch);
	uf_def_function ("pd_defined", 2, type, pd_defined);
	uf_def_function ("pd_removeparam", 2, type, pd_removeparam);
	uf_def_function ("substr_remove", 2, type, substr_remove);
	uf_def_function ("realplatform", 1, type, RealPlatform);
	uf_def_function ("listposition", 2, type, ListPosition);
	uf_def_function ("replstring", 3, type, ReplString);
/*
 * Couple more CLF's with non-string parameters
 */
	type[1] = SYMT_INT;
	uf_def_function ("rmelement", 2, type, RmElement);
	uf_def_function ("replelement", 3, type, ReplElement);
/*
 * Redirect UI output.
 */
	ui_ErrorOutputRoutine (UiErrorReport);
	ui_OutputRoutine (UiPfHandler, UiPfHandler);
/*
 * More initialization
 */
	mc_DefMovieWidget ();		/* Movie control		*/
	mw_DefModelWidget ();		/* Model widget			*/
	fc_InitFrameCache ();		/* Initialize the frame cache	*/
	tl_ChangeHandler (NewTime);
	cp_SetupCmdProto ();		/* Command protocol		*/
/*
 * Indirect variables.
 */
	usy_c_indirect (Vtable, "holdprocess", &HoldProcess, SYMT_BOOL, 0);
	usy_c_indirect (Vtable, "iconpath", IconPath, SYMT_STRING, PathLen);
	usy_c_indirect (Vtable, "mappath", MapPath, SYMT_STRING, PathLen);
	usy_c_indirect (Vtable, "requirepath", RequirePath, SYMT_STRING,
			PathLen);
/*
 * Default values for the path variables.
 */
	sprintf (IconPath, "%s/icons,./icons", GetLibDir ());
	sprintf (MapPath, "%s,./maps", GetLibDir ());
	if (getenv ("GP_MAP_DIR"))
	{
		strcat (MapPath, ",");
		strcat (MapPath, getenv ("GP_MAP_DIR"));
	}
	sprintf (RequirePath, "%s/gplib", GetLibDir ());
/*
 * Pull in the init file, if there is one
 */
	if (initfile[0])
	{
		sprintf (perf, "read %s", initfile);
		ui_perform (perf);
	}
}



static void
WMResize (w, junk, stuff)
Widget w;
XtPointer junk, stuff;
/*
 * Respond to a window manager resize.
 */
{
/*
 * Invalidate the frame cache.
 */
	fc_InvalidateCache ();
/*
 * Force a redisplay.
 */
	if (Pd)
	{
		Eq_AddEvent (PDisplay, I_DoIcons, NULL, 0, Bounce);
		Eq_AddEvent (PDisplay, pc_PlotHandler, NULL, 0, Override);
	}
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




/* ARGSUSED */
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




#ifdef notdef	/* try obviating the kludge by delaying sending window id */
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
	dm_Send (&dmh, sizeof (dmh));
}
#endif


/* ARGSUSED */
int
dispatcher (junk, cmds)
int junk;
struct ui_command *cmds;
/*
 * The GP command dispatcher.
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
		{
			finish_setup ();
			lle_MainLoop ();
		}
		else
			msg_ELog (EF_PROBLEM, "Somebody typed RUN again");
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
		dm_Send (&dme, sizeof (dme));
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
			Eq_AddEvent (PDisplay, I_DoIcons, NULL, 0, Bounce);
			Eq_AddEvent (PDisplay, pc_PlotHandler, NULL, 0, 
				     Override);
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
	 * Field definition.
	 */
	   case GPC_FIELD:
	   	F_DeclareField (UPTR (cmds[1]), UPTR (cmds[2]), UPTR(cmds[3]));
		break;
	   case GPC_ALIAS:
	   	F_Alias (UPTR (cmds[1]), UPTR (cmds[2]));
		break;
	/*
	 * Get some help.
	 */
	   case GPC_HELP:
		dme.dmm_type = DM_EVENT;
		sprintf (dme.dmm_data, "help %s", (cmds[1].uc_ctype == UTT_END)
				? "" : UPTR (cmds[1]));
		dm_Send (&dme, sizeof (dme));
		break;
	/*
	 * The user wants to annotate something.
	 */
	   case GPC_USERANNOT:
		aw_SetLoc ();
		break;

        /*
         * The zoom and unzoom commands
         */
	   case GPC_ZOOM:
		pc_Zoom(cmds +1);
                break;

	   case GPC_UNZOOM:
		pc_UnZoom();
                break;
	/*
	 * Deal with a require.
	 */
	   case GPC_REQUIRE:
		Require (UPTR (cmds[1]));
		break;

	    case GPC_ENQUEUE:
		Enqueue ((EQpriority) UKEY (cmds[1]), UPTR (cmds[2]));
		break;
	/*
	 * Images.
	 */
	    case GPC_IMGDUMP:
		ImageDump (UKEY (cmds[1]), UPTR (cmds[2]));
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




static int
dmgr_message (dmsg)
struct dm_msg *dmsg;
/*
 * Deal with a display manager message.
 *
 * This used to be "dm_message" but hpux:sys/types.h defines their own
 * dm_message type...grumble grumble...
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
		msg_ELog (EF_DEBUG, "reconfig message received from dm");
	   	Eq_AddEvent (PUrgent, eq_reconfig, dmsg, 
			     sizeof (struct dm_reconfig), Override);
		break;
	/*
	 * Geometry query.
	 */
	   case DM_GEOMETRY:
		SendGeometry (dmsg);
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
		msg_ELog (EF_DEBUG, "suspend message received from dm");
		PopdownWidgets ();
		ChangeState (DOWN);
		pc_CancelPlot ();
		break;
	/*
	 * Load a new plot description.
	 */
	   case DM_PDCHANGE:
		msg_ELog (EF_DEBUG, "new pd received from dm");
	   	ChangePD ((struct dm_pdchange *) dmsg);
		break;

	   case DM_PARCHANGE:
	   	ChangeParam ((struct dm_parchange *) dmsg);
		break;
	/*
	 * History mode.  Comes through in the new time format, which we are
	 * not quite prepared to deal with yet.
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
	return (0);
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

	n = 0;
	XtSetArg (args[n], XtNx, (XtArgVal)&x);	n++;
	XtSetArg (args[n], XtNy, (XtArgVal)&y);	n++;
	XtSetArg (args[n], XtNwidth, (XtArgVal)&width);	n++;
	XtSetArg (args[n], XtNheight, (XtArgVal)&height); n++;
	XtGetValues (GrShell, args, n);
	reply.dmm_x = x;
	reply.dmm_y = y;
	reply.dmm_dx = width;
	reply.dmm_dy = height;

	dm_Send (&reply, sizeof (reply));
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




/* ARGSUSED */
void
eq_reconfig (dmsg, len)
struct dm_reconfig *dmsg;
int len;
/*
 * Reconfigure the window.
 */
{
	static bool WindowSent = FALSE;
	Arg args[10];
	Dimension width, height;
	Position x, y;
	bool schanged, wchanged;

	msg_ELog (EF_DEBUG, "reconfig routine entered from event queue");
/*
 * Let dm interface gleen what it wants from the message, like our
 * window name, and then set our UI variable accordingly.
 */
	dm_Reconfig (dmsg);
/*
 * We're being told to reconfig, supposedly from a display configuration
 * change, so pop down any existing widgets and start with a clean slate.
 */
	if (WindowState == UP)
		PopdownWidgets ();
/*
 * Figure out if anything really important has changed.
 */
	schanged = (dmsg->dmr_dx != GWWidth (Graphics)) ||
			(dmsg->dmr_dy != GWHeight (Graphics));
	wchanged = (WindowState == DOWN);

	if (! WindowSent)
	{
	/*
	 * Set the geometry first, in case this is the first time we're
	 * popping up this window and it needs some geometry settings.
	 * These are just defaults and probably won't match our configuration.
	 */
		width = dmsg->dmr_dx;
		height = dmsg->dmr_dy;
		XtSetArg (args[0], XtNwidth, width);
		XtSetArg (args[1], XtNheight, height);
		XtSetValues (Graphics, args, (Cardinal)2);

		x = dmsg->dmr_x;
		y = dmsg->dmr_y;
		XtSetArg (args[0], XtNx, x);
		XtSetArg (args[1], XtNy, y);
		XtSetValues (GrShell, args, (Cardinal)2);
	}
/*
 * If this is the first time we've popped up, send along our newly-realized
 * window id.
 */
	ChangeState (UP);
	if (! WindowSent)
	{
		dm_SendWindowID (XtWindow (GrShell));
		WindowSent = TRUE;
		/*
		 * Graphics context
		 */
		Gcontext = XCreateGC (XtDisplay (Graphics), 
				      XtWindow (Graphics), 0, NULL);
	}
	/*
	 * Change title and geometry
	 */
	XtSetArg (args[0], XtNtitle, dm_WindowName());
	XtSetValues (GrShell, args, (Cardinal)1);
	ConfigureWindow (XtDisplay(GrShell), XtWindow(GrShell), 
			 dmsg->dmr_x, dmsg->dmr_y,
			 dmsg->dmr_dx, dmsg->dmr_dy);
#ifdef notdef
	/*
	 * Do the geometry setting again for those window managers which
	 * don't get it the first time.
	 */
	XtSetArg (args[0], XtNwidth, width);
	XtSetArg (args[1], XtNheight, height);
	XtSetValues (Graphics, args, (Cardinal)2);
 	XtSetArg (args[0], XtNx, x);
	XtSetArg (args[1], XtNy, y);
	XtSetValues (GrShell, args, (Cardinal)2);
#endif
	/*
	 * Set the cursor to our normal value.
	 */
	XDefineCursor (Disp, XtWindow (Graphics), NormalCursor);
	/*
	 * If nothing drastic has changed, we can quit now and not redraw 
	 * everything.
	 */
	if (! schanged && ! wchanged)
		return;
	/*
	 * Invalidate the frame cache if the window size has changed.
	 */
	if (schanged)
		fc_InvalidateCache ();
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



static void
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
	gp_sync ();
}



/*ARGSUSED*/
static int
popdown_ui_widget (symbol, type, v, junk)
char *symbol;
int type, junk;
union usy_value *v;
/*
 * Make sure this widget is popped down.
 */
{
	uw_popdown (symbol);
	return (TRUE);
}



static void
PopdownWidgets ()
/*
 * Pop down any UI widgets which are popped up by traversing the internal
 * 'ui$widget_table' symbol table.
 */
{
	stbl wtable;
	int uw_t_popdown ();

	msg_ELog (EF_DEBUG, "popping down all transient widgets");
	wtable = usy_g_stbl ("ui$widget_table");
	if (wtable != NULL)
		usy_traverse (wtable, popdown_ui_widget, 0, FALSE);
}



void
eq_sync ()
/*
 * Synchronize with the window system.
 */
{
	XSync (Disp, False);
}




void
eq_ReturnPD ()
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
	dm_Send (dmp, len);
/*
 * (1/29/91 jc -- sigh) Free the memory we used.
 */
	pd_RPDRelease (rpd);
	free (dmp);
}




static void
gp_sync ()
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



void
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
	        lc_UnZoom(Zlevel);
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
 * Load Zoom coordinates
 */
	lc_LoadZoom ();
/*
 * Now we need to set up to display the new PD, and also to redo icons.
 */
	Eq_AddEvent (PDisplay, DoRequires, 0, 0, Bounce);
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
	Eq_AddEvent (PDisplay, I_DoIcons, 0, 0, Bounce);
	pdm_ScheduleUpdate ();
}




static void
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
	pdm_ScheduleUpdate ();
}




void
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
	pdm_ScheduleUpdate ();
}




static void
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
 * Redisplay with reinitialization of timer stuff, since anything could 
 * have changed.  (Maybe this can be made smarter in the future?)
 */
	if (Pd)
		Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
}





static void
HistoryMode (when)
ZebTime *when;
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




static void
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




static void
DialTime (motion)
int motion;
/*
 * Simple (for now) time control through dials.
 */
{
	PlotTime.zt_Sec += (motion > 0) ? 60 : -60;
	HistoryMode (&PlotTime);
}




static void
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



/* ARGSUSED */
static void
NewTime (t)
ZebTime *t;
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





/* ARGSUSED */
int
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
	return (0);
}


/* ARGSUSED */
int
pd_removeparam (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
union usy_value *argv, *retv;
/*
 * the pd_removeparam CLF.
 *
 *	pd_removeparam (comp, param)
 *
 * Why this thing is a function I will never know.
 */
{
/*
 * Zap out the parameter.
 */
	*rett = SYMT_BOOL;
	pd_RemoveParam (Pd, argv[0].us_v_ptr, argv[1].us_v_ptr);
	retv->us_v_int = TRUE;
/*
 * Force a replot.
 */
	Eq_AddEvent (PDisplay, pc_PlotHandler, 0, 0, Override);
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
	return (0);
}






/* ARGSUSED */
int
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
	int type;
/*
 * Look at the return type for the answer.  The newer version of the call
 * doesn't require "string" to be specified explicitly.
 */
	type = (narg == 3) ? uit_int_type (argv[2].us_v_ptr) : SYMT_STRING;

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
	return (0);
}
	


/* ARGSUSED */
int
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
	int type = (narg == 4) ? uit_int_type (argv[3].us_v_ptr) : SYMT_STRING;

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
	return (0);
}



/* ARGSUSED */
int
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
		if (strlen (tmp) > (unsigned)0)
			tmp = strcat (tmp, ",");
		tmp = strcat (tmp, string[i]);
	}
	*rett = SYMT_STRING;
	retv->us_v_ptr = usy_string (tmp);
	free (tmp);
	return (0);
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
	char *nl;
	char tbuf[400];
/*
 * Clean out NL's.
 */
	strcpy (tbuf, line);
	while ((nl = strchr (tbuf, '\n')) != 0)
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
		usy_g_symbol (Vtable, "linex0", &type, &v); 
		x0 = XUSER(v.us_v_int);
		usy_g_symbol (Vtable, "liney0", &type, &v); 
		y0 = YUSER(v.us_v_int);
		usy_g_symbol (Vtable, "linex1", &type, &v); 
		x1 = XUSER(v.us_v_int);
		usy_g_symbol (Vtable, "liney1", &type, &v); 
		y1 = YUSER(v.us_v_int);
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
/*
 * Hold everything!  If the window is "all", then we have the display
 * manager broadcast it.
 */
	if (! strcmp (win, "all"))
	{
		dme.dmm_type = DM_EVENT;
		sprintf (dme.dmm_data, "AllXSect %.3f %.3f %.3f %.3f",
				x0, y0, x1, y1);
		dm_Send (&dme, sizeof (dme));
		return;
	}
/*
 * OK, back to the old program.  Get the component.
 */
	if (! pda_Search (Pd, "global", "xsect-component", NULL, comp, 
		SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "No associated cross-section component");
		return;
	}
/*
 * Put the destination plot on hold, send the endpoints, and let it go again
 */
	dme.dmm_type = DM_EVENT;

	sprintf (dme.dmm_data, "param %s global plot-hold true", win);
	dm_Send (&dme, sizeof (dme));

	sprintf (dme.dmm_data, "param %s %s left-endpoint %.3f,%.3f", win, 
		comp, x0, y0);
	dm_Send (&dme, sizeof (dme));

	sprintf (dme.dmm_data, "param %s %s right-endpoint %.3f,%.3f", win, 
		comp, x1, y1);
	dm_Send (&dme, sizeof (dme));

	sprintf (dme.dmm_data, "param %s global plot-hold false", win);
	dm_Send (&dme, sizeof (dme));

	msg_ELog (EF_DEBUG, 
		"Sent endpoints (%.2f,%.2f) (%.2f,%.2f) to '%s/%s'", x0, y0,
		x1, y1, win, comp);
}







static int
AnswerQuery (who)
char *who;
/*
 * Answer a query from this person.
 */
{
	char abuf[200];
	raw_plot_description *rpd;
/*
 * Basic stuff.
 */
	msg_ELog (EF_DEBUG, "Query from %s", who);
	sprintf (abuf, 
		 "%s %s, window '%s' %s, coords [%.2f %.2f] -> [%.2f %.2f]",
		 "Graphics process", msg_myname(), dm_WindowName(), 
		 WindowState == UP ? "UP" : "DOWN", Xlo, Ylo, Xhi,Yhi);
	msg_AnswerQuery (who, abuf);
/*
 * Send back the PD -- they asked for it!
 */
	if (! Pd)
		msg_AnswerQuery (who, "No plot description loaded");
	else
	{
		rpd = pd_Unload (Pd);
		msg_AnswerQuery (who, " ");
		msg_AnswerQuery (who, rpd->rp_data);
		pd_RPDRelease (rpd);
	}
	msg_FinishQuery (who);
	return (0);
}





static int
RealPlatform (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * The "realstation" command line function -- return TRUE if the argument
 * is a real platform.
 */
{
	retv->us_v_int = (ds_LookupPlatform (argv->us_v_ptr) != BadPlatform);
	*rett = SYMT_BOOL;
	return (0);
}



/* ARGSUSED */
void
HelpCallback (w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
	char *topic = (char *)client_data;
	struct dm_event dme;

	dme.dmm_type = DM_EVENT;
	sprintf (dme.dmm_data, "help %s", topic ? topic : "");
	dm_Send (&dme, sizeof (dme));
}




void
Require (module)
char *module;
/*
 * Make sure that we have loaded this module.
 */
{
	int t;
	SValue v;
	char fname[PathLen];
/*
 * See if the module has been loaded already.
 */
	if (usy_g_symbol (RequireTable, module, &t, &v))
		return;	/* Got it already */
/*
 * Nope.  We need to look for it.
 */
	strcpy (fname, "read ");
	if (! FindFile (module, RequirePath, fname + 5))
	{
		msg_ELog (EF_PROBLEM, "Unable to find module %s", module);
		return;
	}
/*
 * Now load the module and make a note of it.
 */
	msg_ELog (EF_INFO, "Loading module %s", module);
	ui_perform (fname);
	usy_s_symbol (RequireTable, module, SYMT_BOOL, &v);
#ifdef notdef
/*
 * Sync any window popups which may have been performed
 */
	uw_sync ();
#endif
}





void
DoRequires ()
/*
 * Pass through the plot description and make sure that we have all of our
 * requires in place.
 */
{
	char **comps = pd_CompList (Pd), rqs[256], *module;
	int comp;
/*
 * Pass through each component, including the global one, and see if there
 * is a require parameter therein.
 */
	msg_ELog (EF_DEBUG, "checking components for module requirements");
	for (comp = 0; comps[comp]; comp++)
	{
		if (! pd_Retrieve (Pd, comps[comp], "require", rqs,
				   SYMT_STRING))
			continue;
		for (module = (char *)strtok (rqs, ", \t"); module;
		     module = (char *)strtok (NULL, ", \t"))
			Require (module);
	}
	msg_ELog (EF_DEBUG, "module requirements check complete");
}
		    




static int
ListPosition (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * CLF: ListPosition (element, list)
 *
 * Returns the cardinal position of the given element in the list.
 */
{
	char *elems[40];
	int nelem = CommaParse (argv[1].us_v_ptr, elems), i;
	*rett = SYMT_INT;
	
	for (i = 0; i < nelem; i++)
		if (! strcmp (elems[i], argv[0].us_v_ptr))
		{
			retv->us_v_int = i;
			return (0);
		}
	retv->us_v_int = -1;
	return (0);
}




static int
RmElement (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * CLF: RmElement (list, nth)
 */
{
	char *elems[40], *ret = usy_string (argv[0].us_v_ptr);
	int nelem = CommaParse (argv[0].us_v_ptr, elems), i;
	bool first = TRUE;

	*ret = '\0';
	for (i = 0; i < nelem; i++)
		if (i != argv[1].us_v_int)
		{
			if (first)
				first = FALSE;
			else
				strcat (ret, ",");
			strcat (ret, elems[i]);
		}
	*rett = SYMT_STRING;
	retv->us_v_ptr = ret;
	return (0);
}


static int
ReplElement (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * CLF: ReplElement (list, nth, new)
 */
{
	char *elems[40], *ret = usy_string (argv[0].us_v_ptr);
	int nelem = CommaParse (argv[0].us_v_ptr, elems), i;

	*ret = '\0';
	for (i = 0; i < nelem; i++)
	{
		if (i > 0)
			strcat (ret, ",");
		strcat (ret, (i == argv[1].us_v_int) ?
			           argv[2].us_v_ptr : elems[i]);
	}
	*rett = SYMT_STRING;
	retv->us_v_ptr = ret;
	return (0);
}



int
ReplString (narg, argv, argt, retv, rett)
int narg, *argt, *rett;
SValue *argv, *retv;
/*
 * ReplString (string, olds, news)
 *
 * Replace "olds" with "news" in string.
 */
{
	char retstr[300], *cp = argv[0].us_v_ptr, *olds = argv[1].us_v_ptr;
	char *rp = retstr;
	int olen = strlen (olds);
/*
 * Go through the string copying until we find a match with the zap string.
 */
	for (; *cp; cp++)
	{
		if (! strncmp (cp, olds, olen))
		{
			strcpy (rp, argv[2].us_v_ptr);
			rp += strlen (argv[2].us_v_ptr);
			cp += olen - 1;
		}
		else
			*rp++ = *cp;
	}
	*rp = '\0';
	*rett = SYMT_STRING;
	retv->us_v_ptr = usy_string (retstr);
	return (0);
}





static void
Enqueue (pri, cmd)
EQpriority pri;
char *cmd;
/*
 * Enqueue this command at the given priority.
 */
{
	Eq_AddEvent (pri, ui_perform, cmd, strlen (cmd) + 1, Augment);
}
