/*
 * Rubber-band interactive drawing routines.
 */
static char *rcsid = "$Id: RBand.c,v 1.6 1991-03-05 21:26:23 corbet Exp $";

# include <X11/Intrinsic.h>
# include <X11/cursorfont.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>

# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "../include/DataStore.h"
# include "GraphProc.h"
# include "PixelCoord.h"

/*
 * Types of things we can rubber band
 */
typedef enum
{
	RBTBox,
	RBTLine,
	RBTPolyLine,
# ifdef notdef		/* not handled yet */
	RBTCircle,
	RBTEllipse,
# endif
} RubberBandType;

/*
 * Cursor to use while rubber banding
 */
static Cursor	RBandCursor = NULL;

/*
 * Rubber band drawing parameters.
 */
static bool RBandActive = FALSE;
static int RBandX0, RBandY0;	/* Fixed point of the rubber band	*/
static int RBandX, RBandY;	/* Floating end of the rubber band	*/
static GC RBandGC = 0;
static RubberBandType	RBandType;
static struct ui_command *RBandCmds;

/*
 * Polyline (boundary) drawing has some additional parameters.
 */
# define MaxPolyLine 100
static XPoint PL[MaxPolyLine];
static int N_PolySeg;
static PlatformId PolyPlatform;
static Widget PolyInstructions = 0;
static enum pmode OldPlotMode;		/* Save plot mode		*/
# define PLINSTR \
"Boundary drawing:  LEFT = enter, MIDDLE = delete, RIGHT = done"

/*
 * Forwards
 */

# ifdef __STDC__
	static int rb_MakeGC (void);
	static void rb_Init (struct ui_command *);
	static void rb_Draw (void);
	static void rb_ButtonUp (XEvent *);
	static void rb_ButtonDown (XEvent *, char *);
	static void rb_Motion (int, int);
	static void rb_PLButtonDown (XEvent *, char *);
	static void rb_PLStart (XEvent *);
	static void rb_PLDelete (void);
	static void rb_PLButtonUp (XEvent *);
	static void rb_PLDone (void);
	static void rb_PLWidget (void);
	static void rb_MakePLInstructions (void);
	static void rb_PLRmInstructions (void);
	static void rb_PLAbort (void);
	static void rb_PLConvert (void);
	static void rb_PLDrawBoundary (void);
# else
	static int rb_MakeGC ();
	static void rb_Init ();
	static void rb_Draw ();
	static void rb_ButtonUp ();
	static void rb_ButtonDown ();
	static void rb_Motion ();
	static void rb_PLButtonDown ();
	static void rb_PLStart ();
	static void rb_PLDelete ();
	static void rb_PLButtonUp ();
	static void rb_PLDone ();
	static void rb_PLWidget ();
	static void rb_MakePLInstructions ();
	static void rb_PLRmInstructions ();
	static void rb_PLAbort ();
	static void rb_PLConvert ();
	static void rb_PLDrawBoundary ();
# endif



void
rb_Box (cmds)
struct ui_command *cmds;
/*
 * Set up to do a rubber-band box.
 */
{
	RBandType = RBTBox;
	rb_Init (cmds);
}




void
rb_Line (cmds)
struct ui_command *cmds;
/*
 * Set up to do a rubber-band box.
 */
{
	RBandType = RBTLine;
	rb_Init (cmds);
}




static void
rb_Init (cmds)
struct ui_command *cmds;
/*
 * Initialize to do rubber banding
 */
{
	Window wjunk;
	int rx, ry, mask;
/*
 * Tell the world that we're doing this.
 */
	RBandActive = TRUE;
	Ue_Override (rb_ButtonDown, rb_ButtonUp, rb_Motion);
	Eq_HoldProcess (); 	/* No replots while we do this	*/
	RBandCmds = uip_clone_clist (cmds);
/*
 * Set up our graphics context.
 */
	if (! RBandGC)
		rb_MakeGC ();
/*
 * Switch to the pencil cursor
 */
	if (! RBandCursor)
		RBandCursor = XCreateFontCursor (Disp, XC_center_ptr);

	XDefineCursor (Disp, XtWindow (Graphics), RBandCursor);
/*
 * Figure out where the pointer is now -- that's our anchor.
 */
	XQueryPointer (Disp, XtWindow (Graphics), &wjunk, &wjunk, &rx, &ry,
		&RBandX0, &RBandY0, (unsigned int *) &mask);
	msg_ELog (EF_DEBUG, "Initial pt at %d %d (%.2f %.2f)", RBandX0,
		RBandY0, XUSER (RBandX0), YUSER (RBandY0));
	msg_ELog (EF_DEBUG, "	(Win is (%2.f %.2f) to (%.2f %.2f))", Xlo, Ylo,
		Xhi, Yhi);
/*
 * Tweak it a bit and draw the first object.
 */
	XWarpPointer (Disp, None, None, 0, 0, 0, 0, 30, 30);
	RBandX = RBandX0 + 30;
	RBandY = RBandY0 + 30;
	rb_Draw ();
	eq_sync ();
}




static int
rb_MakeGC ()
/*
 * Create the rubber band GC.
 */
{
	XGCValues gcv;
	int fg;
/*
 * Figure out which value to use for XORing.  For monochrome, use 1;
 * otherwise we look in the PD.
 */
	if (GWDepth (Graphics) == 1)
		gcv.foreground = 1;
	else
	{
		if (! pda_Search (Pd, "global", "xorvalue", NULL,
					(char *) &fg, SYMT_INT))
			fg = 0x1F;
		gcv.foreground = fg;
	}
/*
 * Fill in the rest of the stuff, and get our GC.
 */
	gcv.function = GXxor;
	gcv.subwindow_mode = IncludeInferiors;
	RBandGC = XCreateGC (Disp, XtWindow (Graphics),
		GCFunction | GCForeground | GCSubwindowMode, &gcv);
}




static void
rb_Draw ()
/*
 * Draw the current object on the screen.
 */
{
	int	x, y;

	switch (RBandType)
	{
	    case RBTBox:
	    /*
	     * Strange things seem to happen if we draw boxes with negative
	     * dimensions.  Rearrange the coords if necessary to only draw
	     * with positive coords.
	     */
		x = (RBandX > RBandX0) ? RBandX0 : RBandX;
		y = (RBandY > RBandY0) ? RBandY0 : RBandY;

		XDrawRectangle (Disp, XtWindow (Graphics), RBandGC, x, y,
			ABS (RBandX - RBandX0), ABS (RBandY - RBandY0));
		break;

	    case RBTLine:
	    case RBTPolyLine:
	    /*
	     * Draw the line
	     */
		XDrawLine (Disp, XtWindow (Graphics), RBandGC, RBandX0,
			RBandY0, RBandX, RBandY);
		break;

	    default:
		msg_ELog (EF_PROBLEM, "Cannot draw rubber band of type %d",
			RBandType);
	}
}





static void
rb_Motion (x, y)
int x, y;
/*
 * Deal with a motion event.
 */
{
/*
 * Draw the current box again, to get rid of it.  Then move and draw the
 * new one.
 */
	if (RBandActive)
	{
		rb_Draw ();
		RBandX = x;
		RBandY = y;
		rb_Draw ();
		eq_sync ();
	}
}





static void
rb_ButtonUp (event)
XEvent *event;
/*
 * Deal with a button up event.
 */
{
	SValue v;
/*
 * Pull the object from the screen.
 */
	rb_Draw ();
/*
 * Switch to the normal cursor
 */
	XDefineCursor (Disp, XtWindow (Graphics), NormalCursor);
/*
 * Clean up.
 */
	msg_ELog (EF_DEBUG, "Final coords at %d %d (%.2f %.2f)",
		RBandX, RBandY, XUSER (RBandX), YUSER (RBandY));
	RBandActive = FALSE;
	Ue_ResetOverride ();
	Eq_ReleaseHold ();
/*
 * Execute the rest of the stuff.
 */
	switch (RBandType)
	{
	    case RBTBox:
	    /*
	     * Save the box corners in the variable table
	     */
		v.us_v_float = RBandX > RBandX0 ? XUSER (RBandX0) :
			XUSER (RBandX);
		usy_s_symbol (Vtable, "boxx0", SYMT_FLOAT, &v);
		v.us_v_float = RBandX > RBandX0 ? XUSER (RBandX) :
			XUSER (RBandX0);
		usy_s_symbol (Vtable, "boxx1", SYMT_FLOAT, &v);

		v.us_v_float = RBandY > RBandY0 ? YUSER (RBandY0) :
			YUSER (RBandY);
		usy_s_symbol (Vtable, "boxy0", SYMT_FLOAT, &v);
		v.us_v_float = RBandY > RBandY0 ? YUSER (RBandY) :
			YUSER (RBandY0);
		usy_s_symbol (Vtable, "boxy1", SYMT_FLOAT, &v);

		break;
	    case RBTLine:
	    /*
	     * Save the line endpoints in the variable table
	     */
		v.us_v_float = XUSER (RBandX0);
		usy_s_symbol (Vtable, "linex0", SYMT_FLOAT, &v);
		v.us_v_float = XUSER (RBandX);
		usy_s_symbol (Vtable, "linex1", SYMT_FLOAT, &v);

		v.us_v_float = YUSER (RBandY0);
		usy_s_symbol (Vtable, "liney0", SYMT_FLOAT, &v);
		v.us_v_float = YUSER (RBandY);
		usy_s_symbol (Vtable, "liney1", SYMT_FLOAT, &v);

		break;
	    default:
		msg_ELog (EF_PROBLEM, "Cannot finish rubber band of type %d",
			RBandType);
	}

	dispatcher (0, RBandCmds);
	uip_release (RBandCmds);
}




static void
rb_ButtonDown (event, name)
XEvent *event;
char *name;
/*
 * Deal with getting a second button down during rubber band drawing.  We
 * interpret such an event as a user abort.
 */
{
/*
 * Switch to the normal cursor
 */
	XDefineCursor (Disp, XtWindow (Graphics), NormalCursor);
/*
 * Clean up
 */
	msg_ELog (EF_DEBUG, "Rubber band abort");
	rb_Draw ();
	RBandActive = FALSE;
	Ue_ResetOverride ();
	uip_release (RBandCmds);	/* No execution	*/
	Eq_ReleaseHold ();
}




void
rb_PolyLine (cmds)
struct ui_command *cmds;
/*
 * Start off a polyline.
 */
{
	char color[40];
	XColor xc;
/*
 * Look up the platform of interest.
 */
	if ((PolyPlatform = ds_LookupPlatform (UPTR (*cmds))) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown polyline platform %s",
				UPTR (*cmds));
		return;
	}
/*
 * Put up the instructions widget.
 */
	rb_PLWidget ();
/*
 * Remember our old plot mode, and move to history.  Cancel timer and
 * notification events.
 */
	OldPlotMode = PlotMode;
	PlotMode = History;
	tl_AllCancel ();
	ds_CancelNotify ();
/*
 * Initialize everything.
 */
	N_PolySeg = 0;
	Ue_Override (rb_PLButtonDown, rb_PLButtonUp, rb_Motion);
	/* Eq_HoldProcess (); */
	if (! RBandGC)
		rb_MakeGC ();
	if (! RBandCursor)
		RBandCursor = XCreateFontCursor (Disp, XC_center_ptr);
	RBandType = RBTPolyLine;
	XDefineCursor (Disp, XtWindow (Graphics), RBandCursor);
/*
 * Figure out the color to use when we draw in the segments.
 */
	if (! pda_Search (Pd, "global", "polyline-color", UPTR (*cmds),
			color, SYMT_STRING));
		strcpy (color, "white");
	if (ct_GetColorByName (color, &xc))
		XSetForeground (Disp, Gcontext, xc.pixel);
	else
		msg_ELog (EF_PROBLEM, "Unknown polyline color '%s'", color);
/*
 * Set up an end of plot handler to redraw the boundary.
 */
	px_SetEOPHandler (rb_PLDrawBoundary);
}





static void
rb_PLButtonDown (event, name)
XEvent *event;
char *name;
/*
 * Deal with a button down event while we are dealing in polylines.
 */
{
/*
 * If we are actually drawing a line now, we take this as a desire to
 * abort this particular line.
 */
	if (RBandActive)
	{
		rb_Draw ();
		RBandActive = FALSE;
		Eq_ReleaseHold ();
		msg_ELog (EF_DEBUG, "PolyLine abort");
		return;
	}
/*
 * Otherwise they want to do something.  Let's see what it is.
 */
	if (! strcmp (name, "mb-left"))
		rb_PLStart (event);
	else if (! strcmp (name, "mb-middle"))
		rb_PLDelete ();
	else if (! strcmp (name, "mb-right"))
		rb_PLDone ();
	else
		msg_ELog (EF_INFO, "Unknown button event '%s' in PL", name);
}






static void
rb_PLStart (event)
XEvent *event;
/*
 * We are starting a segment of the polyline.
 */
{
	XButtonEvent *button = (XButtonEvent *) event;
/*
 * Here is where we check for overflow.
 */
	if ((N_PolySeg + 1) >= MaxPolyLine)
	{
		msg_ELog (EF_PROBLEM, "Max polyline points (%d) exceeded",
				MaxPolyLine);
		return;
	}
/*
 * If this is the first segment, we store the current location in the
 * polyline array and move away a bit.
 */
	if (N_PolySeg == 0)
	{
		RBandX0 = PL[0].x = button->x;
		RBandY0 = PL[0].y = button->y;
		XWarpPointer (Disp, None, None, 0, 0, 0, 0, 30, 30);
		RBandX = RBandX0 + 30;
		RBandY = RBandY0 + 30;
	}
/*
 * Otherwise we anchor the line at the last point.
 */
	else
	{
		RBandX0 = PL[N_PolySeg].x;
		RBandY0 = PL[N_PolySeg].y;
		RBandX = button->x;
		RBandY = button->y;
	}
/*
 * Draw this segment.
 */
	rb_Draw ();
	Eq_HoldProcess ();
	eq_sync ();
	RBandActive = TRUE;
}





static void
rb_PLDelete ()
/*
 * Delete a point from the polyline.
 */
{
	int seg;
/*
 * If there are no points, there are none to return.
 */
	if (N_PolySeg <= 0)
		return;
	N_PolySeg--;
/*
 * Redraw the frame, then redraw the polyline on top of it.
 */
	GWDisplayFrame (Graphics, DisplayFrame);
	rb_PLDrawBoundary ();
}



static void
rb_PLDrawBoundary ()
/*
 * Draw the current boundary on the screen.
 */
{
	if (N_PolySeg > 0)
		XDrawLines (Disp, XtWindow (Graphics), Gcontext, PL,
				N_PolySeg + 1, CoordModeOrigin);
}




static void
rb_PLButtonUp (event)
XEvent *event;
/*
 * The button has come up during polyline drawing.
 */
{
	XButtonEvent *button = (XButtonEvent *) event;
/*
 * If there is no polyline active, we do nothing.  This can happen when
 * the user asks to delete a point.
 */
	if (! RBandActive)
		return;
/*
 * Remove the rubberband line.
 */
	rb_Draw ();
/*
 * Now store the new endpoints and draw the segment "permanently".
 */
	N_PolySeg++;
	PL[N_PolySeg].x = button->x;
	PL[N_PolySeg].y = button->y;
	XDrawLine (Disp, XtWindow (Graphics), Gcontext, PL[N_PolySeg - 1].x,
		PL[N_PolySeg - 1].y, button->x, button->y);
	RBandActive = FALSE;
	
	Eq_ReleaseHold ();
}




static void
rb_PLDone ()
/*
 * All finished with polyline drawing.
 */
{
/*
 * Reset everything and allow the world to continue.
 */
	Ue_ResetOverride ();
	/* Eq_ReleaseHold (); */
	rb_PLRmInstructions ();
	px_ClearEOPHandler ();
	XDefineCursor (Disp, XtWindow (Graphics), NormalCursor);
/*
 * Convert this boundary into a data object and ship it off.  Go back
 * to the old plot mode.
 */
	rb_PLConvert ();
	PlotMode = OldPlotMode;
	pc_PlotHandler ();
}





static void
rb_PLWidget ()
/*
 * Put up our instructions widget.
 */
{
/*
 * If it does not yet exist, create it.
 */
	if (! PolyInstructions)
		 rb_MakePLInstructions ();
/*
 * Put this guy up into the top corner.
 */
	XtManageChild (PolyInstructions);
	eq_sync ();
}




static void
rb_MakePLInstructions ()
/*
 * Make the polyline instructions widget.
 */
{
	Arg args[10];
	Widget label, abort;
	int n;
/*
 * Make a form to hold the whole thing.
 */
	n = 0;
	XtSetArg (args[n], XtNx, 5);		n++;
	XtSetArg (args[n], XtNy, 0);		n++;
	PolyInstructions = XtCreateWidget ("PLInstructions", formWidgetClass,
						Graphics, args, n);
/*
 * Add the label to it.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, PLINSTR);		n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);		n++;
	label = XtCreateManagedWidget ("label", labelWidgetClass,
			PolyInstructions, args, n);
/*
 * And the abort button.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Click here to abort");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
	XtSetArg (args[n], XtNfromVert, label);			n++;
	abort = XtCreateManagedWidget ("abort", commandWidgetClass,
			PolyInstructions, args, n);
	XtAddCallback (abort, XtNcallback, rb_PLAbort, 0);
}




static void
rb_PLRmInstructions ()
/*
 * Get rid of the polyline instructions.
 */
{
	XtUnmanageChild (PolyInstructions);
}



static void
rb_PLAbort ()
/*
 * Abort this polyline.
 */
{
/*
 * Clean up everything and quit.
 */
	Ue_ResetOverride ();
	/* Eq_ReleaseHold (); */
	rb_PLRmInstructions ();
	px_ClearEOPHandler ();
	XDefineCursor (Disp, XtWindow (Graphics), NormalCursor);
	PlotMode = OldPlotMode;
	pc_PlotHandler ();
}






static void
rb_PLConvert ()
/*
 * We have a polyline -- turn it into a data object and put it into
 * the data store.
 */
{
	DataObject dobj;
	int npt = N_PolySeg + 1, pt;
/*
 * If it's empty, what's the point?
 */
	if (! N_PolySeg)
	{
		msg_ELog (EF_PROBLEM, "Empty polyline boundry not stored");
		return;
	}
/*
 * Start putting together our data object.
 */
	dobj.do_id = PolyPlatform;
	tl_GetTime (&dobj.do_begin);
	dobj.do_end = dobj.do_begin;
	dobj.do_org = OrgOutline;
	dobj.do_npoint = 1;
	dobj.do_aloc = (Location *) malloc (npt * sizeof (Location));
	dobj.do_times = (time *) malloc (npt * sizeof (time));
	dobj.do_desc.d_length = &npt;
	dobj.do_nfield = 0;
	dobj.do_flags = 0;
	dobj.do_badval = 0.0;
/*
 * Go through and convert each point.
 */
	for (pt = 0; pt < npt; pt++)
	{
		dobj.do_times[pt] = dobj.do_begin;
		cvt_ToLatLon (XUSER (PL[pt].x), YUSER (PL[pt].y), 
			&dobj.do_aloc[pt].l_lat, &dobj.do_aloc[pt].l_lon);
		dobj.do_aloc[pt].l_alt = 0;
	}
/*
 * Store this polyline, then free our dynamic memory.
 */
	ds_PutData (&dobj, FALSE);
	free (dobj.do_aloc);
	free (dobj.do_times);
}
