/*
 * Rubber-band interactive drawing routines.
 */
static char *rcsid = "$Id: RBand.c,v 2.7 1992-07-02 15:45:27 kris Exp $";
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
# include <X11/cursorfont.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>

# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
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
	RBTOutline,
	RBTPoint,
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
 * Outline drawing has some additional parameters.
 */
#define MaxOutline	100
XPoint		OL[MaxOutline];
int		N_OLSeg;

/*
 * Point picking variables.
 */
XPoint		Point;

/*
 * Forwards
 */
static int rb_MakeGC FP((void));
static void rb_Init FP((struct ui_command *));
static void rb_Draw FP((void));
static void rb_ButtonUp FP((XEvent *));
static void rb_ButtonDown FP((XEvent *, char *));
static void rb_Motion FP((int, int));
static void rb_PLButtonDown FP((XEvent *, char *));
static void rb_PLStart FP((XEvent *));
static void rb_PLDelete FP((void));
static void rb_PLButtonUp FP((XEvent *));
static void rb_PLDone FP((void));
static void rb_PLWidget FP((void));
static void rb_MakePLInstructions FP((void));
static void rb_PLRmInstructions FP((void));
static void rb_PLAbort FP((Widget, XtPointer, XtPointer));
static void rb_PLConvert FP((void));
static void rb_PLDrawBoundary FP((void));

void rb_Outline FP ((void));
void rb_Done FP((void));
static void rb_OLButtonDown FP((XEvent *, char *));
static void rb_OLStart FP((XEvent *));
static void rb_OLDelete FP((void));
static void rb_OLButtonUp FP((XEvent *));
static void rb_OLDrawBoundary FP((void));

void rb_Point FP ((void));
static void rb_PointButtonDown FP((XEvent *, char *));
static void rb_DrawPoint FP((void));
static void rb_PointButtonUp FP((XEvent *));


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
# ifdef notdef
	XQueryPointer (Disp, XtWindow (Graphics), &wjunk, &wjunk, &rx, &ry,
		&RBandX0, &RBandY0, (unsigned int *) &mask);
# endif
	RBandX0 = Event_X;
	RBandY0 = Event_Y;
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
	int fg, lwidth;
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
 * Maybe make the lines fatter.
 */
	if (! pda_Search (Pd, "global", "xor-line-width", NULL,
			(char *) &lwidth, SYMT_INT))
		lwidth = 3;
	gcv.line_width = lwidth;
/*
 * Fill in the rest of the stuff, and get our GC.
 */
	gcv.function = GXxor;
	gcv.subwindow_mode = IncludeInferiors;
	RBandGC = XCreateGC (Disp, XtWindow (Graphics),
		GCLineWidth | GCFunction | GCForeground | GCSubwindowMode, &gcv);
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
	    case RBTOutline:
	    case RBTPolyLine:
	    /*
	     * Draw the line
	     */
		XDrawLine (Disp, XtWindow (Graphics), RBandGC, RBandX0,
			RBandY0, RBandX, RBandY);
		break;
	    case RBTPoint:
		XDrawPoint (Disp, XtWindow (Graphics), RBandGC, RBandX0,
			RBandY0);
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
			color, SYMT_STRING))
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
	XtAddCallback (abort, XtNcallback, (XtCallbackProc) rb_PLAbort, 
		(XtPointer) 0);
}




static void
rb_PLRmInstructions ()
/*
 * Get rid of the polyline instructions.
 */
{
	XtUnmanageChild (PolyInstructions);
}



/* ARGSUSED */
static void
rb_PLAbort (wjunk, ajunk1, ajunk2)
Widget wjunk;
XtPointer ajunk1, ajunk2;
/*
 * Abort this polyline.
 */
{
/*
 * Clean up everything and quit.
 */
	Ue_ResetOverride ();
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
	DataChunk	*dc;
	int	 	npt = N_PolySeg + 1, pt;
	ZebTime		zt;
	Location	*pts;
/*
 * If it's empty, what's the point?
 */
	if (! N_PolySeg)
	{
		msg_ELog (EF_PROBLEM, "Empty polyline boundry not stored");
		return;
	}
/*
 * Create the data chunk.
 */
	dc = dc_CreateDC (DCC_Boundary);
	dc->dc_Platform = PolyPlatform;
/*
 * Go through and convert each point.
 */
	pts = (Location *) malloc (npt * sizeof (Location));
	for (pt = 0; pt < npt; pt++)
	{
		cvt_ToLatLon (XUSER (PL[pt].x), YUSER (PL[pt].y), 
			&pts[pt].l_lat, &pts[pt].l_lon);
		pts[pt].l_alt = 0;
	}
/*
 * Add the boundary.
 */
	tl_Time (&zt);
	dc_BndAdd (dc, &zt, PolyPlatform, pts, npt);   
/*
 * Store this polyline, then free our dynamic memory.
 */
	ds_Store (dc, FALSE, NULL, 0);
	dc_DestroyDC (dc);
	free (pts);
}





/*
 * Outline drawing.  Similar to Polyline, but different.  Used by
 * the Data Insertion Widget.
 */
void
rb_Outline ()
/*
 * Start off an outline.
 */
{
	char color[40];
	XColor xc;
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
	N_OLSeg = 0;
	Ue_Override (rb_OLButtonDown, rb_OLButtonUp, rb_Motion);
	if (! RBandGC)
		rb_MakeGC ();
	if (! RBandCursor)
		RBandCursor = XCreateFontCursor (Disp, XC_center_ptr);
	RBandType = RBTOutline;
	XDefineCursor (Disp, XtWindow (Graphics), RBandCursor);
/*
 * Figure out the color to use when we draw in the segments.
 */
	if (! pda_Search (Pd, "global", "outline-color", NULL, color, 
			SYMT_STRING))
		strcpy (color, "white");
	if (ct_GetColorByName (color, &xc))
		XSetForeground (Disp, Gcontext, xc.pixel);
	else
		msg_ELog (EF_PROBLEM, "Unknown outline color '%s'", color);
/*
 * Set up an end of plot handler to redraw the boundary.
 */
	px_SetEOPHandler (rb_OLDrawBoundary);
}





static void
rb_OLButtonDown (event, name)
XEvent *event;
char *name;
/*
 * Deal with a button down event while we are dealing in outlines.
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
		msg_ELog (EF_DEBUG, "Outline abort");
		return;
	}
/*
 * Otherwise they want to do something.  Let's see what it is.
 */
	if (! strcmp (name, "mb-left"))
		rb_OLStart (event);
	else if (! strcmp (name, "mb-middle"))
		rb_OLDelete ();
	else if (! strcmp (name, "mb-right"))
		rb_Done ();
	else
		msg_ELog (EF_INFO, "Unknown button event '%s' in OL", name);
}






static void
rb_OLStart (event)
XEvent *event;
/*
 * We are starting a segment of the outline.
 */
{
	XButtonEvent *button = (XButtonEvent *) event;
/*
 * Here is where we check for overflow.
 */
	if ((N_OLSeg + 1) >= MaxOutline)
	{
		msg_ELog (EF_PROBLEM, "Max outline points (%d) exceeded",
				MaxOutline);
		return;
	}
/*
 * If this is the first segment, we store the current location in the
 * outline array and move away a bit.
 */
	if (N_OLSeg == 0)
	{
		RBandX0 = OL[0].x = button->x;
		RBandY0 = OL[0].y = button->y;
		XWarpPointer (Disp, None, None, 0, 0, 0, 0, 30, 30);
		RBandX = RBandX0 + 30;
		RBandY = RBandY0 + 30;
	}
/*
 * Otherwise we anchor the line at the last point.
 */
	else
	{
		RBandX0 = OL[N_OLSeg].x;
		RBandY0 = OL[N_OLSeg].y;
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
rb_OLDelete ()
/*
 * Delete a point from the outline.
 */
{
/*
 * If there are no points, there are none to return.
 */
	if (N_OLSeg <= 0)
		return;
	N_OLSeg--;
/*
 * Redraw the frame, then redraw the outline on top of it.
 */
	GWDisplayFrame (Graphics, DisplayFrame);
	rb_OLDrawBoundary ();
}



static void
rb_OLDrawBoundary ()
/*
 * Draw the current boundary on the screen.
 */
{
	if (N_OLSeg > 0)
		XDrawLines (Disp, XtWindow (Graphics), Gcontext, OL,
				N_OLSeg + 1, CoordModeOrigin);
}




static void
rb_OLButtonUp (event)
XEvent *event;
/*
 * The button has come up during outline drawing.
 */
{
	XButtonEvent *button = (XButtonEvent *) event;
/*
 * If there is no outline active, we do nothing.  This can happen when
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
	N_OLSeg++;
	OL[N_OLSeg].x = button->x;
	OL[N_OLSeg].y = button->y;
	XDrawLine (Disp, XtWindow (Graphics), Gcontext, OL[N_OLSeg - 1].x,
		OL[N_OLSeg - 1].y, button->x, button->y);
	RBandActive = FALSE;
	
	Eq_ReleaseHold ();
}





/*
 * Point picking.  Used by the Data Insertion Widget.
 */
void
rb_Point ()
/*
 * Start off a point.
 */
{
	char color[40];
	XColor xc;
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
	Ue_Override (rb_PointButtonDown, rb_PointButtonUp, rb_Motion);
	if (! RBandGC)
		rb_MakeGC ();
	if (! RBandCursor)
		RBandCursor = XCreateFontCursor (Disp, XC_center_ptr);
	RBandType = RBTPoint;
	XDefineCursor (Disp, XtWindow (Graphics), RBandCursor);
/*
 * Initialize the point.
 */
	Point.x = Point.y = -9999;
/*
 * Figure out the color to use when we draw in the point.
 */
	if (! pda_Search (Pd, "global", "point-color", NULL, color, 
			SYMT_STRING))
		strcpy (color, "white");
	if (ct_GetColorByName (color, &xc))
		XSetForeground (Disp, Gcontext, xc.pixel);
	else
		msg_ELog (EF_PROBLEM, "Unknown point color '%s'", color);
/*
 * Set up an end of plot handler to redraw the boundary.
 */
	px_SetEOPHandler (rb_DrawPoint);
}





static void
rb_PointButtonDown (event, name)
XEvent *event;
char *name;
/*
 * Deal with a button down event while we are dealing in points.
 */
{
/*
 * Which button.
 */
	if (! strcmp (name, "mb-left") || ! strcmp (name, "mb-middle"))
	{
		Eq_HoldProcess ();
		eq_sync ();
		RBandActive = TRUE;
	}
	else if (! strcmp (name, "mb-right"))
		rb_Done ();
	else
		msg_ELog (EF_INFO, "Unknown button event '%s'", name);
}





static void
rb_DrawPoint ()
/*
 * Draw the current point on the screen.
 */
{
	XDrawPoint (Disp, XtWindow (Graphics), RBandGC, RBandX0, RBandY0);
}




static void
rb_PointButtonUp (event)
XEvent *event;
/*
 * The button has come up, this is the point.
 */
{
	XButtonEvent *button = (XButtonEvent *) event;
/*
 * Save the point.
 */
	RBandX0 = Point.x = button->x;
	RBandY0 = Point.y = button->y;
/*
 * Draw it.
 */
	rb_Draw ();
	RBandActive = FALSE;
	Eq_ReleaseHold ();
}




void
rb_Done ()
/*
 * All finished with whatever we were doing.
 */
{
/*
 * Reset everything and allow the world to continue.
 */
	Ue_ResetOverride ();
	px_ClearEOPHandler ();
	XDefineCursor (Disp, XtWindow (Graphics), NormalCursor);
/*
 * Go back to the old plot mode.
 */
	PlotMode = OldPlotMode;
# ifdef notdef
	pc_PlotHandler ();
# endif
}

