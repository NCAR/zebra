/*
 * Rubber-band interactive drawing routines.
 */
static char *rcsid = "$Id: RBand.c,v 1.3 1991-01-09 16:17:35 burghart Exp $";

# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "GraphProc.h"
# include "PixelCoord.h"

/*
 * Types of things we can rubber band
 */
typedef enum
{
	RBTBox,
	RBTLine,
# ifdef notdef		/* not handled yet */
	RBTCircle,
	RBTEllipse,
# endif
} RubberBandType;

/*
 * Rubber band drawing parameters.
 */
static bool RBandActive = FALSE;
static int RBandX0, RBandY0;	/* Fixed point of the rubber band	*/
static int RBandX, RBandY;	/* Floating end of the rubber band	*/
static GC RBandGC = 0;
static RubberBandType	RBandType;
static struct ui_command *RBandCmds;

static void	rb_Init (), rb_Motion (), rb_ButtonDown (), rb_ButtonUp ();
static void	rb_Draw ();

# ifdef __STDC__
	static int rb_MakeGC (void);
# else
	static int rb_MakeGC ();
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




void
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
	HoldProcess = TRUE;		/* No replots while we do this	*/
	RBandCmds = uip_clone_clist (cmds);
/*
 * Set up our graphics context.
 */
	if (! RBandGC)
		rb_MakeGC ();
/*
 * Figure out where the pointer is now -- that's our anchor.
 */
	XQueryPointer (Disp, XtWindow (Graphics), &wjunk, &wjunk, &rx, &ry,
		&RBandX0, &RBandY0, (unsigned int *) &mask);
	msg_ELog (EF_DEBUG, "Initial pt at %d %d (%.2f %.2f)", RBandX0, RBandY0,
		XUSER (RBandX0), YUSER (RBandY0));
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




void
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
	rb_Draw ();
	RBandX = x;
	RBandY = y;
	rb_Draw ();
	eq_sync ();
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
 * Clean up.
 */
	msg_ELog (EF_DEBUG, "Final coords at %d %d (%.2f %.2f)", 
		RBandX, RBandY, XUSER (RBandX), YUSER (RBandY));
	RBandActive = FALSE;
	Ue_ResetOverride ();
	HoldProcess = FALSE;
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
	msg_ELog (EF_DEBUG, "Rubber band abort");
	rb_Draw ();
	RBandActive = FALSE;
	Ue_ResetOverride ();
	uip_release (RBandCmds);	/* No execution	*/
	HoldProcess = FALSE;
}
