/*
 * Rubber-band interactive drawing routines.
 */
static char *rcsid = "$Id: RBand.c,v 1.1 1990-09-17 10:22:18 corbet Exp $";

# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "GraphProc.h"
# include "PixelCoord.h"



/*
 * Box-drawing parameters.
 */
static bool BoxActive = FALSE;
static int BoxX, BoxY;			/* The fixed point in the box	*/
static int BoxW, BoxH;			/* Dimensions of the box	*/
static GC BoxGC = 0;
static struct ui_command *BoxCmds;

static void rb_BoxMotion (), rb_BoxBDown (), rb_BoxBUp (), rb_DrawBox ();
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
	Window wjunk;
	int rx, ry, mask;
/*
 * Tell the world that we're doing this.
 */
	BoxActive = TRUE;
	Ue_Override (rb_BoxBDown, rb_BoxBUp, rb_BoxMotion);
	HoldProcess = TRUE;		/* No replots while we do this	*/
	BoxCmds = uip_clone_clist (cmds);
/*
 * Set up our graphics context.
 */
	if (! BoxGC)
		rb_MakeGC ();
/*
 * Figure out where the pointer is now -- that's our anchor.
 */
	XQueryPointer (Disp, XtWindow (Graphics), &wjunk, &wjunk, &rx, &ry,
		&BoxX, &BoxY, (unsigned int *) &mask);
	msg_ELog (EF_DEBUG, "Initial pt at %d %d (%.2f %.2f)", BoxX, BoxY,
		XUSER (BoxX), YUSER (BoxY));
	msg_ELog (EF_DEBUG, "	(Win is (%2.f %.2f) to (%.2f %.2f))", Xlo, Ylo,
		Xhi, Yhi);
/*
 * Tweak it a bit and draw the first box.
 */
	XWarpPointer (Disp, None, None, 0, 0, 0, 0, 30, 30);
	BoxW = 30;
	BoxH = 30;
	rb_DrawBox ();
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
		if (! pda_Search (Pd, "global", "xorvalue", NULL,&fg,SYMT_INT))
			fg = 0x1F;
		gcv.foreground = fg;
	}
/*
 * Fill in the rest of the stuff, and get our GC.
 */
	gcv.function = GXxor;
	gcv.subwindow_mode = IncludeInferiors;
	BoxGC = XCreateGC (Disp, XtWindow (Graphics), 
		GCFunction | GCForeground | GCSubwindowMode, &gcv);
}



void
rb_DrawBox ()
/*
 * Draw the current box on the screen.
 */
{
/*
 * Strange things seem to happen if we draw boxes with negative dimensions.
 * Rather that leave junk on the screen, we'll just rearrange the coords
 * if necessary to only draw with positive coords.
 */
	int x = (BoxW > 0) ? BoxX : BoxX + BoxW;
	int y = (BoxH > 0) ? BoxY : BoxY + BoxH;

	XDrawRectangle (Disp, XtWindow (Graphics), BoxGC, x, y, 
		ABS (BoxW), ABS (BoxH));
}





static void
rb_BoxMotion (x, y)
int x, y;
/*
 * Deal with a motion event.
 */
{
/*
 * Draw the current box again, to get rid of it.  Then move and draw the
 * new one.
 */
	rb_DrawBox ();
	BoxW = x - BoxX;
	BoxH = y - BoxY;
	rb_DrawBox ();
	eq_sync ();
}





static void
rb_BoxBUp (event)
XEvent *event;
/*
 * Deal with a button up event.
 */
{
	SValue v;
/*
 * Pull the box from the screen.
 */
	rb_DrawBox ();
/*
 * Clean up.
 */
	msg_ELog (EF_DEBUG, "Final coords at %d %d (%.2f %.2f)", BoxX + BoxW,
		BoxY + BoxH, XUSER (BoxX + BoxW), YUSER (BoxY + BoxH));
	BoxActive = FALSE;
	Ue_ResetOverride ();
	HoldProcess = FALSE;
/*
 * Execute the rest of the stuff.
 */
	v.us_v_float = BoxW < 0 ? XUSER (BoxX + BoxW) : XUSER (BoxX);
	usy_s_symbol (Vtable, "boxx0", SYMT_FLOAT, &v);
	v.us_v_float = BoxW >= 0 ? XUSER (BoxX + BoxW) : XUSER (BoxX);
	usy_s_symbol (Vtable, "boxx1", SYMT_FLOAT, &v);

	v.us_v_float = BoxH >= 0 ? YUSER (BoxY + BoxH) : YUSER (BoxY);
	usy_s_symbol (Vtable, "boxy0", SYMT_FLOAT, &v);
	v.us_v_float = BoxH < 0 ? YUSER (BoxY + BoxH) : YUSER (BoxY);
	usy_s_symbol (Vtable, "boxy1", SYMT_FLOAT, &v);

	dispatcher (0, BoxCmds);
	uip_release (BoxCmds);
}




static void
rb_BoxBDown (event, name)
XEvent *event;
char *name;
/*
 * Deal with getting a second button down during box drawing.  We 
 * interpret such an event as a user abort.
 */
{
	msg_ELog (EF_DEBUG, "Box abort");
	rb_DrawBox ();
	BoxActive = FALSE;
	Ue_ResetOverride ();
	uip_release (BoxCmds);	/* No execution	*/
	HoldProcess = FALSE;
}
