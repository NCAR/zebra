/*
 * Deal with static (or almost static) overlays.
 */
static char *rcsid = "$Id";

# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "GraphProc.h"
# include "PixelCoord.h"



/*
 * Our internal overlay drawing routines.
 */
# ifdef __STDC__
	static void ov_GridBBox (char *, int);
# else
	static void ov_GridBBox ();
# endif


/*
 * The table to map the overlay type ("field" parameter) onto the function
 * that draws it.
 */
static struct overlay_table
{
	char *ot_type;		/* Overlay type		*/
	void (*ot_func) ();	/* Function.		*/
} Ov_table[] =
{
	{ "gridbbox",	ov_GridBBox	},
	{ 0, 0}
};







void
ov_CAPOverlay (comp, update)
char *comp;
bool update;
/*
 * Draw an overlay onto the screen.
 */
{
	int i;
	char type[50];
/*
 * Pull out the type of overlay they want.
 */
	if (! px_GetParam (comp, "field", NULL, type, SYMT_STRING))
		return;
/*
 * Simply try to find a routine which can do this.
 */	
	for (i = 0; Ov_table[i].ot_type; i++)
		if (! strcmp (Ov_table[i].ot_type, type))
			break;
	if (! Ov_table[i].ot_type)
	{
		msg_ELog (EF_PROBLEM, "Unknown overlay type: %s", type);
		return;
	}
/*
 * Invoke it.
 */
	(*Ov_table[i].ot_func) (comp, update);
}







static void
ov_GridBBox (comp, update)
char *comp;
bool update;
/*
 * Draw the bounding box of the grid indicated by the platform field.
 */
{
	char platform[40], color[40];
	float x0, y0, x1, y1;
	int lwidth, px0, py0, px1, py1;
	GC gcontext;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	Drawable d = GWFrame (Graphics);
/*
 * Find our platform.
 */
	if (! px_GetParam (comp, "platform", NULL, platform, SYMT_STRING))
		return;
/*
 * Now try to pull in the bounding box.
 */
	if (! ga_GridBBox (&PlotTime, platform, &x0, &y0, &x1, &y1))
	{
		msg_ELog (EF_INFO, "Unable to load grid bbox for %s",platform);
		return;
	}
	px0 = XPIX (x0); py0 = YPIX (y0);
	px1 = XPIX (x1); py1 = YPIX (y1);
/*
 * Color information.
 */
	if (! pda_Search (Pd, comp, "color", "overlay", color, SYMT_STRING))
		strcpy (color, "white");
/*
 * Graphics context stuff.
 */
	gcontext = XCreateGC (disp, XtWindow (Graphics), 0, NULL);
	ct_GetColorByName (color, &xc);
	XSetForeground (disp, gcontext, xc.pixel);
	if (pda_Search (Pd, comp, "line-width", "overlay", (char *) &lwidth,
			SYMT_INT))
		XSetLineAttributes (disp, gcontext, lwidth, LineSolid,
			CapButt, JoinMiter);
/*
 * Just draw.
 */
	XDrawLine (disp, d, gcontext, px0, py0, px1, py0);
	XDrawLine (disp, d, gcontext, px1, py0, px1, py1);
	XDrawLine (disp, d, gcontext, px1, py1, px0, py1);
	XDrawLine (disp, d, gcontext, px0, py1, px0, py0);
/*
 * Clean up and go home.
 */
	XFreeGC (disp, gcontext);
}
