/*
 * Miscellaneous utility functions.
 */
static char *rcsid = "$Id: Utilities.c,v 1.1 1991-03-05 21:24:34 corbet Exp $";

# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "GraphProc.h"
# include "PixelCoord.h"





int
GetLocation (platform, t, loc)
char *platform;
time *t;
Location *loc;
/*
 * Find out where this platform is at this time.
 */
{
	char sloc[80];
/*
 * The data store routine for finding a location isn't there yet.  We'll
 * instead use the fallback -- which will be here anyway -- of finding the
 * location in the defaults table.
 */
	if (! pda_ReqSearch (Pd, "global", "location", platform, sloc,
			SYMT_STRING))
		return (FALSE);
	if (sscanf (sloc, "%f %f %f", &loc->l_lat, &loc->l_lon, &loc->l_alt)
				!= 3)
	{
		msg_ELog (EF_PROBLEM, "Bad location string: '%s'", sloc);
		return (FALSE);
	}
/*
 * Range checking?
 */
	return (TRUE);
}




void
SetClip (full)
int full;
/*
 * Set the clip region in Gcontext.  If FULL is set, it is clipped to the
 * full window; otherwise just the graphics region is allowed.
 */
{
	XRectangle clip;
/*
 * If they said "full", simply turn off clipping altogether.
 */
	if (full)
		XSetClipMask (Disp, Gcontext, None);
/*
 * Otherwise limit it to the area outside of annotation.
 */
	else
	{
		clip.x = F_X0 * GWWidth (Graphics);
		clip.y = (1.0 - F_Y1) * GWHeight (Graphics);
		clip.width = (F_X1 - F_X0) * GWWidth (Graphics);
		clip.height = (F_Y1 - F_Y0) * GWHeight (Graphics);
		XSetClipRectangles (Disp, Gcontext, 0, 0, &clip, 1, Unsorted);
	}
}
