/*
 * Miscellaneous utility functions.
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

# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "GraphProc.h"
# include "PixelCoord.h"
MAKE_RCSID ("$Id: Utilities.c,v 2.2 1991-12-18 23:02:23 corbet Exp $")





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









void
ResetGC ()
/*
 * Restore the graphics context to a "clean" state.
 */
{
	XGCValues vals;

	vals.function = GXcopy;
	vals.line_width = 0;
	vals.line_style = LineSolid;
	vals.cap_style = CapButt;
	vals.join_style = JoinMiter;
	vals.fill_style = FillSolid;
	XChangeGC (Disp, Gcontext, GCFunction | GCLineWidth | GCLineStyle |
		GCCapStyle | GCJoinStyle | GCFillStyle, &vals);
}






void
SetColor (comp, param, qual, def)
char *comp, *param, *qual, *def;
/*
 * Set the color in Gcontext;
 */
{
	char color[40];
	XColor xc;

	if (! pda_Search (Pd, comp, param, qual, color, SYMT_STRING))
		strcpy (color, def);
	if (! ct_GetColorByName (color, &xc))
	{
		msg_ELog (EF_PROBLEM, "Unknown color: %s", color);
		ct_GetColorByName ("white", &xc);
	}
	XSetForeground (Disp, Gcontext, xc.pixel);
}




void
FixForeground (pix)
long pix;
/*
 * Set the GC forground to this pixel value.
 */
{
	XSetForeground (Disp, Gcontext, pix);
}




int
SetLWidth (comp, param, qual, def)
char *comp, *param, *qual;
int def;
/*
 * Set the line width in Gcontext;
 */
{
	int lwidth;
	XColor xc;

	if (! pda_Search (Pd, comp, param, qual, (char *) &lwidth, SYMT_INT))
		lwidth = def;
	FixLWidth (lwidth);
	return (lwidth);
}




void
FixLWidth (width)
int width;
/*
 * Set the line width to this value.
 */
{
	XSetLineAttributes (Disp, Gcontext, width, LineSolid, CapButt,
		JoinMiter);
}




int
AgeCheck (comp, t)
char *comp;
time *t;
/*
 * If this component has an age limit, enforce it.  Return FALSE if the
 * given time is too old, relative to the plot time.
 */
{
	char	limit[100], platform[60], *qual;
	int seconds, psec, dsec;
/*
 * Look for the limit.  If none exists, return TRUE.
 */
	if (pd_Retrieve (Pd, comp, "platform", platform, SYMT_STRING))
		qual = platform;
	else
		qual = 0;
	if (! pda_Search (Pd, comp, "age-limit", qual, limit, SYMT_STRING))
		return (TRUE);
/*
 * Turn this thing into a useful number.
 */
	if (! (seconds = pc_TimeTrigger (limit)))
	{
		msg_ELog (EF_PROBLEM, "Funky age limit: '%s'", limit);
		return (TRUE);
	}
/*
 * Now see how close we are.
 */
	return ((TC_FccToSys (&PlotTime) - TC_FccToSys (t)) <= seconds);
}
