/*
 * Altitude control for CAP plots.
 */
static char *rcsid = "$Id: AltControl.c,v 1.1 1990-07-06 11:16:59 corbet Exp $";

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/Scrollbar.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "GraphProc.h"


# define MAXALT		80	/* Max heights we expect to see		*/


void
alt_Step (nstep)
int nstep;
/*
 * Step the altitude by this many steps.
 */
{
	int nalt, alts[50], alt, dist = 99999, closest = 0, i;
	char platform[40], **comps = pd_CompList (Pd), calt[20];
/*
 * Find out our current altitude, and what the choices are.
 */
	pd_Retrieve (Pd, "global", "altitude", (char *) &alt, SYMT_INT);
	if (! pd_Retrieve (Pd, comps[1], "platform", platform, SYMT_STRING))
		return;
	if (! ga_AvailableAlts (&PlotTime, platform, alts, &nalt))
	{
		msg_ELog (EF_DEBUG, "No available alts for %s", platform);
		return;
	}
/*
 * Now go through and find the closest one to where we are now.
 */
	for (i = 0; i < nalt; i++)
		if (ABS (alts[i] - alt) < dist)
		{
			dist = ABS (alts[i] - alt);
			closest = i;
		}
/*
 * Step by the given amount, and apply bounds.
 */
	if ((closest += nstep) < 0)
		closest = 0;
	else if (closest >= nalt)
		closest = nalt - 1;
/*
 * Store the new altitude.  This method is a bit kludgy, but it works, and
 * will cause things to be redrawn.
 */
	sprintf (calt, "%d", alts[closest]);
	parameter ("global", "altitude", calt);
}
