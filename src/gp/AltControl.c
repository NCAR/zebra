/*
 * Altitude control for CAP plots.
 */
static char *rcsid = "$Id: AltControl.c,v 2.3 1992-09-22 20:12:07 corbet Exp $";
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
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/Xaw/Scrollbar.h>

# include <defs.h>
# include <message.h>
# include <pd.h>
# include <DataStore.h>
# include "GraphProc.h"


# define MAXALT		80	/* Max heights we expect to see		*/


# ifdef __STDC__
	static int alt_GetRSAlts (char *, float *);
# else
	static int alt_GetRSAlts ();
# endif



void
alt_Step (nstep)
int nstep;
/*
 * Step the altitude by this many steps.
 */
{
	int nalt, closest = 0, i, rspace = 0;
	float alts[MAXALT], alt, dist = 9999.9;
	char platform[40], **comps = pd_CompList (Pd), calt[20];
/*
 * Find out our current altitude, and whether we are operating in radar
 * space or not.
 */
	pd_Retrieve (Pd, "global", "altitude", (char *) &alt, SYMT_FLOAT);
	if (! pd_Retrieve (Pd, comps[AltControlComp], "platform", platform,
			SYMT_STRING))
		return;
	if (! pd_Retrieve (Pd, "global", "radar-space", (char *) &rspace,
				SYMT_BOOL))
		rspace = FALSE;
/*
 * Now, depending on our model of the world, we choose one way or the other
 * to look for the available altitudes.
 */
	if (! rspace)
	{
		if (! ga_AvailableAlts (&PlotTime, platform, alts, &nalt))
		{
			msg_ELog (EF_DEBUG, "No available alts for %s",
					platform);
			return;
		}
	}
	else
	{
		if ((nalt = alt_GetRSAlts (platform, alts)) <= 0)
		{
			msg_ELog (EF_DEBUG, "No available RS alts for %s",
					platform);
			return;
		}
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
	sprintf (calt, "%.2f", alts[closest]);
	parameter ("global", "altitude", calt);
}





static int
alt_GetRSAlts (platform, alts)
char *platform;
float *alts;
/*
 * Find the list of available radar space altitudes.
 */
{
	ZebTime	stimes[MAXALT], otimes[2];
	PlatformId	pid;
	Location	locs[MAXALT];
	char	cattr[200], *attr = NULL;
	int	i, nalt, ntime;
/*
 * Find our platform first.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform: %s", platform);
		return (-1);
	}
/*
 * See if there is a filter attribute to apply.
 */
	if (pda_Search (Pd, "global", "filter-attribute", platform,
			cattr, SYMT_STRING))
		attr = cattr;
/*
 * Now we have to copy out the altitudes to match what is expected
 * in alt_Step;
 */
	if ((ntime = ds_GetObsTimes (pid, &PlotTime, otimes, 2, attr)) <= 0)
		return (0);
	nalt = ds_GetObsSamples (pid, otimes, stimes, locs, MAXALT);
	for (i = 0; i < nalt; i++)
		alts[i] = locs[i].l_alt;
/*
 * Now look at the previous observation and get any tilts higher than
 * what we have here.
 */
	if (ntime > 1)
	{
		int na = ds_GetObsSamples (pid, otimes + 1, stimes, locs,
				MAXALT);
		for (i = 0; i < na; i++)
			if (locs[i].l_alt > alts[nalt - 1])
				alts[nalt++] = locs[i].l_alt;
	}
	return (nalt);
}
