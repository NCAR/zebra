/*
 * Altitude control for CAP plots.
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
# include "EventQueue.h"

/*
 * Index of the altitude control component
 */
int	AltControlComp;

MAKE_RCSID("$Id: AltControl.c,v 2.8 1994-05-25 14:31:02 burghart Exp $")

# define MAXALT		80	/* Max heights we expect to see		*/

/*
 * Prototypes
 */
static void	alt_GetControlComp FP ((void));
static void	alt_SetAlt FP ((int));
static int	alt_GetRSAlts FP ((char *, float *));



void
alt_Initialize ()
/*
 * Set a good initial "altitude" parameter in our PD
 */
{
	float	alt;
	char	label[32];
/*
 * Find the control component
 */
	alt_GetControlComp ();
/*
 * Now set the "altitude" and "altitude-label" parameters if we don't have them
 */
	if (! pda_Search (Pd, "global", "altitude", NULL, (char *)&alt, 
			  SYMT_FLOAT) ||
	    ! pda_Search (Pd, "global", "altitude-label", NULL, label,
			  SYMT_STRING))
		alt_SetAlt (0);
}


static void
alt_GetControlComp ()
/*
 * Find the altitude control component of our PD.
 */
{
	char altcomp[80], **comps = pd_CompList (Pd), plat[CFG_PLATNAME_LEN];
	bool control;
	int i;
/*
 * Algorithm for finding the control component:
 *
 * (1) See if it is simply specified in the global component.  If so, see 
 *     further that it exists, and is not disabled.
 */
	if (pda_Search (Pd, "global", "altitude-control", NULL, altcomp,
			SYMT_STRING))
	{
		for (i = 0; comps[i]; i++)
			if (! strcmp (comps[i], altcomp))
				break;
		if (comps[i]) 	/* Disabled? */
		{
			bool disabled = FALSE;
			if (! pda_Search (Pd, comps[i], "disable", NULL, 
				(char *) &disabled, SYMT_BOOL) || ! disabled)
			{
				AltControlComp = i;
				return;
			}
		}
		else
			msg_ELog (EF_PROBLEM,
				"Altitude control comp %s missing", altcomp);
	}
/*
 * That didn't work out, so:
 *
 * (2) Search each component for an "altitude control" parameter.  If we 
 *     find it, that component takes over.  "altitude-control" is qualified
 *     by the platform name.
 */
	for (i = 1; comps[i]; i++)
	{
		if (! pda_Search (Pd, comps[i], "platform", NULL, plat,
					SYMT_STRING))
			continue;	/* No plat?? */
		if (pda_Search (Pd, comps[i], "altitude-control", plat,
				(char *) &control, SYMT_BOOL) && control)
		{
			AltControlComp = i;
			return;
		}
	}
/*
 * (3) Just use the base, like things used to be in the good olde days, as
 *     long as there is a base to use.
 */
	if (comps[1])
		AltControlComp = 1;
	else
		AltControlComp = 0;
}


	
	
void
alt_Step (nstep)
/*
 * Step the altitude by this many steps and schedule a replot.
 */
{
	alt_SetAlt (nstep);
        Eq_AddEvent (PDisplay, pc_ParamChange, "altitude", 9, Augment);
}



	
static void
alt_SetAlt (nstep)
int nstep;
/*
 * Set the "altitude" parameter in our PD, using the current value (if any) as
 * a target, and stepping nstep levels from there.  We do not attempt to 
 * schedule a replot from here, since this is often called to initialize the
 * altitude at the beginning of a new plot.
 */
{
	int nalt, closest = 0, i;
	bool rspace, have_target;
	float alts[MAXALT], target_alt, dist, temp;
	char platform[40], field[40], **comps = pd_CompList (Pd), scratch[40];
	FieldId fid;
	PlatformId pid;
	AltUnitType altunits;
/*
 * Get the platform and field from the control component.  If either is 
 * missing, we just use 0 km MSL.
 */
	if (! pd_Retrieve (Pd, comps[AltControlComp], "platform", platform,
			   SYMT_STRING) ||
	    (! pd_Retrieve (Pd, comps[AltControlComp], "field", field, 
			   SYMT_STRING) &&
	     ! pd_Retrieve (Pd, comps[AltControlComp], "u-field", field,
			    SYMT_STRING)))
	{
		pd_Store (Pd, "global", "altitude-label", "",
			  SYMT_STRING);
		return;
	/*
	 * Schedule to send the modified PD to dm and the PD monitor
	 */
		Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
		pdm_ScheduleUpdate ();

		return;
	}
/*
 * Radar space?
 */
	rspace = FALSE;
	pd_Retrieve (Pd, "global", "radar-space", (char *) &rspace, SYMT_BOOL);
/*
 * Now, depending on our model of the world, we choose one way or the other
 * to look for the available altitudes.
 */
	if (! rspace)
	{
	/*
	 * Get platform and field IDs.
	 */
		if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "alt_Step: bad platform '%s'",
				  platform);
			return;
		}

		fid = F_Lookup (field);
	/*
	 * Check it out.
	 */
		if (! ds_GetAlts (pid, fid, &PlotTime, ForecastOffset, alts, 
				  &nalt, &altunits))
		{
			msg_ELog (EF_DEBUG, 
				  "No available alts for %s/%s @ %d h offset",
				  platform, field, ForecastOffset / 3600);
			return;
		}
	/*
	 * semi-KLUGE: Force the array to be increasing if altitude or 
	 * decreasing if pressure
	 */
		if ((altunits == AU_mb && alts[0] < alts[1]) ||
		    (altunits != AU_mb && alts[0] > alts[1]))
		{
			for (i = 0; i < nalt / 2; i++)
			{
				temp = alts[i];
				alts[i] = alts[nalt - i - 1];
				alts[nalt - i - 1] = temp;
			}
		}
	}
	else
	{
		if ((nalt = alt_GetRSAlts (platform, alts)) <= 0)
		{
			pd_RemoveParam (Pd, "global", "altitude");
			pd_Store (Pd, "global", "altitude-label", "?", 
				  SYMT_STRING);

			msg_ELog (EF_DEBUG, "No available RS alts for %s",
				  platform);
			return;
		}
	}
/*
 * Existing "altitude" becomes a target starting place.  Otherwise, we'll
 * start from the zeroth altitude.
 */
	have_target = pd_Retrieve (Pd, "global", "altitude", 
				   (char *) &target_alt, SYMT_FLOAT);
/*
 * Now go through and find the closest one to our target.
 */
	if (have_target)
	{
		closest = 0;
		dist = ABS (alts[0] - target_alt);
		for (i = 1; (dist) && (i < nalt); i++)
		{
			if (ABS (alts[i] - target_alt) < dist)
			{
				dist = ABS (alts[i] - target_alt);
				closest = i;
			}
		}
	}
	else
		closest = 0;
/*
 * Step by the given amount, and apply bounds.
 */
	if ((closest += nstep) < 0)
		closest = 0;
	else if (closest >= nalt)
		closest = nalt - 1;
/*
 * Stash the new altitude
 */
	sprintf (scratch, "%.2f", alts[closest]);
	pd_Store (Pd, "global", "altitude", scratch, SYMT_STRING);
/*
 * and the new altitude label
 */
	if (! rspace)
	{
		sprintf (scratch, "Alt: %s", 
			 au_AltLabel (alts[closest], altunits));
		pd_Store (Pd, "global", "altitude-label", scratch, 
			  SYMT_STRING);
	}
	else
	{
		sprintf (scratch, "Elev: %.1f deg", alts[closest]);
		pd_Store (Pd, "global", "altitude-label", scratch,
			  SYMT_STRING);
	}
/*
 * Schedule to send the modified PD to dm and the PD monitor
 */
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
	pdm_ScheduleUpdate ();
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
