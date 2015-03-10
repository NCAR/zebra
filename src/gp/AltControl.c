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
# include <string.h>

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

MAKE_RCSID("$Id: AltControl.c,v 2.28 1999-11-01 20:18:43 burghart Exp $")

/*
 * Prototypes
 */
static void	alt_GetControlComp FP ((void));
static void	alt_SetAlt FP ((int));
static void	alt_SetLabel FP ((char *label));




void
alt_Initialize ()
/*
 * Set a good initial "altitude" parameter in our PD
 */
{
/*
 * Find the control component
 */
	alt_GetControlComp ();
	msg_ELog (EF_DEBUG, "chose #%d for altitude control", AltControlComp);
	alt_SetAlt (0);
}


static void
alt_GetControlComp ()
/*
 * Find the altitude control component of our PD.
 */
{
	char altcomp[80], **comps = pd_CompList (Pd);
	char plat[PlatformListLen];
	zbool control, disabled;
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
			disabled = FALSE;
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

		disabled = FALSE;
		if (pda_Search (Pd, comps[i], "disable", NULL, 
				(char *) &disabled, SYMT_BOOL) && disabled)
			continue;
		
		if (pda_Search (Pd, comps[i], "altitude-control", NULL,
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
int nstep;
/*
 * Step the altitude by this many steps and schedule a replot.
 */
{
	/*
	 * Searching for altitudes and radar angles can take long
	 * enough to confuse users using arrow buttons to step
	 * through altitudes.
	 */
	ChangeCursor (Graphics, BusyCursor);
	alt_SetAlt (nstep);
        Eq_AddEvent (PDisplay, pc_ParamChange, "altitude", 9, Augment);
	ChangeCursor (Graphics, None);
}




static int
alt_NextStep (alts, nalt, nstep, alt_in)
float *alts;
int nalt;
int nstep;
float *alt_in;
/*
 * 'alt_in' can point to an altitude value to use as the target, else
 * the current altitude parameter is used.
 */
{
	int i;
	int closest;
	int have_target = 0;
	float target_alt;
/*
 * Existing "altitude" becomes a target starting place.  Otherwise, we'll
 * start from the zeroth altitude.
 */
	if (alt_in)
	{
		have_target = 1;
		target_alt = *alt_in;
	}
	else
	{
		have_target = pd_Retrieve (Pd, "global", "altitude", 
					   (char *) &target_alt, SYMT_FLOAT);
	}
/*
 * Now go through and find the closest one to our target.
 */
	if (have_target && nalt > 0)
	{
		float dist = ABS (alts[0] - target_alt);
		closest = 0;
		for (i = 1; i < nalt; i++)
		{
			float delta = ABS (alts[i] - target_alt);
			if (delta < dist)
			{
				dist = delta;
				closest = i;
			}
		}
		msg_ELog (EF_DEBUG, "found alt %.1f at %i => %.1f",
			  target_alt, closest, alts[closest]);
	}
	else
	{
		msg_ELog (EF_DEBUG, "no target or nalt <= 0");
		closest = 0;
	}
/*
 * Step by the given amount, and apply bounds.  nstep==0 when 
 * initializing means we use the closest altitude found above.
 */
	if ((closest += nstep) < 0)
		closest = 0;
	else if (closest >= nalt)
		closest = nalt - 1;
	return (closest);
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
	int nalt, i, next;
	zbool rspace;
	float alts[MAXALT];
	char platform[PlatformListLen];
	char *comma;
	char field[80], **comps = pd_CompList (Pd), scratch[40];
	FieldId fid;
	PlatformId pid;
	AltUnitType altunits;
	ZebraTime datatime;
/*
 * Get the platform and field from the control component.  If either is 
 * missing, we just use 0 km MSL.
 */
	if (! pd_Retrieve (Pd, comps[AltControlComp], "platform", platform,
			   SYMT_STRING) ||
	    (! pd_Retrieve (Pd, comps[AltControlComp], "field", field, 
			   SYMT_STRING) &&
	     ! pd_Retrieve (Pd, comps[AltControlComp], "u-field", field,
			    SYMT_STRING) &&
	     ! pd_Retrieve (Pd, comps[AltControlComp], "wspd-field", field,
			    SYMT_STRING)))
	{
		alt_SetLabel ("");
		return;
	}
/*
 * Use only the first platform name if this is a platform list
 */
	if ((comma = strchr(platform, ',')) != NULL)
		*comma = '\0';
/*
 * Need a valid platform.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		/* There are too many reasons this can fail
		 * in normal situations to flag it as a problem
		 */
		alt_SetLabel ("");
		msg_ELog (EF_DEBUG, "alt_Step: bad platform '%s'",
			  platform);
		return;
	}
/*
 * Radar space?
 */
	rspace = r_RadarSpace (comps[AltControlComp]);
/*
 * Now, depending on our model of the world, we choose one way or the other
 * to look for the available altitudes.
 */
	if (! rspace)
	{
	/*
	 * Get field IDs.
	 */
		if ((fid = F_Lookup (field)) == BadField)
		{
			alt_SetLabel ("");
			msg_ELog (EF_DEBUG, "alt_Step: bad field '%s'", field);
			return;
		}
	/*
	 * Adjust to the correct (model issue) time if we have a model 
	 * platform and we're plotting in validation mode.
	 */
		datatime = PlotTime;
		if (ds_IsModelPlatform (pid) && ValidationMode)
		    datatime.zt_Sec -= ForecastOffset;
	/*
	 * Check it out.
	 */
		if (! ds_GetAlts (pid, fid, &datatime, ForecastOffset, 
				  alts, &nalt, &altunits) || nalt == 0)
		{
			alt_SetLabel ("");
			msg_ELog (EF_DEBUG, 
				  "No available alts for %s/%s @ %d h offset",
				  platform, field, ForecastOffset / 3600);
			return;
		}
	/*
	 * semi-KLUGE: Force the array to be increasing if altitude or 
	 * decreasing if pressure or sigma
	 */
		if (((altunits == AU_mb || altunits == AU_sigma)
		     && alts[0] < alts[1]) ||
		    (altunits != AU_mb && alts[0] > alts[1]))
		{
			for (i = 0; i < nalt / 2; i++)
			{
				float temp = alts[i];
				alts[i] = alts[nalt - i - 1];
				alts[nalt - i - 1] = temp;
			}
		}
	}
	else
	{
		nalt = r_GetAlts (pid, comps[AltControlComp], nstep, alts);

		if (nalt <= 0)
		{
			pd_RemoveParam (Pd, "global", "altitude");
			alt_SetLabel ("?");
			msg_ELog (EF_DEBUG, "No available radar angles for %s",
				  platform);
			return;
		}
	}
/*
 * If we have some altitudes to step through, do so.  Otherwise its
 * pointless.
 */
	next = 0;
	if (nalt > 1)
		next = alt_NextStep (alts, nalt, nstep, NULL);
/*
 * Stash the new altitude
 */
	msg_ELog (EF_DEBUG, "new altitude %.1f", alts[next]);
	sprintf (scratch, "%.4f", alts[next]);
	pd_Store (Pd, "global", "altitude", scratch, SYMT_STRING);
#ifdef notdef
	if (rspace)
		r_NewAlt (comps[AltControlComp], alts[next]);
#endif
/*
 * and the new altitude label
 */
	if (! rspace)
	{
		sprintf (scratch, "Alt: %s",
			 au_AltLabel (alts[next], altunits));
	}
	else
	{
		sprintf (scratch, "%.1f deg", alts[next]);
	}
	alt_SetLabel (scratch);
}




static void
alt_SetLabel (label)
char *label;
{
	pd_Store (Pd, "global", "altitude-label", label, SYMT_STRING);
/*
 * Schedule to send the modified PD to dm and the PD monitor
 */
	Eq_AddEvent (PWhenever, eq_ReturnPD, 0, 0, Override);
	pdm_ScheduleUpdate ();
}



