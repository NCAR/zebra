/*
 * Some code specifically designed for radar platforms, collected here in
 * an attempt to consolidate the variety of parameters and kludges and
 * exceptions instantiated throughout the graphics process for the sake
 * of that most noble of meteorological platforms...
 */

#include <unistd.h>
#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <defs.h>
#include <message.h>

#include <GraphicsW.h>
#include "GraphProc.h"
#include "DrawText.h"
#include "PixelCoord.h"
#include "GC.h"

RCSID ("$Id: Radar.c,v 2.11 2001-01-16 22:27:36 granger Exp $")


static char *ScanNames[] = 
{
	"Any", "sur", "rhi", "ppi", "ver", "idl", NULL
};



char *
r_ScanModeName (R_ScanMode i)
{
	return (ScanNames[i]);
}


char *
r_ScanModeAtt (R_ScanMode i)
{
	return (i == R_ANY ? NULL : ScanNames[i]);
}


/*
 * The limit of our altitude and image searches.  Too big and we take
 * too much time loading when usually the first few will satisfy.
 * Must be smaller than MAXALT in AltControl.c
 */
#define MAXALT 60

/*
 * Be if hereforth attempted here the description of the parameters
 * used for radar platforms.
 */
/* ----------------------------------------------------------------

Name		Source		Searched?	Qualifier	Effect
----		------		---------	---------	------
radar-space	comp,global 	no				

every-sweep	comp,global	yes		none
radar-mode	comp,global	yes		none

fixed-angle	comp,global	
filter-attribute


Without radar-space true, the rest of the radar parameters should
be ignored.

radar-mode selects the type of scan to look for: sur, ppi, rhi, or any.
The default is any.

every-sweep is the opposite of "hold angle mode" (see below).  If true,
ignore scan type and angle when possible, else enforce the current angle
and scan-type for new times.  every-sweep defaults to true ("hold angle"
false).

filter-attribute, though generically named, is only used for radar space
to select particular angles or scans.  It is hereby deprecated in favor
of newer parameters and the access routines in this module, which will
set attributes depending upon the mode parameters

For altitude control, where multiple altitudes all have separate sample
times, initializing the altitude (alt step of 0) means one of two
things:

	if every-sweep mode (aka ignore angles), take the most
	recent sample for the given scan mode.

	if not every-sweep mode, take the most recent image for the type of
	the current scan and its angle.  Note that this means "hold angle"
	implies "hold scan type".

If actually stepping radar angles (alt step non-zero), use the current scan
type to assemble the list of possible angles.  I.e., if rhi, assemble
azimuths.  If sur or ppi, assemble elevations.  Ignore hold angle for
selection, in fact turn hold angle on, and change altitude to new angle.

Fixing the angle useful when?  making movies, setting a new time, ..., in
other words, fixed angle applies only when changing time, forcing the
angle and scan type to stay the same.

What about when toggling between scan types w/ hold angle true?  The hold
angle should change to the most recently held angle for the new scan type.
Keep the angle in a <scan>-angle parameter, and whenever setting the
altitude, either retrieve or set that value directly and use it for the
altitude.



The legend shows these things:

	radar platform
	scan type of current image
	current display scan mode		-- menu of possible scans
	hold angle: off or on			-- menu to turn on or off

-- current angle can be a menu of possible angles to change to (for current
   scan type)

-- scan type doesnt need a menu, maybe the usual radar icon menus


When setting or stepping the altitude, we are actually setting two things:
the target altitude based on the platform, and a target scan type.  Use the
private 'radar-scan' parameter to store the target scan type whenever we
set the target altitude.

* ---------------------------------------------------------------- */


/*
 * Forwards.
 */
static R_ScanMode r_ParseScanType (char *text);

static int r_FindAlts (PlatformId pid, char *comp, int nstep, float *alts,
		       R_ScanMode *scan);
static void r_Insert (float *alts, int *nalt, R_ScanMode scan, float new);
static int r_AlreadyThere (float *alts, int nalt, float new);
static int r_GetLatest (PlatformId pid, char *comp, float *alts, 
			R_ScanMode *scan);



int
r_RadarSpace (comp)
char *comp;
/*
 * Return non-zero if this component exists in "radar space" ooo-eeee-ooooo
 */
{
	zbool rspace = FALSE;

	if (! pd_Retrieve (Pd, comp, "radar-space", 
			   (char *) &rspace, SYMT_BOOL))
		pd_Retrieve (Pd, "global", "radar-space", (char *) &rspace,
			     SYMT_BOOL);
	return (rspace);
}




int
r_HoldAngle (comp)
char *comp;
/* 
 * Return non-zero if this component is set for fixed-angle or "held angle"
 * mode.  Essentially its "any angle" unless every-sweep is set and false.
 * Only valid if radar-space is true.
 */
{
	zbool every = TRUE;

	pda_Search (Pd, comp, "every-sweep", NULL,
		    (char *) &every, SYMT_BOOL);
	return (! every);
}



static int
r_HoldVolume (comp)
char *comp;
/*
 * Return non-zero if the list of possible angles should be limited to a
 * single volume, which at the moment is just another word for observation,
 * which at the moment is another word for file.  The default is to limit
 * to the current volume, and that changes only if the parameter
 * "every-volume" is present and true.
 */
{
	zbool every = FALSE;

	pda_Search (Pd, comp, "every-volume", NULL,
		    (char *) &every, SYMT_BOOL);
	return (! every);
}




static void
r_SetHoldAngle (comp, flag)
char *comp;
int flag;
{
	pd_Store (Pd, comp, "every-sweep", (flag ? "false" : "true"), 
		  SYMT_STRING);
}




static void
r_SetScanAngle (comp, scan, angle)
char *comp;
R_ScanMode scan;
float angle;
{
	char parm[64];

	sprintf (parm, "%s-angle", r_ScanModeName(scan));
	pd_Store (Pd, comp, parm, (char *)&angle, SYMT_FLOAT);
}



static void
r_GetScanAngle (comp, scan, angle)
char *comp;
R_ScanMode scan;
float *angle;
{
#ifdef notdef
	char parm[64];

	sprintf (parm, "%s-angle", r_ScanModeName(scan));
	if (! pd_Retrieve (Pd, comp, parm, (char *)angle, SYMT_FLOAT))
#endif
	{
		*angle = 0.0;
		pd_Retrieve (Pd, "global", "altitude", 
			     (char *)angle, SYMT_FLOAT);
	}
}




static R_ScanMode
r_ParseScanType (text)
char *text;
{
	if (! strcmp (text, "any") ||
	    ! strcmp (text, "Any") ||
	    ! strcmp (text, "ANY"))
		return (R_ANY);
	if (! strcmp (text, "surveillance") ||
	    ! strcmp (text, "survey") ||
	    ! strcmp (text, "sur"))
		return (R_SUR);
	if (! strcmp (text, "rhi") ||
	    ! strcmp (text, "RHI"))
		return (R_RHI);
	if (! strcmp (text, "ppi") ||
	    ! strcmp (text, "PPI"))
		return (R_PPI);
	if (! strcmp (text, "ver") ||
	    ! strcmp (text, "VER"))
		return (R_VER);
	if (! strcmp (text, "idl") ||
	    ! strcmp (text, "IDL"))
		return (R_IDL);
	msg_ELog (EF_PROBLEM, "unknown radar scan mode '%s'", text);
	return (R_ANY);
}




R_ScanMode
r_ScanMode (comp)
char *comp;
/*
 * Return the scan mode for this component.
 */
{
	char text[256];

	if (! pda_Search (Pd, comp, "radar-mode", NULL, text, SYMT_STRING))
		return (R_ANY);
	return (r_ParseScanType (text));
}
	



R_ScanMode
r_TargetScanType (comp)
char *comp;
{
	char text[256];

	if (! pda_Search (Pd, comp, "radar-scan", NULL, text, 
			  SYMT_STRING))
		return (R_ANY);
	return (r_ParseScanType (text));
}
	



static void
r_SetRadarScan (comp, scan)
char *comp;
R_ScanMode scan;
{
	pd_Store (Pd, comp, "radar-scan", 
		  r_ScanModeName(scan), SYMT_STRING);
}




int
r_GetAngle (pid, t, angle, scan)
PlatformId pid;
ZebTime *t;
float *angle;
R_ScanMode *scan;
/*
 * 'angle' and 'scan' can be NULL if not needed.
 */
{
	DataChunk *dc = ds_Fetch (pid, DCC_Location, t, t, 0, 0, 0, 0);
	char *text;
	Location loc;

	if (! dc)
		return (0);
	dc_LocGet (dc, 0, t, &loc);
	if (angle)
		*angle = loc.l_alt;
	if (scan)
	{
		*scan = R_ANY;
		if ((text = dc_GetSampleAttr (dc, 0, "scan_type")))
			*scan = r_ParseScanType (text);
		if (*scan == R_ANY)
			msg_ELog (EF_PROBLEM, "failed to get scan type");
	}
	dc_DestroyDC (dc);
	return (1);
}




int
r_ImageTime (c, pid, angle, when)
char *c;
PlatformId pid;
double angle;
ZebTime *when;	/* in and out */
/*
 * Given a target altitude and target scan type,
 * find a matching scan and return its time in *when.
 * The target scan type comes from the component.
 */
{
	/* The times array is an arbitrary guess of how far back to look */
	ZebraTime times[MAXALT];
	float fixed[MAXALT];
	R_ScanMode attr[MAXALT];
	int i, ntime;
	float tolerance, dist;	/* degrees, depends on scan type */
	int ret = 0;
	R_ScanMode scan;
	int closest, pick;
/*
 * Even if "hold angle" mode is not in effect, the angle (altitude)
 * has already been chosen elsewhere and we must adhere to it here
 * in finding an appropriate image.  Get a list of appropriate scans,
 * then check it for the first scan reasonably close to the angle.
 */
	scan = r_TargetScanType (c);
	ntime = ds_AttrTimes (pid, when, MAXALT, DsBefore, NULL, 
			      r_ScanModeAtt(scan), times);
	msg_ELog (EF_DEBUG, "r_time: need target scan %s @ %.1f, "
		  "found %d scans", r_ScanModeName(scan), angle, ntime);
	tolerance = 0.1;
	if (scan == R_RHI)
		tolerance = 1.0;
	closest = -1;		/* index of scan closest to angle */
	pick = -1;		/* index of latest scan w/in tolerance */
	dist = 400.0;
	for (i = 0; i < ntime; ++i)	/* most recent to oldest */
	{
		if (r_GetAngle (pid, times+i, fixed+i, attr+i))
		{
			float delta = ABS (angle - fixed[i]);
			if (delta > 360.0)
				delta -= 360.0;
			if ((pick < 0) && (delta < tolerance))
				pick = i;
			/* Only take older scans if "significantly" closer */
			if (delta + tolerance < dist)
			{
				dist = delta;
				closest = i;
			}
		}
	}

	if (pick < 0)
		pick = closest;
	if (pick >= 0)
	{
		ret = 1;
		*when = times[pick];
		scan = attr[pick];
		msg_ELog (EF_DEBUG, "r_time: using %s %s, %.1f @ %s", 
			  (pick == closest) ? "closest" : "latest",
			  r_ScanModeName(scan), fixed[pick], 
			  TC_AscTime (when, TC_Full));
	}
	else
	{
		ret = 0;
		msg_ELog (EF_DEBUG, "r_time: no scan found");
	}
/*
 * Just for thoroughness, store the scan type of any image we found,
 * as well as the angle for the found scan.
 */
	if (ret && scan != R_ANY)
	{
		r_SetRadarScan (c, scan);
		r_SetScanAngle (c, scan, fixed[pick]);
	}
	return (ret);
}





int
r_GetAlts (pid, comp, nstep, alts)
PlatformId pid;
char *comp;
int nstep;
float *alts;
/*
 * Determine an array of altitudes for this radar-space component and
 * platform.  If nstep==0, this is assumed to be an initialization of the
 * plot altitude, in which case the behavior is a little different than
 * for a simple altitude step.  For an initialization, we need to force
 * the angle (altitude) to change if not in "hold angle" mode.
 */
{
	int nalt, i;
	int hold = r_HoldAngle (comp);
	R_ScanMode scan = R_ANY;
	char buf[256];

	if (! hold && ! nstep)
	{
		nalt = r_GetLatest (pid, comp, alts, &scan);
	}
	else
	{
	/*
	 * Otherwise we have to go get a list according the current
	 * mode and the current type of scan.
	 */
		nalt = r_FindAlts (pid, comp, nstep, alts, &scan);
	}
	sprintf (buf, "r_alts: hold %d, nstep %d -> nalt %d, scan %s,",
		 hold, nstep, nalt, r_ScanModeName(scan));
	for (i = 0; i < nalt && i < 5; ++i)
	{
		sprintf (buf+strlen(buf), " %.1f", alts[i]);
	}
	msg_ELog (EF_DEBUG, "%s ...", buf);
/*
 * Store the target scan type in the component for future reference.
 * If we're stepping to another altitude, and we *have* more than one
 * altitude, we need to enter "hold angle" mode else we could revert 
 * to the most recent.
 */
	if (nstep != 0 && nalt > 1)
		r_SetHoldAngle (comp, TRUE);
	r_SetRadarScan (comp, scan);
	return (nalt);
}




void
r_NewAlt (char *comp, float alt)
/*
 * AltControl wants to tell us about the new altitude, so we need to
 * update the angle for the particular scan type.
 */
{
	R_ScanMode scan = r_TargetScanType (comp);
	r_SetScanAngle (comp, scan, alt);
}




static int
r_AlreadyThere (float *alts, int nalt, float new)
/*
 * See if this altitude is already present in the list.
 */
{
	int i;

	for (i = 0; i < nalt; i++)
		if (alts[i] == new)
			return (TRUE);
	return (FALSE);
}





static void
r_Insert (float *alts, int *nalt, R_ScanMode scan, float new)
/*
 * Add this altitude to the list, in a sorted manner.  Ugly, linear
 * check, but this list is always short.
 */
{
	int alt, i;

	if (scan == R_RHI)
	{
		if (new > 360.0)
			new -= 360.0;
	}
	/*
	 * This is historical.  I don't know how it might happen,
	 * but we won't take it if it does.
	 */
	else if ((scan == R_SUR || scan == R_PPI) && new > 90.0)
	{
		return;
	}
	for (alt = 0; alt < *nalt; alt++)
	{
		if (alts[alt] > new)
		{
			for (i = *nalt; i > alt; i--)
				alts[i] = alts [i - 1];
			alts[alt] = new;
			(*nalt)++;
			return;
		}
	}
	alts[*nalt] = new;
	(*nalt)++;
}





static int
r_FindAlts (pid, comp, nstep, alts, scan)
PlatformId pid;
char *comp;
int nstep;
float *alts;
R_ScanMode *scan;
/*
 * Find a list of available radar space altitudes, possibly qualified
 * by a scan mode or held angle.  If this is an initialization (nstep==0),
 * we'll take any angle for the current scan mode unless "hold angle" is
 * in effect.  If a regular altitude step, force the step to stick to
 * the type of the current scan and ignore "hold angle".
 */
{
	int hold;
	R_ScanMode attr;
	ZebTime times[MAXALT];
	ZebTime obs;
	int i, nalt, nret;
/*
 * Check for the scan mode in effect, and see if we need to force the scan
 * to the type of the current scan.
 */
	attr = r_ScanMode (comp);
	if (attr == R_ANY && nstep != 0)
	{
		/* Use the type of the current scan, if any */
		attr = r_TargetScanType (comp);
		msg_ELog (EF_DEBUG, "stepping angle: forcing scan type %s",
			  r_ScanModeName (attr));
	}
	*scan = attr;
/*
 * Determine our angle options.  Ignore angle hold for an actual step.
 */
	hold = (nstep != 0) ? 0 : r_HoldAngle (comp);
	if (hold)
	{
		r_GetScanAngle (comp, attr, alts);
		return (1);
	}
/*
 * Finally find all of the available scans and insert their angles into
 * the alts array.  If we don't know the current scan and we're not
 * holding to an angle, take the most recent scan as the type of scan
 * to build altitudes with.
 */
	if (attr == R_ANY)
	{
		float angle;
		nalt = ds_AttrTimes (pid, &PlotTime, MAXALT, DsBefore, NULL, 
				     r_ScanModeAtt(attr), times);
		if (nalt > 0)
			r_GetAngle (pid, times, &angle, &attr);
		msg_ELog (EF_DEBUG, "using latest scan to force scan type %s",
			  r_ScanModeName(attr));
		*scan = attr;
	}
	if (attr != R_ANY)	/* no sense doing it twice if no change */
	{
		nalt = ds_AttrTimes (pid, &PlotTime, MAXALT, DsBefore, NULL, 
				     r_ScanModeAtt(attr), times);
	}
	nret = 0;
	/*
	 * The most recent scan in the list determines the observation
	 * (volume) to which we'll limit the list of angles.
	 */
	if (! r_HoldVolume (comp) ||
	    ! ds_GetObsTimes (pid, times, &obs, 1, NULL))
		obs = ZT_EPOCH;
	for (i = 0; i < nalt; i++)
	{
		float angle;

		if (TC_LessEq (obs, times[i]) &&
		    r_GetAngle (pid, times+i, &angle, &attr) &&
		    (! r_AlreadyThere (alts, nret, angle)))
		{
			r_Insert (alts, &nret, attr, angle);
		}
	}
	return (nret);
}




static int
r_GetLatest (pid, comp, alts, scan)
PlatformId pid;
char *comp;
float *alts;
R_ScanMode *scan;
/*
 * Get the "latest" sweep before the plot time, in order to set the
 * "altitude" in every-sweep mode.
 *
 * This is an unpleasant kludge grafted on top of a horrible one.  We
 * here have to essentially reproduce the selection process that we
 * expect ImageDataTime will go through, for the sole purpose of properly
 * updating the label in the display.  Someday this stuff will get tossed
 * into the recycle bin and the new version will:
 *
 *	- abolish the altitude/elevation kludge
 *	- set the altitude label from the actual data after it's fetched
 *	- have lots less whining comments
 */
{
	R_ScanMode attr;
	int ntime;
	ZebraTime times[MAXALT];
/*
 * Force a certain scan type according to our mode.
 */
	attr = r_ScanMode (comp);
/*
 * Find the most recent, possibly qualified, scan.  We need to look
 * through more than one, even though we only need one. 
 *
 * 5/98 jc: Sure, I'm slow, but I don't see exactly *why* we need to
 *	    look at more than one, and it sure does slow things down.
 *	    Let's try without.
 */
# ifdef WeNeededLotsAfterAll
	ntime = ds_AttrTimes (pid, &PlotTime, MAXALT, DsBefore, NULL, 
			      r_ScanModeAtt(attr), times);
# endif
	ntime = ds_AttrTimes (pid, &PlotTime, 1, DsBefore, NULL, 
			      r_ScanModeAtt(attr), times);
	if (ntime < 1)
		ntime = ds_AttrTimes (pid, &PlotTime, MAXALT, DsBefore, NULL, 
			      r_ScanModeAtt(attr), times);
	if (ntime < 1)
		msg_ELog (EF_DEBUG, "r_latest: no '%s' scan found", 
			  r_ScanModeName (attr));
/*
 * Finally find out the altitude for this radar scan time.
 */
	if (ntime <= 0 || ! r_GetAngle (pid, times, alts, &attr))
		ntime = 0;
	else
		ntime = 1;
	*scan = attr;
	return (ntime);
}


/* ================
 * Radar annotation
 */

#define LINESPACE 1
#define LWIDTH 2
#define GAP 3

static void
AnnotLine (char *string, float scale, int left, int *begin, 
	   int *space, int *right)
{
	int used;
	int sx, sy, ex, ey;

        DT_TextBox (Graphics, GWFrame (Graphics), left, *begin,
		    string, 0.0, scale, JustifyLeft, JustifyTop,
		    &sx, &sy, &ex, &ey);
	if (*space >= ABS(ey - sy))
	{
	    DrawText (Graphics, GWFrame (Graphics), Gcontext, left, *begin,
		      string, 0.0, scale, JustifyLeft, JustifyTop);
	    if (ex > *right)
		*right = ex;
	    used = ABS(ey - sy) + LINESPACE;
	    if (*space < used)
		used = *space;
	    *begin += used;
	    *space -= used;
	}
}



static void
r_Legend (char *comp, char *data, int datalen, int begin, int space)
/*
 * Put the radar mode and other status info in plain view in the legend.
 */
{
        int limit, match;
        XColor xc, bold;
        float scale;
	int right, top, left;

	char buf[256];
	char platform[PlatformListLen];
	int hold = r_HoldAngle (comp);
	R_ScanMode scan = r_ScanMode (comp);
	R_ScanMode current = r_TargetScanType (comp);
/*
 * Get top and side annotation plot description parameters.
 */
        An_GetSideParams (comp, &scale, &limit);
        An_GetTopParams (&xc, &match);
	ct_GetColorByName ("yellow", &bold);
/*
 * Get the data.
 */
        sscanf (data, "%s", platform);
/*
 * Put in the current platform and scan, followed by angle mode and scan mode.
 */
        left = An_GetLeft ();
	/* leave room for the surrounding rectangle */
	left += LWIDTH + GAP;
	begin += LWIDTH + GAP;
	space -= LWIDTH + GAP;
	top = begin;
	right = 0;

        XSetForeground (XtDisplay (Graphics), Gcontext, xc.pixel);
	sprintf (buf, "Radar: %s", platform);
	AnnotLine (buf, scale, left, &begin, &space, &right);

	sprintf (buf, "Current: %s", r_ScanModeName(current));
	AnnotLine (buf, scale, left, &begin, &space, &right);

	if (hold)
		XSetForeground (XtDisplay (Graphics), Gcontext, bold.pixel);
	sprintf (buf, "%s", hold ? "Angle: HOLD" : "Angle: Any");
	AnnotLine (buf, scale, left, &begin, &space, &right);
        XSetForeground (XtDisplay (Graphics), Gcontext, xc.pixel);

	if (scan != R_ANY)
		XSetForeground (XtDisplay (Graphics), Gcontext, bold.pixel);
	sprintf (buf, "Scan: %s", r_ScanModeName(scan));
	AnnotLine (buf, scale, left, &begin, &space, &right);
        XSetForeground (XtDisplay (Graphics), Gcontext, xc.pixel);
/*
 * Surround with a box.
 */
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, LWIDTH, 
			    LineSolid, CapButt, JoinMiter);
	XDrawRectangle (XtDisplay (Graphics), GWFrame (Graphics), Gcontext,
			left-GAP, top-GAP, (right - left + 2*GAP), 
			(begin - top + 2*GAP));
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, LineSolid,
			    CapButt, JoinMiter);
}


	 

void
r_AddAnnot (char *comp, char *platform)
{
	int limit, line;
	float scale;
	int minspace;
	char data[256];

        An_GetSideParams (comp, &scale, &limit);
        line = DT_ApproxHeight (Graphics, scale, 1) + LINESPACE;
	minspace = 4 * line + 2 * GAP + 2 * LWIDTH;
	sprintf (data, platform);
	An_AddAnnotProc (r_Legend, comp, data, strlen (data), 
			 minspace, FALSE, FALSE);
}




