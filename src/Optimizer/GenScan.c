/*
 * Generate scan parameters for a radar that will cover the current
 * volume
 *
 * $Id: GenScan.c,v 1.1 1991-06-16 17:02:25 burghart Exp $
 */
# include "globals.h"
# include "radar.h"
# include "prototypes.h"

/*
 * Report errors or bail out silently?
 */
static bool	ReportError;

/*
 * Private prototypes
 */
# ifdef __STDC__
	void	gs_AngleList (Radar *, double, double, double, double);
	void	gs_SepTest (double, double, char *);
	void	gs_FixedSteps (Radar *, double, double, double);
	void	gs_Error (Radar *);
# else
	void	gs_AngleList ();
	void	gs_SepTest ();
	void	gs_FixedSteps ();
	void	gs_Error ();
# endif




void
GenScan (rad, time, hres, vres, report_error)
Radar	*rad;
float	time, hres, vres;
bool	report_error;
/*
 * Generate a scan for 'rad' which will cover the volume in 'time' seconds
 * with minimum horizontal and vertical resolutions of 'hres' and 'vres'
 * kilometers.  If 'time' is TIME_ASAP, generate the fastest scan that meets
 * the spatial resolution requirements.  
 *
 * All appropriate scan values (angle list, number of sweeps, etc.) will be
 * set in 'rad'.
 *
 * Bail out if an appropriate scan cannot be generated.  Only complain about
 * it if 'report_error' is TRUE.
 */
{
	float	width, angres, accel;
	float	res_srate, max_srate, opt_srate, srate;
	float	swptime, d, val, top, bottom;
	int	hits, min_hits, max_hits;
	s_type	scantype;
/*
 * Make 'report_error' global
 */
	ReportError = report_error;
/*
 * Find our real top and bottom elevation angles (min_range and min_elev
 * may not let us use the true top and bottom of the volume being used)
 */
	top = (rad->rng_front < rad->min_range) ? 
		RAD_TO_DEG (atan2 (Vol_top, rad->min_range)) : rad->el_top;

	bottom = MAX (rad->el_bottom, rad->min_elev);
/*
 * Get the "optimal" angle list to match the spatial resolution
 */
	gs_AngleList (rad, hres, vres, bottom, top);
/*
 * Change the scan type to SUR if the radar is enclosed by the volume
 */
	scantype = rad->scantype;

	if (rad->inside)
		scantype = SUR;
/*
 * Get the volume width and the necessary angular resolution in the direction
 * of scanning
 */
	switch (scantype)
	{
	    case RHI:
		width = top - bottom;
		angres = ATAND (vres / rad->rng_back);
		max_srate = rad->max_v_scanrate;
		accel = rad->v_accel;
		gs_SepTest (angres, Vsep_min, rad->name);
		break;
	    case PPI:
		width = rad->az_right - rad->az_left;
		if (width < 0.0)
			width += 360.0;
		angres = ATAND (hres / rad->rng_back);
		max_srate = rad->max_h_scanrate;
		accel = rad->h_accel;
		gs_SepTest (angres, Hsep_min, rad->name);
		break;
	    case SUR:
		width = 360.0;
		angres = ATAND (hres / rad->rng_back);
		max_srate = rad->max_h_scanrate;
		accel = rad->h_accel;
		gs_SepTest (angres, Vsep_min, rad->name);
		break;
	    default:
		ui_error ("BUG: unknown scan type %d in GenScan", 
			rad->scantype);
	}
/*
 * Find the range of hits we're allowed
 */
	if (rad->fix_hits)
	{
		min_hits = rad->hits;
		max_hits = rad->hits;
	}
	else
	{
		min_hits = rad->min_hits;
		max_hits = rad->max_hits;
	}
/*
 * Find:
 *	o the highest scan rate possible using the given angular resolution 
 *	   and minimum number of hits
 *	o the scan rate that optimizes sweep time based on the acceleration 
 *	   of the radar
 */
	res_srate = angres * rad->prf / min_hits;
	opt_srate = (scantype == SUR) ? max_srate : sqrt (width * accel / 2);
/*
 * Find the best scan rate for this radar
 */
	if (time == TIME_ASAP)
	{
	/*
	 * Use the minimum of the resolution scan rate and optimal scan rate
	 */
		srate = MIN (res_srate, opt_srate);
	}
	else	
	{
	/*
	 * Find the scan rate that will match the chosen time
	 * (The 0.0001 second gets around some round-off problems)
	 */
		swptime = time / rad->nsweeps + 0.0001;
		val = swptime * swptime - 8 * width / accel;
		if (val < 0.0)
			srate = 99999.9;
		else
			srate = 0.25 * accel * (swptime - sqrt (val));
	}
/*
 * Deal with it if the radar is incapable of this scan rate
 */
	if (srate > max_srate || srate > res_srate)
	{
		if (time == TIME_ASAP || rad->status == MatchSpatial)
		/*
		 * We aren't required to match the time, so just scan as
		 * fast as possible while maintaining spatial resolution
		 */
			srate = MIN (res_srate, max_srate);
		else if (rad->status == MatchTime)
		{
		/*
		 * We only need to match the time, so degrade the required
		 * spatial resolution and try again
		 */
			GenScan (rad, time, hres + RES_STEP, vres + RES_STEP,
				report_error);
			return;
		}
		else
		/*
		 * We're supposed to match everything and we can't, so complain
		 */
			gs_Error (rad);
	}

	rad->scanrate = srate;
/*
 * Calculate the volume scan time
 */
	swptime = width / srate;
	if (scantype != SUR)
		swptime += 2 * srate / accel;

	rad->scantime = rad->nsweeps * 
		(width / srate + 2 * srate / accel);
/*
 * Set the hits as high as we can while maintaining the required resolution
 */
	rad->hits = min_hits;

	for (hits = 2 * min_hits; hits <= max_hits; hits *= 2)
	{
		if ((srate * hits / rad->prf) <= angres)
			rad->hits = hits;
		else
			break;
	}
/*
 * Calculate our resulting resolutions
 */
	d = rad->rng_back;

	switch (rad->scantype)
	{
	    case RHI:
		rad->res_horiz = TAND (rad->anglist[1] - rad->anglist[0]) * d;
		rad->res_vert = vres;
		break;
	    default:
		rad->res_horiz = hres;
		rad->res_vert = TAND (rad->anglist[1] - rad->anglist[0]) * d;
	}
/*
 * Done
 */
	return;
}




void
gs_AngleList (radar, hres, vres, bottom, top)
Radar	*radar;
float	hres, vres, bottom, top;
/*
 * Create the angle list for the given radar, using the specified horizontal
 * and vertical resolutions in km.  The values bottom and top are the elevation
 * limits to use.
 */
{
	float	step, switchang, ang;
	float	a, b, d, temp, range, delta;
/*
 * Fixed step list for RHIs
 */
	if (radar->scantype == RHI)
	{
	/*
	 * Get the step angle and truncate to tenths of a degree
	 */
		step = ATAND (hres / radar->rng_back);
		step = (int)(step * 10.0) * 0.1;
	/*
	 * Test against minimum beam separation
	 */
		gs_SepTest (step, Hsep_min, radar->name);
	/*
	 * Build the angle list
	 */
		gs_FixedSteps (radar, radar->az_left, radar->az_right, step);
	}
/*
 * Fixed steps for PPI or SUR if specifically requested
 */
	else if (radar->fix_step)
	{
	/*
	 * Get the step angle and truncate to tenths of a degree
	 */
		step = ATAND (vres / radar->rng_back);
		step = (int)(step * 10.0) * 0.1;
	/*
	 * Test against minimum beam separation
	 */
		gs_SepTest (step, Vsep_min, radar->name);
	/*
	 * Build the angle list
	 */
		gs_FixedSteps (radar, bottom, top, step);
	}
/*
 * Otherwise, build a variable step list
 */
	else
	{
	/*
	 * Find the angle at which we switch from scanning through the
	 * back of the volume to scanning through the top of the volume
	 */
		switchang = ATAND (Vol_top / radar->rng_back);
	/*
	 * Get a and b (semi-major and semi-minor axes of our ellipse)
	 */
		a = 0.5 * hres;
		b = 0.5 * vres;
	/*
	 * Initialize
	 */
		ang = radar->anglist[0] = bottom;
		radar->nsweeps = 1;
	/*
	 * Build the angle list
	 */
		while (ang < top)
		{
		/*
		 * Current range
		 */
			range = (ang < switchang) ? 
				radar->rng_back / COSD (ang) : 
				Vol_top / SIND (ang);
		/*
		 * Calculate the next angle step
		 */
			temp = a / b * TAND (ang);
			d = 2 * b * COSD (ang) * sqrt (1 + temp * temp);

			delta = ATAND (d / range);
		/*
		 * Test against minimum beam separation
		 */
			gs_SepTest (delta, Vsep_min, radar->name);
		/*
		 * Sanity check
		 */
			if (radar->nsweeps == MAX_SWEEPS)
				gs_Error (radar);
		/*
		 * Generate the new angle and add it to the list
		 */
			ang += delta;
			radar->anglist[radar->nsweeps++] = ang;
		}
	}
}




void
gs_SepTest (step, minsep, rname)
float	step, minsep;
char	*rname;
/*
 * Test the desired step against the minimum allowed separation.  The radar
 * name is used for a diagnostic message, if there is one.
 */
{
	if (step > minsep)
		return;

	if (ReportError)
		ui_error ("Scan for %s would violate beam separation limits",
			rname);
	else
		ui_bailout ((char *) 0);
}




void
gs_FixedSteps (radar, ang0, ang1, step)
Radar	*radar;
float	ang0, ang1, step;
/*
 * Create a fixed step angle list for 'radar' running from ang0 to ang1
 * using the given step
 */
{
	float	ang;
/*
 * Make sure ang1 is bigger than ang0
 */
	if (ang1 < ang0)
		ang1 += 360.0;
/*
 * Build the list
 */
	radar->nsweeps = 0;

	for (ang = ang0; ang < ang1 + step; ang += step)
	{
	/*
	 * Sanity check
	 */
		if (radar->nsweeps == MAX_SWEEPS)
			gs_Error (radar);
	/*
	 * Add the new angle to the list
	 */
		if (ang < 360.0)
			radar->anglist[radar->nsweeps++] = ang;
		else
			radar->anglist[radar->nsweeps++] = ang - 360.0;
	}

	return;
}




void
gs_Error (rad)
Radar	*rad;
{
	if (! ReportError)
		ui_bailout (NULL);
	else if (rad->nsweeps == MAX_SWEEPS)
		ui_error ("Too many sweeps for %s, stopping at %d", rad->name,
			MAX_SWEEPS);
	else
		ui_error ("Unable to meet all requirements with %s", 
			rad->name);
}
