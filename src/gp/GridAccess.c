/*
 * Routines for pulling grids out of MUDRAS files.
 */
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "../include/DataStore.h"
# include "GraphProc.h"
# include "rg_status.h"

static char *rcsid = "$Id: GridAccess.c,v 1.7 1991-06-25 14:14:07 corbet Exp $";


# define BADVAL	-32768.0

/*
 * Our routines.
 */
# ifdef __STDC__
	static bool ga_Regularize (DataObject *);
	static void ga_RangeLimit (char *, int, float *);
# else
	static bool ga_Regularize ();
	static void ga_RangeLimit ();
# endif






void
ga_RotateGrid (src, dst, x, y)
float *src, *dst;
int x, y;
/*
 * Rotate this grid into column-major order.
 */
{
	int row, col;
	float *sp, *dp;
/*
 * Copy each row into the destination.
 */
 	sp = src;
	for (row = 0; row < y; row++)
	{
		dp = dst + row;
		for (col = 0; col < x; col++)
		{
			*dp = *sp++;
			dp += y;
		}
	}
}









bool
ga_GridBBox (plot_time, platform, x0, y0, x1, y1)
time	*plot_time;
char 	*platform;
float	*x0, *y0, *x1, *y1;
/*
 * Find the bounding box for this grid.
 */
{
	return (FALSE);		/* for now	*/
}






bool
ga_AvailableAlts (plot_time, platform, heights, nh)
time *plot_time;
char *platform;
float *heights;
int *nh;
/*
 * Obtain the list of available heights for this platform at this time.
 */
{
	Location loc;
	RGrid rg;
	PlatformId pid;
	int i;
/*
 * Make sure this is a real platform.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "AvailableAlts on bad plat '%s'",
				platform);
		return (FALSE);
	}
/*
 * Check it out.
 */
	if (! ds_GetRgridParams (pid, plot_time, &loc, &rg))
		return (FALSE);
/*
 * Now calculate the heights.
 */
	*nh = rg.rg_nZ;
	for (i = 0; i < rg.rg_nZ; i++)
		heights[i] = loc.l_alt + i*rg.rg_Zspacing;
	return (TRUE);
}





float *
ga_GetGrid (plot_time, platform, fname, xdim, ydim, x0, y0, x1, y1, alt)
time	*plot_time;
char 	*platform, *fname;
int	*xdim, *ydim;
float	*x0, *y0, *x1, *y1, *alt;
{
	PlatformId	pid;
	DataObject	*dobj;
	RGrid *rg;
	float *ret;
	time realtime;
/*
 * Look up our platform.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", platform);
		return (0);
	}
/*
 * Find out when we can really get data.
 */
	if (! ds_DataTimes (pid, plot_time, 1, DsBefore, &realtime))
	{
		msg_ELog (EF_INFO, "No data available at all for %s",platform);
		return (0);
	}
# ifdef notdef
	msg_ELog (EF_DEBUG, "Plot time %d %d -> %d %d", plot_time->ds_yymmdd,
		plot_time->ds_hhmmss, realtime.ds_yymmdd, realtime.ds_hhmmss);
# endif
/*
 * Do a DS get for this data.
 */
	if ((dobj = ds_GetData (pid, &fname, 1, &realtime, &realtime,
				Org2dGrid, *alt, BADVAL)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s/%s at %d %06d",
			platform, fname, realtime.ds_yymmdd, 
			realtime.ds_hhmmss);
		return (0);
	}
	*alt = dobj->do_loc.l_alt;
/*
 * Now turn this grid into a regular one if necessary.
 */
	if (dobj->do_org == OrgIRGrid)
		if (! ga_Regularize (dobj))
		{
			ds_FreeDataObject (dobj);
			return (0);
		}
/*
 * Pull out the info and return it.  For now we yank out the data and 
 * assume that it can be freed later.
 */
	rg = &dobj->do_desc.d_rgrid;
	*xdim = rg->rg_nX;
	*ydim = rg->rg_nY;
	cvt_ToXY (dobj->do_loc.l_lat, dobj->do_loc.l_lon, x0, y0);
	*x1 = *x0 + (rg->rg_nX - 1)*rg->rg_Xspacing;
	*y1 = *y0 + (rg->rg_nY - 1)*rg->rg_Yspacing;
	ret = dobj->do_data[0];
	dobj->do_flags &= ~DOF_FREEDATA;
	ds_FreeDataObject (dobj);
	*plot_time = realtime;
	return (ret);
}




bool
ga_Regularize (dobj)
DataObject *dobj;
/*
 * Turn an irregular grid into a regular one.
 */
{
	int		RGRID (), i, j, status;
	float		spline_eval (), *grid, *xpos, *ypos, xp, yp;
	float		xmin = 9999.0, ymin = 9999.0, *scratch;
	float		xmax = -9999.0, ymax = -9999.0, badflag, *dp;
	void		spline ();
	RGrid		rg;
	IRGrid		*irg = &dobj->do_desc.d_irgrid;
/*
 * Be really sure this is an irregular one.
 */
	if (dobj->do_org != OrgIRGrid)
	{
		msg_ELog (EF_PROBLEM, "Attempt to regularize non-IRGrid");
		return (FALSE);
	}
/*
 * Wire the dimension of the grid, and get some more memory.
 */
	rg.rg_nX = rg.rg_nY = 20;			/* XXX */
	grid = (float *) malloc (rg.rg_nX * rg.rg_nY * sizeof (float));
/*
 * Do a pass over the locations, and set everything up.
 */
	xpos = (float *) malloc (irg->ir_npoint * sizeof (float));
	ypos = (float *) malloc (irg->ir_npoint * sizeof (float));
	dp = dobj->do_data[0];
	for (i = 0; i < irg->ir_npoint; i++)
	{
	/*
	 * Turn this location into XY space, and see if it stretches our 
	 * limits.
	 */
	 	cvt_ToXY (irg->ir_loc[i].l_lat, irg->ir_loc[i].l_lon,
				xpos + i, ypos + i);
# ifdef notdef
	msg_ELog (EF_DEBUG, "Data %d at %d %d, %.2f", i, (int) xpos[i], 
			(int) ypos[i], dp[i]);
		if ((i % 4) == 0)
			sleep (1);
# endif
		if (xpos[i] < xmin)
			xmin = xpos[i];
		if (xpos[i] > xmax)
			xmax = xpos[i];
		if (ypos[i] < ymin)
			ymin = ypos[i];
		if (ypos[i] > ymax)
			ymax = ypos[i];
	}
/*
 * Store some of the new position info.
 */
	rg.rg_Xspacing = (xmax - xmin)/(rg.rg_nX - 1);
	rg.rg_Yspacing = (ymax - ymin)/(rg.rg_nY - 1);
	cvt_ToLatLon (xmin, ymin, &dobj->do_loc.l_lat, &dobj->do_loc.l_lon);
/*
 * Fill the grid with bad value flags
 */
	for (i = 0; i < rg.rg_nX*rg.rg_nY; i++)
		grid[i] = BADVAL;
/*
 * Apply limits.
 */
	ga_RangeLimit (dobj->do_fields[0], irg->ir_npoint, dobj->do_data[0]);
/*
 * Use RGRID to generate gridded data
 */
	badflag = BADVAL;
	scratch = (float *) malloc (rg.rg_nX * rg.rg_nY * sizeof (float));
	msg_ELog (EF_DEBUG,
		"Call rgrid, %d x %d, np %d, (%.2f %.2f) to (%.2f %.2f)",
		rg.rg_nX, rg.rg_nY, irg->ir_npoint, xmin, ymin, xmax, ymax);
	status = do_rgrid_ (grid, &rg.rg_nX, &rg.rg_nY, &irg->ir_npoint,
		dobj->do_data[0], &badflag, 
		xpos, ypos, &xmin, &ymin, &xmax, &ymax, scratch);
/*
 * Clean up.
 */
	free (scratch);
	free (xpos);
	free (ypos);
/*
 * See what happened here.
 */
	switch (status)
	{
	    case RG_OK:
		break;
	    case RG_NOTENUFPTS:
		msg_ELog (EF_PROBLEM, 
			"Not enough good points to generate a grid");
		break;
	    case RG_COLLINEAR:
		msg_ELog (EF_PROBLEM,
			"Points are collinear, unable to generate a grid");
		break;
	    default:
		msg_ELog (EF_PROBLEM,
			"Unknown status 0x%x returned by RGRID", status);
	}
/*
 * Finish fixing up the data object, and return.
 */
	dobj->do_org = Org2dGrid;
	if (dobj->do_flags & DOF_FREEDATA)
		free (dobj->do_data[0]);
	dobj->do_data[0] = grid;
	dobj->do_flags |= DOF_FREEDATA;
	dobj->do_desc.d_rgrid = rg;
	return (TRUE);
}





static void
ga_RangeLimit (fname, npt, data)
char *fname;
int npt;
float *data;
/*
 * Apply range limits to this data.
 */
{
	float limit;
	int i, nzapped = 0;
/*
 * If there is a minimum limit, apply it to the data.
 */
	if (pda_Search (Pd, "global", "range-min", fname, (char *) &limit,
			SYMT_FLOAT))
		for (i = 0; i < npt; i++)
			if (data[i] != BADVAL && data[i] < limit)
			{
				data[i] = BADVAL;
				nzapped++;
			}
/*
 * Same for the max.
 */
	if (pda_Search (Pd, "global", "range-max", fname, (char *) &limit,
			SYMT_FLOAT))
		for (i = 0; i < npt; i++)
			if (data[i] != BADVAL && data[i] > limit)
			{
				data[i] = BADVAL;
				nzapped++;
			}
	if (nzapped)
		msg_ELog (EF_INFO, "%d pts range limited", nzapped);
}
