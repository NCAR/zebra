/*
 * Basic grid access and transformation routines.
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
# include <math.h>
# include "defs.h"
# include "message.h"
# include "pd.h"
# include <DataStore.h>
# include <DataChunk.h>
# include "GraphProc.h"
# include "rg_status.h"
MAKE_RCSID ("$Id: GridAccess.c,v 2.23 1995-04-07 22:25:14 burghart Exp $")



/*
 * Our routines.
 */
static bool 	ga_Regularize FP ((DataChunk **, char *, char *, char *));
static bool 	ga_RgridRegularize FP ((DataChunk **, char *));
static bool	ga_BarnesRegularize FP ((DataChunk **, char *, char *, char *,
			int));
static void 	ga_RangeLimit FP ((char *, int, float *, double));
static void 	ga_ImgToCGrid FP ((DataChunk **, char *));
static bool 	ga_DoNSpace FP ((DataChunk **, FieldId));
static bool 	ga_NSSimpleGrid FP ((DataChunk **, FieldId));

int		ga_NSCoordVariable FP ((DataChunk *dc, const char *name,
					FieldId *fid));
int		ga_NSRegularSpacing FP((DataChunk *dc, FieldId fid,
					float *rspacing, unsigned long *rnum,
					float *origin));
DataChunk 	*ga_NSRGrid FP((DataChunk *dc, FieldId fid, Location *location,
				double latspacing, double lonspacing,
				int nlats, int nlons, int transpose));



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
ZebTime	*plot_time;
char 	*platform;
float	*x0, *y0, *x1, *y1;
/*
 * Find the bounding box for this grid.
 */
{
	return (FALSE);		/* for now	*/
}





DataChunk *
ga_GetGrid (plot_time, comp, platform, fname, xdim, ydim, x0, y0, x1, y1,
	    alt, shift)
ZebTime	*plot_time;
char 	*comp, *platform, *fname;
int	*xdim, *ydim, *shift;
float	*x0, *y0, *x1, *y1, *alt;
{
	PlatformId	pid;
	DataChunk	*dc;
	RGrid		rg;
	float		*ret;
	ZebTime		dtime;
	DataOrganization platorg;
	DataClass	platclass;
	Location	loc;
	int 		len;
	char		datestring[32];
	FieldId		fid;
	dsDetail	details[5];
	int		ndet = 0;
/*
 * Initialize 'shift' to false, in case we return early
 */
	*shift = FALSE;
/*
 * Look up our platform.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", platform);
		return (0);
	}

	platorg = ds_PlatformDataOrg (pid);

	fid = F_Lookup (fname);
	switch (platorg)
	{
		case Org3dGrid:
			details[ndet].dd_Name = "altitude";
			details[ndet].dd_V.us_v_float = *alt;
			ndet++;
			/* Fall into */
		case Org2dGrid:
			platclass = DCC_RGrid;
			break;
		case OrgImage:
			platclass = DCC_Image;
			break;
		case OrgIRGrid:
			platclass = DCC_IRGrid;
			break;
		case OrgScalar:	/* <-- historical, should change someday */
		case OrgNSpace:
			details[ndet].dd_Name = "altitude";
			details[ndet].dd_V.us_v_float = *alt;
			ndet++;
			platclass = DCC_NSpace;
			break;
		default:
			msg_ELog (EF_PROBLEM, "ga_GetGrid on bad org");
			return (0);
	}
/*
 * If it's a model platform and validation mode is desired (i.e., we want
 * model data valid at the plot time rather than issued then), back off
 * the data request time
 */
	dtime = *plot_time;

	if (ds_IsModelPlatform (pid) && ValidationMode)
		dtime.zt_Sec -= ForecastOffset;
/*
 * Find out when we can really get data.
 */
	if (! ds_DataTimes (pid, &dtime, 1, DsBefore, &dtime))
	{
		TC_EncodeTime (&dtime, TC_Full, datestring);
		msg_ELog (EF_INFO, "ga_GetGrid: No data for %s at %s", 
			  platform, datestring);
		return (NULL);
	}
/*
 * For image data, we need to do some ugly stuff to deal with altitude
 * steps.
 */
	if (platorg == OrgImage)
		ImageDataTime (comp, pid, *alt, &dtime);
/*
 * Set up the forecast offset time detail
 */
	details[ndet].dd_Name = DD_FORECAST_OFFSET;
	details[ndet].dd_V.us_v_int = ForecastOffset;
	ndet++;
/*
 * Do a DS fetch for this data.
 */
	if (! (dc = ds_Fetch (pid, platclass, &dtime, &dtime, &fid, 1, 
			      details, ndet)))
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s/%s.", platform, fname);
		return (0);
	}
/*
 * If we need to tweak this data into a grid, do so now.
 */
	switch (platorg)
	{
	/*
	 * IRGrids can be interpolated.
	 */
	   case OrgIRGrid:
		if (! ga_Regularize (&dc, fname, platform, comp))
		{
			dc_DestroyDC (dc);
			return (0);
		}
		break;
	/*
	 * Images can be turned into real info.
	 */
	   case OrgImage:
	   	ga_ImgToCGrid (&dc, fname);
		break;
	/*
	 * Scalar implies NSpace stuff, and we'll take a shot at making a
	 * grid from it.
	 */
	    case OrgScalar:
	    case OrgNSpace:
		if (! ga_DoNSpace (&dc, fid))
		{
			dc_DestroyDC (dc);
			return (0);
		}
		break;
	/*
	 * If it's already a grid, we do nothing; otherwise we bail.
	 */
	   case Org2dGrid:
	   case Org3dGrid:
	    	break;
	   default:
	   	msg_ELog (EF_PROBLEM, "Bad grid data org %d", platorg);
		dc_DestroyDC (dc);
		return (0);
	}
/*
 * Apply any needed spatial shift to the data.  We wait this long so that
 * the above transformations can be done first, and irgrids can be shifted.
 */
	*shift = ApplySpatialOffset (dc, comp, plot_time);
/*
 * Pull out the info and return it.  For now we yank out the data and 
 * assume that it can be freed later.
 */
	ret = dc_RGGetGrid (dc, 0, F_Lookup (fname), &loc, &rg, &len);
	cvt_ToXY (loc.l_lat, loc.l_lon, x0, y0);
	*xdim = rg.rg_nX;
	*ydim = rg.rg_nY;
	*x1 = *x0 + (rg.rg_nX - 1)*rg.rg_Xspacing;
	*y1 = *y0 + (rg.rg_nY - 1)*rg.rg_Yspacing;
/*
 * Return
 */
	*plot_time = dtime;
	return (dc);
}




static void
ga_ImgToCGrid (dc, field)
DataChunk	**dc;
char		*field;
/*
 * Turn an image format data chunk into a regular grid with compression.
 */
{
# define COMPRESS 4
	float		*grid, *gp, table[256];
	RGrid		rg;
	ScaleInfo	sc;
	unsigned	char *img;
	int		i, npx, npy, x, y,len;
	float		badval;
	DataChunk	*rdc;
	FieldId		fid;
	ZebTime		when;
	Location	origin;
/*
 * Get the bad value.
 */
	badval = dc_GetBadval (*dc);
/*
 * Get the image from the data chunk.
 */
	fid = F_Lookup (field);
	img = dc_ImgGetImage (*dc, 0, fid, &origin, &rg, &len, &sc);
	npx = rg.rg_nX/COMPRESS; 
	npy = rg.rg_nY/COMPRESS;
/*
 * Allocate a chunk of memory for the grid.
 */
	gp = grid = (float *) malloc (npx * npy * sizeof (float));
/*
 * Go through and calculate the translation table.
 */
	for (i = 0; i < 256; i++)
		table[i] = (float) i*sc.s_Scale + sc.s_Offset;
	table[255] = badval;
/*
 * Populate the new grid.
 */
	for (y = 0; y < npy; y++)
		for (x = 0; x < npx; x++)
		{
			int xp, yp, npa = 0;
			gp = grid + (npy - y - 1)*npx + x;
			*gp = 0.0;
			for (yp = 0; yp < COMPRESS; yp++)
				for (xp = 0; xp < COMPRESS; xp++)
				{
					unsigned char cv;
					cv = img[(y*COMPRESS + yp)*rg.rg_nX
							+ x*COMPRESS + xp];
					if (cv != 255)
					{
						*gp += table[cv];
						npa++;
					}
				}
		/*
		 * Do the average.  If there were no good points here, put
		 * in a bad value flag.
		 */
			if (npa > 0)
				*gp /= (float) npa;
			else
				*gp = badval;
		}
/*
 * Kludge: rotate the grid so it can be rotated again later.
 */
	/* ga_RotateGrid (grid, img, npy, npx); */
/*
 * Fix up the data chunk.
 */
	rg.rg_nX = npx;
	rg.rg_nY = npy;
	rg.rg_nZ = 1;
	rg.rg_Xspacing *= COMPRESS;
	rg.rg_Yspacing *= COMPRESS;
	rdc = dc_CreateDC (DCC_RGrid);
	rdc->dc_Platform = dc_GetPlat (*dc, 0);
	dc_GetTime (*dc, 0, &when);
	dc_RGSetup (rdc, 1, &fid); 
	dc_SetBadval (rdc, dc_GetBadval (*dc));
        dc_RGAddGrid (rdc, 0, fid, &origin, &rg, &when, grid, 0);
/*
 * Free old memory.
 */
	dc_DestroyDC (*dc);
	free (grid);
/*
 * Return the newly created regular grid.
 */
	*dc = rdc;
}




static bool
ga_Regularize (dc, field, platform, comp)
DataChunk 	**dc;
char		*field, *platform, *comp;
/*
 * Turn an irregular grid into a regular one.
 */
{
	char method[80];
/*
 * Find out how they want us to do it.
 */
	if (! pda_Search (Pd, comp, "grid-method", platform, method,
		SYMT_STRING) || ! strcmp (method, "rgrid"))
	{
		if (dc_IRGetNPlatform (*dc) > 100)
		{
			msg_ELog (EF_PROBLEM, "too many stations for rgrid");
			return (ga_BarnesRegularize (dc, field, platform,
					comp, TRUE));
		}
		return (ga_RgridRegularize (dc, field));
	}
	else if (! strcmp (method, "barnes"))
		return ga_BarnesRegularize (dc, field, platform, comp, TRUE);
	else if (! strcmp (method, "closest-point"))
		return ga_BarnesRegularize (dc, field, platform, comp, FALSE);
	else
	{
		msg_ELog (EF_PROBLEM, "Bad grid method: %s", method);
		return (FALSE);
	}
}





static bool
ga_BarnesRegularize (dc, field, platform, comp, dobarnes)
DataChunk **dc;
char *field, *platform, *comp;
int dobarnes;
/*
 * Use the BINTS routine to do barnes/closest point interpolation.
 */
{
	int nsta, i, ip, nqd = 0, nfilt = 0, fullgrid = 0;
	float *xpos, *ypos, xmin = 99999.0, xmax = -99999.0, ymin = 99999.0;
	float ymax = -99999.0, badflag, *grid, *dz, *dzr, radius, rmx, *dp;
	float xspacing, yspacing, spacing, border;
	Location *locs, location;
	FieldId fid;
	RGrid rg;
	ZebTime when;
	DataChunk *rdc;
/*
 * Find out what grid resolution they want.
 */
	if (! pda_Search (Pd, comp, "x-points", platform,
				(char *) &rg.rg_nX, SYMT_INT))
		rg.rg_nX = 20;
	if (! pda_Search (Pd, comp, "y-points", platform,
				(char *) &rg.rg_nY, SYMT_INT))
		rg.rg_nY = 20;
	rg.rg_nZ = 1;
	if (! pda_Search (Pd, comp, "radius", platform,
				(char *) &radius, SYMT_FLOAT))
		radius = 2.0;
	if (! pda_Search (Pd, comp, "max-fill", platform, (char *)
			&rmx, SYMT_FLOAT))
		rmx = 3.0;
	if (! pda_Search (Pd, comp, "full-grid", platform, (char *) &fullgrid,
			SYMT_BOOL))
		fullgrid = FALSE;
/*
 * Get stuff out of the data chunk...number of points (platforms).
 */
	nsta = dc_IRGetNPlatform (*dc);
	locs = (Location *) malloc (sizeof (Location) * nsta);
/*
 * Platform locations, field ID, and data.
 */
	dc_IRGetPlatforms (*dc, NULL /* don't need platids */, locs);
	fid = F_Lookup (field);
	dp = dc_IRGetGrid (*dc, 0, fid);
	badflag = dc_GetBadval (*dc);
/*
 * Do a pass over the locations, and set everything up.
 */
	xpos = (float *) malloc (nsta * sizeof (float));
	ypos = (float *) malloc (nsta * sizeof (float));
	for (i = 0; i < nsta; i++)
	{
	/*
	 * Turn this location into XY space, and see if it stretches our 
	 * limits.
	 */
	 	cvt_ToXY (locs[i].l_lat, locs[i].l_lon, xpos + i, ypos + i);
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
 * If they don't want the full grid, move the limits accordingly.
 */
	if (! fullgrid)
	{
		xmin = Xlo;
		xmax = Xhi;
		ymin = Ylo;
		ymax = Yhi;
	}
/*
 * Figure out grid spacing, taking into account the fact that we want a
 * border (specified in grid widths) outside the bounds of the stations.
 * We force the x and y spacing to be the same since bints requires it.
 */
	border = 0.5;	/* 1/2 grid width border */

	xspacing = (xmax - xmin) / (rg.rg_nX - 1 - (2 * border));
	yspacing = (ymax - ymin) / (rg.rg_nY - 1 - (2 * border));

	spacing = (xspacing > yspacing) ? xspacing : yspacing;
	rg.rg_Xspacing = spacing;
	rg.rg_Yspacing = spacing;
/*
 * Adjust our bounds to reflect the border.
 */
	xmin -= border * spacing;
	ymin -= border * spacing;

	xmax = xmin + (rg.rg_nX - 1) * spacing;
	ymax = ymin + (rg.rg_nY - 1) * spacing;
/*
 * Get the lat/lon of the lower left corner of our grid.
 */
	cvt_ToLatLon (xmin, ymin, &location.l_lat, &location.l_lon);
/*
 * Now that we know our limits, normalize all the points to (Fortran) grid 
 * indices.
 */
	for (i = 0; i < nsta; i++)
	{
		xpos[i] = (xpos[i] - xmin)/rg.rg_Xspacing + 1;
		ypos[i] = (ypos[i] - ymin)/rg.rg_Xspacing + 1;
	}
/*
 * Apply limits.
 */
	ga_RangeLimit (field, nsta, dp, badflag);
/*
 * Allocate other chunks of memory.
 */
	grid = (float *) malloc (rg.rg_nX*rg.rg_nY*sizeof (float));
	dz = (float *) malloc (nsta*sizeof (float));
	dzr = (float *) malloc (rg.rg_nX*rg.rg_nY*sizeof (float));
/*
 * Call the ugly interpolation routine.
 */
	ip = dobarnes ? 2 : 0;
	bints_ (grid, &rg.rg_nX, &rg.rg_nY, xpos, ypos, dp, dz, dzr, &nsta,
			&ip, &radius, &rmx, &nqd, &nfilt, &badflag);
/*
 * Finish fixing up the data chunk, and return.
 */
	rdc = dc_CreateDC (DCC_RGrid);
	rdc->dc_Platform = dc_GetPlat (*dc, 0);
	dc_GetTime (*dc, 0, &when);
	dc_RGSetup (rdc, 1, &fid); 
	dc_SetBadval (rdc, dc_GetBadval (*dc));
	dc_RGAddGrid (rdc, 0, fid, &location, &rg, &when, grid, 0);
/*
 * Free up old memory.
 */
	dc_DestroyDC (*dc);
	free (locs);
	free (xpos);
	free (ypos);
	free (grid);
	free (dz);
	free (dzr);
/*
 * Return the newly created regular grid.
 */
	*dc = rdc;
	return (TRUE);
}



static bool
ga_RgridRegularize (dc, field)
DataChunk **dc;
char *field;
/*
 * Regularize a grid through use of RGRID.
 */
{
	int		RGRID (), i, status;
	float		*grid, *xpos, *ypos;
	float		xmin = 9999.0, ymin = 9999.0, *scratch;
	float		xmax = -9999.0, ymax = -9999.0, badflag, *dp;
	RGrid		rg;
	DataChunk	*rdc;
	int		npoint;
	Location	*locs, location;
	FieldId		fid;
	ZebTime		when;
/*
 * Get stuff out of the data chunk...number of points (platforms).
 */
	npoint = dc_IRGetNPlatform (*dc);
	locs = (Location *) malloc (sizeof (Location) * npoint);
/*
 * Platform locations.
 */
	dc_IRGetPlatforms (*dc, NULL /* don't need platids */, locs);
/*
 * Field id.
 */
	fid = F_Lookup (field);
/*
 * Data values.
 */
	dp = dc_IRGetGrid (*dc, 0, fid);
/*
 * Wire the dimension of the grid, and get some more memory.
 */
	rg.rg_nX = rg.rg_nY = 20;			/* XXX */
	rg.rg_nZ = 1;
	grid = (float *) malloc (rg.rg_nX * rg.rg_nY * sizeof (float));
/*
 * Do a pass over the locations, and set everything up.
 */
	xpos = (float *) malloc (npoint * sizeof (float));
	ypos = (float *) malloc (npoint * sizeof (float));
	for (i = 0; i < npoint; i++)
	{
	/*
	 * Turn this location into XY space, and see if it stretches our 
	 * limits.
	 */
	 	cvt_ToXY (locs[i].l_lat, locs[i].l_lon, xpos + i, ypos + i);
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
 * Store some of the new position info.  The rectangular grid is embedded in
 * the irregular grid with a one grid width border on each side.
 */
	rg.rg_Xspacing = (xmax - xmin)/(rg.rg_nX - 1);
	rg.rg_Yspacing = (ymax - ymin)/(rg.rg_nY - 1);
	cvt_ToLatLon (xmin - rg.rg_Xspacing/2.0, ymin - rg.rg_Yspacing/2.0,
		&location.l_lat, &location.l_lon);
/*
 * Fill the grid with bad value flags
 */
	badflag = dc_GetBadval (*dc);
	for (i = 0; i < rg.rg_nX*rg.rg_nY; i++)
		grid[i] = badflag;
/*
 * Apply limits.
 */
	ga_RangeLimit (field, npoint, dp, badflag);
/*
 * Use RGRID to generate gridded data
 *
 * 9/17/91 cb	NOTE: rgrid expects a bounding box that gives a 
 * 		one grid width border around the grid, rather than
 * 		the bounds of the grid itself.  What a pain...
 */
	scratch = (float *) malloc (rg.rg_nX * rg.rg_nY * sizeof (float));

	msg_ELog (EF_DEBUG,
		"Call rgrid, %d x %d, np %d, (%.2f %.2f) to (%.2f %.2f)",
		rg.rg_nX, rg.rg_nY, npoint, xmin, ymin, xmax, ymax);
	status = do_rgrid_ (grid, &rg.rg_nX, &rg.rg_nY, &npoint, dp, &badflag,
			 xpos, ypos, &xmin, &ymin, &xmax, &ymax, scratch);
/*
 * Clean up.
 */
	free (scratch);
	free (xpos);
	free (ypos);
	free (locs);
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
 * Finish fixing up the data chunk, and return.
 */
	rdc = dc_CreateDC (DCC_RGrid);
	rdc->dc_Platform = dc_GetPlat (*dc, 0);
	dc_GetTime (*dc, 0, &when);
	dc_RGSetup (rdc, 1, &fid); 
	dc_SetBadval (rdc, dc_GetBadval (*dc));
	dc_RGAddGrid (rdc, 0, fid, &location, &rg, &when, grid, 0);
/*
 * Free up old memory.
 */
	dc_DestroyDC (*dc);
	free (grid);
/*
 * Return the newly created regular grid.
 */
	*dc = rdc;
	return (TRUE);
}





static void
ga_RangeLimit (fname, npt, data, badflag)
char	*fname;
int	npt;
float	*data, badflag;
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
			if (data[i] != badflag && data[i] < limit)
			{
				data[i] = badflag;
				nzapped++;
			}
/*
 * Same for the max.
 */
	if (pda_Search (Pd, "global", "range-max", fname, (char *) &limit,
			SYMT_FLOAT))
		for (i = 0; i < npt; i++)
			if (data[i] != badflag && data[i] > limit)
			{
				data[i] = badflag;
				nzapped++;
			}
	if (nzapped)
		msg_ELog (EF_INFO, "%d pts range limited", nzapped);
}




static bool
ga_DoNSpace (dc, fid)
DataChunk	**dc;
FieldId	fid;
/*
 * Try to extract a horizontal plane grid from an NSpace data chunk.  Return
 * TRUE and a new RGrid data chunk if successful, otherwise return FALSE.
 */
{
	if (ga_NSSimpleGrid (dc, fid))
		return (TRUE);
	else
		return (FALSE);
}



	
static bool
ga_NSSimpleGrid (dc, fid)
DataChunk	**dc;
FieldId	fid;
/*
 * Try for a simple to handle but very specific case:
 *	1) our field must have lat and lon as dimensions, and they must be
 *	   the only dimensions with size > 1.
 *	2) lat and lon are both coordinate variables (variables whose 
 *	   dimension has the same name)
 *	3) the values in lat and lon are regularly spaced, i.e., we already
 * 	   have a simple two-dimensional grid
 *	4) variable "alt" exists to give us our vertical position
 *
 * If all these criteria are met, create an RGrid data chunk and return TRUE, 
 * otherwise return FALSE and leave the data chunk untouched.
 *
 * Check for dimensions by name rather than FieldId so that file formats
 * don't have to define every dimension as a FieldId.
 */
{
	DataChunk 	*rdc;
	int		ndims, is_static, i;
	bool		lat_first;
	int		lat_idx, lon_idx;
	float		latspacing, lonspacing;
	float		latorg, lonorg;
	FieldId		lat_id, lon_id, alt_id, dims[DC_MaxDimension];
	char 		*dimns[ DC_MaxDimension ];
	unsigned long	sizes[ DC_MaxDimension ];
	AltUnitType	altunits;
	unsigned long	dimsize, nlats, nlons;
	Location	location;
/*
 * Start by checking the dimensions of our field
 */
	dc_NSGetField (*dc, fid, &ndims, dimns, sizes, &is_static);
	/* dc_NSGetVariable (*dc, fid, &ndims, dims, &is_static); */

	lat_idx = lon_idx = -1;
	lat_first = FALSE;

	for (i = 0; i < ndims; i++)
	{
	/*
	 * lat or lon
	 */
		if (!strcmp(dimns[i], "lat") ||
		    !strcmp(dimns[i], "latitude"))
		{
			lat_idx = i;
			lat_first = (lon_idx < 0);
		}
		else if (!strcmp(dimns[i], "lon") || 
			 !strcmp(dimns[i], "longitude"))
			lon_idx = i;
	/*
	 * or other (make sure its size is exactly one)
	 */
		else
		{
			if (sizes[i] == 1)
				continue;

			msg_ELog (EF_DEBUG, 
				  "ga_NSSimpleGrid: Barf on dim %s, size %d",
				  dimns[i], sizes[i]);

			return (FALSE);
		}
	}
	
			
	if ((lat_idx < 0) || (lon_idx < 0))
	{
		msg_ELog (EF_DEBUG, "ga_NSSimpleGrid: no lat and/or lon");
		return (FALSE);
	}
/*
 * Make sure lat and lon are coordinate variables.  We have the names of the
 * lat and lon dimensions, so see if fields exist by the same name.
 */
	if (!ga_NSCoordVariable (*dc, dimns[lat_idx], &lat_id) ||
	    !ga_NSCoordVariable (*dc, dimns[lon_idx], &lon_id))
		return (FALSE);
/*
 * Check for regular lat and lon spacing
 */
	if (!ga_NSRegularSpacing (*dc, lat_id, &latspacing, &nlats, &latorg) ||
	    !ga_NSRegularSpacing (*dc, lon_id, &lonspacing, &nlons, &lonorg))
		return (FALSE);
/*
 * Build a location.  If "alt" or "altitude" doesn't exist, set the altitude
 * to zero.
 */
	location.l_lat = latorg;
	location.l_lon = lonorg;

	alt_id = F_Declared ("alt");
	if ((alt_id != BadField) && 
	    dc_NSGetVariable (*dc, alt_id, &ndims, dims, &is_static))
	{
		location.l_alt = * (float *) dc_NSGetSample (*dc, 0, alt_id, 
							     NULL);
	}
	else 
	{
		alt_id = F_Declared ("altitude");
		if ((alt_id != BadField) &&
		    dc_NSGetVariable (*dc, alt_id, &ndims, dims, &is_static))
		{
			location.l_alt = * (float *) dc_NSGetSample (*dc, 0, 
								     alt_id, 
								     NULL);
		}
		else
		{
			msg_ELog (EF_DEBUG, 
			  "ga_NSSimpleGrid: No alt variable.  Assuming 0.");
			location.l_alt = 0.0;
		}
	}

/*
 * It looks like all our requirements have been met.  Let's build an 
 * RGrid data chunk.
 */
	rdc = ga_NSRGrid (*dc, fid, &location, latspacing, lonspacing, 
			  nlats, nlons, lat_first);
/*
 * Free up old memory.
 */
	dc_DestroyDC (*dc);
/*
 * Return the newly created regular grid.
 */
	*dc = rdc;
	return (TRUE);
}




DataChunk *
ga_NSRGrid (dc, fid, location, latspacing, lonspacing, nlats, nlons, transpose)
DataChunk *dc;
FieldId fid;
Location *location;
float latspacing;
float lonspacing;
int nlats;
int nlons;
bool transpose;
/*
 * Use the given n-space datachunk and lat/lon spacing parameters to generate
 * a kilometer rgrid.  Return the rgrid datachunk.
 */
{
	DataChunk 	*rdc;
	float 		badflag;
	float 		olat, olon;
	float		*fdata, *grid;
	void		*nsdata;
	short		*sdata;
	RGrid		rg;
	ZebTime		when;
	DC_ElemType	type;
	int		i, j;

	rdc = dc_CreateDC (DCC_RGrid);
        rdc->dc_Platform = dc->dc_Platform;

	dc_RGSetup (rdc, 1, &fid); 

	dc_SetLocAltUnits (rdc, dc_GetLocAltUnits (dc));

	badflag = dc_GetBadval (dc);
	dc_SetBadval (rdc, badflag);
/*
 * Grid info and location
 */
	cvt_GetOrigin (&olat, &olon);
	cvt_ToXY (olat + fabs (latspacing), olon + fabs (lonspacing),
		  &rg.rg_Xspacing, &rg.rg_Yspacing);

	rg.rg_nX = nlons;
	rg.rg_nY = nlats;
	rg.rg_nZ = 1;

	grid = (float *) malloc (nlats * nlons * sizeof (float));

	nsdata = (void *) dc_NSGetSample (dc, 0, fid, NULL);

	switch (type = dc_Type (dc, fid))
	{
	    case DCT_Float:
		fdata = nsdata;
		break;
	    case DCT_ShortInt:
		sdata = (short *) nsdata;
		fdata = (float *) malloc (nlats * nlons * sizeof (float));
		for (i = 0; i < nlats * nlons; i++)
			fdata[i] = (float) sdata[i];
		break;
	    default:
		msg_ELog (EF_PROBLEM, "ga_NSRGrid cannot handle '%s' data",
			  dc_TypeName (type));

		fdata = (float *) malloc (nlats * nlons * sizeof (float));
		for (i = 0; i < nlats * nlons; i++)
			fdata[i] = badflag;
	}
/*
 * Apply limits
 */
/* 
 * default range limits are somewhat (ahem..) limiting for model data, where
 * temperatures get awfully cold as you approach 100 mb...
 */
# ifdef notdef
	ga_RangeLimit (F_GetName (fid), nlats * nlons, nsdata, badflag);
# endif
/*
 * Copy into our new grid.  If the "lat" dimension comes before the "lon"
 * dimension in the NSpace data chunk, we can just do a memcpy.  Otherwise,
 * it's element by element since NSpace data are always stored in row major
 * order.
 */
	if (transpose)
		memcpy ((char *) grid, (char *) fdata, 
			nlats * nlons * sizeof (float));
	else
		for (j = 0; j < nlats; j++)
			for (i = 0; i < nlons; i++)
				grid[nlons * j + i] = fdata[nlats * i + j];

	if (fdata != nsdata)
		free (fdata);
/*
 * Finally, put the grid and time into the new data chunk
 */		
	dc_GetTime (dc, 0, &when);
	dc_RGAddGrid (rdc, 0, fid, location, &rg, &when, grid, 0);
	free (grid);

	return (rdc);
}



int
ga_NSRegularSpacing (dc, fid, rspacing, rnum, org)
DataChunk *dc;			/* N-Space datachunk 	*/
FieldId fid;			/* Field to check	*/
float *rspacing;		/* returned spacing	*/
unsigned long *rnum;		/* number of values 	*/
float *org;			/* This variables origin*/
/*
 * Requires field to be DCT_Float, then checks that its array of values
 * in the datachunk is regularly-spaced.  Note that this will usually
 * only make sense if the field is one-dimensional.  On failure, a 
 * warning is printed and FALSE is returned.  Otherwise, return TRUE.
 */
{
	unsigned long nvals;
	float *vals;
	float delta, spacing;
	int i, onecross = 0;

	if (dc_Type (dc, fid) != DCT_Float)
	{
		msg_ELog (EF_DEBUG, "ga_NSSimpleGrid: non-float %s",
			  F_GetName (fid));
		return (FALSE);
	}
		
	vals = (float *) dc_NSGetSample (dc, 0, fid, &nvals);
	if (nvals > 1)
		spacing = vals[1] - vals[0];
	else
		spacing = 0.0;
	for (i = 2; i < nvals; i++)
	{
		delta = vals[i] - vals[i-1];
	/*
	 * Check for a change in the spacing.  Allow for exactly one
	 * weirdness at (presumably) the international date line.  Does
	 * this look like a kludge to you, too?
	 */
		if (fabs (delta - spacing) > 0.001 && vals[i] > -170.0 &&
			vals[i-1] < 170.0 && ! onecross++)
		{
			msg_ELog (EF_DEBUG, 
				  "ga_NSSimpleGrid: Irregular %s step",
				  F_GetName (fid));
			return (FALSE);
		}
	}

	*org = (spacing < 0.0) ? vals[nvals - 1] : vals[0];
	*rnum = nvals;
	*rspacing = spacing;
	return (TRUE);
}



int
ga_NSCoordVariable (dc, name, fid)
DataChunk *dc;		/* N-Space chunk we're dealing with 		*/
const char *name;	/* Name required to be a coordinate variable 	*/
FieldId *fid;		/* Returned FieldId of coordinate variable 	*/
/*
 * Verify that this name has a coordinate variable.  Print warning and
 * return FALSE on failure; return TRUE and the FielId on success.
 */
{
	FieldId field;
	int ndims;
	char *dimns[ DC_MaxDimension ];
       
/*
 * First make sure the name is actually a field
 */
	field = F_Declared (name);

	if (field == BadField)
	{
		msg_ELog (EF_PROBLEM, "dimn '%s' is not a field", name);
		return (FALSE);
	}
/*
 * Then make sure we have a single dimension and that the dimension matches
 * the expected name
 */
	if (! dc_NSGetField (dc, field, &ndims, dimns, NULL, NULL) ||
	    (ndims != 1) || strcmp(dimns[0], name))
	{
		msg_ELog (EF_DEBUG, "%s: '%s' not a coordinate variable",
			  "ga_NSCoordVariable", name);
		return (FALSE);
	}

	*fid = field;
	return (TRUE);
}

