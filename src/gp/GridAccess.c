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
# include "defs.h"
# include "message.h"
# include "pd.h"
# include <DataStore.h>
# include <DataChunk.h>
# include "GraphProc.h"
# include "rg_status.h"
MAKE_RCSID ("$Id: GridAccess.c,v 2.9 1992-10-06 15:29:00 corbet Exp $")



/*
 * Our routines.
 */
DataChunk	*ga_GetGrid FP ((ZebTime *, char *, char *, char *, int *,
			int *, float *, float *, float *, float *, float *,
			int *));
static bool 	ga_Regularize FP ((DataChunk **, char *));
static void 	ga_RangeLimit FP ((char *, int, float *, double));
static void 	ga_ImgToCGrid FP ((DataChunk **, char *));
# ifdef notdef
static void 	ga_ImgToGrid FP ((DataObject *));
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
ZebTime	*plot_time;
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
ZebTime *plot_time;
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
	ZebTime		realtime;
	DataOrganization platorg;
	DataClass	platclass;
	Location	loc;
	int 		len, ndet = 0;
	FieldId		fid = F_Lookup (fname);
	dsDetail	det;
/*
 * Look up our platform.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", platform);
		return (0);
	}
	platorg = ds_PlatformDataOrg (pid);
# ifdef notdef  /* 6/12/92 jc */
	if (platorg == Org3dGrid)
		platorg = Org2dGrid;
# endif
	switch (platorg)
	{
		case Org3dGrid:
			det.dd_Name = "altitude";
			det.dd_V.us_v_float = *alt;
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
		default:
			msg_ELog (EF_PROBLEM, "ga_GetGrid on bad org");
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
/*
 * Do a DS get for this data.
 */
	if (! (dc = ds_Fetch (pid, platclass, &realtime, &realtime, &fid, 1, 
		&det, ndet)))
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
		if (! ga_Regularize (&dc, fname))
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
	*alt = loc.l_alt;
	cvt_ToXY (loc.l_lat, loc.l_lon, x0, y0);
	*xdim = rg.rg_nX;
	*ydim = rg.rg_nY;
	*x1 = *x0 + (rg.rg_nX - 1)*rg.rg_Xspacing;
	*y1 = *y0 + (rg.rg_nY - 1)*rg.rg_Yspacing;
/*
 * Return.
 */
	*plot_time = realtime;
	return (dc);
}




# ifdef notdef
static void
ga_ImgToGrid (dobj)
DataObject *dobj;
/*
 * Turn an image format data object into a regular grid.
 */
{
	float *grid, *gp, table[256];
	RGrid *rg = dobj->do_desc.d_img.ri_rg;
	ScaleInfo *sc = dobj->do_desc.d_img.ri_scale;
	unsigned char *img = (unsigned char *) dobj->do_data[0];
	int i, npt = rg->rg_nX*rg->rg_nY;
/*
 * Allocate a huge chunk of memory for the grid.
 */
	gp = grid = (float *) malloc (npt * sizeof (float));
/*
 * Go through and calculate the translation table.
 */
	for (i = 0; i < 256; i++)
		table[i] = (float) i/sc->s_Scale + sc->s_Offset;
	table[255] = dobj->do_badval;
/*
 * Populate the new grid.
 */
	for (i = 0; i < npt; i++)
		*gp++ = table[*img++];
/*
 * Fix up the data object, free old memory, and we're done.
 */
	free (dobj->do_data[0]);
	dobj->do_data[0] = grid;
	dobj->do_org = Org2dGrid;
	dobj->do_desc.d_rgrid = *rg;
	dobj->do_loc = dobj->do_aloc[0];
	free (rg);
	free (sc);
}
# endif



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
/*
 * Return the newly created regular grid.
 */
	*dc = rdc;
}




static bool
ga_Regularize (dc, field)
DataChunk 	**dc;
char		*field;
/*
 * Turn an irregular grid into a regular one.
 */
{
	int		RGRID (), i, status;
	float		*grid, *xpos, *ypos;
	float		xmin = 9999.0, ymin = 9999.0, *scratch;
	float		xmax = -9999.0, ymax = -9999.0, badflag, *dp;
	RGrid		rg;
	DataChunk	*rdc;
	int		npoint;
	PlatformId	*platforms;
	Location	*locs, location;
	FieldId		fid;
	ZebTime		when;
/*
 * Get stuff out of the data chunk...number of points (platforms).
 */
	npoint = dc_IRGetNPlatform (*dc);
	locs = (Location *) malloc (sizeof (Location) * npoint);
	platforms = (PlatformId *) malloc (sizeof (PlatformId) * npoint);
/*
 * Platform locations.
 */
	dc_IRGetPlatforms (*dc, platforms, locs);
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
 * Store some of the new position info.  The rectangular grid is embedded in
 * the irregular grid with a one grid width border on each side.
 */
	rg.rg_Xspacing = (xmax - xmin)/(rg.rg_nX + 1);
	rg.rg_Yspacing = (ymax - ymin)/(rg.rg_nY + 1);
	cvt_ToLatLon (xmin + rg.rg_Xspacing, ymin + rg.rg_Yspacing,
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

	status = do_rgrid_ (grid, &rg.rg_nX, &rg.rg_nY, &npoint,
		dp, &badflag, xpos, ypos, &xmin, &ymin, 
		&xmax, &ymax, scratch);
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
