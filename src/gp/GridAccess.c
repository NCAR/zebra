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
# include <config.h>

# include <math.h>
# include <string.h>
# include <X11/Intrinsic.h>

# include "defs.h"
# include "message.h"
# include "pd.h"
# include <DataStore.h>
# include <DataChunk.h>
# include "GraphProc.h"
# include "rg_status.h"
# include "RasterImage.h"
# include "PixelCoord.h"
# if C_CAP_POLAR
# include "PolarPlot.h"
# endif

MAKE_RCSID ("$Id: GridAccess.c,v 2.39 2002-12-04 00:04:58 burghart Exp $")

# define DEG_TO_RAD(x)	((x)*0.017453292)
# define KM_TO_DEG(x)	((x)*0.008982802) /* on a great circle */


/*
 * Our routines.
 */
static zbool 	ga_Regularize FP ((DataChunk **, FieldId, char *, char *));
static zbool 	ga_RgridRegularize FP ((DataChunk **, FieldId));
static zbool	ga_BarnesRegularize FP ((DataChunk **, FieldId, char *, char *,
			int));
static void 	ga_RangeLimit FP ((char *, int, float *, double));
static void 	ga_ImgToCGrid FP ((DataChunk **, FieldId));
# if C_CAP_POLAR
static void	ga_Rasterize FP ((char *, DataChunk **, FieldId));
static void	ga_MkRastDest FP ((DataChunk *, FieldId, int, int,
			DestImage **, PPCookie *pc, float *, float *,
			Location *, RGrid *));
# endif
static zbool 	ga_DoNSpace FP ((DataChunk **, FieldId));
static zbool 	ga_NSSimpleGrid FP ((DataChunk **, FieldId));

int		ga_NSRegularSpacing FP((DataChunk *dc, FieldId fid,
					float *rspacing, unsigned long *rnum,
					float *origin));
DataChunk 	*ga_NSRGrid FP((DataChunk *dc, FieldId fid, Location *location,
				double latspacing, double lonspacing,
				int nlats, int nlons, int transpose));
#ifdef notdef
static void	ga_StoreSpacings FP ((DataChunk *, float, float));
#endif

int             Derive_Vorticity( float *adiv, float *av, float *au, 
		      Location *loc, RGrid *rg, float badvalue, float afact );


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









zbool
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
ga_GetGrid (plot_time, comp, platform, fid, xdim, ydim, x0, y0, x1, y1,
	    alt, shift)
ZebTime	*plot_time;
char 	*comp, *platform;
FieldId fid;
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
	char		dimn_parms[512];
	dsDetail	details[10];
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

	switch (platorg)
	{
	    case Org3dGrid:
#ifdef notdef
		details[ndet].dd_Name = "altitude";
		details[ndet].dd_V.us_v_float = *alt;
		ndet++;
#endif
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
		platclass = DCC_NSpace;
#ifdef notdef	
		if (pda_Search (Pd, comp, "dimensions", NULL, 
				dimn_parms, SYMT_STRING))
			dc_NSFixedDetails(dimn_parms, details, &ndet);
		details[ndet].dd_Name = "altitude";
		details[ndet].dd_V.us_v_float = *alt;
		ndet++;
#endif
		break;
# if C_CAP_POLAR
	/*
	 * Polar data can be rasterized.
	 */
	    case OrgPolar:
		platclass = DCC_Polar;
		break;
# endif
		
	    default:
			msg_ELog (EF_PROBLEM, "ga_GetGrid on bad org");
			return (0);
	}
/*
 * Pass along some slicing parameters.  Not all orgs make use of them yet
 */
	if (pda_Search (Pd, comp, "dimensions", NULL, dimn_parms, SYMT_STRING))
		dc_NSFixedDetails (dimn_parms, details, &ndet);
	details[ndet].dd_Name = "altitude";
	details[ndet].dd_V.us_v_float = *alt;
	ndet++;
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
 * steps.  Polar data has a similar constraint.
 */
	if (platorg == OrgImage || platorg == OrgPolar)
		ImageDataTime (comp, pid, *alt, &dtime);
/*
 * Set up the forecast offset time detail
 */
	details[ndet].dd_Name = DD_FORECAST_OFFSET;
	details[ndet].dd_V.us_v_int = ForecastOffset;
	ndet++;
/*
 * Snarf the data.
 */
	if (! (dc = ds_Fetch (pid, platclass, &dtime, &dtime, &fid, 1, 
			      details, ndet)))
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s/%s.", platform, 
			  F_GetFullName (fid));
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
		if (! ga_Regularize (&dc, fid, platform, comp))
		{
			dc_DestroyDC (dc);
			return (0);
		}
		break;
	/*
	 * Images can be turned into real info.
	 */
	   case OrgImage:
	   	ga_ImgToCGrid (&dc, fid);
		break;
# if C_CAP_POLAR
	/*
	 * Polar data gets to be rasterized...our work is just beginning...
	 */
	    case OrgPolar:
		ga_Rasterize (comp, &dc, fid);
		break;
# endif
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
	ret = dc_RGGetGrid (dc, 0, fid, &loc, &rg, &len);
	prj_Project (loc.l_lat, loc.l_lon, x0, y0);
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






# if C_CAP_POLAR


static void
ga_GetRastParams (char *comp, int *gratio, int *project, FieldId *fids,
		int *ttest, float *tvalue)
/*
 * Get the PD parameters we need to do this rasterization work.
 */
{
	char rep[80], tfield[80];
	char fname[64];
	int enab;

	strcpy (fname, SimpleFieldName (fids[0]));
/*
 * Pull out the representation of this component.
 */
	if (! pd_Retrieve (Pd, comp, "representation", rep, SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "Comp %s has no representation?", comp);
		return;
	}
/*
 * Pull out the grid ratio parameter.  Here we use the representation as
 * a qualifier!  It's quite likely that they might want to use a lower
 * grid resolution when contouring then when doing raster plots.
 */
	if (! pda_Search (Pd, comp, "grid-size-ratio", rep,
			(char *) gratio, SYMT_INT))
		*gratio = 0;
/*
 * Projection?
 */
	if (! pda_Search (Pd, comp, "horizontal-projection", "polar",
			(char *) project, SYMT_BOOL))
		*project = TRUE;
/*
 * Thresholding info.  NOTE that technically this is way too late to be
 * getting this stuff, since the datachunk already exists.  The only
 * implementation of DCC_Polar uses callbacks to sweepfiles, and we can
 * get away with this for now.  Someday it will break and you'll be mad
 * at me.
 */
	if (! pda_Search (Pd, comp, "threshold", fname, (char *) &enab,
			SYMT_BOOL))
		enab = FALSE;
	if (enab && pda_Search (Pd, comp, "threshold-field", fname, tfield,
			SYMT_STRING))
	{
		fids[1] = F_Lookup (tfield);
		if (! pda_Search (Pd, comp, "threshold-test", fname, tfield,
				SYMT_STRING))
			*ttest = FALSE;
		else
			*ttest = ! strcmp (tfield, "over");
		if (! pda_ReqSearch (Pd, comp, "threshold-value", fname,
				(char *) tvalue, SYMT_FLOAT))
			fids[1] = BadField;  /* Turn off thresholding */
	}
	else
		fids[1] = BadField;
}




static void
ga_Rasterize (char *comp, DataChunk **dc, FieldId fid)
/*
 * Rasterize this polar datachunk into a 2dgrid DC.
 */
{
	DestImage *dest;
	PPCookie pc;
	int beam, row, rlen, gratio = 0, project = 0, ttest;
	RGrid rg;
	Location origin;
	SweepInfo sweep;
	PolarBeam *pb;
	DataChunk *gdc;
	ZebTime when;
	float *srcg, *dstg, tvalue, xradar, yradar;
	FieldId fids[2];
/*
 * Pull out needed PD parameters.
 */
	fids[0] = fid;
	ga_GetRastParams (comp, &gratio, &project, fids, &ttest, &tvalue);
/*
 * figure out how the destination grid has to sit, and create it.
 */
	ga_MkRastDest (*dc, fid, gratio, project, &dest, &pc, &xradar,
			&yradar, &origin, &rg);
/*
 * Time to plow through the data and rasterize it all.
 */
	dcp_GetSweepInfo (*dc, 0, &sweep);
	for (beam = 0; beam < sweep.si_NBeam; beam++)
	{
		pb = dcp_GetBeam (*dc, 0, beam, fid, fids[1], ttest, tvalue);
		if (! pb)
			continue;
# ifdef RDEBUG
		for (row = 0; row < 1000; row++)
			pb->pb_Data[row] = row/10.0;
# endif
		pol_PlotBeam (pc, pb, pb->pb_Data, xradar, yradar);
		dcp_FreeBeam (pb);
	}
/*
 * Create the destination data chunk.  Add the data with a null pointer,
 * forcing the data chunk to allocate the space but not populate it with
 * anything.
 */
	gdc = dc_CreateDC (DCC_RGrid);
	gdc->dc_Platform = dc_GetPlat (*dc, 0);
	dc_GetTime (*dc, 0, &when);
	dc_RGSetup (gdc, 1, &fid);
	dc_SetBadval (gdc, dc_GetBadval (*dc));
	dc_RGAddGrid (gdc, 0, fid, &origin, &rg, &when, NULL, 0);
/*
 * Now we painfully copy in the grid data.  The rasterization code works
 * in the top-origin world, while grids expect to have them in the bottom,
 * so we do things this way to get turned back around.
 */
	rlen = rg.rg_nX*sizeof (float);
	dstg = dc_RGGetGrid (gdc, 0, fid, 0, 0, 0);
	srcg = ((float *) dest->di_image) + (rg.rg_nY - 1)*rg.rg_nX;
	for (row = 0; row < rg.rg_nY; row++)
	{
		memcpy (dstg, srcg, rlen);
		dstg += rg.rg_nX;
		srcg -= rg.rg_nX;
	}
/*
 * We're now done with the rasterization grid.  Carefully free it first,
 * then tell the polar plot code to take care of the rest.  Also get rid
 * of the old data chunk, we don't need it any more.
 */
	free (dest->di_image);
	pol_Finished (pc);
	dc_DestroyDC (*dc);
/*
 * Now just substitute in the new one, and life is groovy.
 */
	*dc = gdc;
}






static void
ga_MkRastDest (DataChunk *dc, FieldId fid, int gratio, int project,
		DestImage **dest,
		PPCookie *pc, float *xradar, float *yradar, Location *origin,
		RGrid *rg)
/*
 * Create the destination grid for this rasterization.  This grid, it may
 * be noted, is optimized around the display area, but is, in fact, a bit
 * larger in extent.  The idea is to help the contouring code do the right
 * thing at the edges.
 */
{
	float pixPerKm = (XPIX (Xhi) - XPIX (Xlo))/(Xhi - Xlo);
	float gatesPerKm, rxkm, rykm, badval, *grid;
	PolarBeam *pb;
	Location rloc;
	int n, np;
/*
 * We actually have to pull the first beam out of the data chunk to get
 * the gate spacing.  Here we calculate a rough gates per *diagonal*
 * kilometer (the 1.4 is a rough sqrt(2)).
 */
	pb = dcp_GetBeam (dc, 0, 0, fid, BadField, 0, 0);
	if (! pb)
	    return;	/* Oops */
	gatesPerKm = 2/pb->pb_GateSpacing;
	origin->l_alt = pb->pb_FixedAngle;
	dcp_FreeBeam (pb);
/*
 * Figure out where the radar is in km space.
 */
	dc_GetLoc (dc, 0, &rloc);
	prj_Project (rloc.l_lat, rloc.l_lon, &rxkm, &rykm);
/*
 * If the data resolution is higher than that of the display, or if the user
 * has specified an explicit graphics ratio, create  a grid at display
 * resolution.  
 */
	if (gratio > 0 || gatesPerKm > pixPerKm)
	{
		if (gratio > 1)
			pixPerKm /= gratio;
	}
/*
 * Otherwise go with a grid resolution a bit higher than the data resolution
 * (so that diagonal beams don't lose out).
 */
	else
		pixPerKm = gatesPerKm;
/*
 * Now figure our spacings.
 */
	rg->rg_nX = pixPerKm*(XUSER (GWWidth (Graphics)-1) - XUSER(0));
	rg->rg_nY = pixPerKm*(YUSER (0) - YUSER(GWHeight(Graphics)-1));
	rg->rg_nZ = 1;
	rg->rg_Xspacing = rg->rg_Yspacing = 1.0/pixPerKm;
/*
 * Radar and origin positions...
 */
	*xradar = (rxkm - XUSER (0))*pixPerKm;
	*yradar = (YUSER (0) - rykm)*pixPerKm - 1;
	prj_Reverse (XUSER (0), YUSER (GWHeight (Graphics) - 1),
			&origin->l_lat, &origin->l_lon);
# ifdef GRDEBUG
	msg_ELog (EF_INFO,
		      "Grid res %dx%d, sp %.1f %.1f orig %.1f %.1f, ppkm %.2f",
			rg->rg_nX, rg->rg_nY, rg->rg_Xspacing, rg->rg_Yspacing,
			origin->l_lat, origin->l_lon, pixPerKm);
	msg_ELog (EF_INFO, "Radar at %d %d", *xradar, *yradar);
# endif
/*
 * Now that we've figured out what we want, it's time to create the actual
 * grid.
 */
	grid = (float *) malloc (rg->rg_nX * rg->rg_nY * sizeof (float));
	*dest = ri_MakeMemImage (rg->rg_nX, rg->rg_nY, grid, sizeof (float),
			rg->rg_nX*sizeof (float));
/*
 * Now we have to slog through the whole damn thing and set it to bad value
 * flags.  Since we're setting to a floating point value, we can't use
 * memset or anything so nice as that.
 */
	badval = dc_GetBadval (dc);
	np = rg->rg_nX*rg->rg_nY;
	for (n = 0; n < np; n++)
		*grid++ = badval;
/*
 * Fix up a polar plotter and we're set.
 */
	*pc = pol_GridSetup (project, *dest, XUSER (0),
			YUSER (GWHeight (Graphics) - 1),
			XUSER (GWWidth (Graphics) - 1), YUSER (0));
}



# endif /* C_CAP_POLAR */




static void
ga_StoreSpacings (dc, latspacing, lonspacing)
DataChunk *dc;
float latspacing, lonspacing;
/*
 * Store these values into this data chunk.
 */
{
	float spacings[2];

	spacings[0] = latspacing;
	spacings[1] = lonspacing;
	dc_SetGlobalAttrArray (dc, ATTR_LATLON, DCT_Float, 2, spacings);
}




static void
ga_ImgToCGrid (dc, fid)
DataChunk **dc;
FieldId fid;
/*
 * Turn an image format data chunk into a regular grid with compression.
 */
{
# define COMPRESS 4
	float		*grid, *gp, table[256], olat, olon;
	RGrid		rg;
	ScaleInfo	sc;
	unsigned	char *img;
	int		i, npx, npy, x, y,len;
	float		badval, lonstep, latstep;
	DataChunk	*rdc;
	ZebTime		when;
	Location	origin;
/*
 * Get the bad value.
 */
	badval = dc_GetBadval (*dc);
/*
 * Get the image from the data chunk.
 */
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
 * Figure what the original lat/lon spacings must have been and stash
 * them.
 */
	cvt_GetOrigin (&olat, &olon);
	lonstep = KM_TO_DEG (rg.rg_Xspacing/cos (DEG_TO_RAD (olat)));
	latstep = KM_TO_DEG (rg.rg_Yspacing);
	ga_StoreSpacings (rdc, latstep, lonstep);
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




static zbool
ga_Regularize (dc, fid, platform, comp)
DataChunk 	**dc;
FieldId		fid;
char		*platform, *comp;
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
			return (ga_BarnesRegularize (dc, fid, platform,
						     comp, TRUE));
		}
		return (ga_RgridRegularize (dc, fid));
	}
	else if (! strcmp (method, "barnes"))
		return ga_BarnesRegularize (dc, fid, platform, comp, TRUE);
	else if (! strcmp (method, "closest-point"))
		return ga_BarnesRegularize (dc, fid, platform, comp, FALSE);
	else
	{
		msg_ELog (EF_PROBLEM, "Bad grid method: %s", method);
		return (FALSE);
	}
}





static zbool
ga_BarnesRegularize (dc, fid, platform, comp, dobarnes)
DataChunk **dc;
FieldId fid;
char *platform, *comp;
int dobarnes;
/*
 * Use the BINTS routine to do barnes/closest point interpolation.
 */
{
	int nsta, i, ip, nqd = 0, nfilt = 0, fullgrid = 0;
	float *xpos, *ypos, xmin = 99999.0, xmax = -99999.0, ymin = 99999.0;
	float ymax = -99999.0, badflag, *grid, *dz, *dzr, radius, rmx, *dp;
	float xspacing, yspacing, spacing, border, lats, lons, olat, olon;
	Location *locs, location;
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
	dp = dc_IRGetGrid (*dc, 0, fid);
	badflag = dc_GetBadval (*dc);
/*
 * Do a pass over the locations, and set everything up.
 */
	xpos = (float *) malloc (nsta * sizeof (float));
	ypos = (float *) malloc (nsta * sizeof (float));
	if (nsta)
		location.l_alt = locs[0].l_alt;
	for (i = 0; i < nsta; i++)
	{
	/*
	 * Turn this location into XY space, and see if it stretches our 
	 * limits.
	 *
	 * Here is something truly tragic...  using projection to place
	 * the points creates truly confused plots, since the grid will
	 * be projected a second time when we plot things.  So we stay
	 * with the old cvt_ routines here.  What we really need to be doing
	 * is to grid in lat/lon space.  In a sense, that *is* what we are
	 * doing, if we are careful about it.
	 *
	 * Another nice alternative would be to be able to grid in XY space
	 * and get everybody downstream to recognize an already-projected
	 * grid, but that's harder.
	 */
/*	prj_Project (locs[i].l_lat, locs[i].l_lon, xpos + i, ypos + i);*/
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
	ga_RangeLimit (F_GetName (fid), nsta, dp, badflag);
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
 * Reverse engineer our lat/lon spacings in case projection is being done.
 */
	cvt_GetOrigin (&olat, &olon);
	cvt_ToLatLon (spacing, spacing, &lats, &lons);
	ga_StoreSpacings (rdc, lats - olat, lons - olon);
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



static zbool
ga_RgridRegularize (dc, fid)
DataChunk **dc;
FieldId fid;
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
	if (npoint)
		location.l_alt = locs[0].l_alt;
	for (i = 0; i < npoint; i++)
	{
	/*
	 * Turn this location into XY space, and see if it stretches our 
	 * limits.  See comment in ga_BarnesRegularize about use of the
	 * cvt_ routines.
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
	ga_RangeLimit (F_GetName (fid), npoint, dp, badflag);
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
	status = dorgrid_ (grid, &rg.rg_nX, &rg.rg_nY, &npoint, dp, &badflag,
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




static zbool
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



	
static zbool
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
	int		nflds, ndims, is_static, i;
	zbool		lat_first;
	int		lat_idx, lon_idx;
	float		latspacing, lonspacing, spacings[2];
	float		latorg, lonorg;
	FieldId		lat_id, lon_id, alt_id, *flds;
	char		*dimns[DC_MaxDimension];
	unsigned long	nlats, nlons, sizes[DC_MaxDimension];
	Location	location;
/*
 * Nominally, we want "latitude" and "longitude" dimensions.  These id's 
 * will likely change to id's of real fields that can yield lat and lon.
 */
	lat_id = F_Lookup ("latitude");
	lon_id = F_Lookup ("longitude");
/*
 * Start by checking the dimensions of our field.
 */
	dc_NSGetField (*dc, fid, &ndims, dimns, sizes, &is_static);

	lat_idx = lon_idx = -1;
	lat_first = FALSE;

	for (i = 0; i < ndims; i++)
	{
	/*
	 * lat or lon
	 */
	    if (lat_idx < 0 && ! strncasecmp (dimns[i], "lat", 3))
	    {
		lat_idx = i;
		lat_first = (lon_idx < 0);
	    }
	    else if (lon_idx < 0 && ! strncasecmp (dimns[i], "lon", 3))
		lon_idx = i;
	/*
	 * or other (make sure its size is exactly one)
	 */
	    else
	    {
		if (sizes[i] == 1)
		    continue;
		msg_ELog (EF_DEBUG, "ga_NSSimpleGrid: Barf on dim %s, size %d",
			  dimns[i], sizes[i]);

		return (FALSE);
	    }
	}
	
			
	if ((lat_idx < 0) || (lon_idx < 0))
	{
		msg_ELog (EF_DEBUG, "ga_NSSimpleGrid: missing %s",
			  "lat or lon dimension");
		return (FALSE);
	}
/*
 * Since we require coordinate variables, find the fields named the same as
 * our dimensions.
 */
	lat_id = lon_id = BadField;
	
	flds = dc_GetFields (*dc, &nflds);
	for (i = 0; i < nflds; i++)
	{
	    if (! strcmp (F_GetName (flds[i]), dimns[lat_idx]))
		lat_id = flds[i];
	    else if (! strcmp (F_GetName (flds[i]), dimns[lon_idx]))
		lon_id = flds[i];
	}

	if (lat_id == BadField || lon_id == BadField)
	{
	    msg_ELog (EF_DEBUG, "ga_NSSimpleGrid: missing %s",
		      "lat or lon coordinate variable");
	    return (FALSE);
	}
/*
 * Check for regular lat and lon spacing
 */
	if (!ga_NSRegularSpacing (*dc, lat_id, &latspacing, &nlats, &latorg) ||
	    !ga_NSRegularSpacing (*dc, lon_id, &lonspacing, &nlons, &lonorg))
		return (FALSE);
/*
 * Build a location.  If "altitude" doesn't exist, set the altitude to zero.
 */
	location.l_lat = latorg;
	location.l_lon = lonorg;

	alt_id = F_Lookup ("altitude");
	
	for (i = 0; i < nflds; i++)
	{
	    if (F_CanYield (flds[i], alt_id, NULL, NULL))
	    {
		location.l_alt = * (float *) dc_NSGetSample (*dc, 0, flds[i], 
							     NULL);
		break;
	    }
	}

	if (i == nflds)
	    location.l_alt = 0.0;
/*
 * It looks like all our requirements have been met.  Let's build an 
 * RGrid data chunk.
 */
	rdc = ga_NSRGrid (*dc, fid, &location, latspacing, lonspacing, 
			  nlats, nlons, lat_first);
/*
 * Stash the spacings into the data chunk.  You never know who might want
 * them.
 */
	ga_StoreSpacings (rdc, latspacing, lonspacing);
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
zbool transpose;
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
	unsigned char	*ucdata;
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
	prj_GetOrigin (&olat, &olon);
	prj_Project (olat + fabs (latspacing), olon + fabs (lonspacing),
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
	    case DCT_UnsignedChar:
	        ucdata = (unsigned char*) nsdata;
		fdata = (float *) malloc (nlats * nlons * sizeof (float));
		for (i = 0; i < nlats * nlons; i++)
		    fdata[i] = (float)ucdata[i];
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

	if (fdata != (float*) nsdata)
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



/*============================================================================
 * Functions related to Vorticity and Divergence calculation
 ============================================================================*/

/* 
 * Return the derivation of Vorticity or Divergence as a DataChunk.
 */
DataChunk * GetVorticity( when, comp, platform, fid, xdim, ydim, 
			x0, y0, x1, y1, alt, shifted)
ZebTime *when;
char    *comp, *platform;
FieldId	fid;
int     *xdim, *ydim, *shifted;
float   *x0, *y0, *x1, *y1, *alt;
{
DataChunk   *vortdc;
DataChunk   *udc, *vdc;
float       *ugrid, *vgrid, *adiv;
FieldId     winds[2];
FieldId     vortfld, divfld;
PlatformId  pid;
WindInfo    wi;
Location    loc;
RGrid       rg;
int         len;
zbool        do_divergence;
float       badvalue;
AltUnitType altunits;

/*
 * Look up our platform.
 */
   if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
   {
      msg_ELog (EF_PROBLEM, "Bad platform '%s'", platform);
      return (0);
   }

   FindWindsFields (comp, pid, when, winds, &wi);
/*
 * Get u component.
 */
   if (! (udc = ga_GetGrid (when, comp, platform, winds[0], xdim, ydim, 
			    x0, y0, x1, y1, alt, shifted)))
   {
      msg_ELog (EF_PROBLEM, "No U field for this '%s'", platform);
      return(0);
   }

   ugrid = (float *) dc_RGGetGrid(udc,0,winds[0], &loc, &rg, &len);
   if ( ugrid==NULL )
     {
       msg_ELog (EF_PROBLEM, "Problems in getting U grid");
       return (0);
     }

   badvalue = dc_GetBadval (udc);
   altunits = dc_GetLocAltUnits (udc);

/*
 * Get v component.
 */
   if (! (vdc = ga_GetGrid (when, comp, platform, winds[1], xdim, ydim, 
			    x0, y0, x1, y1, alt, shifted)))
   {
      msg_ELog (EF_PROBLEM, "No V field for this '%s'", platform);
      return(0);
   }

   vgrid = (float *) dc_RGGetGrid(vdc, 0, winds[1], &loc,&rg,&len);
   if ( vgrid==NULL )
     {
       msg_ELog (EF_PROBLEM, "Problems in getting V grid");
       return (0);
     }

   adiv = (float *) malloc ( (*xdim) * (*ydim) * sizeof(float));
   if ( adiv==NULL )
   {
      msg_ELog (EF_PROBLEM, "Could not allocate memory"); 
      return ( 0 );
   }
/*
 * Are we really doing divergence rather than vorticity?
 */
   do_divergence = ! strcasecmp (F_GetName (fid), "divergence");

/*
 * Compute
 */
   
   if ( do_divergence )        /* Calculate Divergence */ 
     {
        msg_ELog (EF_DEBUG, "Deriving Divergence");
        if(!Derive_Vorticity (adiv, vgrid, ugrid, &loc, &rg, badvalue, -1.0 ))
	  return(0);
     }
   else                        /* Calculate Vorticity */
     {
        msg_ELog (EF_DEBUG, "Deriving Vorticity"); 
        if(!Derive_Vorticity (adiv, ugrid, vgrid, &loc, &rg, badvalue, 1.0 ))
	  return(0);
     } 

/*
 * We don't need the data arrays and the data chunks any more.
 */

   dc_DestroyDC (udc);
   dc_DestroyDC (vdc);

/* 
 * Create  a DataChunk using NSpace representation to store the resultant
 * Vorticity or Divergence.  Need to set the bad value flag for this data.
 * Return the vortdc DataChunk with vorticity or divergence.
 */

   vortdc = dc_CreateDC ( DCC_RGrid );
   vortdc->dc_Platform = ds_LookupPlatform (platform);
   dc_SetBadval (vortdc, badvalue );
   dc_SetLocAltUnits (vortdc, altunits );

   dc_RGSetup   ( vortdc, 1, &fid );
   if(!dc_RGAddGrid (vortdc, 0, fid, &loc, &rg, when, adiv, len ))
   { 
       msg_ELog (EF_PROBLEM, "Error to add a sample to datachunk");
       return (0);
   }

   free ( adiv );
   return ( vortdc );

}



/*
 * Divergence and Vorticity algorithm  
 * by Rosmeri Porfirio da Rocha : rocha@ipmet1.ipmet.unesp.br 
 *
 * Adapted to Zebra software 
 * by Sergio Hiroshi Ishikawa   : ishikawa@ipmet1.ipmet.unesp.br
 *                              : ishikawa@stout.atd.ucar.edu
 * 9/30/97
 *
 * This function computes vorticity or divergence depending on the order of 
 * wind_v and wind_u passed as parameters by the calling function and also the
 * value of parameter afact. The derived field will be returned in adiv array.
 * The calling function must pass parameters like:
 * 
 * For divergence :
 *         Derive_Vorticity (adiv, vgrid, ugrid, loc, rg, badvalue, -1.0) 
 * For vorticity :
 *         Derive_Vorticity (adiv, ugrid, vgrid, loc, rg, badvalue, 1.0 ) 
 */  
int
Derive_Vorticity( float *adiv, float *au, float *av, Location *loc, 
		  RGrid *rg, float badvalue, float afact )
{
double  aa, bb, aux, arad;
int     offset, offsetm1, offsetp1;
int     i, j, nlat, nlon;
float   olat, olon, slat, wlon, latstep, lonstep;
double  *alat, *alon;

  cvt_GetOrigin (&olat, &olon);
  lonstep = KM_TO_DEG (rg->rg_Xspacing/cos (M_PI/180.0*olat));
  latstep = KM_TO_DEG (rg->rg_Yspacing);

  nlat = rg->rg_nY;
  nlon = rg->rg_nX;
  slat = loc->l_lat;
  wlon = loc->l_lon;

  msg_ELog (EF_DEBUG, "NLAT '%d' NLON '%d' SLAT '%f' WLON '%f' LATSTEP '%f' LONSTEP '%f' BADVALUE '%f' ", 
	    nlat, nlon, slat, wlon, latstep, lonstep, badvalue);

  if ((alat = (double *) malloc (nlat*sizeof(double)))==NULL)
  {
    msg_ELog(EF_PROBLEM, "Could not allocate memory");
    return(0);
  }

  if ((alon = (double *) malloc (nlon*sizeof(double)))==NULL)
  {
    msg_ELog(EF_PROBLEM, "Could not allocate memory");
    return(0);
  }

  arad = M_PI/180.0;

  for (j=0, aux=0.0; j<nlon; j++)
    { 
      aux += lonstep;
      alon[j] = (double) arad*(wlon+aux);
    }
  
  for (j=0, aux=0.0; j<nlat; j++)
    { 
      aux += latstep;
      alat[j] = (double) arad*(slat+aux);
    }

/* Computing for a given altitude */

  for (i=1; i<nlat-1; i++)
  {
    offset = i*nlon;
    offsetm1 = (i-1)*nlon;
    offsetp1 = (i+1)*nlon;
    for (j=1; j<nlon-1; j++)
    {
       if ( av[offset+j+1] != badvalue && av[offset+j-1] != badvalue &&
	    au[offsetp1+j] != badvalue && au[offsetm1+j] != badvalue )
       {
	  aa = (av[offset+j+1]-av[offset+j-1])/(alon[j+1]-alon[j-1]);
          bb = (au[offsetp1+j]*cos((double)alat[i+1]) -
	       au[offsetm1+j]*cos((double)alat[i-1])) / (alat[i+1]-alat[i-1]);
          adiv[offset+j] = (float) (aa-afact*bb)/(6370000.0*cos(alat[i]));
       }
         else adiv[offset+j] = badvalue;
    }
  }

/* Fill in badvalue on the boundaries */
 
  for (i=0; i<nlon; i++)
    {
      adiv[i]=badvalue;
      adiv[(nlat-1)*nlon+i]=badvalue;
    } 

  for (i=1; i<nlat-1; i++)
    {
      offset = i*nlon;
      adiv[offset]=badvalue;
      adiv[offset+nlon-1]=badvalue;
    }

  free (alat);
  free (alon);

  return (1);
}


