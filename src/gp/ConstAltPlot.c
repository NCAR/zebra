/*
 * Herein lies all the Constant Altitude Plot code, carved from PlotExec.
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
# if C_PT_CAP


# include <X11/Intrinsic.h>
# include <ui.h>
# include <defs.h>
# include <pd.h>
# include <ui_date.h>
# include <message.h>
# include <DataStore.h>
# include <DataChunk.h>
# include "GC.h"
# include "GraphProc.h"
# include "DrawText.h"
# include "PixelCoord.h"
# include "EventQueue.h"

MAKE_RCSID ("$Id: ConstAltPlot.c,v 2.24 1992-12-09 16:42:07 corbet Exp $")


/*
 * Color stuff
 */
static XColor	*Colors, Ctclr;
static int	Ncolors;
static bool 	Monocolor;

/*
 * Other annotation information.
 */
static float	Sascale;
static bool	Sashow;

/*
 * Non-modular kludgery to make things work for now, until something better
 * gets implemented.  All of these are defined in PlotExec.c.
 */
extern XColor Tadefclr;		/* Kludge, for now	*/
extern int Comp_index;
extern Pixel White;

static int Ctlimit;
/*
 * Macro for a pointer to x cast into a char *
 */
# define CPTR(x)	(char *)(&(x))

/*
 * Contour plot types
 */
typedef enum {LineContour, FilledContour} contour_type;


/*
 * This structure is used to describe stations in the station plot rep.
 */
typedef struct _StInfo
{
	int	si_x, si_y;	/* Where it is (on screen)	*/
	bool	si_excl;	/* Excluded?			*/
	bool	si_mark;	/* Used in filtering operations */
} StInfo;



/*
 * Forwards.
 */
void		CAP_FContour FP ((char *, int));
void		CAP_Vector FP ((char *, int));
void		CAP_Station FP ((char *, int));
void		CAP_Raster FP ((char *, int));
void		CAP_LineContour FP ((char *, int));
static void	CAP_Contour FP ((char *, contour_type, char *, float *, 
			float *, char *, int *));
static DataChunk *CAP_ImageGrid FP ((char *, ZebTime *, PlatformId, char *, 
			int *, int *, float *, float *, float *, float *, 
			float *, int *));
void		CAP_RasterSideAnnot FP ((char *, char *, int, int, int));
void		CAP_StaPltSideAnnot FP ((char *, char *, int, int, int));
static bool 	CAP_VecParams FP ((char *, char *, char *, char *, float *,
			char *, int *, float *, XColor *));
static StInfo	*CAP_StationInfo FP ((DataChunk *, Location *, int));
static void 	CAP_SpFilter FP ((float *, float *, float *, StInfo *, int,
			int));




void
CAP_Init (t)
time *t;
/*
 * CAP Plot initialization.
 */
{
	lw_OvInit ("COMPONENT      PLATFORM   FIELD       TIME\n");
}




void
CAP_FContour (c, update)
char	*c;
bool	update;
/*
 * Filled contour CAP plot for the given component
 */
{
	float	center, step;
	char	fname[20], ctable[40], data[100];
	int shift;
/*
 * Use the common CAP contouring routine to do a filled contour plot
 */
	CAP_Contour (c, FilledContour, fname, &center, &step, ctable, &shift);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	An_TopAnnot (px_FldDesc (c, fname), Tadefclr.pixel);
	An_TopAnnot (shift ? " filled contour (SHIFTED)." : " filled contour.",
		Tadefclr.pixel);
/*
 * Side annotation (color bar)
 */
	if (Sashow)
	{
		sprintf (data, "%s %s %f %f", fname, ctable, center, step); 
		An_AddAnnotProc (An_ColorBar, c, data, strlen (data),
			75, TRUE, FALSE);
	}
}




void
CAP_LineContour (c, update)
char	*c;
bool	update;
/*
 * Line contour CAP plot for the given component
 */
{
	float	center, step;
	char	fname[20], data[100], ctable[40];
	bool	tacmatch = FALSE;
	int	shift;
/* 
 * Use the common CAP contouring routine to do a color line contour plot
 */
	CAP_Contour (c, LineContour, fname, &center, &step, ctable, &shift);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	if (pda_Search (Pd, c, "ta-color-match", NULL,
			(char *) &tacmatch, SYMT_BOOL) && tacmatch &&Monocolor)
		An_TopAnnot (px_FldDesc (c, fname), Ctclr.pixel);
	else 
		An_TopAnnot (px_FldDesc (c, fname), Tadefclr.pixel);
	An_TopAnnot (shift ? " contour (SHIFTED)." : " contour.",
		Tadefclr.pixel);
/*
 * Side annotation
 */
	if (Sashow)
	{
		if (! Monocolor)
		{
			sprintf (data, "%s %s %f %f", fname, ctable, 
				center, step); 
			An_AddAnnotProc (An_ColorNumber, c, data, strlen (data),
				75, TRUE, FALSE);
		}
	}
}




void
CAP_Contour (c, type, fname, center, step, ctable, shifted)
char	*c, *fname, *ctable;
contour_type	type;
float	*center, *step;
int *shifted;
/*
 * Execute a CAP contour plot, based on the given plot
 * description, specified component, and contour type.
 * Return the field name, contour center, and step from the plot
 * description.
 */
{
	char	platform[40], ctcolor[40], param[50];
	int	xdim, ydim;
	float	*rgrid, *grid, x0, x1, y0, y1, alt;
	int	pix_x0, pix_x1, pix_y0, pix_y1, linewidth;
	bool	labelflag, dolabels, ok;
	ZebTime	zt;
	XColor	black;
	XRectangle	clip;
	DataChunk	*dc;
	int	len;
	RGrid	rg;
	Location 	loc;
	float	badvalue;
/*
 * Get necessary parameters from the plot description
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fname, SYMT_STRING);
	sprintf (param, "%s-center", fname);
	ok &= pda_ReqSearch (Pd, c, param, "contour", (char *) center, 
		SYMT_FLOAT);
	sprintf (param, "%s-step", fname);
	ok &= pda_ReqSearch (Pd, c, param, "contour", (char *) step, 
		SYMT_FLOAT);
	Monocolor = FALSE;
	pda_Search(Pd, c, "color-mono", "contour", (char *) &Monocolor,
		SYMT_BOOL);
	if(Monocolor)
	{
		ok &= pda_Search(Pd, c, "color", "contour", ctcolor,
			SYMT_STRING);
		if(! ct_GetColorByName(ctcolor, &Ctclr))
		{
			msg_ELog(EF_PROBLEM, "Can't get contour color '%s'.",
				ctcolor);
			strcpy(ctcolor,"white");
			ct_GetColorByName(ctcolor, &Ctclr);
		}
	}
	else ok &= pda_ReqSearch (Pd, c, "color-table", "contour", ctable, 
		SYMT_STRING);
	labelflag = TRUE;
	pda_Search(Pd, c, "label-blanking", "contour", (char *) &labelflag,
		SYMT_BOOL);
	dt_SetBlankLabel(labelflag);

	if (! ok)
		return;
/* 
 * Get annotation information
 */
	if(! pda_Search(Pd, c, "sa-scale", NULL, (char *) &Sascale,SYMT_FLOAT))
		Sascale = 0.02;
	if(! pda_Search(Pd, c, "ct-limit", NULL, (char *) &Ctlimit, SYMT_INT))
		Ctlimit = 1;
	Sashow = TRUE;
	pda_Search(Pd, c, "sa-show", NULL, (char *) &Sashow, SYMT_BOOL);
/*
 * Special stuff for line contours
 */
	if (! pda_Search (Pd, c, "do-labels", "contour", (char *) &dolabels,
		SYMT_BOOL))
		dolabels = TRUE;

	if (! pda_Search (Pd, c, "line-width", "contour", (char *) &linewidth,
		SYMT_INT))
		linewidth = 0;
/*
 * Grab the color table
 */
	if(! Monocolor)
		ct_LoadTable (ctable, &Colors, &Ncolors);
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	alt = Alt;
	/* msg_ELog (EF_INFO, "Get grid at %.2f km", alt); */
	zt = PlotTime;
	if(! (dc = ga_GetGrid (&zt, c, platform, fname, &xdim, &ydim, &x0, &y0,
			&x1, &y1, &alt, shifted)))
		return;
	rgrid = dc_RGGetGrid (dc, 0, F_Lookup (fname), &loc, &rg, &len);

	if (Comp_index == AltControlComp)
		Alt = alt;
/*
 * Get the badvalue flag.
 */
	badvalue = dc_GetBadval (dc);
/*
 * Kludge: rotate the grid into the right ordering.
 */
	grid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, grid, xdim, ydim);
	dc_DestroyDC (dc);
/*
 * Convert the grid limits to pixel values
 */
	pix_x0 = XPIX (x0);	pix_x1 = XPIX (x1);
	pix_y0 = YPIX (y0);	pix_y1 = YPIX (y1);
/*
 * Clip rectangle
 */
	clip.x = F_X0 * GWWidth (Graphics);
	clip.y = (1.0 - F_Y1) * USABLE_HEIGHT;
	clip.width = (F_X1 - F_X0) * GWWidth (Graphics);
	clip.height = (F_Y1 - F_Y0) * USABLE_HEIGHT;
/*
 * Draw the contours
 */
	ct_GetColorByName ("black", &black);

	switch (type)
	{
	    case FilledContour:
		FC_Init (Colors, Ncolors, Ncolors / 2, black, clip, TRUE, 
			badvalue);
		FillContour (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1, *center, *step);
		break;
	    case LineContour:
		if(! Monocolor)
			CO_Init (Colors, Ncolors, Ncolors / 2, black, clip, 
				TRUE, badvalue);
		else CO_InitMono (Ctclr, clip, TRUE, badvalue);
		Contour (Graphics, GWFrame (Graphics), grid, xdim, ydim,
			pix_x0, pix_y0, pix_x1, pix_y1, *center, *step, 
			dolabels, linewidth);
		break;
	    default:
		msg_ELog (EF_PROBLEM, "BUG: bad contour plot type %d", type);
	}
/*
 * Free the data array
 */
	lw_TimeStatus (c, platform, &zt);
	free (grid);
}




void
CAP_Station (c, update)
char *c;
bool update;
/*
 * Deal with a station plot.
 */
{
	char	uname[20], vname[20], cname[30], platform[40], annot[120];
	char	quadrants[120], *quads[6], quadclr[30], string[10], data[100];
	char	*strchr ();
	static const int offset_x[4] = { -15, -15, 15, 15 };
	static const int offset_y[4] = { -10, 10, -10, 10 };
	PlatformId pid, *platforms;
	float vscale, unitlen, badvalue, *ugrid, *vgrid, *qgrid[4];
	int linewidth, numquads = 0, shifted, npts, i, j, pix_x0, pix_y0;
	bool	filter = FALSE, tacmatch, stationname = FALSE;
	ZebTime zt;
	XColor	color, qcolor;
	FieldId	fields[6];
	DataChunk	*dc;
	Location	*locations;
	StInfo		*sinfo;
/*
 * Get necessary parameters from the plot description
 */
	if (! CAP_VecParams (c, platform, uname, vname, &vscale, cname, 
			&linewidth, &unitlen, &color))
		return;
/*
 * Get the platform ID.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", platform);
		return;
	}
/*
 * See when there is data available.
 */
	if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, &zt))
	{
		msg_ELog(EF_INFO,"No data available at all for '%s'",
			platform);
		return;
	}
/*
 * Look up quadrant info.
 */
	if (pd_Retrieve (Pd, c, "quadrants", quadrants, SYMT_STRING))
	{
		if (!pd_Retrieve (Pd, c, "quad-color", quadclr, SYMT_STRING))
		{
			strcpy (quadclr, cname);
			qcolor = color;
		}
		else if(! ct_GetColorByName (quadclr, &qcolor))
		{
			strcpy (quadclr, cname);
			qcolor = color;
		}
		numquads = CommaParse (quadrants, quads);
		if (numquads > 4)
			numquads = 4;
	}
/*
 * Kludge of sorts...see if any of the quadrants is "station".  If so,
 * remove it from the of the list and hide it so that we don't try to
 * get it from the data store....
 */
	for (i = 0; i < numquads; i++)
		if (! strcmp (quads[i], "station"))
		{
			numquads--;
			quads[i] = quads[numquads];
			stationname = TRUE;
		}	
/*
 * Create the field list for our data fetch.
 */
	fields[0] = F_Lookup (uname);
	fields[1] = F_Lookup (vname);
	for (i = 0; i < numquads; i++)
		fields[i + 2] = F_Lookup (quads[i]);
/*
 * Get the data.
 */
	if (! (dc = ds_Fetch (pid, DCC_IRGrid, &zt, &zt, fields, 2 + numquads,
			NULL, 0)))
	{
		msg_ELog (EF_INFO, "Get failed on '%s'", platform);
		return;
	}
	shifted = ApplySpatialOffset (dc, c, &PlotTime);
/*
 * Get some info out of the data chunk.
 */	
	npts = dc_IRGetNPlatform (dc);
	platforms = (PlatformId *) malloc (npts * sizeof (PlatformId));
	locations = (Location *) malloc (npts * sizeof (Location));
	dc_IRGetPlatforms (dc, platforms, locations);
	badvalue = dc_GetBadval (dc);
/*
 * Convert locations and such.
 */
	sinfo = CAP_StationInfo (dc, locations, npts);
/*
 * Get the u and v components, and possibly quadrants.
 */
	ugrid = dc_IRGetGrid (dc, 0, fields[0]);
	vgrid = dc_IRGetGrid (dc, 0, fields[1]);
	for (i = 0; i < numquads; i++)
		qgrid[i] = dc_IRGetGrid (dc, 0, fields[i + 2]);
/*
 * Apply spatial thinning if they want it.
 */
	if (pda_Search (Pd, c, "spatial-filter", platform, CPTR (filter),
			SYMT_BOOL) && filter)
	{
		int res = 50;
		pda_Search (Pd, c, "filter-resolution", platform, CPTR (res),
				SYMT_INT);
		CAP_SpFilter (ugrid, vgrid, &badvalue, sinfo, npts, res);
	}
/*
 * Graphics context stuff.
 */
	XSetForeground (XtDisplay (Graphics), Gcontext, color.pixel);
/*
 * Draw the vectors.
 */
	for (i = 0; i < npts; i++)
	{
	/*
	 * Look at our station info and see if we want to plot this
	 * one at all.
	 */
		if (sinfo[i].si_excl)
			continue;
		pix_x0 = sinfo[i].si_x;
		pix_y0 = sinfo[i].si_y;
	/*
	 * Plot the arrow.
	 */
		ov_PositionIcon ("pam-loc", pix_x0, pix_y0, color.pixel);
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 
			linewidth, LineSolid, CapButt, JoinMiter);
		if ((ugrid[i] != badvalue) && (vgrid[i] != badvalue))
			draw_vector (XtDisplay (Graphics),
				GWFrame (Graphics), Gcontext, pix_x0, pix_y0, 
				ugrid[i], vgrid[i], unitlen);
		XSetForeground (XtDisplay (Graphics), Gcontext, qcolor.pixel);
	/*
	 * Do quadrants if necessary.
	 */
		for (j = 0; j < numquads; j++)
		{
			if (qgrid[j][i] == badvalue)
				continue;
			sprintf(string, "%.1f", qgrid[j][i]); 
			DrawText (Graphics, GWFrame (Graphics), Gcontext,
				pix_x0 + offset_x[j], pix_y0 + offset_y[j],
				string, 0.0, Sascale, JustifyCenter,
				JustifyCenter);
		}
	/*
	 * Station name too if that's what they wanted.
	 */
	 	if (stationname)
		{
			char *slash = ds_PlatformName (platforms[i]);
			if (strchr (slash, '/'))
				slash = strchr (slash, '/') + 1;
			DrawText (Graphics, GWFrame (Graphics), Gcontext,
				pix_x0 + offset_x[3], pix_y0 + offset_y[3],
				slash, 0.0, Sascale, JustifyCenter,
				JustifyCenter);
		}
	/*
	 * Tweak the foreground back.
	 */
		XSetForeground (XtDisplay (Graphics), Gcontext,
				color.pixel);
	}
/*
 * Free the data.
 */
	free (sinfo);
	free (platforms);
	free (locations);
	dc_DestroyDC (dc);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	if (pda_Search (Pd, c, "ta-color-match", NULL, (char *) &tacmatch,
			SYMT_BOOL) && tacmatch)
		An_TopAnnot ("Vector winds", color.pixel);
	else
		An_TopAnnot ("Vector winds", Tadefclr.pixel);
	sprintf (annot, " plot (%s)", platform);
	An_TopAnnot (annot, Tadefclr.pixel);
	if (shifted)
		An_TopAnnot (" (SHIFTED)", Tadefclr.pixel);
	An_TopAnnot (".", Tadefclr.pixel);
/*
 * Side annotation.
 */
	if (numquads > 0)
	{
		sprintf (data, "%s %s %s %f %d ", "10m/sec", 
			cname, quadclr, unitlen, numquads);
		for (i = 0; i < 4; i++)
			if (i < numquads)
			{
				strcat (data, quads[i]);
				strcat (data, " ");
			}
			else strcat (data, "null ");
		An_AddAnnotProc (CAP_StaPltSideAnnot, c, data, 
			strlen (data), 90, FALSE, FALSE);
	}
	else
	{
		sprintf (data, "%s %s %s %f %d %s %s %s %s", "10m/sec", 
			cname, "null", unitlen, numquads, "null",
			"null", "null", "null");
		An_AddAnnotProc (CAP_StaPltSideAnnot, c, data, 
			strlen (data), 40, FALSE, FALSE);
	}
/*
 * Finish up the time widget and we are done.
 */
	lw_TimeStatus (c, platform, &zt);
}





static StInfo *
CAP_StationInfo (dc, locs, nsta)
DataChunk *dc;
Location *locs;
int nsta;
/*
 * Pull together basic info on these stations.
 */
{
	int sta;
	float x0, y0;
	StInfo *sinfo;
/*
 * Allocate the info array.
 */
	sinfo = (StInfo *) malloc (nsta * sizeof (StInfo));
/*
 * Fill it in.
 */
	for (sta = 0; sta < nsta; sta++)
	{
	/*
	 * Convert the location of this station into pixel space.
	 */
		cvt_ToXY (locs[sta].l_lat, locs[sta].l_lon, &x0, &y0);
		if (x0 < Xlo || x0 > Xhi || y0 < Ylo || y0 > Yhi)
			sinfo[sta].si_excl = TRUE;
		else
		{
			sinfo[sta].si_x = XPIX (x0);
			sinfo[sta].si_y = YPIX (y0);
			sinfo[sta].si_excl = FALSE;
		}
	}
	return (sinfo);
}




static
# ifdef __GNUC__
inline
# endif
int
CAP_SDist (sinfo, xp, yp, max)
StInfo *sinfo;
int xp, yp, max;
/*
 * Return the distance from this station to the grid point, or some quick
 * approximation thereof.  (This little routine can get run tens of thousands
 * of times for a single plot, so it needs to be a bit on the speedy side).
 */
{
	int dist;
/*
 * The test on mark is controversial; by testing this way it could select
 * a station that is not the very closest to the grid point.
 */
	if (sinfo->si_excl || sinfo->si_mark)
		return (99999);
	dist = ((xp > sinfo->si_x) ? (xp - sinfo->si_x) : (sinfo->si_x - xp)) +
	       ((yp > sinfo->si_y) ? (yp - sinfo->si_y) : (sinfo->si_y - yp));
	return (dist > max ? 99999 : dist);
}




static void
CAP_SpFilter (ugrid, vgrid, badval, sinfo, nsta, res)
float *ugrid, *vgrid, *badval;
int nsta;
StInfo *sinfo;
/*
 * Apply spatial filtering (thinning) to this data chunk.
 */
{
	float bv = *badval;
	int sta, xp, yp, max = 3*res/2;
/*
 * Pixel limits.
 */
	int xmin = F_X0*GWWidth (Graphics);
	int xmax = F_X1*GWWidth (Graphics);
	int ymin = (1.0 - F_Y1)*GWHeight (Graphics);
	int ymax = (1.0 - F_Y0)*GWHeight (Graphics);
/*
 * Pass once through the station array, reset all the marks, and exclude
 * everything that lacks good wind data.
 */
	for (sta = 0; sta < nsta; sta++)
	{
		sinfo[sta].si_mark = FALSE;
		if (ugrid[sta] == bv || vgrid[sta] == bv)
			sinfo[sta].si_excl = TRUE;
	}
/*
 * Now we need to pass through the grid points and find the closest station
 * to each.
 *
 * (A better algorithm, for someday: pass through each station, find closest
 *  grid point, assign to that point if is closest station.  Should be faster,
 *  but this seems to work, for the moment).
 */
	for (xp = xmin; xp <= xmax; xp += res)
		for (yp = ymin; yp <= ymax; yp += res)
		{
			int cdist = 9999, closest = -1;
			for (sta = 0; sta < nsta; sta++)
			{
				int dist = CAP_SDist (sinfo + sta, xp, yp,res);
				if (dist < cdist)
				{
					cdist = dist;
					closest = sta;
				}
			}
			if (closest >= 0)
				sinfo[closest].si_mark = TRUE;
		}
/*
 * Now one last pass through to exclude everything which was not marked.
 */
	for (sta = 0; sta < nsta; sta++)
	{
		if (! sinfo[sta].si_mark)
			sinfo[sta].si_excl = TRUE;
	}
}





void
CAP_Vector (c, update)
char	*c;
bool	update;
/*
 * Execute a CAP vector plot, based on the given plot
 * description, specified component, and plot time
 */
{
	char	uname[20], vname[20], cname[30], platform[40], annot[120];
	char	data[100];
	float	*rgrid, *ugrid, *vgrid, unitlen;
	float	vscale, x0, x1, y0, y1, alt, badvalue;
	int	pix_x0, pix_x1, pix_y0, pix_y1, xdim, ydim;
	int	linewidth, len, degrade, shifted, ok;
	bool	tacmatch = FALSE, grid = FALSE;
	XColor	color;
	ZebTime zt;
	PlatformId pid;
	DataChunk	*dc;
	Location	loc, *locations;
	RGrid		rg;
/*
 * Check to see if they have set "grid" to FALSE, in which case they should
 * really be using the station representation.
 */
	if (pda_Search (Pd, c, "grid", NULL, (char *) &grid, SYMT_BOOL) &&
			! grid)
	{
		static bool griped = FALSE;
		if (! griped++)
			msg_ELog (EF_INFO,"Converting vector to station rep.");
		CAP_Station (c, update);
		return;
	}
/*
 * Get necessary parameters from the plot description
 */
	if (! CAP_VecParams (c, platform, uname, vname, &vscale, cname, 
			&linewidth, &unitlen, &color))
		return;
/*
 * See if they want to degrade the grid.
 */
	if (! pda_Search (Pd, c, "degrade", platform, (char *) &degrade,
			SYMT_INT))
		degrade = 0;
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	alt = Alt;
	zt = PlotTime;
/*
 * Get U component.
 */
	if (! (dc = ga_GetGrid (&zt, c, platform, uname, &xdim, &ydim, 
			&x0, &y0, &x1, &y1, &alt, &shifted)))
		return;
	rgrid = dc_RGGetGrid (dc, 0, F_Lookup (uname), &loc, &rg, &len);
	if (Comp_index == AltControlComp)
		Alt = alt;
	ugrid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, ugrid, xdim, ydim);
	dc_DestroyDC (dc);
/*
 * Get v component.
 */
	zt = PlotTime;
	if (! (dc = ga_GetGrid (&zt, c, platform, vname, &xdim, &ydim, 
			&x0, &y0, &x1, &y1, &alt, &shifted)))
		return;
	rgrid = dc_RGGetGrid (dc, 0, F_Lookup (vname), &loc,&rg, &len);
	vgrid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, vgrid, xdim, ydim);
/*
 * Convert the grid limits to pixel values
 */
	pix_x0 = XPIX (x0);	pix_x1 = XPIX (x1);
	pix_y0 = YPIX (y0);	pix_y1 = YPIX (y1);
/*
 * Draw the vectors
 */
	badvalue = dc_GetBadval (dc);
	VectorGrid (Graphics, GWFrame (Graphics), Gcontext, ugrid, 
		vgrid, xdim, ydim, pix_x0, pix_y0, pix_x1, pix_y1, 
		vscale, badvalue, color, degrade);
/*
 * Free the data arrays
 */
	free (ugrid);
	free (vgrid);
	dc_DestroyDC (dc);
/*
 * Annotation time.
 */
# ifdef notdef
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, LineSolid, 
		CapButt, JoinMiter);
# endif
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	if (pda_Search (Pd, c, "ta-color-match", NULL, (char *) &tacmatch,
			SYMT_BOOL) && tacmatch)
		An_TopAnnot ("Vector winds", color.pixel);
	else
		An_TopAnnot ("Vector winds", Tadefclr.pixel);
	sprintf (annot, " plot (%s)", platform);
	An_TopAnnot (annot, Tadefclr.pixel);
	if (shifted)
		An_TopAnnot (" (SHIFTED)", Tadefclr.pixel);
	An_TopAnnot (".", Tadefclr.pixel);
/*
 * Side annotation (scale vectors)
 */
	sprintf (data, "%s %s %f %f %f", "10m/sec", cname,
		10.0, 0.0, unitlen); 
	An_AddAnnotProc (An_ColorVector, c, data, strlen (data),
		40, FALSE, FALSE);
	lw_TimeStatus (c, platform, &zt);
}





static bool
CAP_VecParams (c, platform, uname, vname, vscale, cname, linewidth, unitlen,
	color)
char *c, *platform, *uname, *vname, *cname;
float *vscale, *unitlen;
int *linewidth;
XColor *color;
/*
 * Get common parameters for vector plots.
 */
{
	bool ok;
/*
 * Basics.
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "u-field", NULL, uname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "v-field", NULL, vname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "arrow-scale", NULL, (char *) vscale,
		SYMT_FLOAT);
	*unitlen = USABLE_HEIGHT * *vscale;
	if (! ok)
		return;
/*
 * Get annotation information from the plot description
 */
	if(! pda_Search(Pd, c, "sa-scale", NULL, (char *) &Sascale,SYMT_FLOAT))
		Sascale = 0.02;
/*
 * Figure out an arrow color.
 */
	if (! pda_Search (Pd, c, "arrow-color", platform, cname, SYMT_STRING)
		&& ! pda_Search (Pd, c, "color", platform, cname, SYMT_STRING))
		strcpy (cname, "white");
/*
 * Arrow line width.
 */
	if (! pda_Search (Pd, c, "line-width", "vector", (char *) linewidth,
			SYMT_INT))
		*linewidth = 0;
	if (*linewidth == 1) *linewidth = 0;
/*
 * X Stuff.
 */
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, *linewidth,
		LineSolid, CapButt, JoinMiter);
/*
 * Allocate the chosen arrow color
 */
	if (! ct_GetColorByName (cname, color))
	{
		msg_ELog (EF_PROBLEM, "Can't get arrow color '%s'!", cname);
		return (FALSE);
	}
	return (TRUE);
}






void
CAP_StaPltSideAnnot (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * Routine to do station plot side annotation.
 */
{
	char string[40], vcolor[40], qcolor[40], qname[4][40];
	float unitlen, used, scale; 
	int i, left, numquads, limit;
	XColor vc, qc;
	int	offset_x[4], offset_y[4];
/*
 * Do this to satisfy cc.
 */
	offset_x[0] = offset_y[0] = -15;
	offset_x[1] = offset_y[2] = -15;
	offset_y[1] = offset_x[2] = 15;
	offset_x[3] = offset_y[3] = 15;
/*
 * Get annotation parameters.
 */
	An_GetSideParams (comp, &scale, &limit);
/*
 * Get the data.
 */
        sscanf (data, "%s %s %s %f %d %s %s %s %s", string, vcolor, qcolor, 
		&unitlen, &numquads, qname[0], qname[1], qname[2], qname[3]);
	ct_GetColorByName (vcolor, &vc);
	ct_GetColorByName (qcolor, &qc);
/*
 * Put in the vector.
 */
	left = An_GetLeft ();
	XSetForeground (XtDisplay (Graphics), Gcontext, vc.pixel);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, begin, 
		"10 m/sec", 0.0, scale, JustifyLeft, JustifyTop);
	used = scale * (float) USABLE_HEIGHT;
	begin += used;
	space -= used;

	XSetForeground (XtDisplay (Graphics), Gcontext, vc.pixel);
	draw_vector (XtDisplay (Graphics), GWFrame (Graphics), Gcontext,
		left, begin + 5, 10.0, 0.0, unitlen);
	begin += 10;
	space -= 10;
/*
 * Put in the quadrant annotation.
 */
	XSetForeground (XtDisplay (Graphics), Gcontext, qc.pixel);
	if (numquads > 0)
	{
		XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, 
			left + 25, begin + 10, left + 25, begin + 55);
		XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, 
			left, begin + 30, left + 55, begin + 30);
	}
	for (i = 0; i < numquads; i++)
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			left + offset_x[i] + 15, begin + offset_y[i] + 25, 
			qname[i], 0.0, scale, JustifyLeft, JustifyTop);
}




void
CAP_Raster (c, update)
char	*c;
bool	update;
/*
 * Execute a CAP raster plot, based on the given plot
 * description, specified conent, and plot time
 */
{
	char	name[20], ctname[40], platform[40], data[100], hcolor[40];
	char	param[50], outrange[40];
	int	xdim, ydim;
	int	nsteps;
	bool	ok, highlight, fastloop, newrp;
	float	*grid, x0, x1, y0, y1, alt;
	float	min, max, center, step, hvalue, hrange;
	int	pix_x0, pix_x1, pix_y0, pix_y1, image, shifted;
	XRectangle	clip;
	XColor	xc, xoutr;
	ZebTime	zt;
	PlatformId pid;
	DataOrganization org;
	ScaleInfo scale;
	DataChunk *dc;
	Location loc;
	int	len;
	RGrid	rg;
/*
 * Get necessary parameters from the plot description
 */
	strcpy (name, "none");
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, name, SYMT_STRING);
	sprintf (param, "%s-center", name);
	ok &= pda_ReqSearch (Pd, c, param, "raster", CPTR (center), 
		SYMT_FLOAT);
	sprintf (param, "%s-step", name);
	ok &= pda_ReqSearch (Pd, c, param, "raster", CPTR (step), 
		SYMT_FLOAT);
	sprintf (param, "%s-nsteps", name);
	ok &= pda_ReqSearch (Pd, c, param, "raster", CPTR (nsteps), 
		SYMT_INT);
	ok &= pda_ReqSearch (Pd, c, "color-table", "raster", ctname, 
		SYMT_STRING);

	if (! ok)
		return;
/*
 * An out of range color is nice, sometimes.
 */
	if (! pda_Search (Pd, c, "out-of-range-color", platform, outrange,
			SYMT_STRING))
		strcpy (outrange, "black");
	if (! ct_GetColorByName (outrange, &xoutr))
		ct_GetColorByName ("black", &xoutr);
/*
 * Make sure the platform is other than bogus, and get its organization.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform: %s", platform);
		return;
	}
	if ((org = ds_PlatformDataOrg (pid)) == OrgImage)
		image = TRUE;
	else if (org == Org3dGrid || org == Org2dGrid || org == OrgIRGrid)
		image = FALSE;
	else
	{
		msg_ELog (EF_PROBLEM, "Can't do raster plots of %s", platform);
		return;
	}
/*
 * Get info for highlighting and area.
 */
	highlight = FALSE;
	sprintf (param, "%s-highlight-range", name);
	if (pda_Search (Pd, c, param, "raster", CPTR (hrange), SYMT_FLOAT)
		&& (hrange != 0.0))
	{
		highlight = TRUE;
		sprintf (param, "%s-highlight-color", name);
		if (! pda_Search (Pd, c, param, "raster", CPTR (hcolor), 
				SYMT_STRING))
			strcpy (hcolor, "white");
		sprintf (param, "%s-highlight", name);
		if (! pda_Search (Pd, c, param, "raster", CPTR (hvalue), 
				SYMT_FLOAT))
			hvalue = 0.0;
	}
/*
 * Get annotation information from the plot description
 */
	if(! pda_Search(Pd, c, "sa-scale", NULL, (char *) &Sascale,SYMT_FLOAT))
		Sascale = 0.02;
	if(! pda_Search(Pd, c, "ct-limit", NULL, (char *) &Ctlimit, SYMT_INT))
		Ctlimit = 1;
/*
 * Rasterization control.
 */
	if (! pda_Search (Pd, c, "new-raster", NULL, (char *) &newrp,
		SYMT_BOOL))
		newrp = TRUE;
	if (! pda_Search (Pd, c, "fast-raster", NULL, (char *) &fastloop,
		SYMT_BOOL))
		fastloop = FALSE;
/*
 * Field number and color table
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	alt = Alt;
	zt = PlotTime;
	if (image)
	{
		if (! (dc = CAP_ImageGrid (c, &zt, pid, name, &xdim, &ydim,
 				&x0, &y0, &x1, &y1, &alt, &shifted)))
			return;
		grid = (float *) dc_ImgGetImage (dc, 0, 
			F_Lookup (name), &loc, &rg, &len, &scale);
		alt = loc.l_alt;
	}
	else
	{
		if (! (dc = ga_GetGrid(&zt, c, platform, name, &xdim, &ydim,
				&x0, &y0, &x1, &y1, &alt, &shifted)))
			return;
		grid = dc_RGGetGrid (dc, 0, F_Lookup (name), &loc, &rg, &len);
	}
	if (! grid)
	{
		msg_ELog (EF_INFO, "Unable to get grid for %s.", platform);
		return;
	}
	if (Comp_index == AltControlComp)
		Alt = alt;
/*
 * Convert the grid limits to pixel coordinates
 */
	pix_x0 = XPIX (x0);	pix_x1 = XPIX (x1);
	pix_y0 = YPIX (y0);	pix_y1 = YPIX (y1);
/*
 * Clip rectangle
 */
	clip.x = F_X0 * GWWidth (Graphics);
	clip.y = (1.0 - F_Y1) * USABLE_HEIGHT;
	clip.width = (F_X1 - F_X0) * GWWidth (Graphics);
	clip.height = (F_Y1 - F_Y0) * USABLE_HEIGHT;
/*
 * Draw the raster plot
 */
	ct_GetColorByName (hcolor, &xc);
	max = center + (nsteps/2) * step;
	min = center - (nsteps/2) * step;
	RP_Init (Colors, Ncolors, xoutr, clip, min, max, highlight, hvalue, 
		xc, hrange);
	if (image)
# ifdef SHM
		RasterImagePlot (Graphics, DrawFrame, grid, xdim,
			ydim, pix_x0, pix_y0, pix_x1, pix_y1, scale.s_Scale,
			scale.s_Offset);
# else
		RasterXIPlot (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1, fastloop);
# endif
	else if (! newrp)
		RasterPlot (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1);
	else
		RasterXIPlot (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1, fastloop);
/*
 * Free the data chunk.
 */
	dc_DestroyDC (dc);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
	lw_TimeStatus (c, platform, &zt);
/*
 * Top annotation
 */
	An_TopAnnot (px_FldDesc (c, name), Tadefclr.pixel);
	An_TopAnnot (shifted ? " plot (SHIFTED)." : " plot.", Tadefclr.pixel);
/*
 * Side annotation (color bar)
 */
	if (highlight)
		sprintf (data, "%s %s %f %f %d %d %f %s %f", name, ctname, 
	  	     center, step, nsteps, highlight, hvalue, hcolor, hrange);
	else
		sprintf (data, "%s %s %f %f %d %d %f %s %f", name, ctname, 
	  	     center, step, nsteps, highlight, 0.0, "null", 0.0);
	An_AddAnnotProc (CAP_RasterSideAnnot, c, data, strlen (data), 
		140, TRUE, FALSE);
}


void
CAP_RasterSideAnnot (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
{
	char string[40], ctable[40], color[40];
	float center, step, val, maxval, used, scale, value, range, max;
	int i, left, ncolors, bar_height, limit, nsteps, y;
	int highlight;
	XColor *colors, xc;
/*
 * Get annotation parameters.
 */
	An_GetSideParams (comp, &scale, &limit);
/*
 * Get the data.
 */
        sscanf (data,"%s %s %f %f %d %d %f %s %f", string, ctable, &center, 
		&step, &nsteps, &highlight, &value, color, &range);
        ct_LoadTable (ctable, &colors, &ncolors);
	ct_GetColorByName (color, &xc);
/*
 * Throw in the field name.
 */
	left = An_GetLeft ();
	XSetForeground (XtDisplay (Graphics), Gcontext, Tadefclr.pixel);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, 
		begin, string, 0.0, scale, JustifyLeft, 
		JustifyCenter);
	used = scale * (float) USABLE_HEIGHT;
	begin += used;
	space -= used;
/*
 * Add all the colors.
 */
	space -= scale * (float) USABLE_HEIGHT; /* save space for last num */
	bar_height = (float) space / (float) ncolors;
	if (bar_height <= 0) bar_height = 1;
	for (i = 0; i < ncolors; i++)
	{
		XSetForeground (XtDisplay (Graphics), Gcontext, 
			colors[ncolors - i - 1].pixel);
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, left, (int) (begin + i * bar_height), 10, 
			bar_height);
	}
/*
 * Do the numeric labels.
 */
	for (i = 0; i < nsteps; i++)
	{
		val = center + (nsteps/2 - i) * step;
		sprintf (string, "%.1f", val);

		XSetForeground (XtDisplay (Graphics), Gcontext,Tadefclr.pixel);
		y = (float) begin + (float) i * (float) ncolors / (float)
			(nsteps - 1.0) * (float) bar_height;
		DrawText (Graphics, GWFrame (Graphics), Gcontext, left + 15, 
			y, string, 0.0, scale, JustifyLeft, JustifyCenter);
	}
/*
 * Add the special highlight color.
 */
	if (highlight)
	{
		space = bar_height * ncolors; 
		XSetForeground (XtDisplay (Graphics), Gcontext, xc.pixel);
		bar_height = space * range / (step * (nsteps - 1.0));
		max = center + nsteps / 2 * step;
		y = (float) begin + (float) space * (max - value) / 
			(step * (nsteps - 1.0)) - bar_height / 2.0; 
		if ((y + bar_height) > (begin + space)) 
			bar_height = begin + space - y;
		else if (y < begin)
		{
			bar_height -= (begin - y);
			y = begin;
		}
		if (bar_height <= 0) bar_height = 1;
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, left, y, 10, bar_height);
	}
}






static DataChunk *
CAP_ImageGrid (c, when, pid, field, xdim, ydim, x0, y0, x1, y1, alt, shift)
char	*c, *field;
ZebTime *when;
PlatformId pid;
int	*xdim, *ydim, *shift;
float	*x0, *y0, *x1, *y1, *alt;
/*
 * Fetch an image grid from this platform.
 */
{
	ZebTime realtime, stimes[60], obstimes[2];
	RGrid rg;
	ScaleInfo sc;
	float cdiff;
	char *img;
	Location slocs[60], origin;
	int nsample, samp, ntime, len;
	bool all = FALSE;
	DataChunk *dc;
	FieldId	fid = F_Lookup (field);
/*
 * Find out when we can really get data.
 */
	if (! (ntime = ds_DataTimes (pid, when, 1, DsBefore, &realtime)))
	{
		msg_ELog (EF_INFO, "No data available at all for %s",
			ds_PlatformName (pid));
		return (0);
	}
/*
 * Unless they have specified that they want all of the heights, we need
 * to find the specific one of interest.
 */
	if (! pda_Search (Pd, c, "every-sweep", NULL, (char *) &all, SYMT_BOOL)
			|| !all || PlotMode == History)
	{
		char cattr[200], *attr = NULL;
	/*
	 * Look for a filter attribute.
	 */
		if (pda_Search (Pd, "global", "filter-attribute", 
				ds_PlatformName (pid), cattr, SYMT_STRING))
			attr = cattr;
	/*
	 * Look at the previous two observations.
	 */
		if (! (ntime = ds_GetObsTimes (pid, when, obstimes, 2, attr)))
		{
			msg_ELog (EF_PROBLEM, "Strange...no observations");
			return (0);
		}
	/*
	 * Get the samples from the first volume and see which is closest.
	 */
		realtime = obstimes[0];
		nsample = ds_GetObsSamples (pid, &realtime, stimes, slocs, 60);
		cdiff = 99.9;
		for (samp = 0; samp < nsample; samp++)
			if (ABS (*alt - slocs[samp].l_alt) < cdiff)
			{
				cdiff = ABS (*alt - slocs[samp].l_alt);
				realtime = stimes[samp];
			}
	/*
	 * If we don't come within a degree, drop back to the previous
	 * one and try one more time.
	 */
		if (cdiff > 1.0 && ntime > 1)
		{
			nsample = ds_GetObsSamples (pid, obstimes + 1, stimes,
					slocs, 60);
			for (samp = 0; samp < nsample; samp++)
				if (ABS (*alt - slocs[samp].l_alt) < cdiff)
				{
				msg_ELog (EF_DEBUG, "Drop back case");
					cdiff = ABS (*alt - slocs[samp].l_alt);
					realtime = stimes[samp];
				}
		}
	}
/*
 * Snarf it.
 */
	if (! (dc = ds_Fetch (pid, DCC_Image, &realtime, &realtime, &fid, 1,
		NULL, 0)))
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s/%s.", 
			ds_PlatformName (pid), field);
		return (0);
	}
	*shift =  ApplySpatialOffset (dc, c, when);
/*
 * Get some info out of the data chunk.
 */
	img = dc_ImgGetImage (dc, 0, fid, &origin, &rg, &len, &sc);
/*
 * Return the various pieces of info.
 */
	*xdim = rg.rg_nX;
	*ydim = rg.rg_nY;
	cvt_ToXY (origin.l_lat, origin.l_lon, x0, y0);
	*x1 = *x0 + (rg.rg_nX - 1) * rg.rg_Xspacing;
	*y1 = *y0 + (rg.rg_nY - 1) * rg.rg_Yspacing;
/*
 * Free the data object and return the data chunk.
 */
	*when = realtime;
	return (dc);
}





void
CAP_Finish (alt)
float alt;
/*
 * Finish out CAP plots.
 */
{
	char string[80];
	bool deg = FALSE;
/*
 * Kludge for fake CAP's where altitudes are really radar elevations.
 */
	if (pd_Retrieve (Pd, "global", "radar-space", (char *) &deg, SYMT_BOOL)
			&& deg)
		sprintf (string, "El   %.1f\260", Alt);
	else
		sprintf (string, "Alt: %dm", (int) (Alt*1000.0));

	XSetForeground (XtDisplay (Graphics), Gcontext,Tadefclr.pixel);
	DrawText (Graphics, GWFrame (Graphics), Gcontext,
		GWWidth (Graphics) - 10, GWHeight (Graphics) - 10, 
		string, 0.0, TOPANNOTHEIGHT, JustifyRight, JustifyBottom);
}



# endif  /* C_PT_CAP */
