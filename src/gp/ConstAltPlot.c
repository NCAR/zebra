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

# include <math.h>
# include <ctype.h>
# include <stdlib.h>
# include <X11/Intrinsic.h>


# ifdef TIMING
# include <sys/time.h>
# include <sys/resource.h>
# endif


# include <defs.h>
# include <draw.h>
# include <pd.h>
# include <message.h>
# include <GraphicsW.h>
# include <DataStore.h>
# include <DataChunk.h>

# include "GC.h"
# include "GraphProc.h"
# include "DrawText.h"
# include "PixelCoord.h"
# include "EventQueue.h"
# include "Contour.h"
# include "RasterImage.h"
# if C_CAP_POLAR
# include "PolarPlot.h"
# endif


# undef quad 	/* Sun cc header file definition conflicts with variables */

MAKE_RCSID ("$Id: ConstAltPlot.c,v 2.92 2005-08-17 17:26:32 burghart Exp $")


/*
 * Color stuff
 */
static XColor	*Colors, Ctclr;
static int	Ncolors;
static zbool 	Monocolor;

/*
 * Our plot altitude
 */
static float	Alt;

/*
 * Other annotation information.
 */
static float	Sascale;

/*
 * Non-modular kludgery to make things work for now, until something better
 * gets implemented.  All of these are defined in PlotExec.c.
 */
extern int Comp_index;
extern Pixel White;

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
	zbool	si_excl;	/* Excluded?			*/
	zbool	si_mark;	/* Used in filtering operations */
} StInfo;


/*
 * Quadrant format info
 */
typedef char	QFormat[16];


/*----------------------------------------------------------------
 * Prototypes
 */
void		CAP_FContour FP ((char *, int));
void		CAP_Vector FP ((char *, int));
void		CAP_Station FP ((char *, int));
void		CAP_Raster FP ((char *, int));
# if C_CAP_POLAR
void		CAP_Polar FP ((char *, int));
static int	CAP_PolarParams FP ((char *c, char *plat, PlatformId *pid,
				     FieldId *fids, int *nfids, float *tvalue,
				     int *ttest, float *center, float *step,
				     int *nstep, char *ctable, 
				     XColor *outrange, int *transparent,
				     int *project, int *tfill, int *legend));
# endif
void		CAP_LineContour FP ((char *, int));
static int	CAP_Contour FP ((char *, contour_type, char **, char **, 
				 float *, float *, char **, int *));
static DataChunk *CAP_ImageGrid FP ((char *, ZebTime *, PlatformId, FieldId, 
			int *, int *, float *, float *, float *, float *, 
			float *, int *));
static int	CAP_AutoScale FP ((char *c, char *qual, char *platform,
				   char *fname, float *center, float *step));
void		CAP_RasterSideAnnot FP ((char *, char *, int, int, int));
static void	CAP_LegendAnnot FP ((char *, char *, int, int, int));
void		CAP_StaPltSideAnnot FP ((char *, char *, int, int, int));
static zbool	CAP_VecParams FP ((char *c, char *platform, float *vscale,
		   char *cname, int *linewidth, float *unitlen, 
		   XColor *color, zbool *do_vectors));
static StInfo	*CAP_StationInfo FP ((DataChunk *, Location *, int));
static void 	CAP_SpFilter FP ((float *, float *, float *, StInfo *, int,
				  int));
static void	CAP_AddStatusLine FP ((char *, char *, char *, double, 
				       AltUnitType, ZebTime *));
static void	CAP_StDoScalar FP ((char *c, DataChunk *dc, char *platform,
				    FieldId *fields, int nfield,
				    FieldId quadfields[4], QFormat qformats[4],
				    XColor *color, XColor *qcolor, ZebTime *zt,
				    char *sticon, int linewidth, 
				    double unitlen, int do_vectors, 
				    zbool quadstn[4], WindInfo *wi));
static void	CAP_StDoIRGrid FP ((char *c, DataChunk *dc, char *platform,
				    FieldId *fields, int nfield,
				    FieldId quadfields[4], QFormat qformats[4],
				    XColor *color, XColor *qcolor, ZebTime *zt,
				    char *sticon, int linewidth, 
				    double unitlen, int do_vectors, 
				    zbool quadstn[4], WindInfo *wi, float *));
static DataChunk *CAP_StGetData FP ((char *c, PlatformId plat, FieldId *fields,
				     int nfield, int *shifted));
static void	CAP_StPlotVector FP ((char *c, int pt, ZebTime *zt, int x0, 
				      int y0, PlatformId plat, char *sticon,
				      XColor *color, XColor *qcolor,
				      int linewidth, float *ugrid, 
				      float *vgrid, double badvalue, 
				      double unitlen, float *qgrid[4],
				      QFormat qformats[4], int do_vectors, 
				      zbool quadstn[4]));



void
CAP_Init (t)
ZebTime *t;
/*
 * CAP Plot initialization.
 */
{
	char	altlabel[40], prjlabel[80];
	float	annotscale;
	XColor tacolor;
/*
 * Header line for the overlay times widget.
 */
	ot_SetString ("COMPONENT      PLATFORM   FIELD");
	ot_Append ("      ALTITUDE       TIME\n");
/*
 * Grab plot altitude from the PD
 */
	Alt = -999.0;
	pd_Retrieve (Pd, "global", "altitude", (char *) &Alt, SYMT_FLOAT);
/*
 * Get the info we need to do annotation.
 */
	annotscale = TOPANNOTHEIGHT;
	pd_Retrieve (Pd, "global", "ta-scale", (char *) &annotscale, 
		     SYMT_FLOAT);
	An_GetTopParams (&tacolor, 0);
	XSetForeground (XtDisplay (Graphics), Gcontext, tacolor.pixel);
/*
 * Throw in the altitude label.
 */
	sprintf (altlabel, " ");
	pd_Retrieve (Pd, "global", "altitude-label", altlabel, SYMT_STRING);
	DrawText (Graphics, GWFrame (Graphics), Gcontext,
		GWWidth (Graphics) - 10, GWHeight (Graphics) - 10, 
		altlabel, 0.0, annotscale, JustifyRight, JustifyBottom);
# ifdef MAP_PROJECTIONS
/*
 * Tell them about the projection in use.  I require "projection" because
 * it doesn't make much sense to activate the annotation without it.  This
 * is maybe the first such explicit require, and creates a dependency on
 * the libraries from within the code.  I hope that is the right way of
 * going about it.
 */
	sprintf (prjlabel, "%s projection: ", prj_GetProjName ());
	An_DoTopAnnot (prjlabel, tacolor.pixel, "global", "proj");
	Require ("projection");
# endif
}



void
CAP_Parameters (char *c)
{
    /* 
     * This is a parameter overload, since the only place the sa-scale
     * value is used in this module is for the station quadrant text, which
     * is not side annotation.  Perhaps this should be moved there some day.
     */
	Sascale = 0.02;
	pda_Search (Pd, c, "sa-scale", NULL, (char *) &Sascale, SYMT_FLOAT);
}




void
CAP_FContour (c, update)
char	*c;
zbool	update;
/*
 * Filled contour CAP plot for the given component
 */
{
	float	center, step, scale;
	char	*plat, *fname, *ctable, string[128], style[40];
	int shift, lim, lheight, space;
/*
 * Use the common CAP contouring routine to do a filled contour plot
 */
	CAP_Contour (c, FilledContour, &plat, &fname, &center, &step, &ctable,
		     &shift);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	sprintf (string, "%s %s filled contour%s.  ", plat, px_FldDesc (fname),
		 shift ? " (SHIFTED)" : "");
	string[0] = toupper (string[0]);

	An_TopAnnot (string);
/*
 * If there's no side annotation we can go have a beer now.
 */
	if (! An_SaShow (c, NULL))
		return;
/*
 * Sigh, no beer for the weary.  Figure out how much space we really need,
 * and format up a string for the annot proc.
 */
	An_GetSideParams (c, &scale, &lim);
	lheight = DT_ApproxHeight (Graphics, scale, 1);
	space = (Ncolors + 1)*lheight;
	sprintf (string, "%s|%s|%f|%f|%d", px_FldDesc (fname), ctable, center, 
		 step, Ncolors);
/*
 * Now we see if we're really doing a legend, then head out to the
 * right place.
 */
	if (pda_Search (Pd, c, "side-annot-style", fname, style, SYMT_STRING)
			&& ! strcmp (style, "legend"))
		An_AddAnnotProc (CAP_LegendAnnot, c, string, strlen (string),
				space, FALSE, FALSE);
	else
		An_AddAnnotProc (An_ColorBar, c, string, strlen (string),
				space, TRUE, FALSE);
/*
 * NOW we can go have that beer.
 */
}




void
CAP_LineContour (c, update)
char	*c;
zbool	update;
/*
 * Line contour CAP plot for the given component
 */
{
	float	center, step;
	char	*plat, *fname, *ctable, string[100];
	zbool	tacmatch = FALSE;
	int	shift;
/* 
 * Use the common CAP contouring routine to do a color line contour plot
 */
	CAP_Contour (c, LineContour, &plat, &fname, &center, &step, &ctable, 
		     &shift);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	sprintf (string, "%s ", plat);
	string[0] = toupper (string[0]);

	An_TopAnnot (string);
	if (Monocolor)
	    An_TopAnnotMatch (px_FldDesc (fname), Ctclr.pixel, c, 0);
	else
	    An_TopAnnot (px_FldDesc (fname));
	An_TopAnnot (shift ? " contour (SHIFTED).  " : " contour.  ");
/*
 * Side annotation
 */
	if (An_SaShow (c, NULL) && ! Monocolor)
	{
		float scale;
		int lim, lheight, space;
		char style[40];
		
		An_GetSideParams (c, &scale, &lim);
		lheight = DT_ApproxHeight (Graphics, scale, 1);
		sprintf (string, "%s|%s|%f|%f|%d", fname, ctable, center, step,
			Ncolors);
		space = (Ncolors + 1)*lheight;
		if (pda_Search (Pd, c, "side-annot-style", fname, style,
				SYMT_STRING) && ! strcmp (style, "legend"))
			An_AddAnnotProc (CAP_LegendAnnot, c, string,
					strlen (string), space, FALSE, FALSE);
		else
			An_AddAnnotProc (An_ColorNumber, c, string, 
					strlen (string), space, FALSE, FALSE);
	}
}




static int
CAP_Contour (c, type, rplat, rfldname, center, step, rctable, shifted)
char	*c, **rplat, **rfldname, **rctable;
contour_type	type;
float	*center, *step;
int *shifted;
/*
 * Execute a CAP contour plot, based on the given plot description,
 * specified component, and contour type.  Return the platform name, field
 * name, contour center and step, color table name, and whether or not the
 * data were spatially shifted.  The strings returned are all valid until
 * the next call to CAP_Contour, and should not be modified.
 */
{
	static char	platform[PlatformListLen];
	static char	fname[80], ctable[40], outrange[40];
	char	ctcolor[40], param[50];
	int	xdim, ydim;
	float	*rgrid, *grid, x0, x1, y0, y1, alt, lats, lons;
	int	pix_x0, pix_x1, pix_y0, pix_y1, linewidth;
	int	do_outrange;
	zbool	labelflag, dolabels, ok, autoscale;
	ZebTime	zt;
	XColor	c_outrange;
	XRectangle	clip;
	DataChunk	*dc;
	int	len;
	RGrid	rg;
	Location 	loc;
	float	badvalue;
	FieldId	fid;
	AltUnitType	altunits;
	PlatformId	pid;
/*
 * Set up the return strings
 */
	*rplat = platform;
	*rfldname = fname;
	*rctable = ctable;
/*
 * Get necessary parameters from the plot description
 */
	strcpy (fname, "none");
	strcpy (platform, "none");
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fname, SYMT_STRING);
	autoscale = CAP_AutoScale (c, "contour", platform, fname,
				   center, step);
/* ----------------
 * Data time check moved until after all the parameters are loaded, since
 * some of them will still be used even if no data are plotted.
 * ----------------
 */
/*
 * color coding info.
 */
	Monocolor = FALSE;
	if (type == LineContour)
		pda_Search(Pd, c, "color-mono", "contour", (char *) &Monocolor,
			   SYMT_BOOL);
	if (Monocolor)
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
/*
 * An out of range color is nice, sometimes.
 */
	do_outrange = TRUE;
	strcpy (outrange, "black");
	pda_Search (Pd, c, "out-of-range-color", platform, outrange,
		    SYMT_STRING);

	if (! strcmp (outrange, "none"))
		do_outrange = FALSE;
	else if (! ct_GetColorByName (outrange, &c_outrange))
		ct_GetColorByName ("black", &c_outrange);
/*
 * Labeling.
 */
	labelflag = TRUE;
	pda_Search(Pd, c, "label-blanking", "contour", (char *) &labelflag,
		SYMT_BOOL);
	dt_SetBlankLabel(labelflag);

	if (! ok)
		return (0);
/* 
 * Get annotation information
 */
	CAP_Parameters (c);
/*
 * Special stuff for line contours
 */
	dolabels = TRUE;
	pda_Search (Pd, c, "do-labels", "contour", (char *) &dolabels, 
		    SYMT_BOOL);

	linewidth = 0;
	pda_Search (Pd, c, "line-width", "contour", (char *) &linewidth,
		    SYMT_INT);
/*
 * Grab the color table
 */
	if(! Monocolor)
		if (!ct_LoadTable (ctable, &Colors, &Ncolors))
			ct_LoadTable ("Contour", &Colors, &Ncolors);
/*
 * Bail on too old, or unknown, data, if requested
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform: %s", platform);
		return (0);
	}
	if (! DataAgeOK (c, pid))
		return (0);
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	alt = Alt;
	/* msg_ELog (EF_INFO, "Get grid at %.2f km", alt); */
	zt = PlotTime;

	if ( !strcasecmp(fname, "vorticity"))
	{
	    fid = F_Field (fname, 0, "vorticity", "1/s");
	    if (! (dc = GetVorticity ( &zt, c, platform, fid, &xdim, &ydim, 
				       &x0, &y0, &x1, &y1, &alt, shifted)))
		return (0);
        }
	else if (!strcasecmp(fname, "divergence"))
	{
	    fid = F_Field (fname, 0, "divergence", "1/s");
	    if (! (dc = GetVorticity ( &zt, c, platform, fid, &xdim, &ydim, 
				       &x0, &y0, &x1, &y1, &alt, shifted)))
		return (0);
	}
	else
	{
	    fid = F_Lookup (fname);
	    if (! (dc = ga_GetGrid (&zt, c, platform, fid, &xdim, &ydim, 
				    &x0, &y0, &x1, &y1, &alt, shifted)))
		return (0);
	}

	rgrid = (float *)dc_RGGetGrid(dc, 0, fid, &loc, &rg, &len);
/*
 * Get the badvalue flag and altitude units.
 */
	badvalue = dc_GetBadval (dc);
	altunits = dc_GetLocAltUnits (dc);
/*
 * If we are autoscaling get the scale parameters now.  Stash them back into
 * the PD so they will be found later if wanted.
 */
	if (autoscale)
	{
	    char *justname = SimpleFieldName (fid);
	    FindCenterStep (dc, fid, Monocolor ? 10 : Ncolors,
			    center, step);
	    sprintf (param, "%s-center", justname);
	    pd_Store (Pd, c, param, (char *) center, SYMT_FLOAT);
	    sprintf (param, "%s-step", justname);
	    pd_Store (Pd, c, param, (char *) step, SYMT_FLOAT);
	}
/*
 * If we are doing a fancy projection, get the original lat/lon info
 * so that we can pass it through.
 */
	if (prj_FancyProjection ())
		GetLLSpacings (dc, &lats, &lons);
/*
 * Kludge: rotate the grid into the right ordering.
 */
	grid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, grid, xdim, ydim);
/*
 * Done with the data chunk
 */
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
	switch (type)
	{
	    case FilledContour:
		FC_Init (Colors, Ncolors, Ncolors / 2, 
			 do_outrange ? &c_outrange : NULL, clip, TRUE, 
			 badvalue);
		if (prj_FancyProjection ())
			FC_ProjSetup (&loc, lats, lons);
		FillContour (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			     pix_x0, pix_y0, pix_x1, pix_y1, *center, *step);
		break;
	    case LineContour:
		if(! Monocolor)
			CO_Init (Colors, Ncolors, Ncolors / 2, 
				 do_outrange ? &c_outrange : NULL, clip, 
				 TRUE, badvalue);
		else 
			CO_InitMono (Ctclr, clip, TRUE, badvalue);
		if (prj_FancyProjection ())
			CO_ProjSetup (&loc, lats, lons);
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
	CAP_AddStatusLine (c, platform, fname, loc.l_alt, altunits, &zt);
	free (grid);
	return (1);
}



static int
CAP_AutoScale (c, qual, platform, fname, center, step)
char *c;	/* component */
char *qual;	/* qualifier */
char *platform;	/* platform name */
char *fname;	/* field name */
float *center;	/* center of scale */
float *step;	/* scale step */
{
	int ok = 1;
	int autoscale;
	char param[50];
	FieldId fid = F_Lookup (fname);
/*
 * Make a beginning at center/step based on scale-mode.
 */
	if (pda_Search (Pd, c, "scale-mode", platform, param, SYMT_STRING))
		autoscale = ! strncmp (param, "auto", 4);
	else
		autoscale = FALSE;
	if (! autoscale)
	{
	/*
	 * Get the parameters.
	 */
		char *justname = SimpleFieldName (fid);
		sprintf (param, "%s-center", justname);
		ok &= pda_ReqSearch (Pd, c, param, qual, (char *) center, 
				     SYMT_FLOAT);
		sprintf (param, "%s-step", justname);
		ok &= pda_ReqSearch (Pd, c, param, qual, (char *) step, 
				     SYMT_FLOAT);

		if (*step == 0.0)
		{
			msg_ELog (EF_PROBLEM, "%s is zero!", param);
			ok = FALSE;
		}
	/*
	 * If they blew it, give them a second chance by turning on
	 * autoscaling.
	 */
		if (! ok)
		{
			msg_ELog (EF_PROBLEM,
				  "Desperately turning on autoscale");
			ok = autoscale = TRUE;
		}
	}
	return (autoscale);
}



void
CAP_Station (c, update)
char *c;
zbool update;
/*
 * Deal with a station plot.
 */
{
	char	cname[30], annot[120];
	char	platform[PlatformListLen];
	char	quadrants[4][80], quadclr[30];
	char	data[100], sticon[40];
	char	*pnames[MaxPlatforms];
	PlatformId pid;
	float vscale, unitlen;
	int linewidth, shifted = FALSE, i, nplat;
	zbool	tacmatch, do_vectors;
	ZebTime zt;
	XColor	color, qcolor;
	zbool	quadstn[4];
	FieldId	fields[6];
	FieldId quadfields[4];
	Location loc;
	AltUnitType altunits;
	WindInfo wi;
	QFormat	qformats[4];
	int 	nfield;
	float	alt;
	DataChunk	*dc;
/*
 * Get necessary parameters from the plot description
 */
	if (! CAP_VecParams (c, platform, &vscale, cname, &linewidth, &unitlen,
			     &color, &do_vectors))
		return;
/*
 * Defaults for quadrant info
 */
	for (i = 0; i < 4; i++)
	{
		quadstn[i] = FALSE;
		quadfields[i] = BadField;
		quadrants[i][0] = '\0';
		strcpy (qformats[i], "%.1f");
	}
/*
 * Get quadrant fields and formats specified in the PD
 */
	for (i = 0; i < 4; i++)
	{
		char	param[16];

		sprintf (param, "quad%d", i + 1);
		pda_Search (Pd, c, param, NULL, quadrants[i], SYMT_STRING);
		strcat (param, "-format");
		pda_Search (Pd, c, param, NULL, qformats[i], SYMT_STRING);
	}
/*
 * Get a color for the quadrants.  Do this whether or not we have data in
 * the quads -- we'll be drawing the axes regardless.
 */
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
/*
 * Flag any quadrants marked for station labels instead of fields,
 * and ignore any quadrants set to 'none'.
 */
	for (i = 0; i < 4; i++)
	{
		if (! strcmp (quadrants[i], "station"))
			quadstn[i] = TRUE;
		else if (! strcmp (quadrants[i], "none"))
			quadrants[i][0] = '\0';
	}
/*
 * Create the field list for our data fetch.
 */
	nfield = 2;
	for (i = 0; i < 4; i++)
	{
		if (quadrants[i][0] && !quadstn[i])
		{
		    quadfields[i] = F_Lookup (quadrants[i]);
		    fields[nfield++] = quadfields[i];
		}
		else
		    quadfields[i] = BadField;
	}
/*
 * Maybe start with top annotation.
 */
	if (! update)
	{
	    An_TopAnnotMatch ("Station plot", color.pixel, c, 0);
	    An_TopAnnot (" (");
	}
/*
 * Go through all the platforms.
 */
	nplat = CommaParse (platform, pnames);
	for (i = 0; i < nplat; i++)
	{
	/*
	 * Get the platform ID.
	 */
		if ((pid = ds_LookupPlatform (pnames[i])) == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Bad platform '%s'", pnames[i]);
			continue;
		}
		FindWindsFields (c, pid, &PlotTime, fields, &wi);
	/*
	 * See about position icons.
	 */
		if (! pda_Search (Pd, c, "station-icon", pnames[i], sticon,
				SYMT_STRING))
			strcpy (sticon, "pam-loc");
	/*
	 * Get the data.
	 */
		dc = CAP_StGetData (c, pid, fields, nfield, &shifted);
		if (! dc)
			continue;
		dc_GetTime (dc, 0, &zt);
	/*
	 * Throw it on to the screen.
	 */
		if (dc->dc_Class == DCC_IRGrid)
		{
			CAP_StDoIRGrid (c, dc, pnames[i], fields, nfield,
					quadfields, qformats, &color, &qcolor, 
					&zt, sticon, linewidth, unitlen, 
					do_vectors, quadstn, &wi, &alt);
		/*
		 * Check for stations with altitudes
		 */
			if (! ds_GetAlts (pid, fields[0], &zt, 0, 0, 0, 
					  &altunits))
			{
				alt = 0.0;
				altunits = AU_kmAGL;
			}
		}
		else
		{
			dc_GetLoc (dc, 0, &loc);
			alt = loc.l_alt;
			altunits = dc_GetLocAltUnits (dc);
			CAP_StDoScalar (c, dc, pnames[i], fields, nfield,
					quadfields, qformats, &color, &qcolor, 
					&zt, sticon, linewidth, unitlen, 
					do_vectors, quadstn, &wi);
		}
		dc_DestroyDC (dc);
	/*
	 * Overlay times and annotation.
	 */
		if (! update)
		{
			CAP_AddStatusLine (c, pnames[i], "(station)", alt,
					   altunits, &zt);
			sprintf (annot, "%s%s", (i == 0) ? "" : " ",pnames[i]);
			An_TopAnnot (annot);
		}
	}
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	An_TopAnnot (shifted ? " [SHIFTED])." : ").");
/*
 * Side annotation.
 */
	sprintf (data, "%s|%li|%li|%f|%d", "m/s", color.pixel, qcolor.pixel, 
		 do_vectors ? unitlen : 0, 4 /*numquads*/);

	for (i = 0; i < 4; i++)
	{
	    strcat (data, "|");

	    if (quadstn[i])
		strcat (data, "station");
	    else if (quadfields[i] != BadField)
	    {
		char *justname = SimpleFieldName (quadfields[i]);
		if (justname[0] != '\0')
		    strcat (data, justname);
		else
		    strcat (data, F_GetFullName (quadfields[i]));
	    }
	    else
		strcat (data, "none");
	}

	An_AddAnnotProc (CAP_StaPltSideAnnot, c, data, strlen (data), 90, 
			 FALSE, FALSE);
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
		prj_Project (locs[sta].l_lat, locs[sta].l_lon, &x0, &y0);
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




static void
CAP_StPlotVector (c, pt, zt, x0, y0, plat, sticon, color, qcolor, linewidth, 
		  ugrid, vgrid, badvalue, unitlen, qgrid, qformats, 
		  do_vectors, quadstn)
char *c;
int pt;
ZebTime *zt;
int x0, y0;
PlatformId plat;
char *sticon;
XColor *color, *qcolor;
int linewidth;
float *ugrid, *vgrid, badvalue, unitlen, *qgrid[4];
QFormat qformats[4];
zbool do_vectors;
zbool quadstn[4];
/*
 * Actually plot some station plot info.
 */
{
	char buf[64];
	const char *label;
	static const int hjust[4] = { JustifyRight, JustifyLeft, JustifyRight,
				      JustifyLeft };
	static const int vjust[4] = { JustifyBottom, JustifyBottom, 
				      JustifyTop, JustifyTop };
	int j;
/*
 * Place an icon at the station location.
 */
	I_PositionIcon (c, ds_PlatformName (plat), zt, sticon, x0, y0,
			color->pixel);
/*
 * Plot the arrow or barb.
 */
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 
			linewidth, LineSolid, CapButt, JoinMiter);
	if ((ugrid[pt] != badvalue) && (vgrid[pt] != badvalue))
	{
		double	u = ugrid[pt], v = vgrid[pt];

		if (u == 0 && v == 0)
			/* nothing */;
		else if (do_vectors)
			draw_vector (XtDisplay (Graphics), GWFrame (Graphics), 
				     Gcontext, x0, y0, u, v, unitlen);
		else
			draw_barb (XtDisplay (Graphics), GWFrame (Graphics),
				   Gcontext, x0, y0, ATAN2 (-v, -u), 
				   hypot (v, u), unitlen, FALSE);
	}
/*
 * Do quadrants if necessary.
 */
	XSetForeground (XtDisplay (Graphics), Gcontext, qcolor->pixel);

	for (j = 0; j < 4; j++)
	{
		int	border_x, border_y;
	/*
	 * Create station name label for this quadrant
	 */
		if (quadstn[j])
		{
			label = ds_PlatformName (plat);
			if (strchr (label, '/'))
				label = (char *)strchr (label, '/') + 1;
		}
	/*
	 * Else label with a data value if we've got a good one
	 */
		else if (qgrid[j] != NULL && qgrid[j][pt] != badvalue)
		{
			sprintf(buf, qformats[j], qgrid[j][pt]); 
			label = buf;
		}
		else
			continue;

		border_x = ((j == 0) || (j == 2)) ? -2 : 2;
		border_y = ((j == 0) || (j == 1)) ? -2 : 2;
		
		DrawText (Graphics, GWFrame (Graphics), Gcontext,
			  x0 + border_x, y0 + border_y,
			  label, 0.0, Sascale, hjust[j], vjust[j]);
	}
/*
 * Tweak the foreground back.
 */
	XSetForeground (XtDisplay (Graphics), Gcontext,	color->pixel);
}




static DataChunk *	
CAP_StGetData (c, plat, fields, nfield, shifted)
char *c;
PlatformId plat;
FieldId *fields;
int nfield, *shifted;
/* 
* Get some data.
 */
{
	ZebTime zt;
	DataOrganization org = (DataOrganization) ds_PlatformDataOrg (plat);
	DataChunk *dc;
	dsDetail details[5];
	int ndetail;
	char string[256];
/*
 * Make sure this makes sense.
 */
	if (org != OrgIRGrid && org != OrgScalar)
	{
		msg_ELog (EF_PROBLEM, "Funky org on plat %s",
				ds_PlatformName (plat));
		return (0);
	}
/*
 * See when there is data available.
 */
	if (! ds_DataTimes (plat, &PlotTime, 1, DsBefore, &zt))
	{
		msg_ELog(EF_INFO,"No data available at all for '%s'",
			ds_PlatformName (plat));
		return (0);
	}
/*
 * Bail on too old data, if requested
 */
	if (! DataAgeOK (c, plat))
		return (0);
/*
 * Get the data.
 */
	ndetail = 0;
	if (pda_Search (Pd, c, "dimensions", NULL, string, SYMT_STRING))
		dc_NSFixedDetails (string, details, &ndetail);
	details[ndetail].dd_Name = "altitude";
	details[ndetail].dd_V.us_v_float = Alt;
	ndetail++;
	if (! (dc = ds_Fetch (plat, (org==OrgIRGrid) ? DCC_IRGrid : DCC_Scalar,
			&zt, &zt, fields, nfield, details, ndetail)))
	{
		msg_ELog (EF_INFO, "Get failed on '%s'",ds_PlatformName(plat));
		return (0);
	}
	*shifted = ApplySpatialOffset (dc, c, &PlotTime);
	return (dc);
}





static void
CAP_StDoIRGrid (c, dc, platform, fields, nfield, quadfields, qformats, color, 
		qcolor, zt, sticon, linewidth, unitlen, do_vectors, quadstn, 
		wi, alt)
char *c;
DataChunk *dc;
char *platform;
FieldId *fields;
int nfield;
FieldId quadfields[4];
QFormat qformats[4];
XColor *color, *qcolor;
ZebTime *zt;
char *sticon;
int linewidth;
float unitlen;
zbool do_vectors;
zbool quadstn[4];
WindInfo *wi;
float *alt;	/* return an altitude for the irgrid */
/*
 * Plot up an IRGrid.
 */
{
	int npts, i;
	PlatformId *platforms;
	Location	*locations;
	StInfo		*sinfo;
	float *ugrid, *vgrid, *qgrid[4], badvalue;
	zbool filter = FALSE;
/*
 * Get some info out of the data chunk.
 */	
	badvalue = dc_GetBadval (dc);
	npts = dc_IRGetNPlatform (dc);
	platforms = (PlatformId *) malloc (npts * sizeof (PlatformId));
	locations = (Location *) malloc (npts * sizeof (Location));
	dc_IRGetPlatforms (dc, platforms, locations);
	*alt = locations[0].l_alt;
/*
 * Convert locations and such.
 */
	sinfo = CAP_StationInfo (dc, locations, npts);
/*
 * Get the wind components, and possibly quadrants.
 */
	ugrid = (float *) dc_IRGetGrid (dc, 0, fields[0]);
	vgrid = (float *) dc_IRGetGrid (dc, 0, fields[1]);
	for (i = 0; i < 4; i++)
	{
		if (quadfields[i] != BadField)
			qgrid[i] = (float *)dc_IRGetGrid(dc, 0, quadfields[i]);
		else
			qgrid[i] = NULL;
	}
/*
 * Apply spatial thinning if they want it.
 */
	if (pda_Search (Pd, c, "spatial-filter", platform, CPTR (filter),
			SYMT_BOOL) && filter)
	{
		int res = 50;

		pda_Search (Pd, c, "filter-resolution", platform, CPTR (res),
				SYMT_INT);
		if (res <= 0)
		{
			msg_ELog (EF_PROBLEM,
			       "invalid filter resolution for %s:%s, using 50",
				  c, platform);
			res = 50;
		}
		CAP_SpFilter (ugrid, vgrid, &badvalue, sinfo, npts, res);
	}
/*
 * Graphics context stuff.
 */
	ResetGC ();
	XSetForeground (XtDisplay (Graphics), Gcontext, color->pixel);
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
		GetWindData (wi, ugrid + i, vgrid + i, badvalue);
		CAP_StPlotVector (c, i, zt, sinfo[i].si_x, sinfo[i].si_y,
				  platforms[i], sticon, color, qcolor, 
				  linewidth, ugrid, vgrid, badvalue, unitlen, 
				  qgrid, qformats, do_vectors, quadstn);
	}
/*
 * Free the data.
 */
	free (sinfo);
	free (platforms);
	free (locations);
}




static void
CAP_StDoScalar (c, dc, platform, fields, nfield, quadfields, qformats, color, 
		qcolor, zt, sticon, linewidth, unitlen, do_vectors, quadstn,
		wi)
char *c;
DataChunk *dc;
char *platform;
FieldId *fields;
int nfield;
FieldId quadfields[4];
QFormat	qformats[4];
XColor *color, *qcolor;
ZebTime *zt;
char *sticon;
int linewidth;
float unitlen;
zbool do_vectors;
zbool quadstn[4];
WindInfo *wi;
/*
 * Plot up a scalar value
 */
{
	float u, v, badvalue, x0, y0;
	Location loc;
	int i;
/*
 * Ugly kludgery to fit the interface oriented around IRGrids.
 */
	float qv[4];
	float *qgrid[4];
	for (i = 0; i < 4; i++)
		qgrid[i] = qv + i;
/*
 * Get some info out of the data chunk.
 */	
	badvalue = dc_GetBadval (dc);
	dc_GetLoc (dc, 0, &loc);
	prj_Project (loc.l_lat, loc.l_lon, &x0, &y0);
	if (x0 < Xlo || x0 > Xhi || y0 < Ylo || y0 > Yhi)
		return;
/*
 * Get the u and v components, and possibly quadrants.
 */
	u = dc_GetScalar (dc, 0, fields[0]);
	v = dc_GetScalar (dc, 0, fields[1]);
	GetWindData (wi, &u, &v, badvalue);
	for (i = 0; i < 4; i++)
	{
		if (quadfields[i] != BadField)
			qgrid[i][0] = dc_GetScalar (dc, 0, quadfields[i]);
		else
			qgrid[i] = NULL;
	}
/*
 * Graphics context stuff.
 */
	ResetGC ();
	XSetForeground (XtDisplay (Graphics), Gcontext, color->pixel);
/*
 * Draw the vectors.
 */
	CAP_StPlotVector (c, 0, zt, XPIX (x0), YPIX (y0), dc->dc_Platform,
			  sticon, color, qcolor, linewidth, &u, &v, badvalue, 
			  unitlen, qgrid, qformats, do_vectors, quadstn);
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
int res;
/*
 * Apply spatial filtering (thinning) to this data chunk.
 */
{
	float bv = *badval;
	int sta, xp, yp, n_good;
	/* int max = 3*res/2; */
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
	n_good = 0;

	for (sta = 0; sta < nsta; sta++)
	{
		if (! sinfo[sta].si_mark)
			sinfo[sta].si_excl = TRUE;

		n_good += sinfo[sta].si_excl ? 0 : 1;
	}
/*
 * Inform the user if we removed all stations
 */
	if (n_good == 0)
		msg_ELog (EF_INFO, "All stations removed by spatial filter");
}





void
CAP_Vector (c, update)
char	*c;
zbool	update;
/*
 * Execute a CAP vector plot, based on the given plot
 * description, specified component, and plot time
 */
{
	char	cname[30], annot[120];
	char 	platform[PlatformListLen];
	char	data[100];
	float	*rgrid, *ugrid, *vgrid, unitlen, lats, lons;
	float	vscale, x0, x1, y0, y1, alt, badvalue;
	int	pix_x0, pix_x1, pix_y0, pix_y1, xdim, ydim;
	int	linewidth, len, degrade, shifted, i;
	zbool	grid = FALSE, do_vectors, proj;
	XColor	color;
	ZebTime zt;
	DataChunk	*udc, *vdc;
	Location	loc;
	RGrid		rg;
	AltUnitType	altunits;
	FieldId		winds[2];
	WindInfo	wi;
	PlatformId	pid;
/*
 * Check to see if they have set "grid" to FALSE, in which case they should
 * really be using the station representation.  I wonder if we still need
 * this...backward compatibility is such fun...
 */
	if (pda_Search (Pd, c, "grid", NULL, (char *) &grid, SYMT_BOOL) &&
			! grid)
	{
		static zbool griped = FALSE;
		if (! griped++)
			msg_ELog (EF_INFO,"Converting vector to station rep.");
		CAP_Station (c, update);
		return;
	}
/*
 * Get necessary parameters from the plot description
 */
	if (! CAP_VecParams (c, platform, &vscale, cname, 
			     &linewidth, &unitlen, &color, &do_vectors))
		return;
	pid = ds_LookupPlatform(platform);
	FindWindsFields (c, pid, &PlotTime, winds, &wi);
/*
 * See if they want to degrade the grid.
 */
	if (! pda_Search (Pd, c, "degrade", platform, (char *) &degrade,
			SYMT_INT))
		degrade = 0;
/*
 * Setup.
 */
	alt = Alt;
	zt = PlotTime;
	proj = prj_FancyProjection ();
/*
 * Get U component.  Also pull out spacings if need be for projection.
 * If we are projecting, we do NOT rotate the grids into column-major
 * order...I was starting over, so why require the extra work?
 *
 * There is a fair amount of thrashing here to preserve the old "WindGrid"
 * call when possible, on the theory that it is faster.  This merits a
 * serious check, maybe the following can be cleaned up substantially by
 * just using the projection-aware routine all the time.
 */
	if (! (udc = ga_GetGrid (&zt, c, platform, winds[0], &xdim, &ydim, 
				 &x0, &y0, &x1, &y1, &alt, &shifted)))
		return;
	if (proj)
	{
		ugrid = dc_RGGetGrid (udc, 0, winds[0], &loc, &rg, &len);
		GetLLSpacings (udc, &lats, &lons);
	}
	else
	{
		rgrid = (float *)dc_RGGetGrid(udc,0,winds[0], &loc, &rg, &len);
		ugrid = (float *) malloc (xdim * ydim * sizeof (float));
		ga_RotateGrid (rgrid, ugrid, xdim, ydim);
	}
	badvalue = dc_GetBadval (udc);
	altunits = dc_GetLocAltUnits (udc);
/*
 * Get v component.
 */
	zt = PlotTime;
	if (! (vdc = ga_GetGrid (&zt, c, platform, winds[1], &xdim, &ydim, 
				 &x0, &y0, &x1, &y1, &alt, &shifted)))
		return;
	if (proj)
		vgrid = (float *)dc_RGGetGrid(vdc, 0, winds[1], &loc,&rg,&len);
	else
	{
		rgrid = (float *)dc_RGGetGrid(vdc, 0, winds[1], &loc,&rg,&len);
		vgrid = (float *) malloc (xdim * ydim * sizeof (float));
		ga_RotateGrid (rgrid, vgrid, xdim, ydim);
	}
/*
 * Convert the winds to rectangular components
 */
	for (i = 0; i < xdim * ydim; ++i)
		GetWindData (&wi, ugrid + i, vgrid + i, badvalue);
/*
 * If we are projecting, plot the slower way.
 */
	if (proj)
		WindProjGrid (ugrid, vgrid, xdim, ydim, &loc, lats, lons,
				vscale, badvalue, color.pixel, degrade,
				do_vectors);
/*
 * Otherwise do it in pixel space.
 */
	else
	{
	/*
	 * Convert the grid limits to pixel values
	 */
		pix_x0 = XPIX (x0);	pix_x1 = XPIX (x1);
		pix_y0 = YPIX (y0);	pix_y1 = YPIX (y1);
	/*
	 * Draw the vectors or barbs
	 */
		WindGrid (Graphics, GWFrame (Graphics), Gcontext, ugrid,
				vgrid, xdim, ydim, pix_x0, pix_y0, pix_x1,
				pix_y1, vscale, badvalue, color, degrade,
				do_vectors);
	/*
	 * Free the data arrays
	 */
		free (ugrid);
		free (vgrid);
	}
/*
 * We don't need the data chunks any more.
 */
	dc_DestroyDC (udc);
	dc_DestroyDC (vdc);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	An_TopAnnotMatch ("Vector winds", color.pixel, c, 0);
	sprintf (annot, " plot (%s)", platform);
	An_TopAnnot (annot);
	if (shifted)
		An_TopAnnot (" (SHIFTED)");
	An_TopAnnot (".");
/*
 * Side annotation (scale vectors or barbs)
 */
	if (do_vectors)
	{
		int	length = 10;
		sprintf (data, "%s|%li|%f|%f|%f", "m/s", color.pixel, 
			 (float) length, 0.0, unitlen); 
		An_AddAnnotProc (An_ColorVector, c, data, strlen (data),
				 40, FALSE, FALSE);
	}
	else
	{
		sprintf (data, "%s|%li|%d", "m/s", color.pixel, (int)unitlen);
		An_AddAnnotProc (An_BarbLegend, c, data, strlen (data), 100, 
				 FALSE, FALSE);
	}

	CAP_AddStatusLine (c, platform, "(winds)", loc.l_alt, altunits, &zt);
}





static zbool
CAP_VecParams (c, platform, vscale, cname, linewidth, unitlen,
	       color, do_vectors)
char *c;
char *platform;
float *vscale;
char *cname;
int *linewidth;
float *unitlen;
XColor *color;
zbool *do_vectors;
/*
 * Get common parameters for vector plots.
 */
{
	char string[16];
	zbool ok;
/*
 * Vectors or barbs?
 */
	*do_vectors = TRUE;
	if (pda_Search (Pd, c, "wind-style", NULL, string, SYMT_STRING))
		*do_vectors = strncmp (string, "barb", 4);
/*
 * Required stuff
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	if (! ok)
		return (FALSE);
/*
 * Scaling
 */
	if (*do_vectors)
	{
		*vscale = 0.002;
		pda_Search (Pd, c, "arrow-scale", "xsect", (char *)vscale,
			    SYMT_FLOAT);
	}
	else
	{
		*vscale = 0.06;
		pda_Search (Pd, c, "barb-scale", "xsect", (char *)vscale,
			    SYMT_FLOAT);
	}

	*unitlen = USABLE_HEIGHT * *vscale;
/*
 * Get annotation information from the plot description
 */
	CAP_Parameters (c);
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
	char string[40], units[40], qname[4][80];
	float unitlen, used, scale, speed = 10.0; 
	int i, left, numquads, limit, middle;
	Pixel vc, qc;
/*
 * Get annotation parameters.
 */
	An_GetSideParams (comp, &scale, &limit);
/*
 * Get the data.
 */
        sscanf (data, "%[^|]|%li|%li|%f|%d|%[^|]|%[^|]|%[^|]|%[^|]", units, 
		&vc, &qc, &unitlen, &numquads, qname[0], qname[1], qname[2], 
		qname[3]);
/*
 * Put in the vector (unless unitlen == 0, implying wind barbs).
 */
	left = An_GetLeft ();
	XSetForeground (XtDisplay (Graphics), Gcontext, vc);

	if (unitlen > 0)
	{
		sprintf (string, "%.0f %s", speed, units);

		DrawText (Graphics, GWFrame (Graphics), Gcontext, left, begin, 
			  string, 0.0, scale, JustifyLeft, JustifyTop);
		used = DT_ApproxHeight (Graphics, scale, 1);
		begin += used;
		space -= used;

		draw_vector (XtDisplay (Graphics), GWFrame (Graphics), 
			     Gcontext, left, begin + 5, speed, 0.0, unitlen);
		begin += 10;
		space -= 10;
	}
	else	/* just show units for wind barbs */
	{
		sprintf (string, "barbs in %s", units);

		DrawText (Graphics, GWFrame (Graphics), Gcontext, left, begin, 
			  string, 0.0, scale, JustifyLeft, JustifyTop);
		used = DT_ApproxHeight (Graphics, scale, 1);
		begin += used;
		space -= used;
	}
/*
 * Put in the quadrant annotation.
 */
	XSetForeground (XtDisplay (Graphics), Gcontext, qc);
	middle = (left + GWWidth (Graphics))/2;
# ifdef notdef
	if (numquads > 0)
	{
# endif
		XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, 
			middle, begin + 10, middle, begin + 55);
		XDrawLine (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, 
			   left + 10, begin + 30, GWWidth (Graphics) - 10,
			   begin + 30);
# ifdef notdef
	}
# endif
/*
 * Put in the actual strings, and activate the areas.  For now I will keep
 * using the ugly hardwired constants, but it would be nice to fix that
 * eventually.
 */
	for (i = 0; i < 4 /* numquads */; i++)
	{
		char name[12];
	/*
	 * Figure out just where the annotation will go.
	 */
		int tx = (i & 0x1) ? middle + 5 : middle - 5;
		int ty = (i < 2) ? begin + 27 : begin + 33;
		int hjust = (i & 0x1) ? JustifyLeft : JustifyRight;
		int vjust = (i < 2) ? JustifyBottom : JustifyTop;
		int sx, sy, ex, ey;
	/*
	 * Fix up a field name.  Put in a bunch of blanks for empty quads
	 * so that we get a big enough active area to hit.
	 */
		sprintf (name, "quad%d", i + 1);
		if (! strcmp (qname[i], "none"))
			strcpy (qname[i], "      ");
	/*
	 * Throw it onto the screen and activate it.  We really need a
	 * "draw and activate text" routine to do this for us...
	 */
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			  tx, ty, qname[i], 0.0, scale, hjust, vjust);
		DT_TextBox (Graphics, GWFrame (Graphics), tx, ty, qname[i],
			    0.0, scale, hjust, vjust, &sx, &sy, &ex, &ey);
		I_ActivateArea (sx - 2, ey + 1, ex - sx + 4, sy - ey + 2,
				"annot", comp, name, 0);
	}
}




static void
CAP_PlotRaster (char *c, zbool update, char *topannot, char *sideannot,
		zbool *sidelegend)
/*
 * Execute a CAP raster plot, based on the given plot
 * description, specified conent, and plot time
 */
{
	char	fname[80], ctname[40], data[100], hcolor[40], style[40];
	char 	platform[PlatformListLen];
	char	param[50], outrange[40];
	int	xdim, ydim, slow;
	int	nsteps;
	zbool	ok, highlight, autoscale;
	float	*fgrid;			/* Floating point grid	*/
	unsigned char *igrid;		/* Image grid		*/
	float	x0, x1, y0, y1, alt;
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
	FieldId	fid;
	AltUnitType	altunits;
	int transparent = 0;
	char justname[64];
/*
 * Get necessary parameters from the plot description
 */
	strcpy (fname, "none");
	strcpy (platform, "none");
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fname, SYMT_STRING);
	autoscale = CAP_AutoScale (c, "raster", platform, fname, 
				   &center, &step);
	ok &= pda_ReqSearch (Pd, c, "color-table", "raster", ctname, 
		SYMT_STRING);

	fid = F_Lookup (fname);
/*
 * We want special radar annotation whether we get data or not, so the
 * user knows what selection criteria are in effect.
 */
	if (!update && r_RadarSpace (c))
		r_AddAnnot (c, platform);
	if (! ok)
		return;
/*
 * An out of range color is nice, sometimes.  Rather than introduce a
 * new parameter for transparency, we accept 'none' to mean "make the
 * default out-of-range color (black) transparent".
 */
	strcpy (outrange, "black");
	pda_Search (Pd, c, "out-of-range-color", platform, outrange,
		    SYMT_STRING);
	if (! strcmp (outrange, "none"))
	{
	    transparent = 1;
	}
	if (transparent || ! ct_GetColorByName (outrange, &xoutr))
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
	else if (org == Org3dGrid || org == Org2dGrid || org == OrgIRGrid ||
		 org == OrgNSpace || org == OrgPolar)
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
	strcpy (justname, SimpleFieldName (fid));
	sprintf (param, "%s-highlight-range", justname);
	if (pda_Search (Pd, c, param, "raster", CPTR (hrange), SYMT_FLOAT)
		&& (hrange != 0.0))
	{
		highlight = TRUE;
		sprintf (param, "%s-highlight-color", justname);
		if (! pda_Search (Pd, c, param, "raster", hcolor, 
				SYMT_STRING))
			strcpy (hcolor, "white");
		sprintf (param, "%s-highlight", justname);
		if (! pda_Search (Pd, c, param, "raster", CPTR (hvalue), 
				SYMT_FLOAT))
			hvalue = 0.0;
	}
/*
 * Rasterization control.  This determines whether we use the polygon fill
 * (slow) or fancy integer (fast) method of rasterization.  The new-raster
 * and fast-raster parameters no longer mean anything; you have to explicitly
 * say slow-raster to get the slow method.
 *
 * ...unless, of course, you are using map projections, in which case only
 * the slow method will work.  Someday when we decide we want to do rotated
 * grids, slow will be required for that too.
 *
 * We may eventually want a size test here like image rasterize has to
 * cruise through to fast rasterization even when projecting when the grid
 * is not big enough to make it worthwhile.
 */
	slow = FALSE;
	(void) pda_Search (Pd, c, "slow-raster", NULL, (char *) &slow,
			SYMT_BOOL);
	if (prj_FancyProjection ())
		slow = TRUE;
	msg_ELog (EF_DEBUG, "%s: using %s rasterization", c, slow ? "SLOW" :
			"FAST");
/*
 * Field number and color table
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);
/*
 * Default nsteps to number of colors in color table if not set.
 */
	sprintf (param, "%s-nsteps", justname);
	if (! pda_Search (Pd, c, param, "raster", CPTR (nsteps), SYMT_INT))
		nsteps = Ncolors;
/*
 * Initialize our annotation data now in case we return early.
 */
	sprintf (topannot, "%s %s plot.", platform, px_FldDesc (fname));
	*sidelegend = FALSE;
/*
 * Side annotation (color bar)
 */
	if (highlight)
		sprintf (sideannot, "%s|%s|%f|%f|%d|%d|%f|%s|%f", 
			 px_FldDesc (fname), ctname, center, step, nsteps, 
			 highlight, hvalue, hcolor, hrange);
	else
		sprintf (sideannot, "%s|%s|%f|%f|%d|%d|%f|%s|%f", 
			 px_FldDesc (fname), ctname, center, step, nsteps, 
			 highlight, 0.0, "null", 0.0);
/*
 * Bail on too old data, if requested
 */
	if (! DataAgeOK (c, pid))
		return;
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	alt = Alt;
	zt = PlotTime;
	if (image)
	{
		if (! (dc = CAP_ImageGrid (c, &zt, pid, fid, &xdim, &ydim,
 				&x0, &y0, &x1, &y1, &alt, &shifted)))
			return;
		igrid = dc_ImgGetImage (dc, 0, fid, &loc, &rg, &len, &scale);
	}
	else
	{
		if (! (dc = ga_GetGrid (&zt, c, platform, fid, &xdim, &ydim,
				&x0, &y0, &x1, &y1, &alt, &shifted)))
			return;
		fgrid = (float *) dc_RGGetGrid (dc, 0, fid, &loc, &rg, &len);
	}
	if ((image && !igrid) || (!image && !fgrid))
	{
		msg_ELog (EF_INFO, "Unable to get grid for %s.", platform);
		dc_DestroyDC (dc);	/* Ahem! */
		return;
	}

	alt = loc.l_alt;
	altunits = dc_GetLocAltUnits (dc);
/*
 * Convert the grid limits to pixel coordinates
 */
	pix_x0 = IXPIX (x0);	pix_x1 = IXPIX (x1);
	pix_y0 = IYPIX (y0);	pix_y1 = IYPIX (y1);
/*
 * Clip rectangle
 */
	clip.x = F_X0 * GWWidth (Graphics);
	clip.y = (1.0 - F_Y1) * USABLE_HEIGHT;
	clip.width = (F_X1 - F_X0) * GWWidth (Graphics);
	clip.height = (F_Y1 - F_Y0) * USABLE_HEIGHT;
/*
 * Calculate the rasterization limits
 */
	if (autoscale)
	{
		FindCenterStep (dc, fid, nsteps, &center, &step);
		sprintf (param, "%s-center", justname);
		pd_Store (Pd, c, param, (char *) &center, SYMT_FLOAT);
		sprintf (param, "%s-step", justname);
		pd_Store (Pd, c, param, (char *) &step, SYMT_FLOAT);
	}
	max = center + (nsteps/2.0) * step;
	min = center - (nsteps/2.0) * step;
/*
 * Draw the raster plot
 */
	ct_GetColorByName (hcolor, &xc);
	RP_Init (Colors, Ncolors, xoutr, clip, min, max, highlight, hvalue, 
		 xc, hrange);
	RP_Transparent (transparent);
	if (image)
		RasterImagePlot (Graphics, DrawFrame, igrid, xdim,
			ydim, pix_x0, pix_y0, pix_x1, pix_y1, scale.s_Scale,
			scale.s_Offset, &loc, &rg);
	else if (slow)
		RasterPlot (dc, &loc, fgrid, xdim, ydim);
	else
		RasterXIPlot (Graphics, GWFrame (Graphics), fgrid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1, /*fastloop*/ TRUE);
/*
 * Free the data chunk.
 */
	dc_DestroyDC (dc);
	CAP_AddStatusLine (c, platform, fname, alt, altunits, &zt);
/*
 * Update annotations now that we know whether we shifted and now that
 * our color bar may have been autoscaled to the data.
 */
	sprintf (topannot, "%s %s %s",
		 platform, px_FldDesc (fname), 
		 shifted ? " plot (SHIFTED).  " : " plot.  ");
/*
 * Side annotation (color bar), either legend style or a normal
 * numeric color bar.
 */
	if (pda_Search (Pd, c, "side-annot-style", fname, style,
			SYMT_STRING) && ! strcmp (style, "legend"))
	{
	    *sidelegend = TRUE;
	    sprintf (sideannot, "%s|%s|%f|%f|%d", fname, ctname, center, step,
		     Ncolors);
	}
	else
	{
	    *sidelegend = FALSE;
	    if (highlight)
		sprintf (sideannot, "%s|%s|%f|%f|%d|%d|%f|%s|%f", 
			 px_FldDesc (fname), ctname, center, step, nsteps, 
			 highlight, hvalue, hcolor, hrange);
	    else
		sprintf (sideannot, "%s|%s|%f|%f|%d|%d|%f|%s|%f", 
			 px_FldDesc (fname), ctname, center, step, nsteps, 
			 highlight, 0.0, "null", 0.0);
	}
}



void
CAP_Raster (char *c, zbool update)
{
	char topannot[512];
	char sideannot[512];
	zbool use_sidelegend;

	topannot[0] = 0;
	sideannot[0] = 0;
/*
 * Get annotation information from the plot description
 */
	CAP_Parameters (c);
/*
 * The first part of CAP_Raster was moved into CAP_PlotRaster, to align
 * with the CAP_Contour plots which show their top and side annotation
 * even when no data can be plotted.
 */
	CAP_PlotRaster (c, update, topannot, sideannot, &use_sidelegend);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Now add any top or side annotation procedures
 */
	if (topannot[0])
		An_TopAnnot (topannot);
	if (sideannot[0])
	{
	    /*
	     * Are we doing a legend-style side annotation or
	     * a normal numeric color bar?
	     */
	    if (use_sidelegend)
	    {
		float scale;
		int lim, lheight, space;
		
		An_GetSideParams (c, &scale, &lim);
		lheight = DT_ApproxHeight (Graphics, scale, 1);
		space = (Ncolors + 1)*lheight;

		An_AddAnnotProc (CAP_LegendAnnot, c, sideannot, 
				 strlen (sideannot),
				 space, FALSE, FALSE);
	    }
	    else
	    {
		An_AddAnnotProc (CAP_RasterSideAnnot, c, sideannot,
				 strlen (sideannot), 
				 140, TRUE, FALSE);
	    }
	}
}



void
CAP_RasterSideAnnot (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
{
	char string[40], ctable[40], color[40], *spformat;
	float center, step, val, used, scale, value, range, max;
	int i, left, ncolors, limit, nsteps, y;
	int bar_height;
	float step_height;
	int highlight;
	int bpl;
	XColor *colors, xc;
	XColor tacolor;
/*
 * Get annotation parameters.
 */
	An_GetTopParams (&tacolor, 0);
	An_GetSideParams (comp, &scale, &limit);
/*
 * Get the data.
 */
        sscanf (data,"%[^|]|%[^|]|%f|%f|%d|%d|%f|%[^|]|%f", string, ctable, 
		&center, &step, &nsteps, &highlight, &value, color, &range);
        ct_LoadTable (ctable, &colors, &ncolors);
	ct_GetColorByName (color, &xc);
/*
 * Throw in the field name.
 */
	left = An_GetLeft ();
	XSetForeground (XtDisplay (Graphics), Gcontext, tacolor.pixel);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, 
		begin, string, 0.0, scale, JustifyLeft, 
		JustifyCenter);
	/* used = scale * (float) USABLE_HEIGHT; */
	used = DT_ApproxHeight (Graphics, scale, 1);
	begin += used;
	space -= 2*used;  /* 2* so as to leave room at the bottom */
/*
 * Add all the colors.  Keep the bar height integral so that all of our
 * color rectangles are the same height and their edges line up.
 */
	bar_height = space / ncolors;
	if (bar_height <= 0) bar_height = 1;
	for (i = 0; i < ncolors; i++)
	{
		XSetForeground (XtDisplay (Graphics), Gcontext, 
			colors[ncolors - i - 1].pixel);
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, left, begin + i * bar_height, 10, 
			bar_height);
	}
/*
 * Do the numeric labels.  Keep the step height as float to place it
 * accurately rather than just evenly.  Figure the height using the actual
 * span of the color bar, which may be different than the available space
 * because of the truncation above.  Calculate the minimum number of steps
 * per label which will keep the labels uncrowded and legible.
 */
	space = bar_height * ncolors; 
	step_height = (float) space / (float) nsteps;
	spformat = (fabs (step) > .2) ? "%.1f" : "%.2f";
	bpl = (int)((used - 1) / step_height) + 1;
	for (i = 0; i <= nsteps; i += bpl)
	{
		val = center + (nsteps/2.0 - i) * step;
		sprintf (string, spformat, val);

		XSetForeground (XtDisplay (Graphics), Gcontext, tacolor.pixel);
#ifdef notdef
		y = (float) begin + (float) i * (float) ncolors / (float)
			(nsteps - 1.0) * (float) bar_height;
#endif
		y = (float) begin + (float) i * step_height;
		DrawText (Graphics, GWFrame (Graphics), Gcontext, left + 15, 
			y, string, 0.0, scale, JustifyLeft, JustifyCenter);
	}
/*
 * Add the special highlight color.
 */
	if (highlight)
	{
#ifdef notdef
		bar_height = space * range / (step * (nsteps - 1.0));
		max = center + nsteps / 2 * step;
		y = (float) begin + (float) space * (max - value) / 
			(step * (nsteps - 1.0)) - bar_height / 2.0; 
#endif
		XSetForeground (XtDisplay (Graphics), Gcontext, xc.pixel);
		bar_height = space * range / (step * nsteps);
		max = center + (float) nsteps / 2.0 * step;
		y = (float) begin + (float) space * (max - value) / 
			(step * nsteps) - bar_height / 2.0; 
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
CAP_ImageGrid (c, when, pid, fid, xdim, ydim, x0, y0, x1, y1, alt, shift)
char	*c;
ZebTime *when;
PlatformId pid;
FieldId fid;
int	*xdim, *ydim, *shift;
float	*x0, *y0, *x1, *y1, *alt;
/*
 * Fetch an image grid from this platform.
 */
{
	ZebTime realtime;
	RGrid rg;
	ScaleInfo sc;
	Location origin;
	int len;
	DataChunk *dc;
/*
 * Get a data time, applying altitude selection if appropriate.
 */
	realtime = *when;
	if (! ImageDataTime (c, pid, *alt, &realtime))
		return (0);
/*
 * Snarf it.
 */
	if (! (dc = ds_Fetch (pid, DCC_Image, &realtime, &realtime, &fid, 1,
			      NULL, 0)))
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s/%s.", 
			  ds_PlatformName (pid), F_GetFullName (fid));
		return (0);
	}
	*shift =  ApplySpatialOffset (dc, c, &realtime);
/*
 * Get some info out of the data chunk.
 */
	(void) dc_ImgGetImage (dc, 0, fid, &origin, &rg, &len, &sc);
/*
 * Return the various pieces of info.
 */
	*xdim = rg.rg_nX;
	*ydim = rg.rg_nY;
	prj_Project (origin.l_lat, origin.l_lon, x0, y0);
	*x1 = *x0 + (rg.rg_nX - 1) * rg.rg_Xspacing;
	*y1 = *y0 + (rg.rg_nY - 1) * rg.rg_Yspacing;
/*
 * Free the data object and return the data chunk.
 */
	*when = realtime;
	return (dc);
}




static void
CAP_AddStatusLine (comp, plat, fname, alt, altunits, t)
char	*comp, *plat, *fname;
double	alt;
AltUnitType	altunits;
ZebTime *t;
/*
 * Add a "standard" status line to the overlay times widget with component, 
 * platform, field, altitude, and time.
 */
{
	char	string[120], anglabel[16];
	int	rspace;

	if ((rspace = r_RadarSpace (comp)))
		sprintf (anglabel, "%.1f deg", alt);

	sprintf (string, "%-14s %-10s %-10s %-14s ", comp, plat, fname,
		 rspace ? anglabel : au_AltLabel (alt, altunits));
	TC_EncodeTime (t, TC_Full, string + strlen (string));
	strcat (string, "\n");

	ot_Append (string);
}



# if C_CAP_POLAR


void
CAP_Polar (char *c, int update)
/*
 * Perform a polar plot.
 */
{
	PlatformId pid;
	FieldId fids[2];	/* plot and threshold */
	int nfid, ttest, shifted, project, beam, ncolors, nstep, xr, yr, tfill;
	int legend;
	float center, step, tvalue, alt = Alt, min, max, cmult, x, y;
	char ctable[40], plat[CFG_PLATNAME_LEN], adata[300];
	XColor outrange, *colors;
	ZebTime when;
	DataChunk *dc;
	PPCookie pc;
	SweepInfo swpinfo;
	Pixel ccbuf[4096];
	Location rloc;
	int transparent;
	char justname[64];
/*
 * Instrumentation if needed.
 */
# ifdef TIMING
	int msec;
	struct rusage	ru;

	getrusage (RUSAGE_SELF, &ru);
	msec = - ((ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
			(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000);
# endif
/*
 * This is a type of plot that should rationally be able to do updates
 * one of these days, but we don't even try for now.
 */
	if (update)
		return;
/*
 * Get our config info.
 */
	if (! CAP_PolarParams (c, plat, &pid, fids, &nfid, &tvalue, &ttest,
			       &center, &step, &nstep, ctable, &outrange, 
			       &transparent, &project, &tfill, &legend))
		return;
	if (! ct_LoadTable (ctable, &colors, &ncolors))
		return;
/*
 * Figure out color coding.
 */
	min = center - (nstep/2.0)*step;
	max = center + (nstep/2.0)*step;
	cmult = ncolors/(max - min);
/*
 * When should this data come from?
 */
	when = PlotTime;
	if (! ImageDataTime (c, pid, alt, &when))
	  return;
/*
 * Make sure the data are recent enough, if we're checking for
 * that sort of thing.
 */
	if (! AgeCheck (c, plat, &when))
	  return;
/*
 * OK, time to get it.
 */
	if (! (dc = ds_Fetch (pid, DCC_Polar, &when, &when, fids, nfid,
			NULL, 0)))
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s", plat);
		return;
	}
	shifted = ApplySpatialOffset (dc, c, &PlotTime);
/*
 * Get the radar location in pixel space.  Done out here to avoid the
 * overhead for every beam; if we ever find ourselves in the business of
 * plotting data from mobile radars, this will need to move inside the
 * loop.
 */
	dc_GetLoc (dc, 0, &rloc);
	prj_Project (rloc.l_lat, rloc.l_lon, &x, &y);
	xr = XPIX (x);
	yr = YPIX (y);
/*
 * Fix up the overlay times widget before we forget.
 */
	CAP_AddStatusLine (c, plat, F_GetFullName (fids[0]), rloc.l_alt,
			dc_GetLocAltUnits (dc), &when);
/*
 * Get ready to do some serious plotting.
 */
	pc = pol_DisplaySetup (project, tfill, transparent, outrange.pixel);
	dcp_GetSweepInfo (dc, 0, &swpinfo);
/*
 * OK, do some serious plotting.
 */
	for (beam = 0; beam < swpinfo.si_NBeam; beam++)
	{
		PolarBeam *pb;
		int gate;
# ifdef RDEBUG
		XColor red;
		ct_GetColorByName ("red", &red);
# endif
	/*
	 * Pull out the data.
	 */
		pb = dcp_GetBeam (dc, 0, beam, fids[0], (nfid > 1) ? fids[1] :
				BadField, ttest, tvalue);
		if (! pb)
			continue;
	/*
	 * Color code it.
	 *
	 * We have to be careful to check for negative values of fndx here
	 * before casting to int, since a cast to int often truncates
	 * toward zero.  This means that a simple cast from the range 
	 * -1.0 < fndx < 1.0 gives us index 0, and we only want index 0 
	 * when 0.0 <= fndx < 1.0. 
	 */
		for (gate = 0; gate < pb->pb_NGates; gate++)
		{
			float fndx = (pb->pb_Data[gate] - min)*cmult;
			ccbuf[gate] = (fndx < 0 || fndx >= ncolors) ?
				outrange.pixel : colors[(int)fndx].pixel;
		}
# ifdef RDEBUG
		for (gate = 800; gate < 850; gate++)
			ccbuf[gate] = red.pixel;
		for (gate = 900; gate < 950; gate++)
			ccbuf[gate] = red.pixel;
# endif
	/*
	 * Plot it and we're done.
	 */
		pol_PlotBeam (pc, pb, ccbuf, xr, yr);
		dcp_FreeBeam (pb);
	}
/*
 * Get this stuff op on the screen, and clean up.
 */
	pol_Finished (pc);
	dc_DestroyDC (dc);
/*
 * Toss some info on the top of the screen.
 */
	An_TopAnnot (F_GetFullName (fids[0]));
	An_TopAnnot (" (");
	An_TopAnnot (plat);
	An_TopAnnot (")");
	if (shifted)
		An_TopAnnot ("(SHIFTED)");
	An_TopAnnot (".  ");
/*
 * Set up for side annotation.
 */
	strcpy (justname, SimpleFieldName (fids[0]));
	sprintf (adata, "%s|%s|%f|%f|%d|%d|%f|%s|%f", justname,
			ctable, center, step, nstep, FALSE, 0.0, "white", 0.0);
	if (legend)
	{
		int lim, lheight;
		float scale;
		An_GetSideParams (c, &scale, &lim);
		lheight = DT_ApproxHeight (Graphics, scale, 1);
		An_AddAnnotProc (CAP_LegendAnnot, c, adata, strlen (adata),
				(legend/2 + 1)*lheight, FALSE, FALSE);
	}
	else
		An_AddAnnotProc (CAP_RasterSideAnnot, c, adata, strlen (adata),
				140, TRUE, FALSE);
	r_AddAnnot (c, plat);
/*
 * How did the timing work out?
 */
# ifdef TIMING
	getrusage (RUSAGE_SELF, &ru);
	msec += (ru.ru_stime.tv_usec + ru.ru_utime.tv_usec)/1000 +
		(ru.ru_stime.tv_sec + ru.ru_utime.tv_sec)*1000;
	msg_ELog (EF_INFO, "Polar plot time = %.3f sec", (float) msec/1000.0);
# endif
}







static int
CAP_PolarParams (char *c, char *platform, PlatformId *pid, FieldId *fids,
		 int *nfids, float *tvalue, int *ttest, float *center, 
		 float *step, int *nstep, char *ctable, XColor *outrange, 
		 int *transparent, int *project, int *tfill, int *legend)
/*
 * Grab all of the PD parameters controlling polar plots.
 */
{
	int ok, enab = 0;
	char cparam[120], fname[80], param[40], sastyle[40];
	char justname[64];
/*
 * Platform info.
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	if ((*pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform %s", platform);
		return (FALSE);
	}
/*
 * Field info.
 */
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fname, SYMT_STRING);
	fids[0] = F_Lookup (fname);
	strcpy (justname, SimpleFieldName (fids[0]));
/*
 * Are we doing thresholding?
 */
	if (! pda_Search (Pd, c, "threshold", justname,
			  (char *) &enab, SYMT_BOOL))
		enab = FALSE;
	if (enab && pda_Search (Pd, c, "threshold-field", justname,
				cparam, SYMT_STRING))
	{
		fids[1] = F_Lookup (cparam);
		*nfids = 2;
		if (! pda_Search (Pd, c, "threshold-test", justname,
				  cparam, SYMT_STRING))
			*ttest = FALSE;
		else
			*ttest = ! strcmp (cparam, "over");
		if (! pda_ReqSearch (Pd, c, "threshold-value", 
				     justname,
				     CPTR (*tvalue), SYMT_FLOAT))
			*nfids = 1;  /* Turn off thresholding */
	}
	else
	{
		fids[1] = BadField;
		*nfids = 1;
	}
/*
 * Color info.
 */
	ok &= pda_ReqSearch (Pd, c, "color-table", platform, ctable,
			SYMT_STRING);
	sprintf (param, "%s-center", justname);
	ok &= pda_ReqSearch (Pd, c, param, platform, CPTR (*center),
			SYMT_FLOAT);
	sprintf (param, "%s-step", justname);
	ok &= pda_ReqSearch (Pd, c, param, platform, CPTR (*step), SYMT_FLOAT);
	sprintf (param, "%s-nsteps", justname);
	if (! pda_Search (Pd, c, param, platform, CPTR (*nstep), SYMT_INT))
		*nstep = 17;
	if (! pda_Search (Pd, c, "out-of-range-color", platform, cparam,
			SYMT_STRING))
		strcpy (cparam, "black");
/*
 * Transparent background iff they chose "none" as the out-of-range-color
 */
	*transparent = ! strcmp (cparam, "none");
/*
 * Set the out-of-range-color to the requested color, or get a "marker"
 * color (that we hope is not in the color table) if they want transparency.
 */
	if (*transparent)
	    ct_GetColorByName ("HotPink4", outrange);
	else
	    ct_GetColorByName (cparam, outrange);
/*
 * Are we doing horizontal projection?
 */
	if (! pda_Search (Pd, c, "horizontal-projection", platform,
			CPTR (*project), SYMT_BOOL))
		*project = TRUE;
/*
 * How about triangular filling?
 */
	if (! pda_Search(Pd, c, "triangular-fill", platform, CPTR (*tfill),
			SYMT_BOOL))
		*tfill = TRUE;
/*
 * Do they want legend-style annotation?
 */
	*legend = 0;
	if (pda_Search (Pd, c, "side-annot-style", justname,
			sastyle, SYMT_STRING) && ! strcmp (sastyle, "legend"))
	{
	/*
	 * Don't actually hang on to the map now, but do verify that they
	 * have one.
	 */
		char lmap[1024], *me[128];
		if (! pda_Search (Pd, c, "legend-map", justname,
				  lmap, SYMT_STRING))
			msg_ELog (EF_PROBLEM, "No legend map!");
		else
			*legend = CommaParse (lmap, me);
	}
	return (ok);

}

# endif /* C_CAP_POLAR */




static void
CAP_LegendAnnot (char *comp, char *data, int datalen, int begin, int space)
/*
 * Do a legend annotation.
 */
{
	char map[1024], *mentries[128], ctable[40], field[60];
	float scale, center, step, mapv, min, max, cmult;
	int limit, nstep, nmap, left, lheight, i, ncolors, cind;
	XColor *colors;
	XColor tacolor;
/*
 * Pull out the stuff that was stashed into our data array.
 */
	if (sscanf (data, "%[^|]|%[^|]|%f|%f|%d", field, ctable, &center, 
		    &step, &nstep) != 5)
	{
		msg_ELog (EF_PROBLEM, "LegendAnnot data screwup");
		return;
	}
/*
 * Start by getting the legend map
 */
       if (! pda_Search (Pd, comp, "legend-map", field, map, SYMT_STRING))
       {
	       msg_ELog (EF_PROBLEM, "Legend map disappeared!");
	       return;
       }
       nmap = CommaParse (map, mentries);
       if (nmap & 0x1)
       {
	       msg_ELog (EF_PROBLEM, "Odd number of map entries!");
	       return;
       }
       nmap /= 2;
/*
 * Some graphics parameters.
 */
	An_GetTopParams (&tacolor, 0);
	An_GetSideParams (comp, &scale, &limit);
	lheight = DT_ApproxHeight (Graphics, scale, 1);
/*
 * We need the color table.
 */
	ct_LoadTable (ctable, &colors, &ncolors);
	min = center - (nstep/2.0)*step;
	max = center + (nstep/2.0)*step;
	cmult = ncolors/(max - min);
/*
 * Put out the field name.
 */
       	left = An_GetLeft ();
	XSetForeground (XtDisplay (Graphics), Gcontext, tacolor.pixel);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, 
		begin, field, 0.0, scale, JustifyLeft, 	JustifyCenter);
	begin += lheight;
/*
 * Put out all of the entries.
 */
	for (i = 0; i < nmap; i++)
	{
	/*
	 * Get the map value and turn it into a color index.
	 */
		mapv = atof (mentries[2*i]);
		if (mapv < min || mapv >= max)
			continue;
		cind = (mapv - min)*cmult;
	/*
	 * Now draw it.
	 */
		XSetForeground (Disp, Gcontext, colors[cind].pixel);
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, left, begin - lheight/2, 10, lheight);
		XSetForeground (Disp, Gcontext, tacolor.pixel);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, left + 12,
				begin, mentries[2*i + 1], 0.0, scale,
				JustifyLeft, JustifyCenter);
	/*
	 * Move on.
	 */
		begin += lheight;
	}
}

# endif  /* C_PT_CAP */
