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
# include <X11/Intrinsic.h>

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

# undef quad 	/* Sun cc header file definition conflicts with variables */

MAKE_RCSID ("$Id: ConstAltPlot.c,v 2.70 1998-02-19 23:54:45 burghart Exp $")


/*
 * Color stuff
 */
static XColor	*Colors, Ctclr;
static int	Ncolors;
static bool 	Monocolor;

/*
 * Our plot altitude
 */
static float	Alt;

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
void		CAP_LineContour FP ((char *, int));
static void	CAP_Contour FP ((char *, contour_type, char **, char **, 
				 float *, float *, char **, int *));
static DataChunk *CAP_ImageGrid FP ((char *, ZebTime *, PlatformId, FieldId, 
			int *, int *, float *, float *, float *, float *, 
			float *, int *));
static int	CAP_AutoScale FP ((char *c, char *qual, char *platform,
				   char *fname, float *center, float *step));
void		CAP_RasterSideAnnot FP ((char *, char *, int, int, int));
void		CAP_StaPltSideAnnot FP ((char *, char *, int, int, int));
static bool	CAP_VecParams FP ((char *c, char *platform, float *vscale,
		   char *cname, int *linewidth, float *unitlen, 
		   XColor *color, bool *do_vectors));
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
				    bool quadstn[4], WindInfo *wi));
static void	CAP_StDoIRGrid FP ((char *c, DataChunk *dc, char *platform,
				    FieldId *fields, int nfield,
				    FieldId quadfields[4], QFormat qformats[4],
				    XColor *color, XColor *qcolor, ZebTime *zt,
				    char *sticon, int linewidth, 
				    double unitlen, int do_vectors, 
				    bool quadstn[4], WindInfo *wi, float *));
static DataChunk *CAP_StGetData FP ((char *c, PlatformId plat, FieldId *fields,
				     int nfield, int *shifted));
static void	CAP_StPlotVector FP ((char *c, int pt, ZebTime *zt, int x0, 
				      int y0, PlatformId plat, char *sticon,
				      XColor *color, XColor *qcolor,
				      int linewidth, float *ugrid, 
				      float *vgrid, double badvalue, 
				      double unitlen, float *qgrid[4],
				      QFormat qformats[4], int do_vectors, 
				      bool quadstn[4]));
static bool	CAP_TimeCheck FP ((char *c, PlatformId pid));



void
CAP_Init (t)
UItime *t;
/*
 * CAP Plot initialization.
 */
{
	char	altlabel[40], prjlabel[80];
	float	annotscale;
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
	XSetForeground (XtDisplay (Graphics), Gcontext,Tadefclr.pixel);
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
	An_DoTopAnnot (prjlabel, Tadefclr.pixel, "global", "proj");
	Require ("projection");
# endif
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
	char	*plat, *fname, *ctable, string[100];
	int shift;
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

	An_TopAnnot (string, Tadefclr.pixel);
/*
 * Side annotation (color bar)
 * (We have to subtract half a step from the center value that goes to 
 * An_ColorBar to make it label things correctly based on the plot that
 * comes out of CAP_Contour)
 */
	if (Sashow)
	{
		sprintf (string, "%s %s %f %f", fname, ctable, center, step); 
		An_AddAnnotProc (An_ColorBar, c, string, strlen (string),
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
	char	*plat, *fname, *ctable, string[100];
	bool	tacmatch = FALSE;
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
	An_TopAnnot (string, Tadefclr.pixel);

	if (Monocolor && pda_Search (Pd, c, "ta-color-match", NULL, 
				     CPTR (tacmatch), SYMT_BOOL) && tacmatch)
		An_TopAnnot (px_FldDesc (fname), Ctclr.pixel);
	else 
		An_TopAnnot (px_FldDesc (fname), Tadefclr.pixel);

	An_TopAnnot (shift ? " contour (SHIFTED).  " : " contour.  ",
		     Tadefclr.pixel);
/*
 * Side annotation
 */
	if (Sashow)
	{
		if (! Monocolor)
		{
			sprintf (string, "%s %s %f %f", fname, ctable, 
				 center, step); 
			An_AddAnnotProc (An_ColorNumber, c, string, 
					 strlen (string), 75, TRUE, FALSE);
		}
	}
}




static void
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
	static char	fname[40], ctable[40], outrange[40];
	char	ctcolor[40], param[50];
	int	xdim, ydim;
	float	*rgrid, *grid, x0, x1, y0, y1, alt, lats, lons;
	int	pix_x0, pix_x1, pix_y0, pix_y1, linewidth;
	int	do_outrange;
	bool	labelflag, dolabels, ok, autoscale;
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
/*
 * Bail on too old data, if requested
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform: %s", platform);
		return;
	}
	if (! CAP_TimeCheck (c, pid))
		return;
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
		return;
/* 
 * Get annotation information
 */
	Sascale = 0.02;
	pda_Search (Pd, c, "sa-scale", NULL, (char *) &Sascale, SYMT_FLOAT);

	Ctlimit = 1;
	pda_Search (Pd, c, "ct-limit", NULL, (char *) &Ctlimit, SYMT_INT);

	Sashow = TRUE;
	pda_Search (Pd, c, "sa-show", NULL, (char *) &Sashow, SYMT_BOOL);
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
		return;
        }
	else if (!strcasecmp(fname, "divergence"))
	{
	    fid = F_Field (fname, 0, "vorticity", "1/s");
	    if (! (dc = GetVorticity ( &zt, c, platform, fid, &xdim, &ydim, 
				       &x0, &y0, &x1, &y1, &alt, shifted)))
		return;
	}
	else
	{
	    fid = F_Lookup (fname);
	    if (! (dc = ga_GetGrid (&zt, c, platform, fid, &xdim, &ydim, 
				    &x0, &y0, &x1, &y1, &alt, shifted)))
		return;
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
		FindCenterStep (dc, fid, Monocolor ? 10 : Ncolors,
				center, step);
		sprintf (param, "%s-center", fname);
		pd_Store (Pd, c, param, (char *) center, SYMT_FLOAT);
		sprintf (param, "%s-step", fname);
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
		sprintf (param, "%s-center", fname);
		ok &= pda_ReqSearch (Pd, c, param, qual, (char *) center, 
				     SYMT_FLOAT);
		sprintf (param, "%s-step", fname);
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
bool update;
/*
 * Deal with a station plot.
 */
{
	char	cname[30], annot[120];
	char	platform[PlatformListLen];
	char	quadrants[4][20], quadclr[30];
	char	data[100], sticon[40];
	char	*pnames[MaxPlatforms];
	PlatformId pid;
	float vscale, unitlen;
	int linewidth, shifted = FALSE, i, nplat;
	bool	tacmatch, do_vectors;
	ZebTime zt;
	XColor	color, qcolor;
	bool	quadstn[4];
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
	}
/*
 * Maybe start with top annotation.
 */
	if (! update)
	{
		if (pda_Search (Pd, c, "ta-color-match", NULL, (char *)
				&tacmatch, SYMT_BOOL) && tacmatch)
			An_TopAnnot ("Station plot (", color.pixel);
		else
			An_TopAnnot ("Station plot (", Tadefclr.pixel);
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
			An_TopAnnot (annot, Tadefclr.pixel);
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
	if (shifted)
		An_TopAnnot (" [SHIFTED]", Tadefclr.pixel);
	An_TopAnnot (").", Tadefclr.pixel);
/*
 * Side annotation.
 */
	sprintf (data, "%s %li %li %f %d ", "m/s", color.pixel, qcolor.pixel, 
		 do_vectors ? unitlen : 0, 4 /*numquads*/);

	for (i = 0; i < 4; i++)
	{
		if (quadstn[i])
			strcat (data, "station ");
		else if (quadrants[i][0])
		{
			strcat (data, quadrants[i]);
			strcat (data, " ");
		}
		else
			strcat (data, "none ");
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
bool do_vectors;
bool quadstn[4];
/*
 * Actually plot some station plot info.
 */
{
	char buf[64];
	char *label;
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
	if (! CAP_TimeCheck (c, plat))
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
bool do_vectors;
bool quadstn[4];
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
	bool filter = FALSE;
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
bool do_vectors;
bool quadstn[4];
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
bool	update;
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
	bool	tacmatch = FALSE, grid = FALSE, do_vectors, proj;
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
		static bool griped = FALSE;
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
 * Side annotation (scale vectors or barbs)
 */
	if (do_vectors)
	{
		int	length = 10;
		sprintf (data, "%s %li %f %f %f", "m/s", color.pixel, 
			 (float) length, 0.0, unitlen); 
		An_AddAnnotProc (An_ColorVector, c, data, strlen (data),
				 40, FALSE, FALSE);
	}
	else
	{
		sprintf (data, "%s %li %d", "m/s", color.pixel, (int)unitlen);
		An_AddAnnotProc (An_BarbLegend, c, data, strlen (data), 100, 
				 FALSE, FALSE);
	}

	CAP_AddStatusLine (c, platform, "(winds)", loc.l_alt, altunits, &zt);
}





static bool
CAP_VecParams (c, platform, vscale, cname, linewidth, unitlen,
	       color, do_vectors)
char *c;
char *platform;
float *vscale;
char *cname;
int *linewidth;
float *unitlen;
XColor *color;
bool *do_vectors;
/*
 * Get common parameters for vector plots.
 */
{
	char string[16];
	bool ok;
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
#ifdef notdef
	ok &= pda_ReqSearch (Pd, c, "u-field", NULL, uname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "v-field", NULL, vname, SYMT_STRING);
#endif

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
	if(! pda_Search (Pd, c, "sa-scale", NULL, (char *) &Sascale,
			 SYMT_FLOAT))
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
	char string[40], units[40], qname[4][40];
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
        sscanf (data, "%s %li %li %f %d %s %s %s %s", units, &vc, &qc, 
		&unitlen, &numquads, qname[0], qname[1], qname[2], qname[3]);
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
		if (! strcmp (qname[i], "null") || ! strcmp (qname[i], "none"))
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




void
CAP_Raster (c, update)
char	*c;
bool	update;
/*
 * Execute a CAP raster plot, based on the given plot
 * description, specified conent, and plot time
 */
{
	char	fname[20], ctname[40], data[100], hcolor[40];
	char 	platform[PlatformListLen];
	char	param[50], outrange[40];
	int	xdim, ydim, slow;
	int	nsteps;
	bool	ok, highlight, autoscale;
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
 * An out of range color is nice, sometimes.
 */
	strcpy (outrange, "black");
	pda_Search (Pd, c, "out-of-range-color", platform, outrange,
		    SYMT_STRING);

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
	else if (org == Org3dGrid || org == Org2dGrid || org == OrgIRGrid ||
		 org == OrgNSpace)
		image = FALSE;
	else
	{
		msg_ELog (EF_PROBLEM, "Can't do raster plots of %s", platform);
		return;
	}
/*
 * Bail on too old data, if requested
 */
	if (! CAP_TimeCheck (c, pid))
		return;
/*
 * Get info for highlighting and area.
 */
	highlight = FALSE;
	sprintf (param, "%s-highlight-range", fname);
	if (pda_Search (Pd, c, param, "raster", CPTR (hrange), SYMT_FLOAT)
		&& (hrange != 0.0))
	{
		highlight = TRUE;
		sprintf (param, "%s-highlight-color", fname);
		if (! pda_Search (Pd, c, param, "raster", hcolor, 
				SYMT_STRING))
			strcpy (hcolor, "white");
		sprintf (param, "%s-highlight", fname);
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
	sprintf (param, "%s-nsteps", fname);
	if (! pda_Search (Pd, c, param, "raster", CPTR (nsteps), SYMT_INT))
		nsteps = Ncolors;
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
		sprintf (param, "%s-center", fname);
		pd_Store (Pd, c, param, (char *) &center, SYMT_FLOAT);
		sprintf (param, "%s-step", fname);
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
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;

	CAP_AddStatusLine (c, platform, fname, alt, altunits, &zt);
/*
 * Top annotation
 */
	An_TopAnnot (platform, Tadefclr.pixel);
	An_TopAnnot (" ", Tadefclr.pixel);
	An_TopAnnot (px_FldDesc (fname), Tadefclr.pixel);
	An_TopAnnot (shifted ? " plot (SHIFTED).  " : " plot.  ", 
		     Tadefclr.pixel);
/*
 * Side annotation (color bar)
 */
	if (highlight)
		sprintf (data, "%s %s %f %f %d %d %f %s %f", fname, ctname, 
	  	     center, step, nsteps, highlight, hvalue, hcolor, hrange);
	else
		sprintf (data, "%s %s %f %f %d %d %f %s %f", fname, ctname, 
	  	     center, step, nsteps, highlight, 0.0, "null", 0.0);
	An_AddAnnotProc (CAP_RasterSideAnnot, c, data, strlen (data), 
		140, TRUE, FALSE);
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

		XSetForeground (XtDisplay (Graphics), Gcontext,Tadefclr.pixel);
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



static bool
CAP_TimeCheck (c, pid)
char	*c;
PlatformId	pid;
/*
 * Return TRUE if we are showing data of any age, or if time limiting is
 * enabled and available data are new enough to be within the time limit.
 * Otherwise, return FALSE.
 */
{
	bool	limit_data_age;
	int	seconds;
	char	string[16];
	ZebTime	zt;
/*
 * Data age check enabled?
 */
	limit_data_age = FALSE;	/* historical default */
	pda_Search (Pd, c, "limit-data-age", NULL, (char *) &limit_data_age,
		    SYMT_BOOL);
	if (! limit_data_age)
		return (TRUE);
/*
 * Nearest data time
 */
	if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, &zt))
	{
		msg_ELog(EF_INFO,"No data available at all for '%s'",
			 ds_PlatformName (pid));
		return (FALSE);
	}
/*
 * Get the age limit
 */	
	if (pda_Search (Pd, c, "data-age-limit", ds_PlatformName (pid), 
			string, SYMT_STRING) ||
	    pda_Search (Pd, c, "icon-age-limit", ds_PlatformName (pid), 
			string, SYMT_STRING))
		seconds = pc_TimeTrigger (string); 
	else
		seconds = 0;
/*
 * Finally, test to see if the data are recent enough
 */	
	if ((seconds > 0) && (PlotTime.zt_Sec - zt.zt_Sec) > seconds)
	{
		msg_ELog (EF_INFO, "%s data too old, not displayed", 
			  ds_PlatformName (pid));
		return (FALSE);
	}

	return (TRUE);
}



# endif  /* C_PT_CAP */
