/*
 * Vertical cross-sectioning
 */
static char *rcsid = "$Id: XSection.c,v 2.16 1994-01-31 19:36:20 burghart Exp $";
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

# if C_PT_XSECT

# ifndef hpux
# include <alloca.h>
# endif
# include <math.h>
# include <ctype.h>
# include <X11/Intrinsic.h>
# include <ui.h>
# include <ui_date.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"

/*
 * General definitions
 */
# define DEG_TO_RAD(x)	((x) * 0.017453292)
# define RAD_TO_DEG(x)	((x) * 57.29577951)
# define MAXPLAT	 20
# define BADVAL	-999.0
# define BUFLEN	1024

/*
 * Scratch string for building annotations, etc.
 */
static char	Scratch[128];

/*
 * Action to take when we hit a bad sounding
 */
# define BAD_SOUNDING	{ \
				sprintf (Scratch, " (bad)"); \
				An_TopAnnot (Scratch, White.pixel); \
				continue; \
			}

/*
 * Data plane structure
 */
typedef struct _dplane
{
	float	*data;
	int	hdim, vdim;
} DPlane;

/*
 * Zig-zag data plane structure
 */
typedef struct _zz_dplane
{
	int	nobs;		/* number of observations in this plane */
	int	vdim;
	float	*bot, *top;	/* bottom and top heights for each obs */
	float	*dist;		/* distance along the "plane" to each obs */
	ZebTime	*dtime;		/* data time for each obs */
	char	**plats;	/* platform list */
	float	*data;		/* (nobs x vdim) data array */
} ZZ_DPlane;

/*
 * The length, height, and bottom of our plane
 */
float	P_len, P_hgt, P_bot;

/*
 * User-selected grid dimensions (if any)
 */
int	Hgrid, Vgrid;

/*
 * Cross-section endpoints
 */
static float	X0 = 0.0, X1 = 0.0, Y0 = 0.0, Y1 = 0.0;
static ZebTime	T0;

/*
 * Pixel limits for the plot
 */
static int	Pix_left, Pix_right, Pix_bottom, Pix_top;

/*
 * Altitude or pressure on the vertical scale?
 * Field for vertical scale
 * Use filled contours?
 * "Zig-zag" cross-section?
 */
static bool	Use_alt = TRUE;
static char	Zfld[20];
static bool	Fill_contour;
static bool	Zig_zag;

/*
 * Drawing info
 */
static float	Contour_center, Contour_step, Vector_scale;
static int	Line_width;
static bool	Do_labels;

/*
 * Maximum acceptable time difference between sounding time and plot time
 */
static int	Maxdiff = -1;

/*
 * Points for a sounding trace
 */
static XPoint	Trace[BUFLEN];
static int	Tracelen = 0;

/*
 * Color array and indices
 */
static XColor	*Colors;
static int	Ncolors;
static XColor	White, Black;

/*
 * Clip and unclip rectangles
 */
static XRectangle	Clip, Unclip;

/*
 * Prototypes
 */
void	xs_Init FP ((UItime *));
void	xs_LineContour FP ((char *, int));
void	xs_FilledContour FP ((char *, int));
void	xs_Vector FP ((char *, int));
static void	xs_Contour FP ((char *, int)); 
static void	xs_ZZContour FP ((char **, int, char *));
static void	xs_PlaneContour FP ((char *, char **, int, char *));
static void	xs_ZZVector FP ((char **, int, char *, char *));
static void	xs_PlaneVector FP ((char *, char **, int, char *, char *));
static void	xs_ZZFillIn FP ((ZZ_DPlane *));
static void	xs_Background FP ((void));
static void	xs_ExtendTrace FP ((double, double)); 
static DPlane	*xs_HDWeighting FP ((char **, int, char *, ZebTime *));
static DPlane	*xs_Bilinear FP ((char *, char *));
static void	xs_DrawTrace FP ((char *)); 
static void	xs_AddToLevel FP ((DPlane *, DPlane *, int, double, double, 
				   double)); 
static void	xs_BuildLimits FP ((float *, float *, int, double, double, 
				    double));
static int	xs_Pos FP ((char *, ZebTime *, float **, float **)); 
static int	xs_ZIndex FP ((double, int));
static ZZ_DPlane	*xs_ZZGetData FP ((char **, int, char *));
static ZZ_DPlane	*xs_AllocZZ_DPlane FP ((int, int));
static void		xs_FreeZZ_DPlane FP ((ZZ_DPlane *));
static DataChunk	*xs_GetObsDC FP ((char *, char *, ZebTime *));
static DataChunk	*xs_GetGridDC FP ((char *, char *, ZebTime *));




void
xs_Init (t)
UItime	*t;
/*
 * Initialize for a cross-section plot.
 */
{
	float	zmax;
/*
 * Initialize the overlay times widget
 */
	lw_OvInit ("COMPONENT      PLATFORM   FIELD       TIME\n");
/*
 * Altitude or pressure z scaling?
 */
	Use_alt = TRUE;
	pda_Search (Pd, "global", "by-altitude", "xsect", (char *) &Use_alt, 
		SYMT_BOOL);

	if (Use_alt)
		strcpy (Zfld, "alt");
	else
		strcpy (Zfld, "pres");
/*
 * Altitude or pressure limits.  Take the user values if they exist, otherwise
 * default to:  altitude - 0 to 12 km, pressure - 1050 to 200 mb.
 */
	P_bot = Use_alt ? 0.0 : 1050.0;
	P_hgt = Use_alt ? 12.0 : -850.0;

	pda_Search (Pd, "global", "z-min", NULL, (char *) &P_bot, SYMT_FLOAT);
	if (pda_Search (Pd, "global", "z-max", NULL, (char *) &zmax, 
			SYMT_FLOAT))
		P_hgt = zmax - P_bot;
/*
 * User-specified maximum time difference
 */
	if (! pda_Search (Pd, "global", "max-time-diff", "xsect", 
			  (char *) &Maxdiff, SYMT_INT))
		Maxdiff = -1;
	else
		Maxdiff *= 60;	/* Convert to seconds */
/*
 * Pixel plot limits
 */
	Xlo = Ylo = 0.0;
	Xhi = Yhi = 1.0;

	Pix_left = XPIX (Xlo);
	Pix_right = XPIX (Xhi);
	Pix_bottom = YPIX (Ylo);
	Pix_top = YPIX (Yhi);
/*
 * Clip and unclip rectangles
 */
	Clip.x = Pix_left + 1;
	Clip.y = Pix_top + 1;
	Clip.width = Pix_right - Pix_left;
	Clip.height = Pix_bottom - Pix_top;

	Unclip.x = Unclip.y = 0;
	Unclip.width = GWWidth (Graphics);
	Unclip.height = GWHeight (Graphics);
/*
 * Zig-zag cross section?
 */
	Zig_zag = FALSE;
	pda_Search (Pd, "global", "zig-zag", "xsect", (char *) &Zig_zag,
		    SYMT_BOOL);

	sprintf (Scratch, "%s cross-section plot.  ", 
		 Zig_zag ? "Zig-zag" : "Planar");
	An_TopAnnot (Scratch, White.pixel);
/*
 * For planar cross sections, get the endpoints
 */
	if (! Zig_zag)
	{
	/*
	 * Left endpoint
	 */
		if (! pda_ReqSearch (Pd, "global", "left-endpoint", NULL, 
				     Scratch, SYMT_STRING))
			return;

		if (sscanf (Scratch, "%f,%f", &X0, &Y0) != 2)
		{
			msg_ELog (EF_PROBLEM, "Bad left endpoint '%s'", 
				  Scratch);
			return;
		}
	/*
	 * Right endpoint
	 */
		if (! pda_ReqSearch (Pd, "global", "right-endpoint", NULL, 
				     Scratch, SYMT_STRING))
			return;

		if (sscanf (Scratch, "%f,%f", &X1, &Y1) != 2)
		{
			msg_ELog (EF_PROBLEM, "Bad right endpoint '%s'", 
				  Scratch);
			return;
		}
	/*
	 * Find the plane length
	 */
		P_len = hypot (X1 - X0, Y1 - Y0);
	}
}




void
xs_LineContour (c, update)
/*
 * Handle a line contour cross-section component
 */
char	*c;
bool	update;
{
	Fill_contour = FALSE;

	Do_labels = TRUE;
	pda_Search (Pd, c, "do-labels", "xsect", (char *) &Do_labels, 
		    SYMT_BOOL);
	
	Line_width = 0;
	pda_Search (Pd, c, "line-width", "xsect", (char *) &Line_width,
		    SYMT_INT);

	xs_Contour (c, update);
}




void
xs_FilledContour (c, update)
/*
 * Handle a filled contour cross-section component
 */
char	*c;
bool	update;
{
	Fill_contour = TRUE;
	xs_Contour (c, update);
}




static void
xs_Contour (c, update)
char	*c;
bool	update;
/*
 * Draw a cross-section contour plot based on the given PD component.
 */
{
	bool	ok;
	int	nplat;
	char	platforms[120], *pnames[MAXPLAT], fldname[20], ctname[20];
	char	param[50];
/*
 * Platform(s).  Platform must come from the global component for zig-zag
 * plots so we don't have plots with different endpoints overlaying each
 * other.
 */
	if (Zig_zag)
	{
		ok = pda_ReqSearch (Pd, "global", "platform", NULL, platforms, 
				    SYMT_STRING);
		if (! ok)
		{
			strcpy (Scratch, "'platform' must be in the global ");
			strcat (Scratch, "component for zig-zag plots");
			msg_ELog (EF_INFO, Scratch);
		}
	}
	else
		ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, 
				    SYMT_STRING);
/*
 * Get the field, contour limits, and color table
 */
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fldname, SYMT_STRING);
	sprintf (param, "%s-center", fldname);
	ok &= pda_ReqSearch (Pd, c, param, "contour", (char *) &Contour_center,
		SYMT_FLOAT);
	sprintf (param, "%s-step", fldname);
	ok &= pda_ReqSearch (Pd, c, param, "contour", (char *) &Contour_step, 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "color-table", "xsect", ctname, 
		SYMT_STRING);

	if (! ok)
		return;
/*
 * Load the color table + explicitly get black and white
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);

	ct_GetColorByName ("black", &Black);
	ct_GetColorByName ("white", &White);
/*
 * Grid dimensions
 */
	Hgrid = Vgrid = 0;
	pda_Search (Pd, c, "hgrid", NULL, (char *) &Hgrid, SYMT_INT);
	pda_Search (Pd, c, "vgrid", NULL, (char *) &Vgrid, SYMT_INT);
/*
 * Pull apart the platforms (and test for how many, albeit a little late
 * since we'll write past the end of the pnames array if there really are
 * too many platforms.)
 */
	nplat = CommaParse (platforms, pnames);
	if (nplat > MAXPLAT)
	{
		msg_ELog (EF_PROBLEM, "Too many platforms!");
		return;
	}
/*
 * Actually draw the cross-section based on type
 */
	if (Zig_zag)
		xs_ZZContour (pnames, nplat, fldname);
	else
		xs_PlaneContour (c, pnames, nplat, fldname);
/*
 * Draw the background and set up for side annotation
 */
	xs_Background ();

	if (Fill_contour)
	{
		sprintf (Scratch, "%s %s %f %f", fldname, ctname, 
			 Contour_center - Contour_step / 2, Contour_step); 
	/*			 
	 * (We have to subtract half a step from the center value that goes to
	 * An_ColorBar to make it label things correctly)
	 */
		An_AddAnnotProc (An_ColorBar, c, Scratch, strlen (Scratch),
				 75, TRUE, FALSE);
	}
	else
	{
		sprintf (Scratch, "%s %s %f %f", fldname, ctname, 
			 Contour_center, Contour_step); 
		An_AddAnnotProc (An_ColorNumber, c, Scratch, strlen (Scratch), 
				 75, TRUE, FALSE);
	}
}




void
xs_Vector (c, update)
char	*c;
bool	update;
/*
 * Draw vectors from a cross-section based on the given PD component.
 */
{
	bool	ok;
	char	platforms[80], ufldname[20], vfldname[20], *pnames[MAXPLAT];
	char	cname[20];
	int	nplat;
	float	unitlen;
/*
 * Platform(s).  Platform must come from the global component for zig-zag
 * plots so we don't have plots with different endpoints overlaying each
 * other.
 */
	if (Zig_zag)
	{
		ok = pda_ReqSearch (Pd, "global", "platform", NULL, platforms, 
				    SYMT_STRING);
		if (! ok)
		{
			strcpy (Scratch, "'platform' must be in the global ");
			strcat (Scratch, "component for zig-zag plots");
			msg_ELog (EF_INFO, Scratch);
		}
	}
	else
		ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, 
				    SYMT_STRING);
/*
 * Get the fields and arrow scale factor
 */
	ok &= pda_ReqSearch (Pd, c, "u-field", "xsect", ufldname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "v-field", "xsect", vfldname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "arrow-scale", "xsect", 
			     (char *) &Vector_scale, SYMT_FLOAT);

	if (! ok)
		return;
/*
 * Arrow color and line width
 */
	strcpy (cname, "white");
	if (! pda_Search (Pd, c, "arrow-color", "xsect", cname, SYMT_STRING))
		pda_Search (Pd, c, "color", "xsect", cname, SYMT_STRING);
	if (! ct_GetColorByName (cname, &Colors[0]))
	{
		msg_ELog (EF_PROBLEM, "Can't get arrow color '%s'!", cname);
		return;
	}

	Line_width = 0;
	pda_Search (Pd, c, "line-width", "xsect", (char *) &Line_width,
		    SYMT_INT);
/*
 * Grid dimensions
 */
	Hgrid = Vgrid = 0;
	pda_Search (Pd, c, "hgrid", "xsect", (char *) &Hgrid, SYMT_INT);
	pda_Search (Pd, c, "vgrid", "xsect", (char *) &Vgrid, SYMT_INT);
/*
 * Pull apart the platforms (and test for how many, albeit a little late
 * since we'll write past the end of the pnames array if there really are
 * too many platforms.)
 */
	nplat = CommaParse (platforms, pnames);
	if (nplat > MAXPLAT)
	{
		msg_ELog (EF_PROBLEM, "Too many platforms!");
		return;
	}
/*
 * Actually draw the cross-section based on type
 */
	
	if (Zig_zag)
		xs_ZZVector (pnames, nplat, ufldname, vfldname);
	else
		xs_PlaneVector (c, pnames, nplat, ufldname, vfldname);
/*
 * Draw the background and set up to draw side annotation (scale vector)
 */
	xs_Background ();

	sprintf (Scratch, "%s %d %f %f %f", "10m/s", Colors[0].pixel, 10.0, 
		 0.0, Vector_scale * USABLE_HEIGHT); 
	An_AddAnnotProc (An_ColorVector, c, Scratch, strlen (Scratch),
		40, FALSE, FALSE);

}




static void
xs_ZZContour (pnames, nplat, fldname)
char	**pnames, *fldname;
int	nplat;
/*
 * Draw a zig-zag contour plot given the platform string and field name
 */
{
	ZZ_DPlane	*plane;
	float		*data, *dist;
	int		vdim, diff, p;
	ZebTime		dtime;

	sprintf (Scratch, "Contour of %s using: ", fldname);
	An_TopAnnot (Scratch, White.pixel);
/*
 * Get the filled data plane
 */
	plane = xs_ZZGetData (pnames, nplat, fldname);

	if (plane->nobs < 2)
	{
		msg_ELog (EF_INFO, 
			  "Nothing drawn (< 2 good soundings)");
		return;
	}
/*
 * I want shorter names to get at some pieces of the data plane structure
 */
	data = plane->data;
	dist = plane->dist;
	vdim = plane->vdim;
/*
 * Set user coordinates for the graphics
 */
	Xlo = 0.0;
	Xhi = P_len = dist[plane->nobs - 1];
	Ylo = P_bot;
	Yhi = P_bot + P_hgt;
/*
 * Draw the contour in sections
 */
	for (p = 0; p < plane->nobs; p++)
	{
	/*
	 * Contour between this observation and the next one
	 */
		if (p < plane->nobs - 1)
		{
			if (Fill_contour)
			{
				FC_Init (Colors, Ncolors, Ncolors / 2, Black, 
					 Clip, TRUE, BADVAL);
				FillContour (Graphics, GWFrame (Graphics), 
					     data + p * vdim, 2, vdim,
					     XPIX (dist[p]), Pix_bottom, 
					     XPIX (dist[p+1]), Pix_top, 
					     Contour_center, Contour_step);
			}
			else
			{
				CO_Init (Colors, Ncolors, Ncolors / 2, Black, 
					 Clip, TRUE, BADVAL);
				Contour (Graphics, GWFrame (Graphics), 
					 data + p * vdim, 2, vdim, 
					 XPIX (dist[p]), Pix_bottom, 
					 XPIX (dist[p+1]), Pix_top, 
					 Contour_center, Contour_step, 
					 Do_labels, Line_width);
			}
		}
	/*
	 * Site label
	 */
		strcpyUC (Scratch, plane->plats[p]);
		XSetForeground (XtDisplay (Graphics), Gcontext, 
				White.pixel);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			  XPIX (dist[p]), Pix_bottom + 3, Scratch, 
			  0.0, 0.02, JustifyCenter, JustifyTop);
	/*
	 * Trace of altitude span
	 */
		xs_ExtendTrace (dist[p], plane->bot[p]);
		xs_ExtendTrace (dist[p], plane->top[p]);
		xs_DrawTrace ("");
	/*
	 * Top annotation: site name and data time (or date and time if
	 * the difference from the plot time is > 12 hours).
	 */
		if (p != 0)
			An_TopAnnot (", ", White.pixel);

		An_TopAnnot (plane->plats[p], White.pixel);

		sprintf (Scratch, " (");

		dtime = plane->dtime[p];
		diff = PlotTime.zt_Sec - dtime.zt_Sec;

		if (diff > 43200)
			TC_EncodeTime (&dtime, TC_Full, Scratch + 2);
		else
			TC_EncodeTime (&dtime, TC_TimeOnly, 
				       Scratch + 2);

		strcat (Scratch + strlen (Scratch) - 3, ")");

		An_TopAnnot (Scratch, White.pixel);
	}
	
	An_TopAnnot (".  ", White.pixel);

	xs_FreeZZ_DPlane (plane);
}




static void
xs_ZZVector (pnames, nplat, ufldname, vfldname)
char	**pnames, *ufldname, *vfldname;
int	nplat;
/*
 * Draw a zig-zag vector plot given the platform string and field names
 */
{
	ZZ_DPlane	*uplane, *vplane, *plane;
	float		*udata, *vdata, *dist;
	int		vdim, diff, p;
	ZebTime		dtime;

	sprintf (Scratch, "Vectors of (%s,%s) using: ", ufldname, vfldname);
	An_TopAnnot (Scratch, White.pixel);
/*
 * Get the filled data planes.  
 */
	uplane = xs_ZZGetData (pnames, nplat, ufldname);
	vplane = xs_ZZGetData (uplane->plats, uplane->nobs, vfldname);

	if (uplane->nobs != vplane->nobs)
	{
		msg_ELog (EF_PROBLEM, "Not all plats that have '%s' have '%s'",
			  ufldname, vfldname);
		return;
	}

	if (uplane->nobs < 2)
	{
		msg_ELog (EF_INFO, 
			  "Nothing drawn (< 2 good soundings)");
		return;
	}
/*
 * Info other than the actual data should be the same between uplane
 * and vplane, so we'll just create a generic "plane" to get at it
 */
	plane = uplane;
/*
 * I want shorter names to get at some pieces of the data plane structures
 */
	udata = uplane->data;
	vdata = vplane->data;

	dist = plane->dist;
	vdim = plane->vdim;
/*
 * Set user coordinates for the graphics
 */
	Xlo = 0.0;
	Xhi = P_len = dist[plane->nobs - 1];
	Ylo = P_bot;
	Yhi = P_bot + P_hgt;
/*
 * Loop through the observations (platforms)
 */
	for (p = 0; p < plane->nobs; p++)
	{
	/*
	 * Draw the vectors for this observation
	 */
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 
				    Line_width, LineSolid, CapButt, JoinMiter);
		VectorGrid (Graphics, GWFrame (Graphics), Gcontext, 
			    udata + p * vdim, vdata + p * vdim, 1, vdim, 
			    XPIX (dist[p]), Pix_bottom, XPIX (dist[p]), 
			    Pix_top, Vector_scale, BADVAL, Colors[0], 1);
	/*
	 * Return the line width to the default of zero, since this is a
	 * shared GC
	 */
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 
				    0, LineSolid, CapButt, JoinMiter);
	/*
	 * Site label
	 */
		strcpyUC (Scratch, plane->plats[p]);
		XSetForeground (XtDisplay (Graphics), Gcontext, 
				White.pixel);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			  XPIX (dist[p]), Pix_bottom + 3, Scratch, 
			  0.0, 0.02, JustifyCenter, JustifyTop);
	/*
	 * Trace of altitude span
	 */
		xs_ExtendTrace (dist[p], plane->bot[p]);
		xs_ExtendTrace (dist[p], plane->top[p]);
		xs_DrawTrace ("");
	/*
	 * Top annotation: site name and data time (or date and time if
	 * the difference from the plot time is > 12 hours).
	 */
		if (p != 0)
			An_TopAnnot (", ", White.pixel);

		An_TopAnnot (plane->plats[p], White.pixel);

		sprintf (Scratch, " (");

		dtime = plane->dtime[p];
		diff = PlotTime.zt_Sec - dtime.zt_Sec;

		if (diff > 43200)
			TC_EncodeTime (&dtime, TC_Full, Scratch + 2);
		else
			TC_EncodeTime (&dtime, TC_TimeOnly, 
				       Scratch + 2);

		strcat (Scratch + strlen (Scratch) - 3, ")");

		An_TopAnnot (Scratch, White.pixel);
	}
	
	An_TopAnnot (".  ", White.pixel);

	xs_FreeZZ_DPlane (uplane);
	xs_FreeZZ_DPlane (vplane);
}




static ZZ_DPlane *
xs_ZZGetData (pnames, nplat, fldname)
char	**pnames, *fldname;
int	nplat;
/*
 * Create and return a zig-zag "pseudo-planar" data plane.  The caller is
 * responsible for calling xs_FreeZZ_DPlane() to destroy the plane.
 */
{
	int	p, pt, npts, zndx, zndx_prev, nobs;
	int	row, col, vdim, hdim;
	float	badvalue, fdata, val, val_prev, frac;
	float	z, zpos, zstep, z_prev;
	float	loc_x[MAXPLAT], loc_y[MAXPLAT];
	ZebTime	dtime;
	DataChunk	*dc;
	Location	loc;
	ZZ_DPlane	*plane;
/*
 * Get the plane structure
 */
	vdim = Vgrid ? Vgrid : 50;
	hdim = nplat;
	plane = xs_AllocZZ_DPlane (hdim, vdim);
/*
 * Initialize to the bad value flag
 */
	for (pt = 0; pt < vdim * hdim; pt++)
		plane->data[pt] = BADVAL;
/*
 * Vertical grid spacing
 */
	zstep = P_hgt / (float)(vdim - 1);
/*
 * Loop through the platforms
 */
	for (p = 0; p < nplat; p++)
	{
	/*
	 * Get the data
	 */
		if (! (dc = xs_GetObsDC (pnames[p], fldname, &dtime)))
		{
			TC_EncodeTime (&PlotTime, TC_Full, Scratch);
			msg_ELog (EF_INFO, "No data for '%s' at %s", pnames[p],
				  Scratch);
			continue;
		}
	/*
	 * Update the structure
	 */
		col = plane->nobs++;

		plane->plats[col] = (char *) 
			malloc ((strlen (pnames[p]) + 1) * sizeof (char));
		strcpy (plane->plats[col], pnames[p]);

		plane->dtime[col] = dtime;

		plane->bot[col] = 99e6;
		plane->top[col] = -99e6;
	/*
	 * Stash the platform location
	 */
		dc_GetLoc (dc, 0, &loc);
		cvt_ToXY (loc.l_lat, loc.l_lon, &loc_x[col], &loc_y[col]);
	/*
 	 * Get some info from the data chunk
	 */
		npts = dc_GetNSample (dc);
		badvalue = dc_GetBadval (dc);
	/*
	 * Loop through the points
	 */
		val_prev = badvalue;

		for (pt = 0; pt < npts; pt++)
		{
		/*
		 * Bag this point if the datum or position is bad
		 */
			fdata = dc_GetScalar (dc, pt, F_Lookup (fldname));
			zpos = dc_GetScalar (dc, pt, F_Lookup (Zfld));

			if (fdata == badvalue || zpos == badvalue)
				continue;
		/*
		 * Special treatment for the first good point
		 */
			if (val_prev == badvalue)
			{
			/*
			 * Assign the previous point values
			 */
				val_prev = fdata;
				z_prev = zpos;
				zndx_prev = xs_ZIndex (zpos, vdim);
			/*
			 * Go on to the next point
			 */
				continue;
			}
		/*
		 * Adjust bottom or top if necessary
		 */
			plane->bot[p] = zpos < plane->bot[p] ? 
				zpos : plane->bot[p];
			plane->top[p] = zpos > plane->top[p] ? 
				zpos : plane->top[p];
		/*
		 * Quit when we get above the grid
		 */
			if (zndx_prev >= vdim)
				break;
		/*
		 * Find the index of the next grid height at or above zpos[pt]
		 */
			zndx = xs_ZIndex (zpos, vdim);
		/*
		 * Assign values in Plane between this and the previous point
		 */
			for (row = zndx_prev; row < zndx && row < vdim; row++)
			{
			/*
			 * Don't assign anything below the first grid level
			 */
				if (row < 0)
					continue;
			/*
			 * Find the height of this grid index and interpolate
			 * the data and position to this height
			 */
				z = row * zstep + P_bot;
				frac = (z - z_prev) / (zpos - z_prev);

				val = val_prev + frac * (fdata - val_prev);
			/*
			 * Insert this point into the Plane
			 */
				plane->data[col * vdim + row] = val;
			}
		/*
		 * Make this the previous point
		 */
			val_prev = fdata;
			z_prev = zpos;
			zndx_prev = zndx;
		}
	/*
	 */
		if (val_prev == badvalue)
		{
			TC_EncodeTime (&dtime, TC_Full, Scratch);
			msg_ELog (EF_PROBLEM, 
				  "%s or %s data all bad for %s at %s",
				  fldname, Zfld, pnames[p], Scratch);
		}
	}
/*
 * Find the position of each sounding
 */
	plane->dist[0] = 0.0;
	for (p = 1; p < plane->nobs; p++)
		plane->dist[p] = plane->dist[p-1] + 
		    hypot ((loc_x[p] - loc_x[p-1]), (loc_y[p] - loc_y[p-1]));
/*
 * Interpolate data where possible
 */
	xs_ZZFillIn (plane);
	return (plane);
}




static void
xs_ZZFillIn (plane)
ZZ_DPlane	*plane;
/*
 * Interpolate data horizontally in the ZZ_DPlane structure.
 */
{
	int	col, row, prev, next;
	float	*data = plane->data;
	int	vdim = plane->vdim;
	float	*dist = plane->dist;
/*
 * We can potentially interpolate to any of the interior columns (all but the
 * first and last)
 */
	for (col = 1; col < plane->nobs - 1; col++)
	{
		for (row = 0; row < vdim; row++)
		{
		/*
		 * Move on if we have data here
		 */
			if (data[col * vdim + row] != BADVAL)
				continue;
		/*
		 * We have a hole, so try to interpolate
		 *
		 * Find the closest columns on either side with good data in
		 * this row.  If we don't have good data on both sides, we
		 * can't interpolate.
		 */
			for (prev = col - 1; prev >= 0; prev--)
				if (data[prev * vdim + row] != BADVAL)
					break;

			if (prev < 0)
				continue;


			for (next = col + 1; next < plane->nobs; next++)
				if (data[next * vdim + row] != BADVAL)
					break;

			if (next >= plane->nobs)
				continue;
		/*
		 * We have good data on both sides, so do the interpolation
		 */
			data[col * vdim + row] = ((dist[next] - dist[col]) * 
				data[prev * vdim + row] + (dist[col] - 
				dist[prev]) * data[next * vdim + row]) /
				(dist[next] - dist[prev]);
		}
	}
}


			

static ZZ_DPlane *
xs_AllocZZ_DPlane (maxobs, vdim)
int	maxobs, vdim;
/*
 * Allocate a ZZ_Dplane structure and its arrays, with enough space for
 * up to maxobs observations and a vertical dimension of vdim.
 */
{
	int	i;
	ZZ_DPlane	*plane = (ZZ_DPlane *) malloc (sizeof (ZZ_DPlane));

	plane->nobs = 0;
	plane->vdim = vdim;
	plane->bot = (float *) malloc (maxobs * sizeof (float));
	plane->top = (float *) malloc (maxobs * sizeof (float));
	plane->dist = (float *) malloc (maxobs * sizeof (float));
	plane->dtime = (ZebTime *) malloc (maxobs * sizeof (ZebTime));
	plane->plats = (char **) malloc (maxobs * sizeof (char *));
	plane->data = (float *) malloc (vdim * maxobs * sizeof (float));

	for (i = 0; i < vdim * maxobs; i++)
		plane->data[i] = BADVAL;
	
	return (plane);
}




static void
xs_FreeZZ_DPlane (plane)
ZZ_DPlane	*plane;
/*
 * Free the given ZZ_DPlane structure and its component parts
 */
{
	int	i;

	for (i = 0; i < plane->nobs; i++)
		free (plane->plats[i]);

	free (plane->bot);
	free (plane->top);
	free (plane->dist);
	free (plane->dtime);
	free (plane->plats);
	free (plane->data);
	free (plane);
}




static void
xs_PlaneContour (c, pnames, nplat, fldname)
char	*c, **pnames, *fldname;
int	nplat;
/*
 * Draw a contour of a planar cross-section
 */
{
	int	i;
	DPlane	*plane;
/*
 * Annotate
 */
	sprintf (Scratch, "Contour of %s using: ", fldname);
	An_TopAnnot (Scratch, White.pixel);
/*
 * Find the gridding method and grab the data
 */
	strcpy (Scratch, "default");
	pda_Search (Pd, c, "grid-method", "xsect", Scratch, SYMT_STRING);

	if (! strcmp (Scratch, "bilinear"))
	{
	/*
	 * Complain if we got more than one platform
	 */
		if (nplat > 1)
		   msg_ELog (EF_INFO, 
		      "Only one platform used for bilinear method x-sections");
	/*
	 * Add this platform to the top annotation
	 */
		An_TopAnnot (pnames[0], White.pixel);
		An_TopAnnot (".  ", White.pixel);
	/*
	 * Get the data
	 */
		plane = xs_Bilinear (pnames[0], fldname);
	}
	else
	{
		ZebTime	*times = (ZebTime *) alloca (nplat * sizeof (ZebTime));
		int	diff;
	/*
	 * Get the data
	 */
		plane = xs_HDWeighting (pnames, nplat, fldname, times);
	/*
	 * Annotate with the platforms
	 */
		for (i = 0; i < nplat; i++)
		{
			if (i != 0)
				An_TopAnnot (", ", White.pixel);

			An_TopAnnot (pnames[i], White.pixel);
		/*
		 * Annotate the sounding with a time or date and time if the
		 * difference from the plot time is > 12 hours.  
		 */
			sprintf (Scratch, " (");

			diff = PlotTime.zt_Sec - times[i].zt_Sec;

			if (times[i].zt_Sec == 0)
				strcat (Scratch, "BAD");
			else if (diff > 43200)
				TC_EncodeTime (&times[i], TC_Full, 
					       Scratch + 2);
			else
				TC_EncodeTime (&times[i], TC_TimeOnly, 
					       Scratch + 2);

			strcat (Scratch + strlen (Scratch) - 3, ")");

			An_TopAnnot (Scratch, White.pixel);
		}
		An_TopAnnot (".  ", White.pixel);
	}

	if (! plane)
		return;
/*
 * Draw the contours
 */
	if (Fill_contour)
	{
		FC_Init (Colors, Ncolors, Ncolors / 2, Black, Clip, 
			TRUE, BADVAL);
		FillContour (Graphics, GWFrame (Graphics), plane->data, 
			     plane->hdim, plane->vdim, Pix_left, Pix_bottom, 
			     Pix_right, Pix_top, Contour_center, Contour_step);
	}
	else
	{
		CO_Init (Colors, Ncolors, Ncolors / 2, Black, Clip, 
			TRUE, BADVAL);
		Contour (Graphics, GWFrame (Graphics), plane->data, 
			 plane->hdim, plane->vdim, Pix_left, Pix_bottom, 
			 Pix_right, Pix_top, Contour_center, Contour_step, 
			 Do_labels, Line_width);
	}
/*
 * Clean up
 */
	free (plane->data);
	free (plane);
	return;
}




static void
xs_PlaneVector (c, pnames, nplat, ufldname, vfldname)
char	*c, **pnames, *ufldname, *vfldname;
int	nplat;
{
	int	i;
	DPlane	*uplane, *vplane;
/*
 * Annotate
 */
	sprintf (Scratch, "Vectors of (%s,%s) using: ", ufldname, vfldname);
	An_TopAnnot (Scratch, White.pixel);
/*
 * Find the gridding method and grab the data
 */
	strcpy (Scratch, "default");
	pda_Search (Pd, c, "grid-method", "xsect", Scratch, SYMT_STRING);

	if (! strcmp (Scratch, "bilinear"))
	{
	/*
	 * Complain if we got more than one platform
	 */
		if (nplat > 1)
		   msg_ELog (EF_INFO, 
		      "Only one platform used for bilinear method x-sections");
	/*
	 * Add this platform to the top annotation
	 */
		An_TopAnnot (pnames[0], White.pixel);
		An_TopAnnot (".  ", White.pixel);
	/*
	 * Get the data
	 */
		uplane = xs_Bilinear (pnames[0], ufldname);
		vplane = xs_Bilinear (pnames[0], vfldname);
	}
	else
	{
		ZebTime	*times = (ZebTime *) alloca (nplat * sizeof (ZebTime));
		int	diff;
	/*
	 * Get the data
	 */
		uplane = xs_HDWeighting (pnames, nplat, ufldname, times);
		vplane = xs_HDWeighting (pnames, nplat, vfldname, times);
	/*
	 * Annotate with the platforms
	 */
		for (i = 0; i < nplat; i++)
		{
			if (i != 0)
				An_TopAnnot (", ", White.pixel);

			An_TopAnnot (pnames[i], White.pixel);
		/*
		 * Annotate the sounding with a time or date and time if the
		 * difference from the plot time is > 12 hours.  
		 */
			sprintf (Scratch, " (");

			diff = PlotTime.zt_Sec - times[i].zt_Sec;

			if (times[i].zt_Sec == 0)
				strcat (Scratch, "BAD");
			else if (diff > 43200)
				TC_EncodeTime (&times[i], TC_Full, 
					       Scratch + 2);
			else
				TC_EncodeTime (&times[i], TC_TimeOnly, 
					       Scratch + 2);

			strcat (Scratch + strlen (Scratch) - 3, ")");

			An_TopAnnot (Scratch, White.pixel);
		}
		An_TopAnnot (".  ", White.pixel);
	}

	if (! uplane || ! vplane)
	{
		if (uplane)
		{
			free (uplane->data);
			free (uplane);
		}
		else if (vplane)
		{
			free (vplane->data);
			free (vplane);
		}

		return;
	}
/*
 * Draw the vectors
 */
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, Line_width, 
			    LineSolid, CapButt, JoinMiter);
	VectorGrid (Graphics, GWFrame (Graphics), Gcontext, uplane->data, 
		    vplane->data, uplane->hdim, uplane->vdim, 
		    Pix_left, Pix_bottom, Pix_right, Pix_top, Vector_scale, 
		    BADVAL, Colors[0], 1);
/*
 * Return the line width to the default of zero, since this is a
 * shared GC
 */
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, LineSolid, 
			    CapButt, JoinMiter);
/*
 * Clean up
 */
	free (uplane->data);
	free (vplane->data);
	free (uplane);
	free (vplane);

	return;
}




static DPlane *
xs_HDWeighting (pnames, nplat, fldname, times)
char	**pnames, *fldname;
int	nplat;
ZebTime	*times;
/*
 * Create and fill a cross-section array with data from the chosen 
 * soundings, using a horizontal distance weighting scheme.  Return the 
 * data times for each platform also, with a time of zero if data for a
 * given platform are non-existent or bad.
 */
{
	int	pt, plat, npts, iz, zndx, zndx_prev, ih, iv, hdim, vdim;
	int	i, j, diff, offset;
	float	val, x, y, z, val_prev, x_prev, y_prev, z_prev, t_prev;
	float	zstep, hlen, frac, badvalue;
	float	fdata, *xpos = NULL, *ypos = NULL, zpos;
	float	xhighest, yhighest, zhighest;
	float	*floor, *ceiling, *f_wgt, *c_wgt;
	ZebTime	dtime, badtime;
	DataChunk	*dc;
	DPlane	*plane, *p_wgt;
/*
 * Grid size
 */
	hdim = Hgrid ? Hgrid : 50;
	vdim = Vgrid ? Vgrid : 50;
/*
 * Set user coordinates for pixel conversions
 */
	Xlo = 0.0;
	Xhi = P_len;

	Ylo = P_bot;
	Yhi = P_bot + P_hgt;
/*
 * Allocate space for the plane and weight arrays.
 */
	plane = (DPlane *) malloc (sizeof (DPlane));

	plane->hdim = hdim;
	plane->vdim = vdim;
	plane->data = (float *) malloc (hdim * vdim * sizeof (float));
/*
 * Get the plane weight array plus the floor and ceiling arrays and 
 * their associated weight arrays.  These arrays are all only needed
 * for the duration of this routine, so we can use alloca.
 */
	p_wgt = (DPlane *) alloca (sizeof (DPlane));
	p_wgt->hdim = hdim;
	p_wgt->vdim = vdim;
	p_wgt->data = (float *) alloca (hdim * vdim * sizeof (float));

	floor = (float *) alloca (hdim * sizeof (float));
	f_wgt = (float *) alloca (hdim * sizeof (float));
	ceiling = (float *) alloca (hdim * sizeof (float));
	c_wgt = (float *) alloca (hdim * sizeof (float));
/*
 * Fill the plane with BADVALs and set the weights to zero.
 * Initialize the floor and ceiling arrays also.
 */
	for (i = 0; i < hdim; i++)
	{
		for (j = 0; j < vdim; j++)
		{
			offset = (i * vdim) + j;

			plane->data[offset] = BADVAL;
			p_wgt->data[offset] = 0.0;
		}

		floor[i] = P_bot;
		f_wgt[i] = 0.0;
		ceiling[i] = P_bot + P_hgt;
		c_wgt[i] = 0.0;
	}
/*
 * Vertical grid spacing
 */
	zstep = P_hgt / (float)(vdim - 1);
/*
 * Create a bogus time for reporting bad soundings
 */
	badtime.zt_Sec = badtime.zt_MicroSec = 0;
/*
 * Loop through the platforms
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Initialize for keeping track of the highest point
	 */
		if (Use_alt)
			zhighest = -9999.0;
		else
			zhighest = 9999.0;
	/*
	 * Get the data
	 */
		if (! (dc = xs_GetObsDC (pnames[plat], fldname, &dtime)))
		{
			times[plat] = badtime;
			continue;
		}
		else
			times[plat] = dtime;
	/*
 	 * Get some info from the data chunk
	 */
		npts = dc_GetNSample (dc);
		badvalue = dc_GetBadval (dc);
	/*
	 * Put together the x,y position data
	 */
		if (! xs_Pos (pnames[plat], &dtime, &xpos, &ypos))
		{
			TC_EncodeTime (&dtime, TC_Full, Scratch);
			msg_ELog (EF_PROBLEM, "No '%s' position data at %s", 
				pnames[plat], Scratch);
			times[plat] = badtime;
			continue;
		}
	/*
	 * Loop through the points
	 */
		val_prev = badvalue;

		for (pt = 0; pt < npts; pt++)
		{
		/*
		 * Bag this point if the datum or position is bad
		 */
			if (xpos[pt] == badvalue || ypos[pt] == badvalue)

					continue;
	
			fdata = dc_GetScalar (dc, pt, F_Lookup (fldname));
			zpos = dc_GetScalar (dc, pt, F_Lookup (Zfld));

			if (fdata == badvalue || zpos == badvalue)
				continue;
		/*
		 * Update the ceiling if this point is higher
		 */
			if ((Use_alt && zpos > zhighest) ||
				(!Use_alt && zpos < zhighest))
			{
				xhighest = xpos[pt];
				yhighest = ypos[pt];
				zhighest = zpos;
			}
		/*
		 * Special treatment for the first good point
		 */
			if (val_prev == badvalue)
			{
			/*
			 * Assign the previous point values
			 */
				val_prev = fdata;
				z_prev = zpos;
				zndx_prev = xs_ZIndex (zpos, vdim);

				x_prev = xpos[pt];
				y_prev = ypos[pt];
			/*
			 * Use the point to help build the floor array
			 */
				xs_BuildLimits (floor, f_wgt, hdim, xpos[pt], 
					ypos[pt], zpos);
			/*
			 * Go on to the next point
			 */
				continue;
			}
		/*
		 * Quit when we get above the grid
		 */
			if (zndx_prev >= vdim)
				break;
		/*
		 * Find the index of the next grid height at or above zpos[pt]
		 */
			zndx = xs_ZIndex (zpos, vdim);
		/*
		 * Assign values at grid levels between this point and the
		 * previous one
		 */
			for (iz = zndx_prev; iz < zndx && iz < vdim; iz++)
			{
			/*
			 * Don't assign anything below the first grid level
			 */
				if (iz < 0)
					continue;
			/*
			 * Find the height of this grid index and interpolate
			 * the data and position to this height
			 */
				z = iz * zstep + P_bot;
				frac = (z - z_prev) / (zpos - z_prev);

				val = val_prev + frac * (fdata - val_prev);

				x = x_prev + frac * (xpos[pt]-x_prev);
				y = y_prev + frac * (ypos[pt]-y_prev);
			/*
			 * Add this datum in at the current height index
			 */
				xs_AddToLevel (plane, p_wgt, iz, x, y, val);
			}
		/*
		 * Project this point onto the plane, and add a point to 
		 * the trace for this sounding
		 */
			hlen = hypot (xpos[pt] - X0, ypos[pt] - Y0) *
				cos (atan2 (ypos[pt]-Y0, xpos[pt]-X0) -
				atan2 (Y1-Y0, X1-X0));

			xs_ExtendTrace (hlen, zpos);
		/*
		 * Make this the previous point
		 */
			val_prev = fdata;
			z_prev = zpos;
			zndx_prev = zndx;
			x_prev = xpos[pt];
			y_prev = ypos[pt];
		}
	/*
	 * Free the position arrays returned by xs_Pos ()
	 */
		if (xpos)
			free (xpos);

		if (ypos)
			free (ypos);
	/*
	 * Only go on if we had at least one good data value
	 */
		if (val_prev == badvalue)
		{
			TC_EncodeTime (&dtime, TC_Full, Scratch);
			msg_ELog (EF_PROBLEM, 
				"%s or %s data missing for %s at %s",
				fldname, Zfld, pnames[plat], Scratch);
			times[plat] = badtime;
			continue;
		}
	/*
	 * Use the highest point of this sounding to help build the 
	 * ceiling array
	 */
		xs_BuildLimits (ceiling, c_wgt, hdim, xhighest, yhighest, 
				zhighest);
	/*
	 * Draw the trace for this sounding
	 */
		xs_DrawTrace (pnames[plat]);
	/*
	 * The plane array is populated, free the allocated memory and return
	 */
		dc_DestroyDC (dc);
	}
/*
 * Remove data below the floor and above the ceiling
 */
	for (ih = 0; ih < hdim; ih++)
	{
		for (iv = 0; iv < xs_ZIndex (floor[ih], vdim); iv++)
			plane->data[ih * vdim + iv] = BADVAL;

		for (iv = xs_ZIndex (ceiling[ih], vdim); iv < vdim; iv++)
			plane->data[ih * vdim + iv] = BADVAL;
	}

	return (plane);
}



static DPlane *
xs_Bilinear (platform, fldname)
char	*platform, *fldname;
/*
 * Create a cross-section array with data from a cartesian grid, using
 * bilinear interpolation.  It is assumed that the data source will be a
 * 3d cartesian grid.  The DPlane is alloc'ed here and should be free'd by the
 * caller.  Return NULL if there's a problem.
 */
{
	int	len, h, v, i, j, k, hdim, vdim;
	float	*sourcegrid, *sgp, sgbad, *pp;
	float	f_i0, f_istep, f_j0, f_jstep, f_k0, f_kstep, f_i, f_j;
	float	grid_x0, grid_y0, grid_z0, di, dj, val0, val1, val2, val3;
	RGrid	rg;
	DPlane	*plane;
	ZebTime dtime;
	Location	loc;
	DataChunk	*dc;
/*
 * Can't use pressure for the vertical scale for this type of plot
 */
	if (! Use_alt)
	{
		msg_ELog (EF_PROBLEM, 
			  "Can't do bilinear plots with pressure vert. scale");
		return (NULL);
	}
/*
 * Get the data
 */
	if (! (dc = xs_GetGridDC (platform, fldname, &dtime)))
		return (NULL);
/*
 * Get the info we need from the data chunk
 */
	sourcegrid = dc_RGGetGrid (dc, 0, F_Lookup (fldname), &loc, &rg, &len);
	cvt_ToXY (loc.l_lat, loc.l_lon, &grid_x0, &grid_y0);
	grid_z0 = loc.l_alt;

	sgbad = dc_GetBadval (dc);
/*
 * If we don't have enough points for interpolation, just return a 1x1 plane
 * with a bad flag in it.
 */
	if (rg.rg_nX < 2 || rg.rg_nY < 2 || rg.rg_nZ < 2)
	{
		msg_ELog (EF_INFO, 
		  "xs_Bilinear: Can't interpolate from %dx%dx%d %s data\n",
		  rg.rg_nX, rg.rg_nY, rg.rg_nZ, platform);

		plane = (DPlane *) malloc (sizeof (DPlane));
		plane->hdim = plane->vdim = 1;
		plane->data = (float *) malloc (sizeof (float));
		plane->data[0] = BADVAL;

		dc_DestroyDC (dc);
		return (plane);
	}
/*
 * Use the specified grid size, if any.  Otherwise use x spacing of the source
 * grid as the horizontal spacing for our plane and the z spacing of the source
 * grid as the vertical spacing for the plane.
 */
	hdim = Hgrid ? Hgrid : (P_len / rg.rg_Xspacing) + 1;
	vdim = Vgrid ? Vgrid : (P_hgt / rg.rg_Zspacing) + 1;
/*
 * Allocate the plane structure
 */
	plane = (DPlane *) malloc (sizeof (DPlane));

	plane->hdim = hdim;
	plane->vdim = vdim;
	plane->data = (float *) malloc (hdim * vdim * sizeof (float));
/*
 * Set user coordinates for pixel conversions (Note that "x" and "y" here refer
 * to user coordinates on the screen, which correspond to the direction of the
 * plane cut and altitude in the source grid.)
 */
	Xlo = 0.0;
	Xhi = P_len;

	Ylo = P_bot;
	Yhi = P_bot + P_hgt;
/*
 * Come up with numbers so we can easily translate horizontal steps in 
 * the plane into indices in the source grid. (Now we're using x and y
 * as defined in the source grid)
 */
	f_i0 = (X0 - grid_x0) / rg.rg_Xspacing;
	f_j0 = (Y0 - grid_y0) / rg.rg_Yspacing;
	f_k0 = (P_bot - grid_z0) / rg.rg_Zspacing;

	if (hdim > 1)
	{
		f_istep = (X1 - X0) / (rg.rg_Xspacing * (hdim - 1));
		f_jstep = (Y1 - Y0) / (rg.rg_Yspacing * (hdim - 1));
	}
	else
		f_istep = f_jstep = 0.0;

	f_kstep = 1.0;
/*
 * h is the horizontal index and v is the vertical index into the vertical 
 * plane we're building.
 *
 * i and j are the horizontal indices and k is the vertical index into the 
 * source grid.
 */
	for (v = 0; v < vdim; v++)
	{
		k = nint (f_k0 + v * f_kstep);

		for (h = 0; h < hdim; h++)
		{
			pp = plane->data + h * vdim + v;

			f_i = f_i0 + h * f_istep;
			f_j = f_j0 + h * f_jstep;

			i = nint (f_i);
			j = nint (f_j);
		/*
		 * Simple if we're outside the source grid
		 */
			if (i < 0 || j < 0 || k < 0 ||
			    i > rg.rg_nX-2 || j > rg.rg_nY-2 || k > rg.rg_nZ-1)
			{
				*pp = BADVAL;
				continue;
			}
		/*
		 * Do a bilinear interpolation using the four source grid
		 * points (.) surrounding the plane point (+).  Point 0 is
		 * at grid position (i,j) and di and dj are fractions of 
		 * the x and y grid spacing to the plane point, respectively.
		 *
		 *     2	 3
		 *	.	.
		 *	     +    -
		 *		   |
		 *		   | dj
		 *	.	. -
		 *     0	 1
		 *
		 *	|____|
		 *        di
		 *
		 *
		 *	  val =	(1-di)(1-dj) val0 + (di)(1-dj) val1 +
		 *		(1-di)(dj) val2 + (di)(dj) val3
		 */
			di = f_i - i;
			dj = f_j - j;

			sgp = sourcegrid + k * (rg.rg_nX * rg.rg_nY) + 
				j * rg.rg_nX + i;
			val0 = *sgp;
			val1 = *(sgp + 1);
			val2 = *(sgp + rg.rg_nX);
			val3 = *(sgp + rg.rg_nX + 1);
		/*
		 * If any of the surrounding grid points are bad, assign
		 * a bad value, otherwise do the interpolation
		 */
			if (val0 == sgbad || val1 == sgbad || 
				val2 == sgbad || val3 == sgbad)	
				*pp = BADVAL;
			else
				*pp = 	(1 - di) * (1 - dj) * val0 +
					di * (1 - dj) * val1 + 
					(1 - di) * dj * val2 + 
					di * dj * val3;
		}
	}
/*
 * The plane array is populated, free the data chunk and return
 */
	dc_DestroyDC (dc);
	return (plane);
}




static int
xs_ZIndex (z, vdim)
float	z;
int	vdim;
/*
 * Return the index of the first grid level at or above z
 */
{
	float	fndx = (z - P_bot) / P_hgt * (vdim - 1);

	if ((float)((int) fndx) == fndx)
		return ((int) fndx);	
	else
		return ((int) fndx + 1);
}




static void
xs_AddToLevel (plane, p_wgt, iv, xdat, ydat, vdat)
DPlane	*plane, *p_wgt;
int	iv;
float	vdat, xdat, ydat;
/*
 * Apply the point with value vdat located at (xdat,ydat) to the plane
 * at vertical index iv.
 */
{
	int	ih, offset;
	float	x, y, d, xstep, ystep, wgt, *dp, *wp;
/*
 * Sanity check
 */
	if (iv < 0 || iv >= plane->vdim)
		ui_error ("*BUG* Bad vertical index in xs_AddToLevel");
/*
 * Step through the grid horizontally at vertical index iv and use a distance
 * weighting scheme to apply the given point
 */
	xstep = (X1 - X0) / (plane->hdim - 1);
	ystep = (Y1 - Y0) / (plane->hdim - 1);

	for (ih = 0; ih < plane->hdim; ih++)
	{
		x = X0 + ih * xstep;
		y = Y0 + ih * ystep;

		d = sqrt ((xdat - x) * (xdat - x) + (ydat - y) * (ydat - y));
	/*
	 * Use a 1/d^2 weighting scheme, truncated at a weight of 100
	 */
		if (d < 0.1)
			wgt = 100;
		else
			wgt = 1.0 / (d * d);
	/*
	 * Apply the point
	 */
		offset = (ih * plane->vdim) + iv;

		dp = plane->data + offset;
		wp = p_wgt->data + offset;

		*dp = ((*dp) * (*wp) + vdat * wgt) / ((*wp) + wgt);
		*wp = (*wp) + wgt;
	}
}




static void
xs_BuildLimits (array, weight, hdim, xdat, ydat, zdat)
float	*array, *weight;
float	xdat, ydat, zdat;
int	hdim;
/*
 * Use the given point to help build either the given ceiling or floor
 * array.
 */
{
	int	ih;
	float	pos, d, hstep, wgt;
/*
 * Horizontal step length in the plane
 */
	hstep = P_len / (hdim - 1);
/*
 * Find the distance from the left endpoint of the plane to the projection
 * of the given point onto the plane.
 */
	pos = hypot (xdat - X0, ydat - Y0) * 
		cos (atan2 (ydat-Y0, xdat-X0) - atan2 (Y1-Y0, X1-X0));
/*
 * Step through the array and use a distance weighting scheme to apply 
 * the given point.  The point is projected onto the plane before distances
 * are calculated. 
 */
	for (ih = 0; ih < hdim; ih++)
	{
	/*
	 * Find the distance from this array point to the projection of
	 * the input point
	 */
		d = fabs (ih * hstep - pos);
	/*
	 * Use a 1/d weighting scheme, truncated at a weight of 100
	 */
		if (d < 0.1)
			wgt = 100;
		else
			wgt = 1.0 / (d * d);
	/*
	 * Apply the point
	 */
		array[ih] = (array[ih] * weight[ih] + zdat * wgt) /
			(weight[ih] + wgt);
		weight[ih] = weight[ih] + wgt;
	}
}




static void
xs_Background ()
/*
 * Draw the background for this cross-section
 */
{
	float	tick, tickinc, lolim, hilim;
	int	dolabel;
	XPoint	pts[5];
/*
 * Draw a box
 */
	pts[0].x = Pix_left;	pts[0].y = Pix_bottom;
	pts[1].x = Pix_right;	pts[1].y = Pix_bottom;
	pts[2].x = Pix_right;	pts[2].y = Pix_top;
	pts[3].x = Pix_left;	pts[3].y = Pix_top;
	pts[4].x = Pix_left;	pts[4].y = Pix_bottom;

	XSetForeground (XtDisplay (Graphics), Gcontext, White.pixel);
	XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, pts, 
		5, CoordModeOrigin);
/*
 * Get the lower and upper limits of the vertical axes
 */
	if (P_hgt > 0.0)
	{
		lolim = P_bot;
		hilim = P_bot + P_hgt;
	}
	else
	{
		lolim = P_bot + P_hgt;
		hilim = P_bot;
	}
/*
 * Figure out where to put ticks on the vertical axes
 */
	tickinc = pow (10.0, floor (log10 (fabs (P_hgt))));

	if (fabs (P_hgt / tickinc) < 1.5)
		tickinc *= 0.1;
	else if (fabs (P_hgt / tickinc) < 3.0)
		tickinc *= 0.2;
	else if (fabs (P_hgt / tickinc) < 8.0)
		tickinc *= 0.5;
/*
 * Adjust the lower limit to a reasonable label value
 */
	lolim = tickinc * ceil (lolim / tickinc);
/*
 * Loop to put tick marks and labels on vertical axes
 */
	dolabel = ((int)(lolim / tickinc) % 2) == 0;

	for (tick = lolim; tick <= hilim; tick += tickinc)
	{
	/*
	 * Draw the tick
	 */
		pts[0].y = pts[1].y = YPIX (tick);
		pts[0].x = Pix_left;
		pts[1].x = XPIX (0.025 * P_len);
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);

		pts[0].x = XPIX (0.975 * P_len);
		pts[1].x = Pix_right;
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);
	/*
	 * Label every other tick
	 */
		if (dolabel)
		{
			if (tickinc < 1.0)
				sprintf (Scratch, "%.1f", tick);
			else
				sprintf (Scratch, "%d", (int) tick);

			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				XPIX (-0.005 * P_len), YPIX (tick), 
				Scratch, 0.0, 0.02, JustifyRight, 
				JustifyCenter);
		}
		dolabel = ! dolabel;
	}

	if (Use_alt)
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			XPIX (-0.04 * P_len), YPIX (P_bot + 0.5 * P_hgt), 
			"Altitude (km MSL)", 90.0, 0.02, JustifyCenter, 
			JustifyBottom);
	else
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			XPIX (-0.04 * P_len), YPIX (P_bot + 0.5 * P_hgt), 
			"Pressure (mb)", 90.0, 0.02, JustifyCenter, 
			JustifyBottom);
/*
 * No horizontal axis labels for zig-zag plots
 */
	if (Zig_zag)
		return;
/*
 * Figure out where to put ticks on horizontal axes
 */
	tickinc = P_len / 8.0;
/*
 * Label the horizontal axes
 */
	dolabel = TRUE;

	for (tick = 0.0; tick <= 1.005 * P_len; tick += tickinc)
	{
	/*
	 * Draw the tick
	 */
		pts[0].x = pts[1].x = XPIX (tick);
		pts[0].y = Pix_bottom;
		pts[1].y = YPIX (P_bot + 0.025 * P_hgt);
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);

		pts[0].y = YPIX (P_bot + 0.975 * P_hgt);
		pts[1].y = Pix_top;
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);
	/*
	 * Label every other tick
	 */
		if (dolabel)
		{
			sprintf (Scratch, "(%.1f,%.1f)", 
				X0 + tick / P_len * (X1 - X0),
				Y0 + tick / P_len * (Y1 - Y0));

			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				XPIX (tick), YPIX (P_bot - 0.005 * P_hgt), 
				Scratch, 0.0, 0.02, JustifyCenter, JustifyTop);
		}
		dolabel = ! dolabel;
	}

	DrawText (Graphics, GWFrame (Graphics), Gcontext, XPIX (0.5 * P_len), 
		  YPIX (P_bot - 0.04 * P_hgt), "Position (km)", 0.0, 0.02, 
		  JustifyCenter, JustifyTop);
}




static int
xs_Pos (plat, ptime, xpp, ypp)
char	*plat;
ZebTime	*ptime;
float	**xpp, **ypp;
/*
 * Return the x and y positions of the data for platform 'pid'
 * The xpp and ypp arrays are malloc'ed by xs_Pos () and must
 * be freed by the caller.
 */
{
	float	*xpos, *ypos, lat, lon, *wspd, *wdir, *wtime, *dummy;
	float	ws, wd, t, dt, site_x, site_y, badvalue;
	int	i, pt, npts, navail;
	bool	have_lat, have_lon;
	float	snd_s_lat (), snd_s_lon ();
	FieldId	fieldlist[2], available[50], f_lat, f_lon;
	PlatformId	pid;
	DataChunk	*dc;
	Location	loc;
/*
 * Get the ID of this platform
 */
	pid = ds_LookupPlatform (plat);
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", plat);
		return (FALSE);
	}
/*
 * Get a field list so we can see if lat and lon are available
 */
	ds_GetFields (pid, ptime, &navail, available);

	f_lat = F_Lookup ("lat");
	f_lon = F_Lookup ("lon");

	have_lat = have_lon = FALSE;

	for (i = 0; i < navail; i++)
	{
		have_lat |= (available[i] == f_lat);
		have_lon |= (available[i] == f_lon);
	}
/*
 * Derive the (x,y) positions from (lat,lon) if possible
 */
	if (have_lat && have_lon)
	{
	/*
	 * Lat and lon are available (can be converted directly to x,y)
	 */
		fieldlist[0] = f_lat;
		fieldlist[1] = f_lon;
	/*
	 * Get the data chunk
	 */
		if (! (dc = ds_FetchObs (pid, DCC_Scalar, ptime, fieldlist, 2,
			NULL, 0)))
			return (FALSE);

		npts = dc_GetNSample (dc); 
		dc_GetLoc (dc, 0, &loc);
	/*
	 * Get the (x,y) site location
	 */
		cvt_ToXY (loc.l_lat, loc.l_lon, &site_x, &site_y);
	/*
	 * Allocate the x and y position arrays
	 */
		*xpp = xpos = (float *) malloc (npts * sizeof (float));
		*ypp = ypos = (float *) malloc (npts * sizeof (float));
	/*
	 * Convert the (lat,lon) points to (x,y) and put them in the arrays
	 */
		badvalue = dc_GetBadval (dc);

		for (pt = 0; pt < npts; pt++)
		{
			lat = dc_GetScalar (dc, pt, F_Lookup ("lat"));
			lon = dc_GetScalar (dc, pt, F_Lookup ("lon"));
			if (lat == badvalue || lon == badvalue)
				xpos[pt] = ypos[pt] = badvalue;
			else
				cvt_ToXY (lat, lon, &(xpos[pt]), &(ypos[pt]));
		}
	/*
	 * Free the data chunk
	 */
		dc_DestroyDC (dc);
	}
# ifdef notdef
	else if (snd_has_field (sid, f_wspd) && snd_has_field (sid, f_wdir) && 
		snd_has_field (sid, f_time))
	{
	/*
	 * Derive the (x,y) positions from wind speed, wind direction, 
	 * and time
	 */
		wspd = (float *) malloc (BUFLEN * sizeof (float));
		wdir = (float *) malloc (BUFLEN * sizeof (float));
		wtime = (float *) malloc (BUFLEN * sizeof (float));

		npts = snd_get_data (sid, wspd, BUFLEN, f_wspd, BADVAL);
		snd_get_data (sid, wdir, BUFLEN, f_wdir, BADVAL);
		snd_get_data (sid, wtime, BUFLEN, f_time, BADVAL);

		xpos[0] = site_x;
		ypos[0] = site_y;

		ws = wd = t = dt = 0.0;

		for (pt = 0; pt < npts - 1; pt++)
		{
			if (wspd[pt] != BADVAL && wdir[pt] != BADVAL)
			{
				ws = wspd[pt] * 0.001;	/* km/s */
				wd = wdir[pt];
			}

			if (wtime[pt] != BADVAL)
			{
				dt = wtime[pt] - t;
				t = wtime[pt];
			}
			else
				dt = 0.0;

			xpos[pt+1] = xpos[pt] + dt * ws * 
				cos (DEG_TO_RAD (-90.0 - wd));
			ypos[pt+1] = ypos[pt] + dt * ws *
				sin (DEG_TO_RAD (-90.0 - wd));
		}
	/*
	 * Free our data arrays
	 */
		free (wspd);
		free (wdir);
		free (wtime);
	}
# endif
	else
	{
	/*
	 * We have no position data, just assume the sounding
	 * goes straight up
	 *
	 * KLUGE: Ask for a data chunk of some field just to find out how
	 * many data points we need to do and the site location.
	 */
		fieldlist[0] = f_lat;
		if (! (dc = ds_FetchObs (pid, DCC_Scalar, ptime, fieldlist, 1,
			NULL, 0)))
			return (FALSE);
	/*
	 * Grab the location and the number of points
	 */
		dc_GetLoc (dc, 0, &loc);
		cvt_ToXY (loc.l_lat, loc.l_lon, &site_x, &site_y);

		npts = dc_GetNSample (dc);
	/*
	 * Allocate the x and y position arrays
	 */
		*xpp = xpos = (float *) malloc (npts * sizeof (float));
		*ypp = ypos = (float *) malloc (npts * sizeof (float));
	/*
	 * Just put the site location in each point
	 */
		for (pt = 0; pt < npts; pt++)
		{
			xpos[pt] = site_x;
			ypos[pt] = site_y;
		}
	}

	return (TRUE);
}




static void
xs_ExtendTrace (x, y)
float	x, y;
/*
 * Add this (x,y) point to the trace for the current sounding
 */
{
	if (Tracelen >= BUFLEN)
		ui_error ("BUG! Too many points in x-section sounding trace!");

	Trace[Tracelen].x = XPIX (x);
	Trace[Tracelen].y = YPIX (y);
	Tracelen++;
}




static void
xs_DrawTrace (name)
char	*name;
/*
 * Draw the current platform's trace
 */
{
	char	dash[2];
	int	i, label_x, label_y;
/*
 * Use a white, dotted line
 */
	XSetForeground (XtDisplay (Graphics), Gcontext, White.pixel);

	dash[0] = 2;
	dash[1] = 4;

	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, LineOnOffDash, 
		CapButt, JoinMiter);
	XSetDashes (XtDisplay (Graphics), Gcontext, 0, dash, 2);
/*
 * Turn on clipping
 */
	XSetClipRectangles (XtDisplay (Graphics), Gcontext, 0, 0, &Clip, 1, 
		Unsorted);
/*
 * Draw the line
 */
	XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, Trace, 
		Tracelen, CoordModeOrigin);
/*
 * Make the GC use LineSolid again and turn off clipping
 */
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, LineSolid, 
		CapButt, JoinMiter);
	XSetClipRectangles (XtDisplay (Graphics), Gcontext, 0, 0, &Unclip, 1, 
		Unsorted);
/*
 * Find the first point of the trace which lies in the plot region
 */
	for (i = 0; i < Tracelen; i++)
		if (Trace[i].x >= Pix_left && Trace[i].x <= Pix_right &&
			Trace[i].y <= Pix_bottom && Trace[i].y >= Pix_top)
			break;
/*
 * Put the label at this point (or return if there isn't a point
 * in the plot region
 */
	if (i == Tracelen)
	{
		Tracelen = 0;
		return;
	}
	else
	{
		label_x = Trace[i].x;
		label_y = Trace[i].y;
	}
/*
 * Draw the label
 */
	Scratch[0] = ' ';
	strcpyUC (Scratch + 1, name);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, label_x, label_y, 
		Scratch, -90.0, 0.015, JustifyRight, JustifyCenter);
/*
 * Set up to start a new trace
 */
	Tracelen = 0;
}




static DataChunk *
xs_GetObsDC (plat, fldname, dtime)
char	*plat, *fldname;
ZebTime *dtime;
/*
 * Find an observation from the given platform before the current plot time
 * and within the user-specified maximum time difference.  Return
 * a good data chunk if this is possible, otherwise return NULL.
 * Return the data time if a real data chunk is returned.
 */
{
	int	diff;
	FieldId	fieldlist[2];
	PlatformId	pid;
	DataChunk	*dc;
/*
 * Get the ID of this platform
 */
	pid = ds_LookupPlatform (plat);
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", plat);
		return (NULL);
	}
/*
 * Find the closest data time before PlotTime
 */
	if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, dtime))
	{
		TC_EncodeTime (&PlotTime, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, "No data for '%s' at %s", plat, Scratch);
		return (NULL);
	}
/*
 * Make sure the data start time is within the user-specified
 * limit (if any)
 */
	diff = PlotTime.zt_Sec - dtime->zt_Sec;
	if (Maxdiff > 0 && diff > Maxdiff)
	{
		msg_ELog (EF_INFO, "No data recent enough from '%s'", plat);
		return (NULL);
	}
/*
 * Get the vertical position (altitude or pressure) and field data
 */
	fieldlist[0] = F_Lookup (Zfld);
	fieldlist[1] = F_Lookup (fldname);

	if (! (dc = ds_FetchObs (pid, DCC_Scalar, dtime, fieldlist, 2,
		NULL, 0)))
	{
		TC_EncodeTime (&PlotTime, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, "'%s' or '%s' missing for '%s' at %s", 
			fldname, Zfld, plat, Scratch);
		return (NULL);
	}
	else
		return (dc);
}




static DataChunk *
xs_GetGridDC (plat, fldname, dtime)
char	*plat, *fldname;
ZebTime *dtime;
/*
 * Find the 3d grid for the given platform before the current plot time
 * and within the user-specified maximum time difference.  Return
 * a good data chunk if this is possible, otherwise return NULL.
 * Return the data time if a real data chunk is returned.
 */
{
	int	diff;
	FieldId	fld;
	PlatformId	pid;
	DataChunk	*dc;
/*
 * Get the ID of this platform
 */
	pid = ds_LookupPlatform (plat);
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", plat);
		return (NULL);
	}
/*
 * Find the closest data time before PlotTime
 */
	if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, dtime))
	{
		TC_EncodeTime (&PlotTime, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, "No data for '%s' at %s", plat, Scratch);
		return (NULL);
	}
/*
 * Make sure the data start time is within the user-specified
 * limit (if any)
 */
	diff = PlotTime.zt_Sec - dtime->zt_Sec;
	if (Maxdiff > 0 && diff > Maxdiff)
	{
		msg_ELog (EF_INFO, "No data recent enough from '%s'", plat);
		return (NULL);
	}
/*
 * Get the data
 */
	fld = F_Lookup (fldname);

	if (! (dc = ds_Fetch (pid, DCC_RGrid, dtime, dtime, &fld, 1, NULL, 0)))
	{
		TC_EncodeTime (&PlotTime, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, "'%s' missing for '%s' at %s", 
			fldname, plat, Scratch);
		return (NULL);
	}
	else
		return (dc);
}

# endif  /* C_PT_XSECT */
