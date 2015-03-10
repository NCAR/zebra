/*
 * Vertical cross-sectioning
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

# if C_PT_XSECT

# if !defined (hpux) && !defined(AIXV3)
# include <alloca.h>
# endif
# include <math.h>
# include <ctype.h>
# include <X11/Intrinsic.h>

# include <defs.h>
# include <pd.h>
# include <message.h>
# include <GraphicsW.h>
# include <DataStore.h>
# include "GC.h"
# include "GraphProc.h"
# include "Contour.h"
# include "PixelCoord.h"
# include "DrawText.h"

RCSID ("$Id: XSection.c,v 2.56 2004-01-15 18:28:52 burghart Exp $")

/*
 * General definitions
 */
# define DEG_TO_RAD(x)	((x) * 0.017453292)
# define RAD_TO_DEG(x)	((x) * 57.29577951)
# define MAXPLAT	 20
# define BADVAL	-999.0
# define BUFLEN	1024

/*
 * Is a between b and c (inclusive)?
 */
# define BETWEEN(a,b,c)	((((a)-(b))*((a)-(c))) <= 0)

/*
 * Same, except we allow for a fraction of the range (fudge * (c - b)) of
 * overspill on either end of the range.  This allows us to compensate for
 * floating point round- off error. 
 */
# define ALMOST_BETWEEN(a,b,c,fudge) \
		(((((a)-(b))*((a)-(c)))/(((c)-(b))*((c)-(b)))) <= (fudge))

/*
 * Scratch string for building annotations, etc.
 */
static char	Scratch[128];

/*
 * Action to take when we hit a bad sounding
 */
# define BAD_SOUNDING	{ \
				sprintf (Scratch, " (bad)"); \
				An_TopAnnot (Scratch); \
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
AltUnitType	AltUnits;

/*
 * User-selected grid dimensions (if any)
 */
int	Hgrid, Vgrid;

/*
 * Cross-section endpoints
 */
static float	X0 = 0.0, X1 = 0.0, Y0 = 0.0, Y1 = 0.0;

/*
 * Pixel limits for the plot
 */
static int	Pix_left, Pix_right, Pix_bottom, Pix_top;

/*
 * Field for vertical scale
 * Use filled contours?
 * "Zig-zag" cross-section?
 * Horizontal axis labeling
 */
static char	Zfld[80];
static zbool	Fill_contour;
static zbool	Zig_zag;

typedef enum 
{
	Label_LatLon, Label_XY, Label_Length, Label_None
} LabelType;

LabelType	BottomLabel;

/*
 * Drawing info
 */
static float	Contour_center, Contour_step, Wind_scale, Text_size;
static int	Line_width, Degrade;
static zbool	Do_labels, Mono_color, Autoscale, Do_vectors;
/*
 * Project vectors onto the cross-section plane.
 */
static zbool	ProjectVectors;

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
static XColor	White, Black, Ccolor, C_outrange;
static int	Do_outrange;	/* do we want out-of-range contours? */


/*
 * Clip and unclip rectangles
 */
static XRectangle	Clip, Unclip;

/*
 * Prototypes
 */
void	xs_Init FP ((ZebTime *));
void	xs_LineContour FP ((char *, int));
void	xs_FilledContour FP ((char *, int));
void	xs_Vector FP ((char *, int));
void	xs_Raster FP ((char *, int));
void	xs_Track FP ((char *, int));
static void	xs_Contour FP ((char *, int)); 
static void	xs_ZZContour FP ((char *, char **, int, char *));
static void	xs_PlaneContour FP ((char *, char **, int, char *, char *, char *));
static void	xs_ZZVector FP ((char *, char **, int, char *, char *));
static void	xs_PlaneVector FP ((char *, char **, int, char *, char *, char *));
static void	xs_ZZFillIn FP ((ZZ_DPlane *));
static void	xs_Background FP ((void));
static void	xs_ExtendTrace FP ((double, double)); 
static DPlane	*xs_HDWeighting (char *comp, char **pnames, int nplat, 
				 char *fldname, ZebTime *times);
static DPlane	*xs_Bilinear (char *comp, char *platform, char *fldname, 
			      ZebTime *dtime);
static void     xs_FreeDPlane (DPlane *dp);
static void	xs_DrawTrace FP ((char *)); 
static void	xs_AddToLevel FP ((DPlane *, DPlane *, int, double, double, 
				   double)); 
static void	xs_BuildLimits FP ((float *, float *, int, double, double, 
				    double));
static int	xs_Pos FP ((char *, ZebTime *, float **, float **)); 
static int	xs_AltIndex FP ((double, float *, int));
static int	xs_ZIndex FP ((double, int));
static ZZ_DPlane	*xs_ZZGetData (char* comp, char **plats, int nplat, 
				       char *fldname);
static ZZ_DPlane	*xs_AllocZZ_DPlane FP ((int, int));
static void		xs_FreeZZ_DPlane FP ((ZZ_DPlane *));
static DataChunk	*xs_GetObsDC (char *comp, char *plat, char *fldname, 
				      ZebTime *dtime, DataClass *class);
static DataChunk	*xs_GetGridDC (char *comp, char *plat, char *fldname, 
				       ZebTime *dtime, float *alts);
static DataChunk	*xs_NSpaceToRGrid FP ((DataChunk *, FieldId, float *));
static FieldId	xs_FindCV (DataChunk *dc, const char *dimname);




void
xs_Init (t)
ZebTime	*t;
/*
 * Initialize for a cross-section plot.
 */
{
	zbool	use_alt;
	float	zmax;
/*
 * Initialize the overlay times widget
 */
	ot_SetString ("COMPONENT      PLATFORM   FIELD     TIME\n");
/*
 * Look for user values for vertical limits.  Otherwise later stuff will
 * set defaults.
 */
	P_bot = P_hgt = 0.0;

	pda_Search (Pd, "global", "z-min", NULL, (char *) &P_bot, SYMT_FLOAT);
	if (pda_Search (Pd, "global", "z-max", NULL, (char *) &zmax, 
			SYMT_FLOAT))
		P_hgt = zmax - P_bot;
/*
 * Field for the vertical dimension.  We now try for "z-field" in favor of the 
 * old "by-altitude".
 */
	if (! pda_Search (Pd, "global", "z-field", "xsect", Zfld, SYMT_STRING))
	{
		use_alt = TRUE;
		pda_Search (Pd, "global", "by-altitude", "xsect", 
			    (char *) &use_alt, SYMT_BOOL);

		strcpy (Zfld, use_alt ? "alt" : "pres");
	}
/*
 * Set some defaults based on the z field
 */
	if (! strcmp (Zfld, "alt"))
	{
		AltUnits = AU_kmMSL;
		if (P_hgt == 0.0)
		{
			P_bot = 0.0;
			P_hgt = 12.0;
		}
	}
	else if (! strcmp (Zfld, "depth"))
	{
		AltUnits = AU_kmMSL;
		if (P_hgt == 0.0)
		{
			P_bot = -0.6;
			P_hgt = 0.6;
		}
	}
	else if (! strcmp (Zfld, "pres"))
	{
		AltUnits = AU_mb;
		if (P_hgt == 0.0)
		{
			P_bot = 1050.0;
			P_hgt = -850.0;
		}
	}
	else
	{
		AltUnits = AU_kmMSL;	/* really unknown */
		if (P_hgt == 0.0)
		{
			P_bot = 0.0;
			P_hgt = 10.0;
		}
	}
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
 * Black and white
 */
	ct_GetColorByName ("black", &Black);
	ct_GetColorByName ("white", &White);
/*
 * Zig-zag cross section?
 */
	Zig_zag = FALSE;
	pda_Search (Pd, "global", "zig-zag", "xsect", (char *) &Zig_zag,
		    SYMT_BOOL);

	sprintf (Scratch, "%s cross-section plot.  ", 
		 Zig_zag ? "Zig-zag" : "Planar");
	An_TopAnnot (Scratch);
/*
 * Text size
 */
	Text_size = 0.03;
	pda_Search (Pd, "global", "annot-height", "xsect", 
		    (char *) &Text_size, SYMT_FLOAT);
/*
 * For planar cross sections, get the endpoints and bottom label type
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
	/*
	 * Bottom label type
	 */
		BottomLabel = Label_LatLon;
		if (pda_Search (Pd, "global", "bottom-label-type", NULL, 
				Scratch, SYMT_STRING))
		{
			if (! strcasecmp (Scratch, "latlon"))
				BottomLabel = Label_LatLon;
			else if (! strcasecmp (Scratch, "xy"))
				BottomLabel = Label_XY;
			else if (! strcasecmp (Scratch, "length"))
				BottomLabel = Label_Length;
			else if (! strcasecmp (Scratch, "none"))
				BottomLabel = Label_None;
			else
			{
				msg_ELog (EF_INFO, 
				  "Bad bottom-label-type '%s', using 'latlon'",
				  Scratch);
				BottomLabel = Label_LatLon;
			}
		}
	}
}




void
xs_LineContour (c, update)
/*
 * Handle a line contour cross-section component
 */
char	*c;
zbool	update;
{
	zbool	blank;

	Do_labels = TRUE;
	pda_Search (Pd, c, "do-labels", "contour", (char *) &Do_labels, 
		    SYMT_BOOL);
	
	Line_width = 0;
	pda_Search (Pd, c, "line-width", "contour", (char *) &Line_width,
		    SYMT_INT);

	Mono_color = FALSE;
	pda_Search (Pd, c, "color-mono", "contour", (char *) &Mono_color,
		    SYMT_BOOL);

	blank = TRUE;
	pda_Search (Pd, c, "label-blanking", "contour", (char *) &blank,
		    SYMT_BOOL);
	dt_SetBlankLabel (blank);

	Fill_contour = FALSE;
	xs_Contour (c, update);
}




void
xs_FilledContour (c, update)
/*
 * Handle a filled contour cross-section component
 */
char	*c;
zbool	update;
{
	Fill_contour = TRUE;
	Mono_color = FALSE;
	xs_Contour (c, update);
}




static void
xs_Contour (c, update)
char	*c;
zbool	update;
/*
 * Draw a cross-section contour plot based on the given PD component.
 */
{
	zbool	ok;
	int	nplat;
	char	platforms[PlatformListLen];
	char	*pnames[MaxPlatforms], fldname[80], cname[20];
	char	ufldname[40], vfldname[40];
	char	param[50], outrange[40];
	char    justname[64];
	FieldId	fid;
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
 * Field
 */
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fldname, SYMT_STRING);
	if (! strcmp (fldname, "normal") || ! strcmp (fldname, "inplane"))  {
		ok &= pda_ReqSearch (Pd, c, "u-field", NULL, ufldname, SYMT_STRING);
		ok &= pda_ReqSearch (Pd, c, "v-field", NULL, vfldname, SYMT_STRING);
		if (! strcmp (fldname, "normal"))
			strcpy (justname, "normal");
		else
			strcpy (justname, "inplane");
	}
	else  {
		fid = F_Lookup (fldname);
		strcpy (justname, SimpleFieldName (fid));
	}
/*
 * Autoscale?
 */
	if (pda_Search (Pd, c, "scale-mode", NULL, param, SYMT_STRING))
		Autoscale = ! strncmp (param, "auto", 4);
	else
		Autoscale = FALSE;
/*
 * If not autoscaling, we must have a center and step
 */
	if (ok && ! Autoscale)
	{
		sprintf (param, "%s-center", justname);
		ok &= pda_ReqSearch (Pd, c, param, "contour", 
				     (char *) &Contour_center, SYMT_FLOAT);

		sprintf (param, "%s-step", justname);
		ok &= pda_ReqSearch (Pd, c, param, "contour", 
				     (char *) &Contour_step, SYMT_FLOAT);
		if (! ok) /* Give them a second chance */
		{
			msg_ELog (EF_PROBLEM, "You get autoscale after all");
			ok = Autoscale = TRUE;
		}
	}
/*
 * Color or color table
 */
	if (Mono_color)
		ok &= pda_ReqSearch (Pd, c, "color", "contour", cname,
				     SYMT_STRING);
	else
		ok &= pda_ReqSearch (Pd, c, "color-table", "contour", cname, 
				     SYMT_STRING);
/*
 * Out of range color?
 */
	Do_outrange = TRUE;
	strcpy (outrange, "black");
	pda_Search (Pd, c, "out-of-range-color", NULL, outrange, SYMT_STRING);

	if (! strcmp (outrange, "none"))
		Do_outrange = FALSE;
	else if (! ct_GetColorByName (outrange, &C_outrange))
		ct_GetColorByName ("black", &C_outrange);
/*
 * Give up if we didn't get all the required parameters
 */
	if (! ok)
		return;
/*
 * Load the color table
 */
	if (Mono_color)
		ct_GetColorByName (cname, &Ccolor);
	else
		ct_LoadTable (cname, &Colors, &Ncolors);
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
		xs_ZZContour (c, pnames, nplat, fldname);
	else
		xs_PlaneContour (c, pnames, nplat, fldname, ufldname, vfldname);
/*
 * Draw the background and set up for side annotation (unless it's 
 * specifically disabled)
 */
	xs_Background ();

	if (Fill_contour)
	{
	    sprintf (Scratch, "%s|%s|%f|%f", fldname, cname, 
		     Contour_center, Contour_step); 
	    An_AddAnnotProc (An_ColorBar, c, Scratch, strlen (Scratch),
			     75, TRUE, FALSE);
	}
	else if (! Mono_color)
	{
	    sprintf (Scratch, "%s|%s|%f|%f", fldname, cname, 
		     Contour_center, Contour_step); 
	    An_AddAnnotProc (An_ColorNumber, c, Scratch, strlen (Scratch), 
			     75, TRUE, FALSE);
	}
}




void
xs_Vector (c, update)
char	*c;
zbool	update;
/*
 * Draw vectors from a cross-section based on the given PD component.
 */
{
	zbool	ok;
	char	platforms[PlatformListLen];
	char	ufldname[40], vfldname[40], wfldname[40], *pnames[MaxPlatforms];
	char	cname[20], style[16];
	int	nplat;
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
 * Vectors (default) or barbs?
 */
	Do_vectors = TRUE;
	if (pda_Search (Pd, c, "wind-style", NULL, style, SYMT_STRING))
		Do_vectors = strncmp (style, "barb", 4);
/*
 * Degrade factor (every n'th barb or vector)
 */
	Degrade = 1;
	pda_Search (Pd, c, "degrade", NULL, (char *)&Degrade, SYMT_INT);
/*
 * Project 3-D vectors onto the plane?  For the moment this only applies to Plane
 * cross-sections and not zig-zags.
 */
	ProjectVectors = FALSE;
	strcpy(wfldname, "none");
	pda_Search (Pd, c, "project-vectors", NULL, (char *)&ProjectVectors, SYMT_BOOL);
/*
 * Get the fields and arrow or barb scale factor
 */
	ok &= pda_ReqSearch (Pd, c, "u-field", "xsect", ufldname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "v-field", "xsect", vfldname, SYMT_STRING);
	if (! Zig_zag && ProjectVectors)
		ok &= pda_ReqSearch (Pd, c, "w-field", "xsect", wfldname, SYMT_STRING);
	if (Do_vectors)
	{
		Wind_scale = 0.002;
		pda_Search (Pd, c, "arrow-scale", "xsect", (char *)&Wind_scale,
			    SYMT_FLOAT);
	}
	else
	{
		Wind_scale = 0.06;
		pda_Search (Pd, c, "barb-scale", "xsect", (char *)&Wind_scale,
			    SYMT_FLOAT);
	}

	if (! ok)
		return;
/*
 * Arrow color and line width
 */
	strcpy (cname, "white");
	if (! pda_Search (Pd, c, "arrow-color", "xsect", cname, SYMT_STRING))
		pda_Search (Pd, c, "color", "xsect", cname, SYMT_STRING);
	if (! ct_GetColorByName (cname, &Ccolor))
	{
		msg_ELog (EF_PROBLEM, "Cannot get arrow color '%s'!", cname);
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
		xs_ZZVector (c, pnames, nplat, ufldname, vfldname);
	else
		xs_PlaneVector (c, pnames, nplat, ufldname, vfldname, wfldname);
/*
 * Draw the background and set up to draw side annotation (scale vector)
 */
	xs_Background ();

	if (Do_vectors)
	{
	    sprintf (Scratch, "m/s|%li|%f|%f|%f", Ccolor.pixel, 
		     10.0, 0.0, Wind_scale * USABLE_HEIGHT); 
	    An_AddAnnotProc (An_ColorVector, c, Scratch, strlen (Scratch),
			     40, FALSE, FALSE);
	}
	else
	{
	    sprintf (Scratch, "m/s|%li|%d", Ccolor.pixel, 
		     (int)(Wind_scale * USABLE_HEIGHT));
	    An_AddAnnotProc (An_BarbLegend, c, Scratch, strlen (Scratch), 
			     100, FALSE, FALSE);
	}
}




static void
xs_ZZContour (c, pnames, nplat, fldname)
char	*c, **pnames, *fldname;
int	nplat;
/*
 * Draw a zig-zag contour plot given the platform string and field name
 */
{
	ZZ_DPlane	*plane;
	float		*data, *dist;
	int		vdim, diff, p;
	ZebTime		dtime;

	An_TopAnnot ("Contour of ");
	An_TopAnnotMatch (px_FldDesc (fldname), Ccolor.pixel,
			  Mono_color ? c : 0, 0);
	An_TopAnnot (" using: ");
/*
 * Get the filled data plane
 */
	plane = xs_ZZGetData (c, pnames, nplat, fldname);

	if (plane->nobs < 2)
	{
		msg_ELog (EF_INFO, 
			  "Nothing drawn (< 2 good soundings)");
		return;
	}
/*
 * Autoscale now if necessary
 */
	if (Autoscale)
	{
		float	min, max;
		FieldId fid = F_Lookup (fldname);
		char *justname = SimpleFieldName (fid);

		GetRange (plane->data, plane->nobs * plane->vdim, BADVAL, 
			  &min, &max);
		CalcCenterStep (min, max, Mono_color ? 8 : Ncolors, 
				&Contour_center, &Contour_step);

		sprintf (Scratch, "%s-center", justname);
		pd_Store (Pd, c, Scratch, (char *) &Contour_center, 
			  SYMT_FLOAT);
		sprintf (Scratch, "%s-step", justname);
		pd_Store (Pd, c, Scratch, (char *) &Contour_step, SYMT_FLOAT);
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
	 * Add a line to the overlay times widget
	 */
		ot_AddStatusLine (c, plane->plats[p], fldname, 
				  &(plane->dtime[p]));
	/*
	 * Contour between this observation and the next one
	 */
		if (p < plane->nobs - 1)
		{
			if (Fill_contour)
			{
				FC_Init (Colors, Ncolors, Ncolors / 2, 
					 Do_outrange ? &C_outrange : NULL, 
					 Clip, TRUE, BADVAL);
				FillContour (Graphics, GWFrame (Graphics), 
					     data + p * vdim, 2, vdim,
					     XPIX (dist[p]), Pix_bottom, 
					     XPIX (dist[p+1]), Pix_top, 
					     Contour_center, Contour_step);
			}
			else
			{
				if (Mono_color)
					CO_InitMono (Ccolor, Clip, TRUE, 
						     BADVAL);
				else
					CO_Init (Colors, Ncolors, Ncolors / 2,
					    Do_outrange ? &C_outrange : NULL, 
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
			  0.0, Text_size, JustifyCenter, JustifyTop);
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
			An_TopAnnot (", ");

		An_TopAnnot (plane->plats[p]);

		sprintf (Scratch, " (");

		dtime = plane->dtime[p];
		diff = PlotTime.zt_Sec - dtime.zt_Sec;

		if (diff > 43200)
			TC_EncodeTime (&dtime, TC_Full, Scratch + 2);
		else
			TC_EncodeTime (&dtime, TC_TimeOnly, 
				       Scratch + 2);

		strcat (Scratch + strlen (Scratch) - 3, ")");

		An_TopAnnot (Scratch);
	}
	
	An_TopAnnot (".  ");

	xs_FreeZZ_DPlane (plane);
}




static void
xs_ZZVector (c, pnames, nplat, ufldname, vfldname)
char	*c, **pnames, *ufldname, *vfldname;
int	nplat;
/*
 * Draw a zig-zag vector plot given the platform string and field names
 */
{
	ZZ_DPlane	*uplane, *vplane, *plane;
	float		*udata, *vdata, *dist;
	int		vdim, diff, p;
	ZebTime		dtime;

	sprintf (Scratch, "%s of (%s,%s) using: ", 
		 Do_vectors ? "Vectors" : "Barbs", ufldname, vfldname);
	An_TopAnnotMatch (Scratch, Ccolor.pixel, c, 0);
/*
 * Get the filled data planes.  
 */
	uplane = xs_ZZGetData (c, pnames, nplat, ufldname);
	vplane = xs_ZZGetData (c, uplane->plats, uplane->nobs, vfldname);

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
	 * Add a line to the overlay times widget
	 */
		ot_AddStatusLine (c, plane->plats[p], "(winds)", 
				  &(plane->dtime[p]));
	/*
	 * Draw the vectors for this observation
	 */
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 
				    Line_width, LineSolid, CapButt, JoinMiter);
		WindGrid (Graphics, GWFrame (Graphics), Gcontext, 
			  udata + p * vdim, vdata + p * vdim, 1, vdim, 
			  XPIX (dist[p]), Pix_bottom, XPIX (dist[p]), 
			  Pix_top, Wind_scale, BADVAL, Ccolor, Degrade, 
			  Do_vectors);
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
			  0.0, Text_size, JustifyCenter, JustifyTop);
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
			An_TopAnnot (", ");

		An_TopAnnot (plane->plats[p]);

		sprintf (Scratch, " (");

		dtime = plane->dtime[p];
		diff = PlotTime.zt_Sec - dtime.zt_Sec;

		if (diff > 43200)
			TC_EncodeTime (&dtime, TC_Full, Scratch + 2);
		else
			TC_EncodeTime (&dtime, TC_TimeOnly, 
				       Scratch + 2);

		strcat (Scratch + strlen (Scratch) - 3, ")");

		An_TopAnnot (Scratch);
	}
	
	An_TopAnnot (".  ");

	xs_FreeZZ_DPlane (uplane);
	xs_FreeZZ_DPlane (vplane);
}




static ZZ_DPlane *
xs_ZZGetData (char *comp, char **pnames, int nplat, char *fldname)
/*
 * Create and return a zig-zag "pseudo-planar" data plane.  The caller is
 * responsible for calling xs_FreeZZ_DPlane() to destroy the plane.
 */
{
	int	p, pt, npts, zndx, zndx_prev, incr;
	int	row, col, vdim, hdim;
	float	badvalue, fdatum, val, val_prev, frac, *data, *zdata;
	float	z, zpos, zstep, z_prev;
	float	loc_x[MAXPLAT], loc_y[MAXPLAT];
	ZebTime	dtime;
	FieldId		fid = F_Lookup (fldname), zid = F_Lookup (Zfld);
	DataChunk	*dc;
	DataClass	class;
	RGrid		rginfo;
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
		if (! (dc = xs_GetObsDC (comp, pnames[p], fldname, &dtime, 
					 &class)))
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
 	 * Get platform location, point count, and bad value from the data 
	 * chunk
	 */	
		if (class == DCC_Scalar)
		{
			npts = dc_GetNSample (dc);

			dc_GetLoc (dc, 0, &loc);
			prj_Project (loc.l_lat, loc.l_lon, &loc_x[col], 
				  &loc_y[col]);
		}
		else if (class == DCC_RGrid)
		{
			data = dc_RGGetGrid (dc, 0, fid, &loc, &rginfo, NULL);
			zdata = dc_RGGetGrid (dc, 0, zid, NULL, NULL, NULL);
			npts = rginfo.rg_nX * rginfo.rg_nY * rginfo.rg_nZ;
			prj_Project (loc.l_lat, loc.l_lon, &loc_x[col], 
				  &loc_y[col]);
		}

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
			if (class == DCC_Scalar)
			{
				fdatum = dc_GetScalar (dc, pt, fid);
				zpos = dc_GetScalar (dc, pt, zid);
			}
			else
			{
				fdatum = data[pt];
				zpos = zdata[pt];
			}

			if (fdatum == badvalue || zpos == badvalue)
				continue;
		/*
		 * Special treatment for the first good point
		 */
			if (val_prev == badvalue)
			{
			/*
			 * Assign the previous point values
			 */
				val_prev = fdatum;
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
			incr = (zndx < zndx_prev) ? -1 : 1;
			for (row = zndx_prev; row != zndx; row += incr)
			{
			/*
			 * Don't assign anything below the first grid level
			 * or above the last grid level
			 */
				if (row < 0 || row >= vdim)
					continue;
			/*
			 * Find the height of this grid index and interpolate
			 * the data and position to this height
			 */
				z = row * zstep + P_bot;
				frac = (z - z_prev) / (zpos - z_prev);

				val = val_prev + frac * (fdatum - val_prev);
			/*
			 * Insert this point into the Plane
			 */
				plane->data[col * vdim + row] = val;
			}
		/*
		 * Make this the previous point
		 */
			val_prev = fdatum;
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
xs_PlaneContour (c, pnames, nplat, fldname, ufldname, vfldname)
char	*c, **pnames, *fldname, *ufldname, *vfldname;
int	nplat;
/*
 * Draw a contour of a planar cross-section
 */
{
	int	i;
	DPlane	*plane, *uplane, *vplane;
	ZebTime	dtime;
/*
 * Initialize
 */
	plane = 0;
	uplane = 0;
	vplane = 0;
/*
 * Annotate
 */
	An_TopAnnot ("Contour of ");
	if (! strcmp (fldname, "normal"))
		An_TopAnnotMatch ("normal winds", Ccolor.pixel, 
			Mono_color ? c : 0, 0);
	else if (! strcmp (fldname, "inplane")) 
		An_TopAnnotMatch ("inplane winds", Ccolor.pixel, 
			Mono_color ? c : 0, 0);
	else
		An_TopAnnotMatch (px_FldDesc(fldname), Ccolor.pixel, 
			Mono_color ? c : 0, 0);
	An_TopAnnot (" using: ");
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
		An_TopAnnot (pnames[0]);
		An_TopAnnot (".  ");
	/*
	 * Get the data
	 */
		if (! strcmp(fldname, "normal") || 
		    ! strcmp(fldname, "inplane"))  
		{
		    uplane = xs_Bilinear (c, pnames[0], ufldname, &dtime);
		    vplane = xs_Bilinear (c, pnames[0], vfldname, &dtime);
		}
		else
		    plane = xs_Bilinear (c, pnames[0], fldname, &dtime);
	/*
	 * Add a line to the overlay times widget
	 */
		if (uplane && vplane)  
		{
		    if (! strcmp(fldname, "normal"))
			ot_AddStatusLine (c, pnames[0], 
					  "(normal winds)", &dtime);
		    else
			ot_AddStatusLine (c, pnames[0], 
					  "(inplane winds)", &dtime);
		}
		else if (plane)
		    ot_AddStatusLine (c, pnames[0], fldname, &dtime);
	}
	else
	{
		ZebTime	*times = (ZebTime *) alloca (nplat * sizeof (ZebTime));
		int	diff;
	/*
	 * Get the data
	 */
		if (! strcmp(fldname, "normal") || 
		    ! strcmp(fldname, "inplane"))  
		{
		    uplane = xs_HDWeighting (c, pnames, nplat, ufldname, 
					     times);
		    vplane = xs_HDWeighting (c, pnames, nplat, vfldname, 
					     times);
		}
		else
		    plane = xs_HDWeighting (c, pnames, nplat, fldname, times);
	/*
	 * Annotate with the platforms
	 */
		for (i = 0; i < nplat; i++)
		{
			if (i != 0)
				An_TopAnnot (", ");

			An_TopAnnot (pnames[i]);
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

			An_TopAnnot (Scratch);
		/*
		 * Add a line to the overlay times widget, too.
		 */
			if (times[i].zt_Sec != 0)
			    ot_AddStatusLine (c, pnames[i], fldname, 
					      times + i);
		}
		An_TopAnnot (".  ");
	}
/*
 * If we have horizontal vector components, compute the normal or inplane wind 
 * magnitude, depending on fldname.
 */
	if (uplane && vplane)
	{
	    double pangle = atan2 (Y1 - Y0, X1 - X0);

	    for (i = 0; i < uplane->hdim * uplane->vdim; ++i)
	    {
		  float u = uplane->data[i];
		  float v = vplane->data[i];
		  if (u == BADVAL || v == BADVAL)
		  {
			vplane->data[i] = BADVAL;
		  }
		  else
		  {
			double theta = pangle - atan2(v, u);
			if (! strcmp (fldname, "normal"))
				vplane->data[i] = - sqrt(u*u + v*v) * sin(theta);
			else
				vplane->data[i] = sqrt(u*u + v*v) * cos(theta);
		  }
	    }
	    /*
	     * From here on the computed plane can be treated like any other
	     * field plane...
	     */
	    plane = vplane;
	    xs_FreeDPlane (uplane);
	}

	if (! plane)
	    return;
/*
 * Autoscale now if necessary
 */
	if (Autoscale)
	{
		float	min, max;
		FieldId fid = F_Lookup (fldname);
		char *justname = SimpleFieldName (fid);

		GetRange (plane->data, plane->hdim * plane->vdim, BADVAL, 
			  &min, &max);
		CalcCenterStep (min, max, Mono_color ? 8 : Ncolors, 
				&Contour_center, &Contour_step);

		sprintf (Scratch, "%s-center", justname);
		pd_Store (Pd, c, Scratch, (char *) &Contour_center, 
			  SYMT_FLOAT);
		sprintf (Scratch, "%s-step", justname);
		pd_Store (Pd, c, Scratch, (char *) &Contour_step, SYMT_FLOAT);
	}
/*
 * Draw the contours
 */
	if (Fill_contour)
	{
		FC_Init (Colors, Ncolors, Ncolors / 2, 
			 Do_outrange ? &C_outrange : NULL, Clip, TRUE, BADVAL);
		FillContour (Graphics, GWFrame (Graphics), plane->data, 
			     plane->hdim, plane->vdim, Pix_left, Pix_bottom, 
			     Pix_right, Pix_top, Contour_center, Contour_step);
	}
	else
	{
		if (Mono_color)
			CO_InitMono (Ccolor, Clip, TRUE, BADVAL);
		else
			CO_Init (Colors, Ncolors, Ncolors / 2, 
				 Do_outrange ? &C_outrange : NULL, Clip, 
				 TRUE, BADVAL);

		Contour (Graphics, GWFrame (Graphics), plane->data, 
			 plane->hdim, plane->vdim, Pix_left, Pix_bottom, 
			 Pix_right, Pix_top, Contour_center, Contour_step, 
			 Do_labels, Line_width);
	}
/*
 * Clean up
 */
	xs_FreeDPlane (plane);
	return;
}




static void
xs_PlaneVector (c, pnames, nplat, ufldname, vfldname, wfldname)
char	*c, **pnames, *ufldname, *vfldname, *wfldname;
int	nplat;
{
	int	i;
	DPlane	*uplane, *vplane, *wplane;
	ZebTime	dtime;
/*
 * Annotate
 */
	sprintf (Scratch, "%s%s of (%s,%s) using: ", 
		 ProjectVectors ? "Projected " : "",
		 Do_vectors ? "Vectors" : "Barbs", ufldname, vfldname);
	An_TopAnnotMatch (Scratch, Ccolor.pixel, c, 0);
/*
 * Find the gridding method and grab the data
 */
 	wplane = 0;
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
		An_TopAnnot (pnames[0]);
		An_TopAnnot (".  ");
	/*
	 * Get the data
	 */
		uplane = xs_Bilinear (c, pnames[0], ufldname, &dtime);
		vplane = xs_Bilinear (c, pnames[0], vfldname, &dtime);
		if (ProjectVectors)
			wplane = xs_Bilinear (c, pnames[0], wfldname, &dtime);
	/*
	 * Add a line to the overlay times widget
	 */
		if (uplane && vplane && wplane)
			ot_AddStatusLine (c, pnames[0], "(projected winds)",
					  &dtime);
		else if (! ProjectVectors && uplane && vplane) 
			ot_AddStatusLine (c, pnames[0], "(winds)", &dtime);
	}
	else
	{
		ZebTime	*times = (ZebTime *) alloca (nplat * sizeof (ZebTime));
		int	diff;
	/*
	 * Get the data
	 */
		uplane = xs_HDWeighting (c, pnames, nplat, ufldname, times);
		vplane = xs_HDWeighting (c, pnames, nplat, vfldname, times);
		if (ProjectVectors)
			wplane = xs_HDWeighting (c, pnames, nplat, wfldname, 
						 times);
	/*
	 * Annotate with the platforms
	 */
		for (i = 0; i < nplat; i++)
		{
			if (i != 0)
				An_TopAnnot (", ");

			An_TopAnnot (pnames[i]);
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

			An_TopAnnot (Scratch);
		/*
		 * Add a line to the overlay times widget, too
		 */
			if (times[i].zt_Sec != 0)
				ot_AddStatusLine (c, pnames[i], 
						  ProjectVectors ? 
						  "(projected winds)" :
						  "(winds)",
						  times + i);
		}
		An_TopAnnot (".  ");
	}
/*
 * If we have 3-D vector components, project the vector onto the vertical
 * plane, computing the u and v components in the u and v data planes. 
 */
	if (uplane && vplane && wplane)
	{
	    DPlane *tmp;
	    double pangle = atan2 (Y1 - Y0, X1 - X0);

	    /* Project the horizontal wind speed onto the vertial plane.
	     * The vertical component of the projected wind is exactly w. 
	     */
	    for (i = 0; i < uplane->hdim * uplane->vdim; ++i)
	    {
		float u = uplane->data[i];
		float v = vplane->data[i];
		if (u == BADVAL || v == BADVAL)
		{
		    uplane->data[i] = BADVAL;
		}
		else
		{
		    double theta = pangle - atan2(v, u);
		    uplane->data[i] = sqrt(u*u + v*v) * cos(theta);
		}
	    }

	    /* Swap v and w so that both will still be freed later. */
	    tmp = vplane; vplane = wplane; wplane = tmp;
	}
	if (uplane && vplane && (! ProjectVectors || wplane))
	{ 
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 
				    Line_width, LineSolid, CapButt, JoinMiter);
	/*
	 * Draw the vectors or barbs
	 */
		WindGrid (Graphics, GWFrame (Graphics), Gcontext, uplane->data,
			  vplane->data, uplane->hdim, uplane->vdim, Pix_left, 
			  Pix_bottom, Pix_right, Pix_top, Wind_scale, BADVAL, 
			  Ccolor, Degrade, Do_vectors);
	/*
	 * Return the line width to the default of zero, since this is a
	 * shared GC
	 */
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, 
				    LineSolid, CapButt, JoinMiter);
	}
/*
 * Clean up
 */
	xs_FreeDPlane (uplane);
	xs_FreeDPlane (vplane);
	xs_FreeDPlane (wplane);
	return;
}




static DPlane *
xs_HDWeighting (char *comp, char **pnames, int nplat, char *fldname, 
		ZebTime *times)
/*
 * Create and fill a cross-section array with data from the chosen 
 * soundings, using a horizontal distance weighting scheme.  Return the 
 * data times for each platform also, with a time of zero if data for a
 * given platform are non-existent or bad.
 */
{
	int	pt, plat, npts, iz, zndx, zndx_prev, ih, iv, hdim, vdim;
	int	i, j, offset;
	float	val, x, y, z, val_prev, x_prev, y_prev, z_prev;
	float	zstep, hlen, frac, badvalue;
	float	fdata, *xpos = NULL, *ypos = NULL, zpos;
	float	xhighest, yhighest, zhighest;
	float	*floor, *ceiling, *f_wgt, *c_wgt;
	ZebTime	dtime, badtime;
	DPlane	*plane, *p_wgt;
	DataChunk	*dc;
	DataClass	class;
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
		zhighest = (P_hgt > 0) ? -99e9 : 99e9;
	/*
	 * Get the data
	 */
		if (! (dc = xs_GetObsDC (comp, pnames[plat], fldname, &dtime, 
					 &class)))
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
			if ((P_hgt > 0 && zpos > zhighest) ||
				(P_hgt < 0 && zpos < zhighest))
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
				cos (ATAN2 (ypos[pt]-Y0, xpos[pt]-X0) -
				ATAN2 (Y1-Y0, X1-X0));

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


static void
xs_FreeDPlane (DPlane *dp)
{
    if (dp)
    {
	if (dp->data)
	    free (dp->data);
	free (dp);
    }
}


static DPlane *
xs_Bilinear (char *comp, char *platform, char *fldname, ZebTime *dtime)
/*
 * Create a cross-section array with data from a cartesian grid, using
 * bilinear interpolation.  It is assumed that the data source will be a
 * 3d cartesian grid.  The DPlane is alloc'ed here and should be free'd by the
 * caller.  Return NULL if there's a problem.  We also return a data time if
 * we create a good grid.
 */
{
	int	len, h, v, i, j, k, hdim, vdim, nalts;
	float	*sourcegrid, *sgp, sgbad, *pp;
	float	f_i0, f_istep, f_j0, f_jstep, f_i, f_j;
	float	grid_x0, grid_y0, di, dj, z, below, above;
	float	val0, val1, val2, val3;
	float	alts[1024];	/* should be enough... */
	RGrid	rg;
	DPlane	*plane;
	Location	loc;
	DataChunk	*dc;
/*
 * Get the data in the form of a bastardized 3d RGrid data chunk.  The
 * bastardization comes in the form of a list of (potentially irregularly
 * spaced) altitudes. 
 */
	if (! (dc = xs_GetGridDC (comp, platform, fldname, dtime, alts)))
		return (NULL);
/*
 * Get the info we need from the data chunk
 */
	sourcegrid = dc_RGGetGrid (dc, 0, F_Lookup (fldname), &loc, &rg, &len);
	prj_Project (loc.l_lat, loc.l_lon, &grid_x0, &grid_y0);

	nalts = rg.rg_nZ;

	sgbad = dc_GetBadval (dc);
/*
 * Set default vertical limits if no limits exist yet.
 */
	AltUnits = dc_GetLocAltUnits (dc);

	if (P_hgt == 0.0)
	{
		switch (AltUnits)
		{
		    case AU_kmMSL:
		    case AU_kmAGL:
			P_bot = 0.0;
			P_hgt = 12.0;
			break;
		    case AU_mMSL:
		    case AU_mAGL:
			P_bot = 0.0;
			P_hgt = 12000.0;
			break;
		    case AU_mb:
			P_bot = 1000.0;
			P_hgt = -900.0;
			break;
		    case AU_sigma:
			P_bot = 1.0;
			P_hgt = -1.0;
			break;
		    case AU_level:
			P_bot = 0;
			P_hgt = 20;
			break;
		}
	}
/*
 * If we don't have enough points for interpolation, just return a 1x1 plane
 * with a bad flag in it.
 */
	if (rg.rg_nX < 2 || rg.rg_nY < 2 || rg.rg_nZ < 2)
	{
		msg_ELog (EF_INFO, 
		  "xs_Bilinear: Cannot interpolate from %dx%dx%d %s data",
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
 * grid as the horizontal spacing for our plane and just (rather aritrarily)
 * choose 19 vertical levels;
 */
	hdim = Hgrid ? Hgrid : abs ((int)(P_len / rg.rg_Xspacing)) + 1;
	vdim = Vgrid ? Vgrid : 19;
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

	if (hdim > 1)
	{
		f_istep = (X1 - X0) / (rg.rg_Xspacing * (hdim - 1));
		f_jstep = (Y1 - Y0) / (rg.rg_Yspacing * (hdim - 1));
	}
	else
		f_istep = f_jstep = 0.0;
/*
 * h is the horizontal index and v is the vertical index into the vertical 
 * plane we're building.
 *
 * i and j are the horizontal indices and k is the vertical index into the 
 * source grid.
 */
	for (v = 0; v < vdim; v++)
	{
		z = P_bot + (v * P_hgt) / (vdim - 1);
		k = xs_AltIndex (z, alts, nalts);

		for (h = 0; h < hdim; h++)
		{
			pp = plane->data + h * vdim + v;

			f_i = f_i0 + h * f_istep;
			f_j = f_j0 + h * f_jstep;

			i = (int) f_i;
			j = (int) f_j;
		/*
		 * Simple if we're outside the source grid
		 */
			if (i < 0 || j < 0 || k < 0 ||
			    i > rg.rg_nX-2 || j > rg.rg_nY-2 || k > rg.rg_nZ-2)
			{
				*pp = BADVAL;
				continue;
			}
		/*
		 * Do two bilinear interpolations using data from the two
		 * vertical planes bracketing this point.  For each bilinear 
		 * interpolation, we use the four source grid 
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
		/* Plane below */	
			sgp = sourcegrid + k * (rg.rg_nX * rg.rg_nY) + 
				j * rg.rg_nX + i;
			val0 = *sgp;
			val1 = *(sgp + 1);
			val2 = *(sgp + rg.rg_nX);
			val3 = *(sgp + rg.rg_nX + 1);

			if (val0 == sgbad || val1 == sgbad || 
				val2 == sgbad || val3 == sgbad)	
				below = BADVAL;
			else
				below =	(1 - di) * (1 - dj) * val0 +
					di * (1 - dj) * val1 + 
					(1 - di) * dj * val2 + 
					di * dj * val3;
		/* Plane above */
			sgp = sourcegrid + (k + 1) * (rg.rg_nX * rg.rg_nY) + 
				j * rg.rg_nX + i;
			val0 = *sgp;
			val1 = *(sgp + 1);
			val2 = *(sgp + rg.rg_nX);
			val3 = *(sgp + rg.rg_nX + 1);

			if (val0 == sgbad || val1 == sgbad || 
				val2 == sgbad || val3 == sgbad)	
				above = BADVAL;
			else
				above =	(1 - di) * (1 - dj) * val0 +
					di * (1 - dj) * val1 + 
					(1 - di) * dj * val2 + 
					di * dj * val3;
		/*
		 * Now do a linear interpolation between the values from
		 * above and below
		 */
			if (above == BADVAL || below == BADVAL)
				*pp++ = BADVAL;
			else
				*pp++ = (above * (z - alts[k]) + 
					 below * (alts[k+1] - z)) / 
					(alts[k+1] - alts[k]);
		}
	}
/*
 * The plane array is populated, free the data chunk and return
 */
	dc_DestroyDC (dc);
	return (plane);
}



static int
xs_AltIndex (z, alts, nalts)
double	z;
float	*alts;
int	nalts;
/*
 * Return the index into alts such that z lies between alts[index] and
 * alts[index+1].  The alts array must be sorted, but increasing or 
 * decreasing doesn't matter.  Return -1 if z is not encompassed within
 * alts.  We use the ALMOST_BETWEEN function to allow for a bit of round-off
 * error.
 */
{
	int	a;
/*
 * Quick bailout test
 */
	if (! ALMOST_BETWEEN (z, alts[0], alts[nalts-1], 0.001))
		return (-1);
/*
 * Loop until we find the right spot
 */
	for (a = 0; a < nalts - 1; a++)
		if (ALMOST_BETWEEN (z, alts[a], alts[a+1], 0.001))
			return (a);
	return (-1);
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
	{
		msg_ELog (EF_EMERGENCY, 
			  "*BUG* Bad vertical index in xs_AddToLevel");
		return;
	}
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
		cos (ATAN2 (ydat-Y0, xdat-X0) - ATAN2 (Y1-Y0, X1-X0));
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
	float	tick, tickinc, lolim, hilim, xpos, ypos, lat, lon;
	int	lbltoggle;
	XPoint	pts[5];
	int text_pixelht = (Text_size < 1.0) ? 
	    Text_size * (Pix_bottom - Pix_top) : Text_size;
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
	lbltoggle = ((int)(lolim / tickinc) % 2) == 0;

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
		if (lbltoggle)
		{
			if (tickinc < 1.0)
				sprintf (Scratch, "%.1f", tick);
			else
				sprintf (Scratch, "%d", (int) tick);

			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				XPIX (-0.005 * P_len), YPIX (tick), 
				Scratch, 0.0, Text_size, JustifyRight, 
				JustifyBottom);
		}
		lbltoggle = ! lbltoggle;
	}
/*
 * Vertical scale units
 */
	DrawText (Graphics, GWFrame (Graphics), Gcontext, 
		  Pix_left - 2 * text_pixelht,
		  YPIX (P_bot + 0.5 * P_hgt), 
		  (char *) au_LongUnitsName (AltUnits), 
		  90.0, Text_size, JustifyCenter, JustifyBottom);
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
	lbltoggle = TRUE;

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
		lbltoggle = ! lbltoggle;
		if (lbltoggle)
			continue;

		xpos = tick / P_len * (X1 - X0);
		ypos = tick / P_len * (Y1 - Y0);

		switch (BottomLabel)
		{
		    case Label_LatLon:
			prj_Reverse (X0 + xpos, Y0 + ypos, &lat, &lon);

			sprintf (Scratch, "(%.1f/%.1f)", lon, lat);
			break;
		    case Label_XY:
			sprintf (Scratch, "(%.1f,%.1f)", X0 + xpos, Y0 + ypos);
			break;
		    case Label_Length:
			sprintf (Scratch, "%.1f", hypot (xpos, ypos));
			break;
		    default:
			Scratch[0] = '\0';
		}			

		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			  XPIX (tick), YPIX (P_bot) + 2, Scratch, 0.0,
			  Text_size, JustifyCenter, JustifyTop);
	}

	switch (BottomLabel)
	{
	    case Label_LatLon:
		sprintf (Scratch, "Position (lon/lat)");
		break;
	    case Label_XY:
		sprintf (Scratch, "Position (x,y) km from origin");
		break;
	    case Label_Length:
		sprintf (Scratch, "Distance in km");
		break;
	    default:
		Scratch[0] = '\0';
	}			

	DrawText (Graphics, GWFrame (Graphics), Gcontext, XPIX (0.5 * P_len), 
		  Pix_bottom + 1.2 * text_pixelht, Scratch, 0.0, Text_size, 
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
	float	*xpos, *ypos, lat, lon;
	float	site_x, site_y, badvalue;
	int	i, pt, npts, navail;
	zbool	have_lat, have_lon;
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
		prj_Project (loc.l_lat, loc.l_lon, &site_x, &site_y);
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
				prj_Project (lat, lon, &(xpos[pt]), &(ypos[pt]));
		}
	/*
	 * Free the data chunk
	 */
		dc_DestroyDC (dc);
	}
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
		prj_Project (loc.l_lat, loc.l_lon, &site_x, &site_y);

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
	{
		msg_ELog (EF_EMERGENCY, "*BUG* %s",
			  "Too many points in x-section sounding trace!");
		return;
	}

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
xs_GetObsDC (char *comp, char *plat, char *fldname, ZebTime *dtime, 
	     DataClass *class)
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
	DataOrganization	org;
/*
 * Get the ID and data organization of this platform
 */
	pid = ds_LookupPlatform (plat);
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", plat);
		return (NULL);
	}

	org = ds_PlatformDataOrg (pid);

	switch (org)
	{
	   case OrgScalar:
	   case OrgFixedScalar:
		*class = DCC_Scalar;
		break;
	   case Org1dGrid:
		*class = DCC_RGrid;
		break;
	    default:
		msg_ELog (EF_PROBLEM, "Cannot handle %s's data organization",
			  plat);
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
 * Also check for the newer "limit-data-age" method
 */
	if (! AgeCheck (comp, plat, dtime))
	    return (NULL);
/*
 * Get the vertical position and field data
 */
	fieldlist[0] = F_Lookup (Zfld);
	fieldlist[1] = F_Lookup (fldname);

	if (! (dc = ds_FetchObs (pid, *class, dtime, fieldlist, 2, NULL, 0)))
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
xs_GetGridDC (char *comp, char *plat, char *fldname, ZebTime *dtime, 
	      float *alts)
/*
 * Find the 3d grid for the given platform before the current plot time
 * and within the user-specified maximum time difference.  Return
 * a good data chunk if this is possible, otherwise return NULL.  We
 * use an RGrid data chunk, but we bastardize it by returning a separate array
 * of the real, possibly irregularly spaced, altitudes, rather than relying
 * on a fixed altitude spacing.  Return the data time if a non-NULL data chunk
 * is returned.
 */
{
	int	diff, i;
	FieldId	fld;
	PlatformId	pid;
	DataChunk	*dc;
	DataClass	dclass;
	dsDetail	det;
	Location	loc;

	RGrid		rg;
	ZebTime		want_time;
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
	want_time = PlotTime;
	if (ds_IsModelPlatform (pid) && ValidationMode)
		want_time.zt_Sec -= ForecastOffset;
	
	if (! ds_DataTimes (pid, &want_time, 1, DsBefore, dtime))
	{
		TC_EncodeTime (&want_time, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, "No data for '%s' at %s", plat, Scratch);
		return (NULL);
	}
/*
 * Make sure the data start time is within the user-specified
 * limit (if any)
 */
	diff = want_time.zt_Sec - dtime->zt_Sec;
	if (Maxdiff > 0 && diff > Maxdiff)
	{
		msg_ELog (EF_INFO, "No data recent enough from '%s'", plat);
		return (NULL);
	}
/*
 * Also check for the newer "limit-data-age" method
 */
	if (! AgeCheck (comp, plat, dtime))
	    return (NULL);
/*
 * Set the data class based on the platform organization
 */
	switch (ds_PlatformDataOrg (pid))
	{
	    case Org3dGrid:
		dclass = DCC_RGrid;
		break;
	    case OrgScalar:	/* <-- historical, should go away */
	    case OrgNSpace:
		dclass = DCC_NSpace;
		break;
	    default:
		msg_ELog (EF_PROBLEM, "xs_GetGridDC on bad org");
		return (NULL);
	}
/*
 * Detail for forecast offset time
 */
	det.dd_Name = DD_FORECAST_OFFSET;
	det.dd_V.us_v_int = ForecastOffset;
/*
 * Get the data
 */
	fld = F_Lookup (fldname);

	if (! (dc = ds_Fetch (pid, dclass, dtime, dtime, &fld, 1, &det, 1)))
	{
		TC_EncodeTime (&want_time, TC_Full, Scratch);
		msg_ELog (EF_PROBLEM, 
			  "xs_GetGridDC: '%s' missing for '%s' at %s", 
			  fldname, plat, Scratch);
		return (NULL);
	}
/*
 * If we got a true RGrid data chunk, we just need to build the alts array.
 * Otherwise, we need to try to make an RGrid data chunk from an NSpace one.
 */
	if (dclass == DCC_RGrid)
	{
		dc_RGGetGrid (dc, 0, fld, &loc, &rg, NULL);

		for (i = 0; i < rg.rg_nZ; i++)
			alts[i] = loc.l_alt + i * rg.rg_Zspacing;
	}
	if (dclass == DCC_NSpace)
	{
		DataChunk	*rdc;

		rdc = xs_NSpaceToRGrid (dc, fld, alts);
		dc_DestroyDC (dc);
		dc = rdc;
	}
/*
 * Return our data chunk
 */
	return (dc);
}



static DataChunk *
xs_NSpaceToRGrid (dc, fid, alts)
DataChunk	*dc;
FieldId		fid;
float		*alts;
/*
 * If we can convert the given NSpace data chunk into a bastardized RGrid
 * data chunk (with a separate altitude list), return the RGrid data chunk,
 * otherwise return NULL.  The data chunk must meet the following criteria
 * for the conversion to work:
 *
 *	1) The field must have 'lat', 'lon', and 'alt' as dimensions, and they
 *	   must be the only dimensions with size > 1.
 *	2) 'lat', 'lon', and 'alt' are coordinate variables (i.e., variables
 *	   whose dimension has the same name.
 *	3) The values in 'lat' and 'lon' are regularly spaced.
 */
{
    int i, ndims, dimorder;
    float *lat, *lon, *dc_alts, latspacing, lonspacing;
    float delta, olat, olon, *grid, *nsdata;
    char *dnames[DC_MaxField];
    unsigned long dsizes[DC_MaxField];
    FieldId lat_id, lon_id, alt_id;
    RGrid rg;
    unsigned long nlats, nlons, nalts;
    Location location;
    DataChunk *rdc;
    ZebTime when;
/*
 * Start by checking the dimensions of our field.  'dimorder' is an integer
 * used to keep track of the ordering of the lat, lon, and alt dimensions,
 * where alt is assigned value 1, lat is assigned 2, and lon is assigned 3.
 * Decimal positions are used to store the ordering.  (E.g., if the
 * dimensions occur in order lat,alt,lon then dimorder will end up 213.)
 * We use 'dimorder' below when copying data from one data chunk to the other.
 */
    dc_NSGetField (dc, fid, &ndims, dnames, dsizes, NULL);
    dimorder = 0;
	
    for (i = 0; i < ndims; i++)
    {
    /*
     * Alt, lat, or lon
     */
	if (! strcmp (dnames[i], "alt") || 
	    ! strcmp (dnames[i], "altitude"))
	{
	    dimorder = dimorder * 10 + 1;
	    nalts = dsizes[i];
	/*
	 * Get the associated coordinate variable
	 */
	    if ((alt_id = xs_FindCV (dc, dnames[i])) == BadField)
	    {
		msg_ELog (EF_PROBLEM, "%s is not a coordinate variable", 
			  dnames[i]);
		return NULL;
	    }
	}
	else if (! strcmp (dnames[i], "lat") || 
		 ! strcmp (dnames[i], "latitude"))
	{
	    dimorder = dimorder * 10 + 2;
	    nlats = dsizes[i];
	    lat_id = F_Lookup (dnames[i]);
	/*
	 * Get the associated coordinate variable
	 */
	    if ((lat_id = xs_FindCV (dc, dnames[i])) == BadField)
	    {
		msg_ELog (EF_PROBLEM, "%s is not a coordinate variable", 
			  dnames[i]);
		return NULL;
	    }
	}
	else if (! strcmp (dnames[i], "lon") || 
		 ! strcmp (dnames[i], "longitude"))
	{
	    dimorder = dimorder * 10 + 3;
	    nlons = dsizes[i];
	    lon_id = F_Lookup (dnames[i]);
	/*
	 * Get the associated coordinate variable
	 */
	    if ((lon_id = xs_FindCV (dc, dnames[i])) == BadField)
	    {
		msg_ELog (EF_PROBLEM, "%s is not a coordinate variable", 
			  dnames[i]);
		return NULL;
	    }
	}
    /*
     * Other dimension (make sure its size is exactly one)
     */
	else
	{
	    if (dsizes[i] != 1)
	    {
		msg_ELog (EF_PROBLEM, 
			  "xs_NSpaceToRGrid: dim '%s' too big (%d)",
			  dnames[i], dsizes[i]);
		return (NULL);
	    }
	}
    }
/*
 * Make sure we got all three of alt, lat, and lon
 */
    if (dimorder < 100)
    {
	msg_ELog (EF_INFO, 
		  "xs_NSpaceToRGrid: lat, lon, or alt dimension missing");
	return (NULL);
    }
/*
 * Make sure our dimensions are all non-zero.  (We can occasionally get 
 * a 0x0x0 grid from GRIB files...)
 */
	if (nlats * nlons * nalts == 0)
	{
		msg_ELog (EF_DEBUG, "xs_NSpaceToRGrid: zero size grid");
		return (NULL);
	}
/*
 * Check for regular lat and lon spacing
 */
	if (dc_Type (dc, lat_id) != DCT_Float || 
	    dc_Type (dc, lon_id) != DCT_Float ||
	    dc_Type (dc, alt_id) != DCT_Float)
	{
		msg_ELog (EF_PROBLEM, 
			  "xs_NSpaceToRGrid: non-float lat, lon, or alt");
		return (NULL);
	}

	lat = (float *) dc_NSGetSample (dc, 0, lat_id, &nlats);
	latspacing = lat[1] - lat[0];
	for (i = 2; i < nlats; i++)
	{
		delta = lat[i] - lat[i-1];
		if (fabs (delta - latspacing) > 0.001)
		{
			msg_ELog (EF_PROBLEM, 
				  "xs_NSpaceToRGrid: Irregular lat step");
			return (NULL);
		}
	}
	
	lon = (float *) dc_NSGetSample (dc, 0, lon_id, &nlons);
	lonspacing = lon[1] - lon[0];

	if (lonspacing < -180.0)
		lonspacing += 360.0;
	else if (lonspacing > 180.0)
		lonspacing -= 360.0;
	
	for (i = 2; i < nlons; i++)
	{
		delta = lon[i] - lon[i-1];

		if (delta < -180.0)
			delta += 360.0;
		else if (delta > 180.0)
			delta -= 360.0;

		if (fabs (delta - lonspacing) > 0.001)
		{
			msg_ELog (EF_PROBLEM, 
				  "xs_NSpaceToRGrid: Irregular lon step");
			return (NULL);
		}
	}
/*
 * Get the altitudes and copy them into the user's array.
 */
	dc_alts = (float *) dc_NSGetSample (dc, 0, alt_id, &nalts);
	memcpy ((void *) alts, (void *) dc_alts, nalts * sizeof (float));
/*
 * Our criteria have been met.  Let's build an RGrid data chunk.
 */
	rdc = dc_CreateDC (DCC_RGrid);
        rdc->dc_Platform = dc->dc_Platform;

	dc_RGSetup (rdc, 1, &fid); 

	dc_SetLocAltUnits (rdc, dc_GetLocAltUnits (dc));

	dc_SetBadval (rdc, dc_GetBadval (dc));
/*
 * Grid info and location
 */
	prj_GetOrigin (&olat, &olon);
	prj_Project (olat + fabs (latspacing), olon + fabs (lonspacing),
		  &rg.rg_Xspacing, &rg.rg_Yspacing);

	rg.rg_Zspacing = 0.0;	/* bastardized RGrid */

	rg.rg_nX = nlons;
	rg.rg_nY = nlats;
	rg.rg_nZ = nalts;

	location.l_lat = (latspacing > 0) ? lat[0] : lat[nlats-1];
	location.l_lon = (lonspacing > 0) ? lon[0] : lon[nlons-1];
	location.l_alt = alts[0];
/*
 * Allocate space for the RGrid data and get a pointer to the NSpace data
 */
	grid = (float *) malloc (nlats * nlons * nalts * sizeof (float));
	nsdata = (float *) dc_NSGetSample (dc, 0, fid, NULL);
/*
 * Copy into our new grid.  If the dimension order in the NSpace data chunk
 * is (alt,lat,lon), then we can just do a memcpy.  Otherwise, it's element
 * by element since NSpace data are always stored in row major order.
 */
	if (dimorder == 123)
		memcpy ((char *) grid, (char *) nsdata, 
			nlats * nlons * nalts * sizeof (float));
	else
	{
		msg_ELog (EF_PROBLEM, 
		  "xs_NSpaceToRGrid: NSpace reordering not yet implemented");
		dc_DestroyDC (rdc);
		free (grid);
		return (NULL);
	}
/*
 * Finally, put the grid and time into the new data chunk
 */		
	dc_GetTime (dc, 0, &when);
	dc_RGAddGrid (rdc, 0, fid, &location, &rg, &when, grid, 0);
/*
 * Free the space we created and return the newly created regular grid.
 */
	free (grid);
	return (rdc);
}



void
xs_Raster (c, update)
char	*c;
zbool	update;
/*
 * Project a vertical raster image onto our cross-section plane
 */
{
	zbool	ok, image, xiraster;
	int	nplat, nsteps;
	char	platform[PlatformListLen];
	char	*pnames[MaxPlatforms], fldname[80], cname[20], hcolor[20];
	char	justname[64];
	char	param[50], outrange[40];
	unsigned char *igrid = 0;
	float	center, step, step_per_color, hrange, hvalue, alt;
	float	*fgrid = 0;
	float	xleft, xright, yleft, yright, max, min, d_left, d_right;
	float	bot, top, wanted_azim, azim, hlen, ang;
	int	hdim, vdim, shifted, highlight;
	int	pix_x0, pix_x1, pix_y0, pix_y1;
	FieldId	fid;
	ZebTime	zt;
	RGrid	rg;
	XColor	xc, xoutr;
	ScaleInfo	scale;
	XRectangle	clip;
	Location	loc;
	PlatformId	pid;
	DataChunk	*dc;
	DataOrganization	org;
/*
 * Platform
 */
	if (Zig_zag)
	{
		msg_ELog (EF_INFO, 
			  "Cannot do raster plot on a zig-zag cross-section");
		return;
	}
	else
		ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, 
				    SYMT_STRING);
/*
 * Get the chosen field, turn it into a FieldId, then get just the name
 * portion
 */
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fldname, SYMT_STRING);
	fid = F_Lookup (fldname);
	strcpy (justname, SimpleFieldName (fid));
/*
 * Autoscale?
 */
	if (pda_Search (Pd, c, "scale-mode", NULL, param, SYMT_STRING))
		Autoscale = ! strncmp (param, "auto", 4);
	else
		Autoscale = FALSE;
/*
 * Scale and color table info
 */
	if (ok && ! Autoscale)
	{
		sprintf (param, "%s-center", justname);
		ok &= pda_ReqSearch (Pd, c, param, "raster", (char *) &center, 
				     SYMT_FLOAT);

		sprintf (param, "%s-step", justname);
		ok &= pda_ReqSearch (Pd, c, param, "raster", (char *) &step, 
				     SYMT_FLOAT);

		nsteps = 11;
		sprintf (param, "%s-nsteps", justname);
		pda_Search (Pd, c, param, "raster", (char *) &nsteps, 
			    SYMT_INT);

		if (! ok)
		{
			msg_ELog (EF_PROBLEM, 
				  "xs_Raster: forcing autoscaling");
			ok = Autoscale = TRUE;
		}
	}
		
	ok &= pda_ReqSearch (Pd, c, "color-table", "raster", cname, 
			     SYMT_STRING);
/*
 * Out of range color?
 */
	Do_outrange = TRUE;
	strcpy (outrange, "black");
	pda_Search (Pd, c, "out-of-range-color", NULL, outrange, SYMT_STRING);

	if (! strcmp (outrange, "none"))
		Do_outrange = FALSE;
	else if (! ct_GetColorByName (outrange, &C_outrange))
		ct_GetColorByName ("black", &C_outrange);
/*
 * Give up if we didn't get all the required parameters
 */
	if (! ok)
		return;
/*
 * Load the color table
 */
	ct_LoadTable (cname, &Colors, &Ncolors);
/*
 * Get our platform
 */
	nplat = CommaParse (platform, pnames);
	if (nplat > 1)
		msg_ELog (EF_INFO, 
			  "Only the first platform is used by xs_Raster()");
/*
 * Make sure the platform is other than bogus, and get its organization.
 */
	if ((pid = ds_LookupPlatform (pnames[0])) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "xs_Raster: Unknown platform '%s'", 
			  pnames[0]);
		return;
	}
	if ((org = ds_PlatformDataOrg (pid)) == OrgImage)
		image = TRUE;
# ifdef notdef
	else if (org == Org3dGrid || org == Org2dGrid || org == OrgNSpace)
		image = FALSE;
# endif
	else
	{
		msg_ELog (EF_PROBLEM, "xs_Raster: Cannot do raster plot of %s",
			  pnames[0]);
		return;
	}
/*
 * Get info for highlighting and area.
 */
	highlight = FALSE;
	sprintf (param, "%s-highlight-range", justname);

	hrange = 0.0;
	pda_Search (Pd, c, param, "raster", (char *) &hrange, SYMT_FLOAT);
	if (hrange != 0.0)
	{
		highlight = TRUE;

		strcpy (hcolor, "white");
		sprintf (param, "%s-highlight-color", justname);
		pda_Search (Pd, c, param, "raster", hcolor, SYMT_STRING);

		hvalue = 0.0;
		sprintf (param, "%s-highlight", justname);
		pda_Search (Pd, c, param, "raster", (char *) &hvalue, 
			    SYMT_FLOAT);
	}
/*
 * Set user coordinates for the graphics
 */
	Xlo = 0.0;
	Xhi = P_len;
	Ylo = P_bot;
	Yhi = P_bot + P_hgt;
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	zt = PlotTime;
	if (image)
	{
		wanted_azim = 90.0 - RAD_TO_DEG (ATAN2 (Y1-Y0, X1-X0));

		if (! ClosestRHI (c, pid, wanted_azim, &zt, &azim))
			return;

		if (!(dc = ds_Fetch (pid, DCC_Image, &zt, &zt, &fid, 1, 
				     NULL, 0)))
			return;

		igrid = (unsigned char *) 
			dc_ImgGetImage (dc, 0, fid, &loc, &rg, NULL, &scale);
	/*
	 * In kluged RHI raster files, x is the horizontal dimension, and y
	 * is the vertical dimension.
	 */
		hdim = rg.rg_nX;
		vdim = rg.rg_nY;
		hlen = (rg.rg_nX - 1) * rg.rg_Xspacing;

		cvt_ToXY (loc.l_lat, loc.l_lon, &xleft, &yleft);


		ang = DEG_TO_RAD (90.0 - azim);
		xright = xleft + hlen * cos (ang);
		yright = yleft + hlen * sin (ang);

		bot = 0.0;
		top = (rg.rg_nY - 1) * rg.rg_Yspacing;
	}
	else
	/*
	 * Raster plots only from image data for now
	 */
		msg_ELog (EF_PROBLEM, "xs_Raster: How did we get here?!");

	if (!igrid && !fgrid)
	{
		msg_ELog (EF_INFO, "xs_Raster: Unable to get grid for %s", 
			  pnames[0]);
		dc_DestroyDC (dc);
		return;
	}
/*
 * Project the endpoints of the raster grid onto our plane
 */
	d_left = hypot (xleft-X0, yleft-Y0) *
		cos (ATAN2 (yleft-Y0, xleft-X0) - ATAN2 (Y1-Y0, X1-X0));
	d_right = hypot (xright-X0, yright-Y0) *
		cos (ATAN2 (yright-Y0, xright-X0) - ATAN2 (Y1-Y0, X1-X0));
/*
 * If everything's off the screen, we're done.
 */
	if ((d_left < 0 && d_right < 0) || (d_left > P_len && d_right > P_len))
		return;
/*
 * Compute screen coordinates
 */
	pix_x0 = XPIX (d_left);
	pix_x1 = XPIX (d_right);
	pix_y0 = YPIX (bot);
	pix_y1 = YPIX (top);
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
	RP_Init (Colors, Ncolors, C_outrange, clip, min, max, highlight, 
		 hvalue, xc, hrange);
	
	xiraster = TRUE;

	if (image)
	{
	/*
	 * Pass NULL for the last two parameters so that 
	 * RasterImagePlot() doesn't apply a map projection...
	 */
		RasterImagePlot (Graphics, DrawFrame, igrid, hdim, vdim, 
				 pix_x0, pix_y0, pix_x1, pix_y1, scale.s_Scale,
				 scale.s_Offset, NULL, NULL);
	}
# ifdef notdef
	else if (xiraster)
		RasterXIPlot (Graphics, GWFrame (Graphics), fgrid, hdim, vdim, 
			      pix_x0, pix_y0, pix_x1, pix_y1, FALSE);
	else
		RasterPlot (Graphics, GWFrame (Graphics), fgrid, hdim, vdim, 
			    pix_x0, pix_y0, pix_x1, pix_y1);
# endif
/*
 * Free the data chunk.
 */
	dc_DestroyDC (dc);
/*
 * Draw the background and set up for side annotation.  At some point, the
 * color bar should become more like the one in CAP_RasterSideAnnot.
 */
	xs_Background ();

	step_per_color = (step * nsteps) / Ncolors;
	sprintf (Scratch, "%s|%s|%f|%f", fldname, cname, center, 
		 step_per_color); 
	An_AddAnnotProc (An_ColorBar, c, Scratch, strlen (Scratch), 75, 
			 TRUE, FALSE);
}




void
xs_Track (c, update)
char	*c;
zbool	update;
/*
 * Project a track onto our cross-section plane
 */
{
	msg_ELog (EF_INFO, "xs_Track: no cross-section tracks yet");
}


static FieldId
xs_FindCV (DataChunk *dc, const char *dimname)
/*
 * From the dc, find a coordinate variable matching the given dimension name,
 * and return its FieldId.  If no such coordinate variable exists, return
 * BadField.
 */
{
    FieldId var_ids[DC_MaxField];
    int nvars = dc_NSGetAllVariables(dc, var_ids, 0);
    char *dnames[DC_MaxField];
    int v, ndims;
/*
 * Find a variable with a name match first.
 */   
    for (v = 0; v < nvars; v++)
	if (! strcmp (F_GetName (var_ids[v]), dimname))
	    break;

    if (v == nvars)
	return (BadField);
/*
 * We have a name match, so verify that this field has exactly one dimension
 * and that the dimension name is the same as the passed-in dimension name.
 */
    dc_NSGetField (dc, var_ids[v], &ndims, dnames, NULL, NULL);
    if (ndims == 1 && ! strcmp (dnames[0], dimname))
	return (var_ids[v]);
    else
	return (BadField);
}



# endif  /* C_PT_XSECT */
