/*
 * Deal with static (or almost static) overlays.
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
# include <stdio.h>
# include <X11/Intrinsic.h>
# include <string.h>

# include <ui.h>

# include <config.h>
# include <defs.h>

RCSID("$Id: Overlay.c,v 2.57 1997-01-03 17:13:44 granger Exp $")

# include <pd.h>
# include <GraphicsW.h>
# include <message.h>
# include <dm.h>
# include <DataStore.h>
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"
# include "gp_cmds.h"

# ifndef M_PI
# define M_PI 3.14159265358979323846
# endif

/*
 * Stuff for locations and other things needing icons.
 */
typedef struct _OvIcon
{
	Pixmap	oi_pixmap;		/* The pixmap for this icon	*/
	unsigned int oi_w, oi_h;	/* Dimensions of the pixmap	*/
	int	oi_xh, oi_yh;		/* Hot spot			*/
} OvIcon;

static stbl OvIcons = 0;	/* Symbol table for icons		*/

/* //////////////////////////////////////////////////////////////////// */

# if C_CAP_OVERLAY

typedef enum
{
	NoLabel, LabelPlatform, LabelString
} LabelOpt;


/*
 * The internals of a predefined feature.
 */
# define MAXFSTRING 120
# define NFEATURE 20
# define DEFAULT_TEXT_SIZE 0.02

/*
 * Modulus operation which is always positive given any a and some b > 0
 */
#define FMOD(a,b)   ( ((a)>=0) ? (fmod((a),(b))) \
		    : ( fmod((a)+((b)*(floor(fabs(a)/(b))+1)),(b)) ) )
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)
# define DEG_TO_KM(x)	((x)*111.3238367) /* on a great circle */
# define KM_TO_DEG(x)	((x)*0.008982802) /* on a great circle */
# define LONDEG_TO_KM(lon,olat) (DEG_TO_KM (lon) * cos (DEG_TO_RAD(olat)))
# define LATDEG_TO_KM(lat) (DEG_TO_KM (lat))
# define KM_TO_LATDEG(ykm) (KM_TO_DEG (ykm))
# define KM_TO_LONDEG(xkm,olat) (KM_TO_DEG((xkm)/cos(DEG_TO_RAD(olat))))
# define TOLERANCE  (0.0001)
# define NM_TO_KM	(1.609344 * 6080.0 / 5280.0)
# define KM_TO_NM	(0.62137119 * 5280.0 / 6080.0)


typedef enum { FText, FCircle, FMarker } FeatureType;
typedef struct feature
{
	FeatureType	f_type;			/* Feature type		*/
	float		f_lat, f_lon;		/* Location		*/
	float		f_radius;		/* Circle radius	*/
	char		f_string[MAXFSTRING];	/* Text string		*/
	float		f_size;			/* Text size		*/
	UItime		f_when;			/* Time			*/
	struct feature	*f_next;		/* Next in chain	*/
} Feature;

struct FList
{
	char		fl_name[40];
	Feature		*fl_features;
	Feature		*fl_tail;
};

static stbl Ftable = 0;


/*
 * How many polyline segments we can get away with drawing at once.
 */
# define MAXPLSEG 100


/*
 * Map drawing stuff.  With big maps, a lot of time can go into the
 * reading and coordinate-conversion of the data, so we do it once and
 * store it.
 */
static stbl MapTable = 0;
typedef struct _MapPoints
{
	int mp_npt;		/* Number of points in this segment. 	*/
	int mp_pkey;		/* Projection key			*/
	bool mp_fill;		/* Draw as filled polygon if true 	*/
	float *mp_x, *mp_y;	/* The points themselves		*/
	struct _MapPoints *mp_next;	/* Next entry in chain		*/
} MapPoints;



/*
 * Our internal overlay drawing routines.
 */
static void 	ov_GridBBox FP ((char *, int));
static void 	ov_DrawFeature FP ((char *, int));
static void	ov_DoFeature FP ((Feature *fp));
static void 	ov_Map FP ((char *, int));
static void 	ov_WBounds FP ((char *, int));
static void 	ov_RangeRings FP ((char *, int));
static void 	ov_Location FP ((char *, int));
void 	ov_Grid FP ((char *, int));	/* moved to Ov_Grid.c */
static void 	ov_AzLimits FP ((char *, int));
static bool 	ov_GetWBounds FP ((char *, char *, float *, float *, float *,
			float *, float *));
static int 	ov_FindWBReply FP ((struct message *, struct dm_rp_wbounds *));
static void 	ov_Boundary FP ((char *, int));
static bool 	ov_GetBndParams FP ((char *, char *, XColor *, int *, bool *,
			LabelOpt *, char *, float *, int *, char *, int *));
static int 	ov_RRInfo FP ((char *, char *, Location *, float *, float *,
			float *, float *, int *, float *, XColor *, int *,
			float *, bool *, float *, bool *));
static OvIcon 	*ov_GetIcon FP ((char *));
static int 	ov_LocSetup FP ((char *, char **, int *, char *, LabelOpt *,
			char *, bool *, float *, int *, PlatformId **, int *));
static void	ov_LocPlot FP ((char *, char *, Location *, ZebTime *, char *,
			int, LabelOpt, char *, double, int));
static MapPoints *ov_LoadMap FP ((char *));
static void	ov_DrawMap FP ((const MapPoints *));
static void	ov_ZapMap FP ((MapPoints *));
static int ov_InFeature ();
static void	ov_FillPolygon FP ((float *x, float *y, int));


/*
 * The table to map the overlay type ("field" parameter) onto the function
 * that draws it.
 */
static struct overlay_table
{
	char *ot_type;		/* Overlay type		*/
	void (*ot_func) ();	/* Function.		*/
} Ov_table[] =
{
	{ "gridbbox",	ov_GridBBox	},
	{ "wbox",	ov_WBounds	},
	{ "wbounds",	ov_WBounds	},
	{ "feature",	ov_DrawFeature	},
	{ "map",	ov_Map		},
	{ "boundary",	ov_Boundary	},
	{ "range-rings", ov_RangeRings	},
	{ "location",	ov_Location	},
	{ "solidgrid",	ov_Grid		},
	{ "grid",	ov_Grid		},
	{ "azimuth-limits", ov_AzLimits },
	{ 0, 0}
};






void
ov_CAPOverlay (comp, update)
char *comp;
bool update;
/*
 * Draw an overlay onto the screen.
 */
{
	int i;
	char type[50];
/*
 * Pull out the type of overlay they want.
 */
	if (! pda_ReqSearch (Pd, comp, "field", NULL, type, SYMT_STRING))
		return;
/*
 * Simply try to find a routine which can do this.
 */	
	for (i = 0; Ov_table[i].ot_type; i++)
		if (! strcmp (Ov_table[i].ot_type, type))
			break;
	if (! Ov_table[i].ot_type)
	{
		msg_ELog (EF_PROBLEM, "Unknown overlay type: %s", type);
		return;
	}
/*
 * Invoke it.
 */
	(*Ov_table[i].ot_func) (comp, update);
}




/*ARGSUSED*/
static void
ov_AzLimits (comp, update)
char *comp;
bool update;
/*
 * Do an azimuth limits plot.
 */
{
	char color[40], plat[40];
	int lwidth, range, pox, poy, lx, ly, rx, ry;
	unsigned int bbsize;
	float ox, oy, left, right;
	XColor xc;
	PlatformId pid;
	Location loc;
	DataChunk *dc;
	UItime	t;
	ZebTime zt;
	FieldId fieldlist[2];
/*
 * Platform stuff.
 */
	if (! pda_ReqSearch (Pd, comp, "platform", NULL, plat, SYMT_STRING))
		return;
	if (! GetLocation (plat, &PlotTime, &loc))
	{
		msg_ELog (EF_PROBLEM, "Unable to locate platform '%s'", plat);
		return;
	}
/*
 * How far do we go.
 */
	if (! pda_Search (Pd, comp, "range", plat, (char *) &range, SYMT_INT))
		range = 100;
/*
 * Find our limits platform.
 */
	strcat (plat, "-az-limits");
	if ((pid = ds_LookupPlatform (plat)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "No az limits plat %s", plat);
		return;
	}
/*
 * Make the field list.
 */
	fieldlist[0] = F_Lookup ("left");
	fieldlist[1] = F_Lookup ("right");
/*
 * Color information.
 */
	if (! pda_Search (Pd, comp, "color", "overlay", color, SYMT_STRING))
		strcpy (color, "white");
/*
 * Graphics context stuff.
 */
	ct_GetColorByName (color, &xc);
	XSetForeground (Disp, Gcontext, xc.pixel);
	if (pda_Search (Pd, comp, "line-width", "overlay", (char *) &lwidth,
			SYMT_INT))
		XSetLineAttributes (Disp, Gcontext, lwidth, LineSolid,
			CapButt, JoinMiter);
/*
 * Find out when there is something available.
 */
	if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, &zt))
		return;
/*
 * Check into age limits.
 */
	TC_ZtToUI (&zt, &t);
 	if (! AgeCheck (comp, plat, &zt))
	{
		msg_ELog (EF_INFO, "Az limits %s too old %d %d", plat,
			t.ds_yymmdd, t.ds_hhmmss);
		return;
	}
/*
 * Snarf it.
 */
	if (! (dc = ds_Fetch (pid, DCC_Scalar, &zt, &zt, fieldlist, 2, 
		NULL, 0)))
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s", plat);
		return;
	}
/*
 * Get values of right and left azimuth limits.
 */
	left = dc_GetScalar (dc, 0, fieldlist[0]);
	right = dc_GetScalar (dc, 0, fieldlist[1]);
/*
 * Figure out end points.
 */
	prj_Project (loc.l_lat, loc.l_lon, &ox, &oy);
	pox = XPIX (ox);  poy = YPIX (oy);
	lx = XPIX (ox + range*sin (left*M_PI/180.0));
	ly = YPIX (oy + range*cos (left*M_PI/180.0));
	rx = XPIX (ox + range*sin (right*M_PI/180.0));
	ry = YPIX (oy + range*cos (right*M_PI/180.0));
/*
 * Draw.
 */
	XDrawLine (Disp, GWFrame (Graphics), Gcontext, pox, poy, lx, ly);
	XDrawLine (Disp, GWFrame (Graphics), Gcontext, pox, poy, rx, ry);
	bbsize = 2*(XPIX (ox + range) - pox);
	XDrawArc (Disp, GWFrame (Graphics), Gcontext, XPIX (ox - range),
		YPIX (oy + range), bbsize, bbsize,
		(int) (90 - left)*64, (int) (left - right)*64);
/*
 * Free up the data object and chunk.
 */
	dc_DestroyDC (dc);
}






/*ARGSUSED*/
static void
ov_GridBBox (comp, update)
char *comp;
bool update;
/*
 * Draw the bounding box of the grid indicated by the platform field.
 */
{
	char platform[40], color[40];
	float x0, y0, x1, y1;
	int lwidth, px0, py0, px1, py1;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	Drawable d = GWFrame (Graphics);
/*
 * Find our platform.
 */
	if (! pda_ReqSearch (Pd,comp, "platform", NULL, platform, SYMT_STRING))
		return;
/*
 * Now try to pull in the bounding box.
 */
	if (! ga_GridBBox (&PlotTime, platform, &x0, &y0, &x1, &y1))
	{
		msg_ELog (EF_INFO, "Unable to load grid bbox for %s",platform);
		return;
	}
	px0 = XPIX (x0); py0 = YPIX (y0);
	px1 = XPIX (x1); py1 = YPIX (y1);
/*
 * Color information.
 */
	if (! pda_Search (Pd, comp, "color", "overlay", color, SYMT_STRING))
		strcpy (color, "white");
/*
 * Graphics context stuff.
 */
	ct_GetColorByName (color, &xc);
	XSetForeground (disp, Gcontext, xc.pixel);
	if (pda_Search (Pd, comp, "line-width", "overlay", (char *) &lwidth,
			SYMT_INT))
		XSetLineAttributes (disp, Gcontext, lwidth, LineSolid,
			CapButt, JoinMiter);
/*
 * Just draw.
 */
	XDrawLine (disp, d, Gcontext, px0, py0, px1, py0);
	XDrawLine (disp, d, Gcontext, px1, py0, px1, py1);
	XDrawLine (disp, d, Gcontext, px1, py1, px0, py1);
	XDrawLine (disp, d, Gcontext, px0, py1, px0, py0);
/*
 * Return to zero-width lines and we're done
 */
	XSetLineAttributes (disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
}




/*ARGSUSED*/
static void
ov_WBounds (comp, update)
char *comp;
bool update;
/*
 * Draw the bounding box of the window indicated by the platform field.
 */
{
	char platform[40], color[40], ptype[40];
	float x0, y0, x1, y1, alt;
	int lwidth, px0, py0, px1, py1;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	Drawable d = GWFrame (Graphics);
/*
 * Find our platform.
 */
	if (! pda_ReqSearch (Pd,comp, "platform", NULL, platform, SYMT_STRING))
		return;
/*
 * Now try to pull in the bounding box.
 */
	if (! ov_GetWBounds (platform, ptype, &x0, &y0, &x1, &y1, &alt))
	{
		msg_ELog (EF_INFO, "Unable to load win bbox for %s", platform);
		return;
	}
	px0 = XPIX (x0); py0 = YPIX (y0);
	px1 = XPIX (x1); py1 = YPIX (y1);
/*
 * Color information.
 */
	if (! pda_Search (Pd, comp, "color", "overlay", color, SYMT_STRING))
		strcpy (color, "white");
/*
 * Graphics context stuff.
 */
	ct_GetColorByName (color, &xc);
	XSetForeground (disp, Gcontext, xc.pixel);
	if (pda_Search (Pd, comp, "line-width", "overlay", (char *) &lwidth,
			SYMT_INT))
		XSetLineAttributes (disp, Gcontext, lwidth, LineSolid,
			CapButt, JoinMiter);
/*
 * Draw a box or a line based on the plot type.  If a box, the upper left
 * corner of the rectangle is actually (x0,y1), and py0 > py1.
 */
	if (! strcmp (ptype, "CAP"))
		XDrawRectangle (disp, d, Gcontext, px0, py1, px1 - px0 + 1,
				py0 - py1 + 1);
	else
		XDrawLine (disp, d, Gcontext, px0, py0, px1, py1);
/*
 * Return to zero-width lines and we're done
 */
	XSetLineAttributes (disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
}




static bool
ov_GetWBounds (window, ptype, x0, y0, x1, y1, alt)
char *window, *ptype;
float *x0, *y0, *x1, *y1, *alt;
/*
 * Get the window bounds and plot type.
 */
{
	struct dm_rq_wbounds wb;
	struct dm_rp_wbounds repl;
/*
 * Fill in and send out request.  Then wait for a reply.
 */
	wb.dmm_type = DM_WBOUNDS;
	strcpy (wb.dmm_window, window);
	dm_Send (&wb, sizeof (wb));
	msg_Search (MT_DISPLAYMGR, ov_FindWBReply, &repl);
/*
 * If it failed, so do we.
 */
	if (! repl.dmm_success)
		return (FALSE);
/*
 * Return the info.
 */
	strcpy (ptype, repl.dmm_pltype);
	*x0 = repl.dmm_x0;
	*y0 = repl.dmm_y0;
	*x1 = repl.dmm_x1;
	*y1 = repl.dmm_y1;
	*alt = repl.dmm_alt;
	return (TRUE);
}





static int
ov_FindWBReply (msg, repl)
struct message *msg;
struct dm_rp_wbounds *repl;
/*
 * Pick through messages until we find the one we need.
 */
{
	struct dm_rp_wbounds *rp = (struct dm_rp_wbounds *) msg->m_data;
/*
 * If this is the expected reply, return the info and we're done.
 */
	if (rp->dmm_type == DM_WBOUNDS)
	{
		*repl = *rp;
		return (0);
	}
/*
 * If this is a DM_DIE, the reply may never arrive, so we better just go
 * away.
 */
 	else if (rp->dmm_type == DM_DIE)
		GPShutDown ();
/*
 * Otherwise we ignore this one for now.
 */
	return (1);
}



/*ARGSUSED*/
static void
ov_DrawFeature (comp, update)
char *comp;
bool update;
/*
 * Draw a predefined feature onto the screen.
 */
{
	char platform[40], color[40];
	int lwidth, type;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	struct FList *fl;
	union usy_value v;
	Feature *last, *fp;
	UItime temptime;
/*
 * Find our "platform".  (Really the name of the feature of interest).
 */
	if (! pda_ReqSearch (Pd,comp, "platform", NULL, platform, SYMT_STRING))
		return;
	if (! Ftable || ! usy_g_symbol (Ftable, platform, &type, &v))
	{
		msg_ELog (EF_PROBLEM, "Unknown feature: %s", platform);
		return;
	}
	fl = (struct FList *) v.us_v_ptr;
/*
 * Color information.
 */
	if (! pda_Search (Pd, comp, "color", "overlay", color, SYMT_STRING))
		strcpy (color, "white");
/*
 * Graphics context stuff.
 */
	ct_GetColorByName (color, &xc);
	XSetForeground (disp, Gcontext, xc.pixel);
	if (pda_Search (Pd, comp, "line-width", "overlay", (char *) &lwidth,
			SYMT_INT))
		XSetLineAttributes (disp, Gcontext, lwidth, LineSolid,
			CapButt, JoinMiter);
/*
 * Find the closest set of features.
 */
	if (fl->fl_features && fl->fl_features->f_type != FMarker)
		fp = fl->fl_features;
	else
	{
		TC_ZtToUI (&PlotTime, &temptime);
		last = 0;
		for (fp = fl->fl_features; fp; fp = fp->f_next)
			if (fp->f_type == FMarker &&
				fp->f_when.ds_yymmdd == temptime.ds_yymmdd &&
				fp->f_when.ds_hhmmss <= temptime.ds_hhmmss)
				last = fp;
		fp = last ? last->f_next : 0;
	}
	if (! fp)
	{
		msg_ELog (EF_DEBUG, "No features found");
		return;
	}
/*
 * Go through each piece of the feature and draw it.
 */
	for (; fp && fp->f_type != FMarker; fp = fp->f_next)
	{
		ov_DoFeature (fp);
	}
/*
 * Go back to zero-width lines and we're done
 */
	XSetLineAttributes (disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
}




static void
ov_DoFeature (fp)
Feature *fp;
/*
 * Draw a single feature onto the screen.
 */
{
	float x, y;
	int px, py, width;
/*
 * Turn our location into something we can use.
 */
	prj_Project (fp->f_lat, fp->f_lon, &x, &y);
/*
 * Now just do some drawing.
 */
	switch (fp->f_type)
	{
	/*
	 * For text, we just pass off to the text drawing routines.
	 */
	   case FText:
		px = XPIX (x); py = YPIX (y);
	   	DrawText (Graphics, GWFrame (Graphics), Gcontext, px, py, 
			fp->f_string, 0.0, fp->f_size, JustifyCenter, 
			JustifyCenter);
		break;
	/*
	 * Circles we do ourselves.
	 */
	   case FCircle:
		px = XPIX (x - fp->f_radius); py = YPIX (y + fp->f_radius);
		width = XPIX (x + fp->f_radius) - px;
	   	XDrawArc (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, 
			px, py, width, width, 0, 360*64);
		break;
	   case FMarker:
		msg_ELog (EF_DEBUG, "Marker not handled in DoFeature");
		break;
	}
}


/*ARGSUSED*/
static void
ov_Map (comp, update)
char *comp;
bool update;
/*
 * Put a map overlay onto the screen.
 */
{
	char platform[40], color[40];
	int lwidth;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	MapPoints *points;
/*
 * Find our platform -- the map name.
 */
	if (! pda_ReqSearch (Pd,comp, "platform", NULL, platform, SYMT_STRING))
		return;
/*
 * Color information.
 */
	if (! pda_Search (Pd, comp, "color", "map", color, SYMT_STRING))
		strcpy (color, "white");
/*
 * Graphics context stuff.
 */
	ct_GetColorByName (color, &xc);
	XSetForeground (disp, Gcontext, xc.pixel);
/*
 * Use a line-width if we got it, zero otherwise.  Set our attributes whether
 * we found a parameter or not.
 */
	if (! pda_Search (Pd, comp, "line-width", "map", (char *) &lwidth, 
			  SYMT_INT))
		lwidth = 0;
	XSetLineAttributes (disp, Gcontext, lwidth, LineSolid,
			    CapButt, JoinMiter);
/*
 * Load up the map file.
 */
	if ((points = ov_LoadMap (platform)) == 0)
		return;
/*
 * Draw it.
 */
	SetClip (FALSE);
	ov_DrawMap (points);
/*
 * Restore the GC
 */
	XSetLineAttributes (disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
	ResetGC ();
}



static void
ov_DrawMap (points)
const MapPoints * points;
/*
 * Draw this map file onto the screen.
 */
{
	int pt, xp;
	XPoint pts[MAXPLSEG];
/*
 * Just plow through the file.
 */
	for (; points; points = points->mp_next)
	{
	/* 
	 * Do a filled polygon if requested
	 */
		if (points->mp_fill)
		{
		    ov_FillPolygon (points->mp_x, points->mp_y, 
				    points->mp_npt);
		    continue;
		}
	/*
	 * Pass through each point.
	 */
		xp = 0;
		for (pt = 0; pt < points->mp_npt; ++pt)
		{
		/*
		 * Convert the point into pixel space.
		 */
		 	pts[xp].x = XPIX (points->mp_x[pt]);
			pts[xp].y = YPIX (points->mp_y[pt]);
			++xp;
		/*
		 * Lest we get some bogus pixel coords when we're zoomed
		 * really close, don't draw segments which don't intersect
		 * the user coordinates and don't have at least one end point
		 * within the user window.
		 */
			if (xp >= 2 && (! Intersects (points->mp_x[pt - 1],
				      points->mp_y[pt - 1], points->mp_x[pt], 
				      points->mp_y[pt])))
			{
			/*
			 * Skip this segment.  Draw what we've got and start
			 * over with the next point.
			 */
				if (xp >= 3)
				{
					XDrawLines (XtDisplay (Graphics),
					    GWFrame (Graphics), Gcontext, pts, 
					    xp - 1, CoordModeOrigin);
				}
				pts[0] = pts[xp - 1];
				xp = 1;

			}
		/*
		 * If we've filled our array, shove it out.
		 */
		 	else if (xp >= MAXPLSEG)
			{
				XDrawLines (XtDisplay (Graphics),
					GWFrame (Graphics), Gcontext, pts, xp,
					CoordModeOrigin);
				pts[0] = pts[xp - 1];
				xp = 1;
			}
		}
	/*
	 * Draw any points remaining.
	 */
		if (xp > 1)
			XDrawLines (XtDisplay (Graphics), GWFrame (Graphics),
				Gcontext, pts, xp, CoordModeOrigin);
	}
}



static void
ov_FillPolygon (x, y, npts)
float *x;
float *y;
int npts;
{
    static XPoint	*poly = 0;
    static int		polysize = 0;
    int	i;

    if (! poly || polysize < npts)
    {
	poly = (XPoint *) realloc (poly, npts * sizeof (XPoint));
	polysize = npts;
    }

    for (i = 0; i < npts; i++)
    {
	poly[i].x = XPIX (x[i]);
	poly[i].y = YPIX (y[i]);
    }

    XSetFillRule (XtDisplay (Graphics), Gcontext, WindingRule);
    XFillPolygon (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, poly,
		  npts, Complex, CoordModeOrigin);
}



static MapPoints *
ov_LoadMap (name)
char *name;
/*
 * Load the map by this name.
 */
{
	char fname[200], line[200], fill[4];
	int c, pkey = prj_ProjKey ();
	float lat, lon, junk;
	int type, npt, pt;
	SValue v;
	FILE *mapfp;
	MapPoints *mp, *list;
/*
 * Make sure the symbol table exists.  If so, see if we've already done
 * this one.  If we have, make sure the projection matches what we are
 * doing now.
 *
 * NOTE that this code catches a change in projection, but not a change
 * 	in origin.  We need to fix that.
 */
	if (! MapTable)
		MapTable = usy_c_stbl ("MapTable");
	else if (usy_g_symbol (MapTable, name, &type, &v))
	{
		mp = (MapPoints *) v.us_v_ptr;
		if (mp->mp_pkey == pkey)
			return (mp);
		ov_ZapMap (mp);
		usy_z_symbol (MapTable, name);
	}
/*
 * Nope.  Try to open up a file.
 */
	strcpy (line, name);
	strcat (line, ".map");
	if (! FindFile (line, MapPath, fname) ||
	    ((mapfp = fopen (fname, "r")) == NULL))
	{
		msg_ELog (EF_PROBLEM, "Unable to open map %s", name);
		return (NULL);
	}
/*
 * Plow through the points and convert them to XY space.
 */
 	list = 0;
	while (fgets (line, 200, mapfp))
	{
	/*
	 * Figure out how many points we have.  Ignore the four floats that
	 * give us the bounding box, but try for the optional "FILL" string.
	 */
		strncpy (fill, "    ", 4);
	 	sscanf (line, "%d %f %f %f %f %4s", &npt, &junk, &junk, &junk,
			&junk, fill);
	/*
	 * Do landmarks separately.
	 */
		if (npt == 1)
		{
			/* ov_DrawLandmark (line, disp, d, gcontext); */
			msg_ELog (EF_INFO, "Landmark, ignored");
			continue;
		}
	/*
	 * Get the memory to hold this set of points, and add it to
	 * the list.
	 */
		npt /= 2;
	 	mp = ALLOC (MapPoints);
		mp->mp_x = (float *) malloc (npt*sizeof (float));
		mp->mp_y = (float *) malloc (npt*sizeof (float));
		mp->mp_npt = npt;
		mp->mp_fill = ! strncmp (fill, "FILL", 4);
		mp->mp_pkey = pkey;
		mp->mp_next = list;
		list = mp;
	/*
	 * Pass through each point.
	 */
	 	for (pt = 0; pt < npt; pt++)
		{
		/*
		 * Read the stuff and convert it to our space.
		 */
		 	fscanf (mapfp, "%f %f", &lat, &lon);
			prj_Project (lat, lon, mp->mp_x + pt, mp->mp_y + pt);
		}
	/*
	 * Eat a newline if need be.
	 */
		while ((c = fgetc (mapfp)) != '\n' && c != EOF)
			;
	}
/*
 * Save this set of points, and return it to the caller.
 */
	v.us_v_ptr = (char *) list;
	usy_s_symbol (MapTable, name, SYMT_POINTER, &v);
	fclose (mapfp);
	return (list);
}




static void
ov_ZapMap (mp)
MapPoints *mp;
/*
 * Clean out these structures and start over.
 */
{
	while (mp)
	{
		MapPoints *zap = mp;
		mp = zap->mp_next;
		free (zap->mp_x);
		free (zap->mp_y);
		free (zap);
	}
}

			





/*
 * Feature definition stuff.
 */
void
ov_Feature (cmds)
struct ui_command *cmds;
/*
 * Interactively define a feature.
 */
{
	struct FList *fl;
	union usy_value v;
/*
 * Create our table if necessary.
 */
	if (! Ftable)
		Ftable = usy_c_stbl ("Features");
/*
 * Allocate a new feature list.
 */
	fl = (struct FList *) malloc (sizeof (struct FList));
	strcpy (fl->fl_name, UPTR (*cmds));
	fl->fl_features = fl->fl_tail = 0;
/*
 * Now parse the internals.
 */
	ui_subcommand ("in-feature", "Feature>", ov_InFeature, (int) fl);
/*
 * Define this feature.
 */
	v.us_v_ptr = (char *) fl;
	usy_s_symbol (Ftable, fl->fl_name, SYMT_POINTER, &v);
}






static int
ov_InFeature (fl, cmds)
struct FList *fl;
struct ui_command *cmds;
/*
 * Deal with the internals of a feature definition.
 */
{
	Feature *fp = (Feature *) ALLOC (Feature);
/*
 * See what we're dealing with.
 */
	switch (UKEY (*cmds))
	{
	/*
	 * Text strings.
	 */
	   case GPC_TEXT:
	   	fp->f_type = FText;
		strcpy (fp->f_string, UPTR (cmds[3]));
		if (cmds[4].uc_ctype != UTT_END)
			fp->f_size = UFLOAT (cmds[4]);
		else
			fp->f_size = DEFAULT_TEXT_SIZE;
	   	break;

	/*
	 * Circles.
	 */
	   case GPC_CIRCLE:
	   	fp->f_type = FCircle;
		fp->f_radius = UFLOAT (cmds[3]);
		break;

	/*
	 * Time markers.
	 */
	   case GPC_AT:
	   	fp->f_type = FMarker;
		fp->f_when = UDATE (cmds[1]);
		break;
	/*
	 * All done.
	 */
	   case GPC_ENDFEATURE:
	   	free (fp);
	   	return (FALSE);
	}
/*
 * Fill in the common stuff.
 */
	if (fp->f_type != FMarker)
	{
	   	fp->f_lat = UFLOAT (cmds[1]);
		fp->f_lon = UFLOAT (cmds[2]);
	}
	fp->f_next = 0;
/*
 * Add this one to the list.
 */
	if (! fl->fl_features)
		fl->fl_features = fl->fl_tail = fp;
	else
	{
		fl->fl_tail->f_next = fp;
		fl->fl_tail = fp;
	}
	return (TRUE);
}






/*ARGSUSED*/
static void
ov_Boundary (comp, update)
char *comp;
int update;
/*
 * Draw a boundary overlay.
 *
 * TODO:
 *	overlay widget entry
 *	annotation
 *
 * Modified 6/24/92 to read the "penup-point" attribute of a DataChunk
 * and use this to separate boundary data into multiple polylines.
 */
{
	char platform[PlatformListLen];
	char label[20];
	char *pnames[MaxPlatforms];
	char iconname[40], *lat, *lon;
	PlatformId pid;
	int lwidth, npt, nplats, i;
	int pt, total_pts;
	int showicon, adjust;
	bool closed;
	short first_valid;
	ZebTime t, target;
	DataChunk *dc;
	XPoint *xpts;
	XColor xc;
	LabelOpt opt;
	float x, y, x0, y0;
	float asize;
	int ix = 0, iy = 0;
	char *penup;
	float penup_pt = 0; /* The x OR y lat-lon value indicating a break */
	Location *lp, c;
/*
 * Get the various parameters that control boundary drawing.
 */
	if (! ov_GetBndParams (comp, platform, &xc, &lwidth, &closed, &opt, 
		label, &asize, &showicon, iconname, &adjust))
		return;
/*
 * Adjust the time we look for data at.
 */
	target = PlotTime;
	msg_ELog (EF_DEBUG, "target %d", target.zt_Sec);
	msg_ELog (EF_DEBUG, "adjust %d", adjust);
	target.zt_Sec += adjust * 60;
	msg_ELog (EF_DEBUG, "target %d", target.zt_Sec);
/*
 * Loop through the platforms.
 */
	nplats = CommaParse (platform, pnames);
	for (i = 0; i < nplats; i++)
	{
		if ((pid = ds_LookupPlatform (pnames[i])) == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Unknown boundary platform %s",
				pnames[i]);
			continue;
		}
	/*
	 * Find out when there is something available.
	 */
		if (! ds_DataTimes (pid, &target, 1, DsBefore, &t))
		{
			msg_ELog (EF_DEBUG, "No %s boundary available", 
				pnames[i]);
			continue;
		}
	/*
	 * Check into age limits.
	 */
 		if (! AgeCheck (comp, pnames[i], &t))
		{
			msg_ELog (EF_DEBUG, "Boundary %s too old", pnames[i]);
			continue;
		}
	/*
	 * Snarf it.
	 */
		if (! (dc = ds_Fetch (pid, DCC_Boundary, &t, &t, NULL, 0,
			NULL, 0)))
		{
			msg_ELog (EF_PROBLEM, "Get failed on %s boundary", 
				pnames[i]);
			continue;
		}
		lp = dc_BndGet (dc, 0, &npt);
	/*
	 * Get set up to draw the thing.
	 */
		xpts = (XPoint *) malloc ((closed ? npt + 1 : npt) * 
			sizeof (XPoint));
		XSetForeground (Disp, Gcontext, xc.pixel);
		XSetLineAttributes (Disp, Gcontext, lwidth, LineSolid, CapButt,
				JoinMiter);
		if ((penup = dc_GetSampleAttr(dc, 0, "penup-point")) != NULL)
		{
			penup_pt = (float)atof(penup);
			msg_ELog (EF_DEBUG, 
				"Found penup-point value %f for platform %s",
				penup_pt, pnames[i]);
		}
	/*
	 * Convert all points into window system space.
	 * When a "penup" point is found, draw the line and start over
	 * with a new one.  Each successive line is stored starting at
	 * xpts[0].
	 */
		pt = 0;
		first_valid = 1;
		msg_ELog (EF_DEBUG, "Boundary %s: datachunk has %i points",
			  pnames[i], npt);
		for (total_pts = 0; total_pts < npt; total_pts++)
		{
			int skip = 0;
		/*
		 * Convert lat/lon to x,y and hold it in xpts[]
		 */
			if (penup /* The penup-point attr existed */ &&
			    ((lp->l_lat == penup_pt) ||
			     (lp->l_lon == penup_pt)))
				skip = 1;
			else
				prj_Project (lp->l_lat, lp->l_lon, &x, &y);
		/*
		 * Break the line if penup point detected or the next segment
		 * does not intersect the plot region
		 */
			if (skip || (pt > 1 && !Intersects (x0, y0, x, y)))
			{
				/* Draw all points from 0 to pt-1
				 */
				if (pt > 1)  /* Need at least two points */
				{
				    if (closed)
					xpts[pt++] = xpts[0];
				    XDrawLines (Disp, GWFrame (Graphics), 
					    	Gcontext, xpts, pt,
					    	CoordModeOrigin);
				}
				pt = 0;
				lp++;
				continue;
			}
		/*
		 * Otherwise we convert lat/lon to x,y and hold it in xpts[]
		 */
			prj_Project (lp->l_lat, lp->l_lon, &x, &y);
			xpts[pt].x = XPIX (x);
			xpts[pt].y = YPIX (y);
		/*
		 * Annotate beneath the icon if called for.
		 */
 			if (first_valid)
			{
				first_valid = 0;
				ix = xpts[pt].x;
				iy = xpts[pt].y;
				if (opt != NoLabel)
				{
				   XSetFillStyle (Disp, Gcontext, FillSolid);
				   DrawText (Graphics, GWFrame (Graphics), 
					Gcontext, ix + 5, iy - 5, 
					(opt == LabelString) ? label : 
					pnames[i], 0.0, asize, JustifyCenter, 
					JustifyBottom);
				}
			}
		/*
		 * Increment the location pointer and xpts[] index
		 */
			lp++;
			pt++;
			x0 = x;
			y0 = y;
		}
	/* 
	 * Draw whatever is left in xpts[]:
	 */
		if (pt > 1)  /* Make sure more than 1 point left */
		{
		/*
		 * Wrap back to the beginning if this is a closed boundary.
		 */
			if (closed)
				xpts[pt++] = xpts[0];
		/*
		 * Draw them, clean up.
		 */
			XDrawLines (Disp, GWFrame (Graphics), Gcontext, xpts, 
				    pt, CoordModeOrigin);
		}
		ot_AddStatusLine (comp, pnames[i], " ", &t);
	/*
	 * Add an icon if necessary.
	 */
		if (showicon)
		{
		/*
		 * Where to put the icon? 
		 * The default, (ix,iy), is the first valid point found above
		 */
		    if (((lat = dc_GetSampleAttr(dc, 0, "center_lat")) != NULL)
			&& ((lon = dc_GetSampleAttr(dc, 0, "center_lon"))
			    != NULL))
			{
				msg_ELog(EF_DEBUG,
			   "Using DC atts center_lat/lon %s,%s for %s icon",
				   lat, lon, pnames[i]);
				c.l_lat = (float) atof (lat);
				c.l_lon = (float) atof (lon);
				prj_Project (c.l_lat, c.l_lon, &x, &y);
				ix = XPIX (x);
				iy = YPIX (y);
			}
		/*
		 * Put it there.
		 */
			I_PositionIcon (comp, pnames[i], &t, iconname, ix, iy, 
					xc.pixel); 
		}
	/*
	 * Free the data chunk and memory.
	 */
		if (dc)
			dc_DestroyDC (dc);
		if (xpts)
			free (xpts);
	}
	XSetLineAttributes (Disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
}




static bool
ov_GetBndParams (comp, platform, xc, lwidth, closed, opt, label, asize,
	showicon, iconname, adjust)
char	*comp, *platform, *label, *iconname;
XColor	*xc;
int	*lwidth, *showicon;
bool	*closed;
LabelOpt *opt;
float	*asize;
int	*adjust;
/*
 * Get all of the parameters which control boundary drawing.
 */
{
	char color[40];
/*
 * Look up our platform.
 */
	if (! pd_Retrieve (Pd, comp, "platform", platform, SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "Missing platform in boundary overlay");
		return (FALSE);
	}
/*
 * Get the color that we will use to draw this boundary.
 */
	if (! pda_Search (Pd, comp, "color", "boundary", color, SYMT_STRING))
		strcpy (color, "white");
	if (! ct_GetColorByName (color, xc))
	{
		msg_ELog (EF_PROBLEM, "Unknown color: %s", color);
		ct_GetColorByName ("white", xc);
	}
/*
 * Figure a line width too.
 */
	if (! pda_Search (Pd, comp, "line-width", "boundary", (char *) lwidth,
				SYMT_INT))
		*lwidth = 0;
/*
 * See if they want the boundary drawn open or closed.
 */
	if (! pda_Search (Pd, comp, "closed-boundary", "boundary",
			(char *) closed, SYMT_BOOL))
		*closed = TRUE;
/*
 * Labeling
 */
	if (! pda_Search (Pd, comp, "label", "boundary", label, SYMT_STRING) ||
			! strcmp (label, "platform"))
		*opt = LabelPlatform;
	else if (! strcmp (label, "none"))
		*opt = NoLabel;
	else
		*opt = LabelString;
	if (*opt != NoLabel && ! pda_Search(Pd, comp, "label-size","boundary", 
			(char *) asize, SYMT_FLOAT))
		*asize = 0.015;
/*
 * Get the icon information.
 */
	*showicon = TRUE;
	pda_Search (Pd, comp, "show-icon", "boundary", (char *) showicon,
		SYMT_INT);
	if (*showicon && (! pda_Search (Pd, comp, "position-icon", "boundary",
			iconname, SYMT_STRING)))
		*showicon = FALSE;
/*
 * Check for that kludgey adjust data time parameter (in minutes).
 */
	*adjust = 0;
	pda_Search (Pd, comp, "data-time-adjust", "boundary", (char *) adjust,
		SYMT_INT);

	return (TRUE);
}






/*ARGSUSED*/
static void
ov_RangeRings (comp, update)
char *comp;
int update;
/*
 * Draw range rings onto the display.
 */
{
	Location loc;
	bool dolabels, do_nm;
	float ringint, azint, rannot, aannot, maxrange, x, y, az, azoff;
	float radius, azrad, textrot, labelsize;
	int lastring, ring, lwidth, px, py, pradius, farx, fary;
	char platform[40], label[8];
	XColor xc;
/*
 * Get our information.
 */
	if (! ov_RRInfo (comp, platform, &loc, &ringint, &azint, &rannot,
			 &aannot, &lastring, &maxrange, &xc, &lwidth, &azoff,
			 &dolabels, &labelsize, &do_nm))
		return;
/*
 * Convert the ring interval to km if necessary
 */
	if (do_nm)
	{
		ringint *= NM_TO_KM;
		aannot *= NM_TO_KM;
	}
/*
 * Set up the graphics context.
 */
	XSetForeground (Disp, Gcontext, xc.pixel);
	XSetLineAttributes (Disp, Gcontext, lwidth, LineSolid, CapButt,
			JoinMiter);
	SetClip (FALSE);
/*
 * Draw the rings and range labels.
 */
	prj_Project (loc.l_lat, loc.l_lon, &x, &y);
	for (ring = 1; ring <= lastring; ring++)
	{
		int labelval;

		radius = ring * ringint;
		labelval = (do_nm ? (int)(radius * KM_TO_NM + 0.5) : 
			    (int)(radius + 0.5));
	/*
	 * Ring
	 */
		px = XPIX (x - radius);
		py = YPIX (y + radius);
		pradius = XPIX (x + radius) - XPIX (x);
		XDrawArc (Disp, GWFrame (Graphics), Gcontext, px, py, 
			  2 * pradius, 2 * pradius, 0, 360 * 64);
	/*
	 * Label
	 */
		if (! dolabels)
			continue;

		sprintf (label, "%d%s", labelval, (do_nm ? " NM" : ""));
		
		azrad = DEG_TO_RAD (90.0 - rannot);
		px = XPIX (x + radius * cos (azrad));
		py = YPIX (y + radius * sin (azrad));

		textrot = sin (azrad) >= 0.0 ? -rannot : 180.0 - rannot;
		DrawText (Graphics, GWFrame (Graphics), Gcontext, px, py, 
			  label, textrot, labelsize, JustifyCenter, 
			  JustifyCenter);
	}
/*
 * Draw the azimuth lines.
 */
	for (az = azoff ; az < 360 + azoff; az += azint)
	{
		azrad = DEG_TO_RAD (90.0 - az);
		px = XPIX (x + ringint * cos (azrad));
		py = YPIX (y + ringint * sin (azrad));
		farx = XPIX (x + lastring * ringint * cos (azrad));
		fary = YPIX (y + lastring * ringint * sin (azrad));
		XDrawLine (Disp, GWFrame (Graphics), Gcontext, px, py,
			farx, fary);
	/*
	 * Label
	 */
		if (! dolabels)
			continue;

		sprintf (label, "%d", (int)(az - azoff + 0.5));
		
		px = XPIX (x + aannot * cos (azrad));
		py = YPIX (y + aannot * sin (azrad));

		textrot = cos (azrad) >= 0.0 ? 90.0 - az : 270.0 - az;
		if (textrot == 0.0)
			textrot = 0.5; /* force stroke text */

		DrawText (Graphics, GWFrame (Graphics), Gcontext, px, py, 
			  label, textrot, labelsize, JustifyCenter, 
			  JustifyCenter);
	}
/*
 * Clean up.
 */
	ResetGC ();
}





static int
ov_RRInfo (comp, platform, loc, ringint, azint, rannot, aannot, lastring,
	   maxrange, xc, lwidth, azoff, dolabels, labelsize, do_nm)
char *comp, *platform;
Location *loc;
float *ringint, *maxrange, *azint, *rannot, *aannot, *azoff, *labelsize;
int *lastring, *lwidth;
XColor *xc;
bool *dolabels, *do_nm;
/*
 * Get all of the parameters which control the plotting of range rings.
 */
{
	char color[40];
/*
 * Get the platform, then turn that into a location.
 */
	if (! pda_ReqSearch (Pd, comp, "platform", "ring", platform,
			     SYMT_STRING))
		return (FALSE);

	if (! GetLocation (platform, &PlotTime, loc))
	{
		msg_ELog (EF_PROBLEM, "No location for %s", platform);
		return (FALSE);
	}
/*
 * Intervals.
 */
	*ringint = 20.0;
	pda_Search (Pd, comp, "ring-interval", platform, (char *) ringint,
		    SYMT_FLOAT);

	*azint = 30.0;
	pda_Search (Pd, comp, "azimuth-interval", platform, (char *) azint, 
		    SYMT_FLOAT);
/*
 * Kludge in the number of rings for now.
 */
	*lastring = 8;
/*
 * VOR/DME rings have azimuth offsets.
 */
	*azoff = 0.0;
	pda_Search (Pd, comp, "azimuth-offset", platform, (char *) azoff,
		    SYMT_FLOAT);
/*
 * Put on labels?
 */
	*dolabels = TRUE;
	pda_Search (Pd, comp, "do-labels", "range-ring", (char *) dolabels,
		    SYMT_BOOL);

	*labelsize = 0.020;
	pda_Search (Pd, comp, "label-size", "range-ring", (char *) labelsize,
		    SYMT_FLOAT);
/*
 * Nautical miles instead of km?
 */
	*do_nm = FALSE;
	pda_Search (Pd, comp, "do-nm", "range-ring", (char *) do_nm, 
		    SYMT_BOOL);
/*
 * Find out where to annotate.
 */
	*rannot = *azoff + *azint * 1.5;
	pda_Search (Pd, comp, "ring-annot-azimuth", platform, (char *) rannot,
		    SYMT_FLOAT);

	*aannot = *ringint * 1.5;
	pda_Search (Pd, comp, "azimuth-annot-range", platform, (char *) aannot,
		    SYMT_FLOAT);
/*
 * Color.
 */
	strcpy (color, "white");
	pda_Search (Pd, comp, "color", "range-ring", color, SYMT_STRING);

	if (! ct_GetColorByName (color, xc))
	{
		msg_ELog (EF_PROBLEM, "Bad range-ring color '%s'", color);
		ct_GetColorByName ("white", xc);
	}
/*
 * Line width
 */
	*lwidth = 0;
	pda_Search (Pd, comp, "line-width", "range-ring", (char *) lwidth,
		    SYMT_INT);

	return (TRUE);
}




/*ARGSUSED*/
static void
ov_Location (comp, update)
char *comp;
int update;
/*
 * Plot the location of a series of platforms.
 */
{
	char *plist[MaxPlatforms];
	char label[40], icon[40];
	int nplat, plat, fg, npid, pid, expid, nexcl;
	bool tlabel;
	Location loc;
	float asize;
	LabelOpt opt;
	ZebTime loctime;
	PlatformId *pids, *expids;
/*
 * Do our initialization.
 */
	if (! ov_LocSetup (comp, plist, &nplat, icon, &opt, label, &tlabel,
			 &asize, &fg, &expids, &nexcl))
		return;
	SetClip (FALSE);
/*
 * Go through and place each platform.
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * If we can find a location on this platform directly, we do so,
	 * plot it, and get on with our lives.
	 */
		if (FancyGetLocation (comp, plist[plat], &PlotTime,
					&loctime, &loc))
		{
			ov_LocPlot (comp, plist[plat], &loc, &loctime, icon,
					fg, opt, label, asize, tlabel);
			continue;
		}
	/*
	 * Otherwise let's see if we can regexp it.
	 */
		if (! (pids = ds_GatherPlatforms (plist[plat], &npid, FALSE,
				FALSE)))
		{
			msg_ELog (EF_PROBLEM, "Unknown platform: %s",
					plist[plat]);
			continue;
		}
	/*
	 * We got a list of platform id's; let's plot them, except for
	 * any excluded ones.
	 */
		for (pid = 0; pid < npid; pid++)
		{
			char *pname = ds_PlatformName (pids[pid]);
		/*
		 * Check for exclusion.
		 */
			for (expid = 0; expid < nexcl; expid++)
				if (pids[pid] == expids[expid])
					break;
			if (expid < nexcl)
				continue;
		/*
		 * OK, we're doing this one.
		 */
			if (! FancyGetLocation (comp, pname,  &PlotTime,
					&loctime, &loc))
				continue;
			ov_LocPlot (comp, pname, &loc, &loctime, icon, fg,
					opt, label, asize, tlabel);
		}
	}
/*
 * Clean up and we are done.
 */
	if (nexcl > 0)
		free (expids);
	ResetGC ();
}




static void
ov_LocPlot (comp, pname, loc, loctime, icon, fg, opt, label, asize, tlabel)
char *pname, *icon, *comp, *label;
Location *loc;
ZebTime *loctime;
int fg, tlabel;
LabelOpt opt;
double asize;
/*
 * Actually plot a location.
 */
{
	float x, y;
	int px, py, xh, yh, width, height;
/*
 * Convert to pixel space, then offset to put the hot spot of
 * the icon there.
 */
	prj_Project (loc->l_lat, loc->l_lon, &x, &y);
/*
 * Though we set the clip above, I_PositionIcon clears it.  So we
 * resort to some manual clipping.
 */
	if (x < Xlo || x > Xhi || y < Ylo || y > Yhi)
		return;
	px = XPIX (x);
	py = YPIX (y);
	I_PositionIcon (comp, pname, loctime, icon, px, py, fg);
	(void) I_GetPMap (icon, &xh, &yh, &width, &height);
/*
 * Annotate beneath the icon if called for.
 */
	if (opt != NoLabel)
	{
		char *plabel;
		if ((plabel = strrchr (pname, '/')))
			plabel++;
		else
			plabel = pname;
		XSetFillStyle (Disp, Gcontext, FillSolid);
		DrawText (Graphics, GWFrame (Graphics), Gcontext,
				px + width/2 - xh, py + height - yh, 
				(opt == LabelString) ? label : plabel,
				0.0, asize, JustifyCenter, JustifyTop);
		XSetFillStyle (Disp, Gcontext, FillStippled);
	}
/*
 * Also look into time labelling.
 */
	if (tlabel)
	{
		int m, d, h, min, ptm, ptd;
		bool tlocal = FALSE;
		int tzoffset = 0;
		char *tzstr;
		char lstr[40];
	/*
	 * See if this time should be displayed in the location's
	 * local time.
	 */
		(void) pda_Search (Pd, comp, "local-time", pname,
				&tlocal, SYMT_BOOL);
		tzstr = "z";
		if (tlocal)
		{
			if (! pda_Search (Pd, comp, "timezone-offset",
					pname, (char *)&tzoffset, SYMT_INT))
			{
				msg_ELog(EF_PROBLEM,
						"no 'timezone-offset' parameter, though 'local-time' true");
			}
			else
			{
			/* 
			 * Local time is GMT time plus the timezones
			 * offset in minutes from GMT (i.e., minutes
			 * east of Greenwich).  So eastern hemisphere
			 * has positive offsets.
			 */
				loctime->zt_Sec += tzoffset * 60;
				tzstr = "loc";
			}
		}
	/*
	 * Format up the date.  Only put in the month/day portion
	 * if it differs from the plot time.
	 */
		TC_ZtSplit (loctime, 0, &m, &d, &h, &min, 0, 0);
		TC_ZtSplit (&PlotTime, 0, &ptm, &ptd, 0, 0, 0, 0);
		if (m == ptm && d == ptd)
			sprintf (lstr, "%d:%02d", h, min);
		else
			sprintf (lstr, "%d/%d,%d:%02d", m, d, h, min);
		if (tlocal)
			sprintf (lstr+strlen(lstr)," %s",tzstr);
	/*
	 * Put it onto the screen.
	 */
		XSetFillStyle (Disp, Gcontext, FillSolid);
		DrawText (Graphics, GWFrame (Graphics), Gcontext,
				px + width, py, lstr,
				0.0, asize, JustifyLeft, JustifyTop);
		XSetFillStyle (Disp, Gcontext, FillStippled);
	}
}







static int
ov_LocSetup (comp, plist, nplat, icon, opt, label, tlabel, asize, fg, expids,
		nexcl)
char *comp, **plist, *label, *icon;
int *nplat;
bool *tlabel;
LabelOpt *opt;
float *asize;
int *fg, *nexcl;
PlatformId **expids;
/*
 * Do the setup required to plot locations.
 */
{
	static char platform[PlatformListLen];	/* XXX static */
	char explat[PlatformListLen], *exlist[MaxPlatforms];
	int i;
	char color[40];
	XColor xc;
	XGCValues vals;
/*
 * Deal with platforms.
 */
	if (! pda_ReqSearch (Pd, comp, "platform", NULL, platform,SYMT_STRING))
		return (FALSE);
	if ((*nplat = CommaParse (platform, plist)) <= 0)
	{
		msg_ELog (EF_PROBLEM, "No platforms for location plot");
		return (FALSE);
	}
/*
 * Check for an exclude list.
 */
	if (pda_Search (Pd, comp, "exclude", NULL, explat, SYMT_STRING))
	{
		*nexcl = CommaParse (explat, exlist);
		*expids = (PlatformId *) malloc (*nexcl * sizeof (PlatformId));
		for (i = 0; i < *nexcl; i++)
			(*expids)[i] = ds_LookupPlatform (exlist[i]);
	}
	else
	{
		*expids = 0;
		*nexcl = 0;
	}
/*
 * Color.
 */
	if (! pda_Search (Pd, comp, "color", "location", color, SYMT_STRING))
		strcpy (color, "white");
	if (! ct_GetColorByName (color, &xc))
	{
		msg_ELog (EF_PROBLEM, "Unknown color: %s", color);
		ct_GetColorByName ("white", &xc);
	}
	*fg = xc.pixel;
/*
 * Find our icon.
 */
	if (! pda_Search (Pd, comp, "location-icon", plist[0], icon, 
		SYMT_STRING) && 
	    ! pda_Search (Pd, comp, "icon", plist[0], icon, SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "No location icon for %s", plist[0]);
		return (FALSE);
	}
/*
 * Labeling
 */
	if (! pda_Search (Pd, comp, "label", "location", label, SYMT_STRING) ||
			! strcmp (label, "platform"))
		*opt = LabelPlatform;
	else if (! strcmp (label, "none"))
		*opt = NoLabel;
	else
		*opt = LabelString;
	if (*opt != NoLabel && ! pda_Search(Pd, comp, "label-size","location", 
			(char *) asize, SYMT_FLOAT))
		*asize = 0.015;
	*tlabel = FALSE;
	(void) pda_Search (Pd, comp, "time-label", platform, (char *) tlabel,
			SYMT_BOOL);
/*
 * Now that we seem to have everything, fix up the graphics context.
 */
	vals.foreground = xc.pixel;
	XChangeGC (Disp, Gcontext, GCForeground, &vals);
	return (TRUE);
}


# endif		/* C_CAP_OVERLAY */

/* //////////////////////////////////////////////////////////////////// */

/*
 * These routines are special, and should be here even without overlays.
 */

static OvIcon *
ov_GetIcon (name)
char *name;
/*
 * Get the icon by this name.
 */
{
	SValue v;
	int type;
	OvIcon *icon;
	char filename[80];
/*
 * Make sure our symbol table exists.
 */
	if (! OvIcons)
		OvIcons = usy_c_stbl ("LocationIcons");
/*
 * If this icon is already cached in the symbol table, just return it.
 */
	if (usy_g_symbol (OvIcons, name, &type, &v))
		return ((OvIcon *) v.us_v_ptr);
/*
 * Nope.  Time to get it from a file. 
 */
	icon = ALLOC (OvIcon);
	sprintf (filename, "%s/icons/%s", GetLibDir (), name);
	if (XReadBitmapFile (Disp, RootWindow (Disp, 0), filename, &icon->oi_w,
		&icon->oi_h, &icon->oi_pixmap, &icon->oi_xh, &icon->oi_yh)
		!= BitmapSuccess)
	{
		free (icon);
		return (NULL);
	}
/*
 * Cache this one, and we're done.
 */
	v.us_v_ptr = (char *) icon;
	usy_s_symbol (OvIcons, name, SYMT_POINTER, &v);
	return (icon);
}



int
ov_PositionIcon (name, x, y, fg)
char	*name;
int	x, y, fg;
{
	OvIcon	*icon;
	Display	*Disp = XtDisplay (Graphics);
	XGCValues vals;

	SetClip (TRUE);
	icon = ov_GetIcon (name);
	if (icon == NULL)
	{
		msg_ELog (EF_PROBLEM, "Can't get icon %s.", name);
		return (FALSE);
	}
	x -= icon->oi_xh;
	y -= icon->oi_yh;

	vals.foreground = fg;
	vals.fill_style = FillStippled;
	vals.stipple = icon->oi_pixmap;
	XChangeGC (Disp, Gcontext, GCForeground|GCFillStyle|GCStipple,
			&vals);
	XSetTSOrigin (Disp, Gcontext, x, y);
	XFillRectangle (Disp, GWFrame (Graphics), Gcontext, x, y,
			icon->oi_w, icon->oi_h);
	ResetGC ();
	return (TRUE);
}

