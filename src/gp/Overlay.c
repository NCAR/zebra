/*
 * Deal with static (or almost static) overlays.
 */
static char *rcsid = "$Id: Overlay.c,v 2.19 1992-12-18 10:05:19 granger Exp $";
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

/* 
 * Since the annotate widget uses ov_PositionIcon, and the annotate widget is
 * always compiled, the PositionIcon function will always be compiled...
 */
# if (1 || C_CAP_OVERLAY || C_CAP_VECTOR || C_CAP_LIGHTNING || C_CAP_TRACKS)
# define OV_POSITION_ICON
# else
# undef OV_POSITION_ICON
# endif

# ifdef OV_POSITION_ICON

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <math.h>
# include <string.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <dm.h>
# include <DataStore.h>
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"
# include "gp_cmds.h"


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

# endif /* ifdef OV_POSITION_ICON */

/*//////////////////////////////////////////////////////////////////////*/

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

typedef enum { FText, FCircle, FMarker } FeatureType;
typedef struct feature
{
	FeatureType	f_type;			/* Feature type		*/
	float		f_lat, f_lon;		/* Location		*/
	float		f_radius;		/* Circle radius	*/
	char		f_string[MAXFSTRING];	/* Text string		*/
	float		f_size;			/* Text size		*/
	time		f_when;			/* Time			*/
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
	float *mp_x, *mp_y;	/* The points themselves		*/
	struct _MapPoints *mp_next;	/* Next entry in chain		*/
} MapPoints;



/*
 * Our internal overlay drawing routines.
 */
static void 	ov_GridBBox FP ((char *, int));
static void 	ov_DrawFeature FP ((char *, int));
static void 	ov_Map FP ((char *, int));
static void 	ov_WBounds FP ((char *, int));
static void 	ov_RangeRings FP ((char *, int));
static void 	ov_Location FP ((char *, int));
static void 	ov_Grid FP ((char *, int));
static void 	ov_AzLimits FP ((char *, int));
static bool 	ov_GetWBounds FP ((char *, char *, float *, float *, float *,
			float *, float *));
static int 	ov_FindWBReply FP ((struct message *, struct dm_rp_wbounds *));
static void 	ov_Boundary FP ((char *, int));
static bool 	ov_GetBndParams FP ((char *, char *, XColor *, int *, bool *,
			LabelOpt *, char *, float *, int *, char *, int *));
static int 	ov_RRInfo FP ((char *, char *, Location *, float *, float *,
			float *, float *, int *, float *, XColor *, int *,
			float *));
static OvIcon 	*ov_GetIcon FP ((char *));
static int 	ov_LocSetup FP ((char *, char **, int *, OvIcon **, LabelOpt *,
			char *, bool *, float *));
static void	ov_SGSetup FP ((char *, float *, float *, float *, bool *,
			int *, bool *, float *, float *));
static void 	ov_SolidGrid FP ((int, int, int, int, double, double,
			double, int, double, double));
static void	ov_TicGrid FP ((int, int, int, int, double, double,
			double, int, int, double, double));
static void	ov_LLTicGrid FP ((int, int, int, int, double, double,
				double, int, int));
static MapPoints *ov_LoadMap FP ((char *));
static void	ov_DrawMap FP ((const MapPoints *));


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




static inline float
FloatTrunc (v, interval)
float v, interval;
{
	if (v < 0)
	{
		float ret = interval * (int) ((-v)/interval);
		return (-ret);
	}
	else
		return (interval * (int) (v/interval));
}



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
	time	t;
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
	cvt_ToXY (loc.l_lat, loc.l_lon, &ox, &oy);
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
 * Draw a box or a line based on the plot type
 */
	if (! strcmp (ptype, "CAP"))
		XDrawRectangle (disp, d, Gcontext, px0, py0, px1 - px0 + 1,
			py1 - py0 + 1);
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
	msg_send ("Displaymgr", MT_DISPLAYMGR, FALSE, &wb, sizeof (wb));
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
	time temptime;
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
	cvt_ToXY (fp->f_lat, fp->f_lon, &x, &y);
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
	}
}


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
	if (pda_Search (Pd, comp, "line-width", "map", (char *) &lwidth,
			SYMT_INT))
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
	ov_DrawMap (points);
	XSetLineAttributes (disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
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
	 * Pass through each point.
	 */
		xp = 0;
	 	for (pt = 0; pt < points->mp_npt; pt++)
		{
		/*
		 * Convert the point into pixel space.
		 */
		 	pts[xp].x = XPIX (points->mp_x[pt]);
			pts[xp].y = YPIX (points->mp_y[pt]);
		/*
		 * If we've filled our array, shove it out.
		 */
		 	if (++xp >= MAXPLSEG)
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





static MapPoints *
ov_LoadMap (name)
char *name;
/*
 * Load the map by this name.
 */
{
	char fname[200], line[200], c;
	float lat, lon;
	int type, npt, pt;
	SValue v;
	FILE *mapfp;
	MapPoints *mp, *list;
/*
 * Make sure the symbol table exists.  If so, see if we've already done
 * this one.
 */
	if (! MapTable)
		MapTable = usy_c_stbl ("MapTable");
	else if (usy_g_symbol (MapTable, name, &type, &v))
		return ((MapPoints *) v.us_v_ptr);
/*
 * Nope.  Try to open up a file.
 */
	fixdir_t ("GP_MAP_DIR", LIBDIR, name, fname, ".map");
	/* sprintf (fname, "../lib/%s.map", name); */
	if ((mapfp = fopen (fname, "r")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Unable to open map file %s", fname);
		return (NULL);
	}
/*
 * Plow through the points and convert them to XY space.
 */
 	list = 0;
	while (fgets (line, 200, mapfp))
	{
	/*
	 * Figure out how many points we have.  Then discard the rest of the
	 * line, which seems to be a sort of bounding box.
	 */
	 	sscanf (line, "%4d", &npt);
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
			cvt_ToXY (lat, lon, mp->mp_x + pt, mp->mp_y + pt);
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









/*
 * Feature definition stuff.
 */

ov_Feature (cmds)
struct ui_command *cmds;
/*
 * Interactively define a feature.
 */
{
	struct FList *fl;
	static int ov_InFeature ();
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
	char platform[500], label[20], *pnames[40];
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
	float x, y, asize;
	int ix, iy;
	char *penup;
	float penup_pt;	/* The x OR y value in lat-lon indicating a break */
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
		msg_ELog(EF_DEBUG,"Boundary platform %s: datachunk has %i points",
			 pnames[i],npt);
		for (total_pts = 0; total_pts < npt; total_pts++)
		{
			if (penup /* The penup-point attr existed */ &&
			   ((lp->l_lat == penup_pt) || (lp->l_lon == penup_pt)))
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
				lp ++;
				continue;
			}
		/*
		 * Otherwise we convert lat/lon to x,y and hold it in xpts[]
		 */
			cvt_ToXY (lp->l_lat, lp->l_lon, &x, &y);
			xpts[pt].x = XPIX (x);
			xpts[pt].y = YPIX (y);
		/*
		 * Annotate beneath the icon if called for.
		 */
 			if ((first_valid) && (opt != NoLabel))
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
			lp ++;
			pt++;
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
			XDrawLines (Disp, GWFrame (Graphics), Gcontext, xpts, pt,
					CoordModeOrigin);
		}
		lw_TimeStatus (comp, pnames[i], &t);
	/*
	 * Add an icon if necessary.
	 */
		if (showicon)
		{
		/*
		 * Where to put the icon? 
		 * The default, (ix,iy), is the first valid point found above
		 */
		    if (((lat = dc_GetSampleAttr (dc, 0, "center_lat")) != NULL)
			 && ((lon = dc_GetSampleAttr (dc, 0, "center_lon")) 
				!= NULL))
			{
				msg_ELog(EF_DEBUG,
			   "Using DC atts center_lat/lon %s,%s for %s icon",
				   lat, lon, pnames[i]);
				c.l_lat = (float) atof (lat);
				c.l_lon = (float) atof (lon);
				cvt_ToXY (c.l_lat, c.l_lon, &x, &y);
				ix = XPIX (x);
				iy = YPIX (y);
			}
		/*
		 * Put it there.
		 */
			I_PositionIcon (comp, pnames[i], t, iconname, ix, iy, 
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
	char color[40], eplat[500];
	char *ptr;
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







static void
ov_RangeRings (comp, update)
char *comp;
int update;
/*
 * Draw range rings onto the display.
 */
{
	Location loc;
	float ringint, azint, rannot, aannot, maxrange, x, y, az, azoff;
	int lastring, ring, lwidth, px, py, radius, farx, fary;
	char platform[40];
	XColor xc;
/*
 * Get our information.
 */
	if (! ov_RRInfo (comp, platform, &loc, &ringint, &azint, &rannot,
			&aannot, &lastring, &maxrange, &xc, &lwidth, &azoff))
		return;
/*
 * Set up the graphics context.
 */
	XSetForeground (Disp, Gcontext, xc.pixel);
	XSetLineAttributes (Disp, Gcontext, lwidth, LineSolid, CapButt,
			JoinMiter);
	SetClip (FALSE);
/*
 * Draw the rings.
 */
	cvt_ToXY (loc.l_lat, loc.l_lon, &x, &y);
	for (ring = 1; ring <= lastring; ring++)
	{
		px = XPIX (x - ring*ringint); py = YPIX (y + ring*ringint);
		radius = XPIX (x + ring*ringint) - XPIX (x);
		XDrawArc (Disp, GWFrame (Graphics), Gcontext, px, py, 
			2*radius, 2*radius, 0, 360*64);
	}
/*
 * Draw the azimuth lines.
 */
# ifdef ERNEST
if (strcmp(platform,"mlb") == 0 || strcmp(platform,"orl") == 0 ||
    strcmp(platform,"omn") == 0)
az = 3;
else
az = 0;
# endif
	for (az = azoff ; az < 360 + azoff; az += azint)
	{
		float azrad = az*M_PI/180.0;
		px = XPIX (x + ringint*cos (azrad));
		py = YPIX (y + ringint*sin (azrad));
		farx = XPIX (x + lastring*ringint * cos (azrad));
		fary = YPIX (y + lastring*ringint * sin (azrad));
		XDrawLine (Disp, GWFrame (Graphics), Gcontext, px, py,
			farx, fary);
	}
/*
 * Clean up.
 */
	ResetGC ();
	SetClip (TRUE);
}





static int
ov_RRInfo (comp, platform, loc, ringint, azint, rannot, aannot, lastring,
		maxrange, xc, lwidth, azoff)
char *comp, *platform;
Location *loc;
float *ringint, *maxrange, *azint, *rannot, *aannot, *azoff;
int *lastring, *lwidth;
XColor *xc;
/*
 * Get all of the parameters which control the plotting of range rings.
 */
{
	char color[40];
/*
 * Get the platform, then turn that into a location.
 */
	if (! pda_ReqSearch (Pd, comp, "platform","ring",platform,SYMT_STRING))
		return (FALSE);
	if (! GetLocation (platform, &PlotTime, loc))
	{
		msg_ELog (EF_PROBLEM, "No location for %s", platform);
		return (FALSE);
	}
/*
 * Intervals.
 */
	if (! pda_Search (Pd, comp, "ring-interval", platform, (char *)ringint,
			SYMT_FLOAT))
		*ringint = 20.0;
	if (! pda_Search (Pd, comp, "azimuth-interval", platform,
			(char *) azint, SYMT_FLOAT))
		*azint = 30.0;
/*
 * Find out where to annotate.
 */
	if (! pda_Search (Pd, comp, "ring-annot-azimuth", platform,
			(char *) rannot, SYMT_FLOAT))
		*rannot = 45;
	if (! pda_Search (Pd, comp, "azimuth-annot-range", platform,
			(char *) aannot, SYMT_FLOAT))
		*aannot = 30;
/*
 * Kludge in the number of rings for now.
 */
	*lastring = 8;
/*
 * VOR/DME rings have azimuth offsets.
 */
	if (! pda_Search (Pd, comp, "azimuth-offset", platform, (char *) azoff,
			SYMT_FLOAT))
		*azoff = 0.0;
/*
 * Color.
 */
	if (! pda_Search (Pd, comp, "color", "range-ring", color, SYMT_STRING))
		strcpy (color, "white");
	if (! ct_GetColorByName (color, xc))
	{
		msg_ELog (EF_PROBLEM, "Unknown color: %s", color);
		ct_GetColorByName ("white", xc);
	}
	if (! pda_Search(Pd, comp, "line-width", "range-ring", (char *) lwidth,
			SYMT_INT))
		*lwidth = 0;
	return (TRUE);
}




static void
ov_Location (comp, update)
char *comp;
int update;
/*
 * Plot the location of a series of platforms.
 */
{
	char *plist[100], label[40];
	int nplat, plat, px, py;
	bool tlabel;
	OvIcon *icon;
	Location loc;
	float x, y, asize;
	LabelOpt opt;
	ZebTime loctime;
/*
 * Do our initialization.
 */
	if (! ov_LocSetup (comp, plist, &nplat, &icon, &opt, label, &tlabel,
			 &asize))
		return;
	SetClip (FALSE);
/*
 * Go through and place each platform.
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Find this platform.
	 */
		if (! FancyGetLocation (comp, plist[plat], &PlotTime,
				&loctime, &loc))
			continue;
	/*
	 * Convert to pixel space, then offset to put the hot spot of
	 * the icon there.
	 */
		cvt_ToXY (loc.l_lat, loc.l_lon, &x, &y);
		px = XPIX (x) - icon->oi_xh;
		py = YPIX (y) - icon->oi_yh; 	
		XSetTSOrigin (Disp, Gcontext, px, py);
		XFillRectangle (Disp, GWFrame (Graphics), Gcontext, px, py,
			icon->oi_w, icon->oi_h);
	/*
	 * Annotate beneath the icon if called for.
	 */
	 	if (opt != NoLabel)
		{
			XSetFillStyle (Disp, Gcontext, FillSolid);
			DrawText (Graphics, GWFrame (Graphics), Gcontext,
				px + icon->oi_w/2,
				YPIX (y) + icon->oi_h - icon->oi_yh, 
				(opt == LabelString) ? label : plist[plat],
				0.0, asize, JustifyCenter, JustifyTop);
			XSetFillStyle (Disp, Gcontext, FillStippled);
		}
	/*
	 * Also look into time labelling.
	 */
		if (tlabel)
		{
			int m, d, h, min, ptm, ptd;
			char lstr[40];
		/*
		 * Format up the date.  Only put in the month/day portion
		 * if it differs from the plot time.
		 */
			TC_ZtSplit (&loctime, 0, &m, &d, &h, &min, 0, 0);
			TC_ZtSplit (&PlotTime, 0, &ptm, &ptd, 0, 0, 0, 0);
			if (m == ptm && d == ptd)
				sprintf (lstr, "%d:%02d", h, min);
			else
				sprintf (lstr, "%d/%d,%d:%02d", m, d, h, min);
		/*
		 * Put it onto the screen.
		 */
			XSetFillStyle (Disp, Gcontext, FillSolid);
			DrawText (Graphics, GWFrame (Graphics), Gcontext,
				px + icon->oi_w, py, lstr,
				0.0, asize, JustifyLeft, JustifyTop);
			XSetFillStyle (Disp, Gcontext, FillStippled);
		}
	}
/*
 * Do side annotation.
 */

/*
 * Clean up and we are done.
 */
	ResetGC ();
	SetClip (TRUE);
}






static int
ov_LocSetup (comp, plist, nplat, icon, opt, label, tlabel, asize)
char *comp, **plist, *label;
int *nplat;
bool *tlabel;
OvIcon **icon;
LabelOpt *opt;
float *asize;
/*
 * Do the setup required to plot locations.
 */
{
	static char platform[1000];	/* XXX */
	char iconname[40], color[40];
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
 * Color.
 */
	if (! pda_Search (Pd, comp, "color", "location", color, SYMT_STRING))
		strcpy (color, "white");
	if (! ct_GetColorByName (color, &xc))
	{
		msg_ELog (EF_PROBLEM, "Unknown color: %s", color);
		ct_GetColorByName ("white", &xc);
	}
/*
 * Find our icon.
 */
	if (! pda_Search (Pd, comp, "location-icon", plist[0], iconname, 
		SYMT_STRING) && 
	    ! pda_Search (Pd, comp, "icon", plist[0], iconname, SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "No location icon for %s", plist[0]);
		return (FALSE);
	}
	if (! (*icon = ov_GetIcon (iconname)))
	{
		msg_ELog (EF_PROBLEM, "Can't find icon '%s'", iconname);
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
	vals.fill_style = FillStippled;
	vals.stipple = (*icon)->oi_pixmap;
	XChangeGC (Disp, Gcontext, GCForeground|GCFillStyle|GCStipple,
			&vals);
	return (TRUE);
}








static void
ov_Grid (comp, update)
char *comp;
int update;
/*
 * Draw a solid grid overlay.
 */
{
	float xs, ys, theight, xoff, yoff;
	int top, bottom, left, right, aint, tic;
	bool solid, ll;
/*
 * Dig out our info.
 */
	ov_SGSetup (comp, &xs, &ys, &theight, &solid, &tic, &ll, &xoff, &yoff);
	bottom = YPIX (Yhi) - 10;
	top = YPIX (Ylo) - 12;
	left = XPIX (Xlo);
	right = XPIX (Xhi);
/*
 * Figure out how often to annotate lines along the bottom.
 */
	if ((aint = 70/(XPIX (xs) - XPIX (0))) <= 0)
		aint = 1;
/*
 * Draw the grid.
 */
	if (solid)
		ov_SolidGrid (left, right, top, bottom, xs, ys, theight, aint,
				xoff, yoff);
	else
	{
		if (ll)
			ov_LLTicGrid (left, right, top, bottom, xs, ys,
				theight, aint, tic);
		else
			ov_TicGrid (left, right, top, bottom, xs, ys, theight,
				aint, tic, xoff, yoff);
	}
}





static void
ov_TicGrid (left, right, top, bottom, xs, ys, theight, aint, ticwidth, 
		xoff, yoff)
int left, right, top, bottom, aint, ticwidth;
float xs, ys, theight, xoff, yoff;
/*
 * Draw a tic-style cartesian grid.
 */
{
	float xpos, ypos;
	int xp, yp, nx = 0, ny = 0;
	char label[30];
	Drawable frame = GWFrame (Graphics);
/*
 * Pass along the rows.
 */
 	for (xpos = FloatTrunc (Xlo + xoff, xs); xpos <= Xhi+xoff; xpos += xs)
	{
		if ((xp = XPIX (xpos - xoff)) < left)
			continue;
	/*
	 * And down the columns.
	 */
	 	for (ypos = FloatTrunc (Ylo + yoff, ys); ypos <= Yhi + yoff;
				ypos += ys)
		{
			if ((yp = YPIX (ypos - yoff)) > top)
				continue;
		/*
		 * Draw the tic marks.
		 */
			XDrawLine (Disp, frame, Gcontext, xp - ticwidth, yp,
					xp + ticwidth, yp);
			XDrawLine (Disp, frame, Gcontext, xp, yp - ticwidth,
					xp, yp + ticwidth);
		/*
		 * Annotate if this is the first time through.
		 */
			if (nx == 0 && (ny++ % aint) == 0)
			{
				sprintf (label, "%.1f", ypos);
				DrawText (Graphics, frame, Gcontext, left - 1,
					yp, label, 0.0, theight, JustifyRight,
					JustifyCenter);
			}
		}
	/*
	 * Bottom annotation.
	 */
		if ((nx++ % aint) == 0)
		{
			sprintf (label, "%.1f", xpos);
			DrawText (Graphics, frame, Gcontext, xp, top + 1,
				label, 0.0, theight,JustifyCenter, JustifyTop);
		}
	}
}




static void
ov_CGFixLL (blat, blon, xs, ys)
float *blat, *blon, xs, ys;
/*
 * Fix these values up to nice increments.
 */
{
	int iblat, iblon, ixs, iys;
/*
 * Put everything into easy integer increments.
 */
	cvt_ToLatLon (Xlo, Ylo, blat, blon);
	iblat = (int) (*blat * 3600.0 + 0.5);
	iblon = (int) ((-*blon) * 3600 + 0.5);
	ixs = (int) (xs * 60.0 + 0.5);
	iys = (int) (ys * 60.0 + 0.5);
/*
 * Now truncate things accordingly.
 */
	if (iblat < 0)
		iblat += (-iblat) % iys;
	else
	{
		iblat -= iblat % iys;
		iblat += iys;
	}
	iblon -= iblon % ixs;
	/* iblon += ixs; */
# ifdef notdef
	*blat = ((float) iblat + 0.5)/3600.0;
	*blon = -((float) iblon + 0.5)/3600.0;
# endif
	*blat = ((float) iblat)/3600.0;
	*blon = -((float) iblon)/3600.0;
}




static void
ov_LLTicGrid (left, right, top, bottom, xs, ys, theight, aint, ticwidth)
int left, right, top, bottom, aint, ticwidth;
float xs, ys, theight;
/*
 * Draw a tic-style lat/lon cartesian grid.
 */
{
	float xpos, ypos, blat, blon, maxlat, maxlon;
	int xp, yp, nx = 0, ny = 0;
	char label[30];
	Drawable frame = GWFrame (Graphics);
/*
 * Figure out where we are.
 */
	ov_CGFixLL (&blat, &blon, xs, ys);
	cvt_ToLatLon (Xhi, Yhi, &maxlat, &maxlon);
	xs /= 60.0;
	ys /= 60.0;
/*
 * Pass along the rows.
 */
	for (xpos = blon; xpos <= maxlon; xpos += xs)
	{
	/*
	 * And down the columns.
	 */
		for (ypos = blat; ypos <= maxlat; ypos += ys)
		{
			float xkm, ykm;
			cvt_ToXY (ypos, xpos, &xkm, &ykm);
			xp = XPIX (xkm);
			yp = YPIX (ykm);
		/*
		 * Draw the tic marks.
		 */
			XDrawLine (Disp, frame, Gcontext, xp - ticwidth, yp,
					xp + ticwidth, yp);
			XDrawLine (Disp, frame, Gcontext, xp, yp - ticwidth,
					xp, yp + ticwidth);
		/*
		 * Annotate if this is the first time through.
		 */
			if (nx == 0 && (ny++ % aint) == 0)
			{
				sprintf (label, "%d  ", (int) ypos);
				DrawText (Graphics, frame, Gcontext, left - 1,
					yp, label, 0.0, theight, JustifyRight, 
					JustifyBottom);
				sprintf (label, "%d' %d\"", (int) (ypos*60)%60,
					(int) (ypos*3600)%60);
				DrawText (Graphics, frame, Gcontext, left - 1,
					yp, label, 0.0, theight, JustifyRight,
					JustifyTop);
			}
		}
	/*
	 * Bottom annotation.
	 */
		if ((nx++ % aint) == 0)
		{
			sprintf (label, "%d", (int) xpos);
			DrawText (Graphics, frame, Gcontext, xp, top + 1,
				label, 0.0, theight,JustifyCenter, JustifyTop);
			sprintf (label, "%d' %d\"", (int)(-xpos*60)%60,
					(int) (-xpos*3600)%60);
			DrawText (Graphics, frame, Gcontext, xp, 
				top + theight * GWHeight(Graphics), label, 0.0,
				theight, JustifyCenter, JustifyTop);
		}
	}
}






static void
ov_SolidGrid (left, right, top, bottom, xs, ys, theight, aint, xoff, yoff)
int left, right, top, bottom, aint;
float xs, ys, theight, xoff, yoff;
/*
 * Draw a solid grid.
 */
{
	char label[30];
	float pos;
	int n;
	Drawable frame = GWFrame (Graphics);
/*
 * Draw the vertical lines.
 */
	n = 0;
	for (pos = FloatTrunc (Xlo + xoff, xs); pos <= Xhi + xoff; pos += xs)
	{
		if (XPIX (pos - xoff) < left)
			continue;
		XDrawLine (Disp, frame, Gcontext, XPIX (pos - xoff), top,
				XPIX (pos - xoff), bottom);
		if ((n++ % aint) == 0)
		{
			sprintf (label, "%.1f", pos);
			DrawText (Graphics, frame, Gcontext, XPIX (pos - xoff),
				top + 1, label, 0.0, theight,
				JustifyCenter, JustifyTop);
		}
	}
/*
 * And horizontal.
 */
	n = 0;
	for (pos = FloatTrunc (Ylo + yoff, ys); pos <= Yhi + yoff; pos += ys)
	{
		if (YPIX (pos - yoff) > top)
			continue;
		XDrawLine (Disp, frame, Gcontext, left, YPIX (pos - yoff),
				right, YPIX (pos - yoff));
		if ((n++ % aint) == 0)
		{
			sprintf (label, "%.1f", pos);
			DrawText (Graphics, frame, Gcontext, left - 1,
				YPIX (pos - yoff), label, 0.0, theight,
				JustifyRight, JustifyCenter);
		}
	}
}




static void
ov_SGSetup (comp, xs, ys, theight, solid, ticwidth, ll, xoff, yoff)
char *comp;
float *xs, *ys;
float *theight, *xoff, *yoff;
int *ticwidth;
bool *solid, *ll;
/*
 * Get everything set up to draw a grid.
 */
{
	int lwidth;
	Location loc;
	char origin[80];
/*
 * Find our spacings.
 */
	if (! pda_Search (Pd, comp, "x-spacing", "grid", (char *) xs,
			SYMT_FLOAT))
		*xs = 10.0;
	if (! pda_Search (Pd, comp, "y-spacing", "grid", (char *) ys,
			SYMT_FLOAT))
		*ys = 10.0;
/*
 * Go ahead and set the line drawing parameters.
 */
	SetColor (comp, "color", "grid", "gray60");
	if (! pda_Search (Pd, comp, "line-width", "grid", (char *) &lwidth,
			SYMT_INT))
		lwidth = 0;
	XSetLineAttributes (Disp, Gcontext, lwidth, LineSolid, CapButt,
			JoinMiter);
/*
 * Text height.
 */
	if (! pda_Search (Pd, comp, "annot-height", "grid", (char *) theight,
			SYMT_FLOAT))
		*theight = 0.02;
/*
 * Solidness.
 */
	if (! pda_Search(Pd, comp, "solid", "grid", (char *) solid, SYMT_BOOL))
		*solid = FALSE;
	if (! pda_Search (Pd, comp, "tic-width", "grid", (char *) ticwidth,
			SYMT_INT))
		*ticwidth = 8;
/*
 * Do we do this in lat/lon?
 */
	*ll = 0;
	if (! pda_Search (Pd, comp, "lat-lon", "grid", (char *) ll, SYMT_BOOL))
		*ll = FALSE;
/*
 * See if we should displace the origin.
 */
	*xoff = *yoff = 0.0;
	if (pda_Search (Pd, comp, "origin", "grid", origin, SYMT_STRING))
	{
		if (GetLocation (origin, &PlotTime, &loc))
		{
			cvt_ToXY (loc.l_lat, loc.l_lon, xoff, yoff);
			*xoff = - *xoff;
			*yoff = - *yoff;
		}
		else
			msg_ELog (EF_PROBLEM, "Bad origin '%s'", origin);
	}
}



# endif		/* C_CAP_OVERLAY */


/*
 * These routines are special, and should be here even without overlays.
 */
# ifdef OV_POSITION_ICON

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
	sprintf (filename, "%s/icons/%s", LIBDIR, name);
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

# endif
