/*
 * Deal with static (or almost static) overlays.
 */
static char *rcsid = "$Id: Overlay.c,v 1.13 1991-02-26 22:41:11 corbet Exp $";

# include <stdio.h>
# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "../include/dm.h"
# include "../include/DataStore.h"
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"
# include "gp_cmds.h"


/*
 * Our internal overlay drawing routines.
 */
# ifdef __STDC__
	static void ov_GridBBox (char *, int);
	static void ov_DrawFeature (char *, int);
	static void ov_Map (char *, int);
	static void ov_WBounds (char *, int);
	static bool ov_GetWBounds (char *, char *, float *, float *, float *,
			float *, float *);
	static int ov_FindWBReply (struct message *, struct dm_rp_wbounds *);
	static void ov_Boundary (char *, int);
	static bool ov_GetBndParams (char *, char *, XColor *, int *, int *);
# else
	static void ov_GridBBox ();
	static void ov_DrawFeature ();
	static void ov_Map ();
	static void ov_WBounds ();
	static bool ov_GetWBounds ();
	static int ov_FindWBReply ();
	static void ov_Boundary ();
	static bool ov_GetBndParams ();
# endif

# define BADVAL -9999.9

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
	{ 0, 0}
};



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
	GC gcontext;
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




bool
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
	int lwidth, type, i;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	Drawable d = GWFrame (Graphics);
	struct FList *fl;
	union usy_value v;
	Feature *last, *fp;
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
		last = 0;
		for (fp = fl->fl_features; fp; fp = fp->f_next)
			if (fp->f_type == FMarker &&
				fp->f_when.ds_yymmdd == PlotTime.ds_yymmdd &&
				fp->f_when.ds_hhmmss <= PlotTime.ds_hhmmss)
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
	char platform[40], color[40], fname[80];
	float x0, y0, x1, y1;
	int lwidth, px0, py0, px1, py1;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	Drawable d = GWFrame (Graphics);
	FILE *mapfp;
/*
 * Find our platform -- the map name.
 */
	if (! pda_ReqSearch (Pd,comp, "platform", NULL, platform, SYMT_STRING))
		return;
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
	if (pda_Search (Pd, comp, "line-width", "map", (char *) &lwidth,
			SYMT_INT))
		XSetLineAttributes (disp, Gcontext, lwidth, LineSolid,
			CapButt, JoinMiter);
/*
 * Open up the map file.
 */
	sprintf (fname, "../lib/%s.map", platform);
	if ((mapfp = fopen (fname, "r")) == NULL)
	{
		msg_ELog (EF_PROBLEM, "Unable to open map file %s", fname);
		return;
	}
/*
 * Draw it.
 */
	ov_DrawMap (mapfp);
/*
 * Clean up and go home.
 */
	fclose (mapfp);

	XSetLineAttributes (disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
}





ov_DrawMap (mapfp)
FILE *mapfp;
/*
 * Draw this map file onto the screen.
 */
{
	float lat, lon, x, y;
	int px, py, npt, pt, xp;
	XPoint pts[MAXPLSEG];
	char line[200], c;
/*
 * Just plow through the file.
 */
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
	 * Pass through each point.
	 */
		npt /= 2;
		xp = 0;
	 	for (pt = 0; pt < npt; pt++)
		{
		/*
		 * Read the stuff, and convert it to something useful, and 
		 * stash it into the XPoint structure.
		 */
		 	fscanf (mapfp, "%f %f", &lat, &lon);
			cvt_ToXY (lat, lon, &x, &y);
		 	pts[xp].x = XPIX (x);
			pts[xp].y = YPIX (y);
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
	/*
	 * Eat a newline if need be.
	 */
		while ((c = fgetc (mapfp)) != '\n' && c != EOF)
			;
	}
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
 *	name label?
 */
{
	char platform[40], *junk = "junk";
	PlatformId pid;
	int lwidth, pt, npt, closed;
	time t, target = PlotTime;
	DataObject *dobj;
	XPoint *xpts;
	XColor xc;
	float x, y;
/*
 * Get the various parameters that control boundary drawing.
 */
	if (!ov_GetBndParams (comp, platform, &xc, &lwidth, &closed))
		return;
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown boundary platform %s",platform);
		return;
	}
/*
 * Find out when there is something available.
 */
	if (! ds_DataTimes (pid, &target, 1, DsBefore, &t))
	{
		msg_ELog (EF_INFO, "No %s boundary available", platform);
		return;
	}
/*
 * Snarf it.
 */
	if ((dobj = ds_GetData (pid, &junk, 0, &t, &t, OrgOutline, 0, BADVAL))
				== 0)
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s boundary", platform);
		return;
	}
/*
 * Get set up to draw the thing.
 */
	npt = *dobj->do_desc.d_length;
	xpts = (XPoint *) malloc ((closed ? npt + 1 : npt) * sizeof (XPoint));
	XSetForeground (Disp, Gcontext, xc.pixel);
	XSetLineAttributes (Disp, Gcontext, lwidth, LineSolid, CapButt,
			JoinMiter);
/*
 * Convert all points into window system space.
 */
	for (pt = 0; pt < npt; pt++)
	{
		Location *lp = dobj->do_aloc + pt;
		cvt_ToXY (lp->l_lat, lp->l_lon, &x, &y);
		xpts[pt].x = XPIX (x);
		xpts[pt].y = YPIX (y);
	}
/*
 * Wrap back to the beginning if this is a closed boundary.
 */
	if (closed)
		xpts[npt++] = xpts[0];
/*
 * Draw them, clean up, and we're done.
 */
	XDrawLines (Disp, GWFrame (Graphics), Gcontext, xpts, npt,
			CoordModeOrigin);
	lw_TimeStatus (comp, &t);
	XSetLineAttributes (Disp, Gcontext, 0, LineSolid, CapButt, JoinMiter);
	free (xpts);
	ds_FreeDataObject (dobj);
}








static bool
ov_GetBndParams (comp, platform, xc, lwidth, closed)
char *comp, *platform;
XColor *xc;
int *lwidth, *closed;
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
	if (! pda_Search (Pd, comp, "color", platform, color, SYMT_STRING))
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
	if (! pda_Search (Pd, comp, "closed-boundary", platform,
			(char *) closed, SYMT_BOOL))
		*closed = TRUE;
	return (TRUE);
}
