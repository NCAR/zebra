/*
 * Deal with static (or almost static) overlays.
 */
static char *rcsid = "$Id: Overlay.c,v 1.3 1990-07-08 12:55:08 corbet Exp $";

# include <stdio.h>
# include <X11/Intrinsic.h>
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
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
# else
	static void ov_GridBBox ();
	static void ov_DrawFeature ();
	static void ov_Map ();
# endif


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
	{ "feature",	ov_DrawFeature	},
	{ "map",	ov_Map	},
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
	if (! px_GetParam (comp, "field", NULL, type, SYMT_STRING))
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
	GC gcontext;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	Drawable d = GWFrame (Graphics);
/*
 * Find our platform.
 */
	if (! px_GetParam (comp, "platform", NULL, platform, SYMT_STRING))
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
	gcontext = XCreateGC (disp, XtWindow (Graphics), 0, NULL);
	ct_GetColorByName (color, &xc);
	XSetForeground (disp, gcontext, xc.pixel);
	if (pda_Search (Pd, comp, "line-width", "overlay", (char *) &lwidth,
			SYMT_INT))
		XSetLineAttributes (disp, gcontext, lwidth, LineSolid,
			CapButt, JoinMiter);
/*
 * Just draw.
 */
	XDrawLine (disp, d, gcontext, px0, py0, px1, py0);
	XDrawLine (disp, d, gcontext, px1, py0, px1, py1);
	XDrawLine (disp, d, gcontext, px1, py1, px0, py1);
	XDrawLine (disp, d, gcontext, px0, py1, px0, py0);
/*
 * Clean up and go home.
 */
	XFreeGC (disp, gcontext);
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
	GC gcontext;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	Drawable d = GWFrame (Graphics);
	struct FList *fl;
	union usy_value v;
	Feature *last, *fp;
/*
 * Find our "platform".  (Really the name of the feature of interest).
 */
	if (! px_GetParam (comp, "platform", NULL, platform, SYMT_STRING))
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
	gcontext = XCreateGC (disp, XtWindow (Graphics), 0, NULL);
	ct_GetColorByName (color, &xc);
	XSetForeground (disp, gcontext, xc.pixel);
	if (pda_Search (Pd, comp, "line-width", "overlay", (char *) &lwidth,
			SYMT_INT))
		XSetLineAttributes (disp, gcontext, lwidth, LineSolid,
			CapButt, JoinMiter);
/*
 * Find the closest set of features.
 */
	if (fl->fl_features && fl->fl_features->f_type != FMarker)
	{
		fp = fl->fl_features;
		msg_ELog (EF_DEBUG, "Default feature fp = 0x%x", fp);
	}
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
		msg_ELog (EF_DEBUG, "Do ftr 0x%x", fp);
		ov_DoFeature (fp, disp, d, gcontext, xc.pixel);
	}
/*
 * Clean up and go home.
 */
	XFreeGC (disp, gcontext);
}





ov_DoFeature (fp, disp, d, gc, color)
Feature *fp;
Display *disp;
Drawable d;
GC gc;
int color;
/*
 * Draw a single feature onto the screen.
 */
{
	float x, y;
	int px, py, width;
/*
 * Turn our location into something we can use.
 */
	msg_ELog (EF_DEBUG, "ov_DoFeature (0x%x)", fp);
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
	   	DrawText (Graphics, d, color, px, py, fp->f_string, 0.0,
			fp->f_size, JustifyCenter, JustifyCenter);
		break;
	/*
	 * Circles we do ourselves.
	 */
	   case FCircle:
		px = XPIX (x - fp->f_radius); py = YPIX (y + fp->f_radius);
		width = XPIX (x + fp->f_radius) - px;
	   	XDrawArc (disp, d, gc, px, py, width, width, 0, 360*64);
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
	GC gcontext;
	XColor xc;
	Display *disp = XtDisplay (Graphics);
	Drawable d = GWFrame (Graphics);
	FILE *mapfp;
/*
 * Find our platform -- the map name.
 */
	if (! px_GetParam (comp, "platform", NULL, platform, SYMT_STRING))
		return;
/*
 * Color information.
 */
	if (! pda_Search (Pd, comp, "color", "overlay", color, SYMT_STRING))
		strcpy (color, "white");
/*
 * Graphics context stuff.
 */
	gcontext = XCreateGC (disp, XtWindow (Graphics), 0, NULL);
	ct_GetColorByName (color, &xc);
	XSetForeground (disp, gcontext, xc.pixel);
	if (pda_Search (Pd, comp, "line-width", "map", (char *) &lwidth,
			SYMT_INT))
		XSetLineAttributes (disp, gcontext, lwidth, LineSolid,
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
	ov_DrawMap (mapfp, disp, d, gcontext);
/*
 * Clean up and go home.
 */
	fclose (mapfp);
	XFreeGC (disp, gcontext);
}





ov_DrawMap (mapfp, disp, d, gcontext)
FILE *mapfp;
Display *disp;
Drawable d;
GC gcontext;
/*
 * Draw this map file onto the screen.
 */
{
	float lat, lon, x, y;
	int px, py, npt, pt;
	static int nalloc = 0;
	static XPoint *pts = 0;
	char line[200], c;
/*
 * Just plow through the file.
 */
	while (fgets (line, 200, mapfp))
	{
	/*
	 * Figure out how many points we have.
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
	 * OK, it's a line of some kind.  Look into our memory allocation.
	 */
		npt /= 2;
	 	if (npt > nalloc)
		{
			nalloc = npt;
			if (pts)
				free (pts);
			pts = (XPoint *) malloc (npt*sizeof (XPoint));
		}
	/*
	 * Pass through each point.
	 */
	 	for (pt = 0; pt < npt; pt++)
		{
		/*
		 * Read the stuff, and convert it to something useful, and 
		 * stash it into the XPoint structure.
		 */
		 	fscanf (mapfp, "%f %f", &lat, &lon);
			cvt_ToXY (lat, lon, &x, &y);
		 	pts[pt].x = XPIX (x);
			pts[pt].y = YPIX (y);
		}
	/*
	 * Now draw them.
	 */
		XDrawLines (disp, d, gcontext, pts, npt, CoordModeOrigin);
	/*
	 * Eat a newline if need be.
	 */
	 	if ((c = fgetc (mapfp)) != '\n')
			ungetc (c, mapfp);
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
	ui_subcommand ("in-feature", "Feature>", ov_InFeature, fl);
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
			fp->f_size = UINT (cmds[4]);
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

