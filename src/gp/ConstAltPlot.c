/*
 * Herein lies all the Constant Altitude Plot code, carved from PlotExec.
 */
static char *rcsid = "$Id: ConstAltPlot.c,v 1.2 1991-02-12 21:16:03 corbet Exp $";

# include <X11/Intrinsic.h>
# include <ui.h>
# include <defs.h>
# include <pd.h>
# include <ui_date.h>
# include <message.h>
# include "GC.h"
# include "GraphProc.h"
# include "DrawText.h"
# include "PixelCoord.h"
# include "EventQueue.h"



/*
 * Color stuff
 */
static XColor	*Colors, Ctclr;
static int	Ncolors;
static int  	Monocolor;

/*
 * Other annotation information.
 */
static float Sascale;
static int Sashow;

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

# define BADVAL	-32768.0

/*
 * Contour plot types
 */
typedef enum {LineContour, FilledContour} contour_type;



/*
 * Forwards.
 */
# ifdef __STDC__
	void	CAP_FContour (char *, int);
	void	CAP_Vector (char *, int);
	void	CAP_Raster (char *, int);
	void	CAP_LineContour (char *, int);
	void	CAP_Contour (char *, contour_type, char *, float *, float *);
# else
	void	CAP_FContour ();
	void	CAP_Vector (), CAP_Raster (), CAP_LineContour ();
	void	CAP_Contour ();
# endif




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
Boolean	update;
/*
 * Filled contour CAP plot for the given component
 */
{
	float	center, step, bar_height, cval;
	int	i, left, right, top, bottom;
	char	string[10], fname[20];
/*
 * Use the common CAP contouring routine to do a filled contour plot
 */
	CAP_Contour (c, FilledContour, fname, &center, &step);
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
	An_TopAnnot (" filled contour", Tadefclr.pixel);
	An_TopAnnot (".  ", Tadefclr.pixel);
/*
 * Side annotation (color bar)
 */
	if(Sashow)
	{
	/*
	 * Get the limits of our space, and tweak them a bit.
	 */
		An_AnnotLimits (&top, &bottom, &left, &right);
		left += 5;
		top += 5;
		bottom -= 5;
		bar_height = (float)(bottom - top) / (float) Ncolors;
	/*
	 * Put in the field name.
	 */
		XSetForeground (XtDisplay (Graphics), Gcontext,Tadefclr.pixel);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, left, 
			top, fname, 0.0, Sascale, 
			JustifyLeft, JustifyCenter);
		top += Sascale * USABLE_HEIGHT;
	/*
	 * Now put each color into the color bar.
	 */
		for (i = 0; i <= Ncolors; i += Ctlimit)
		{
		/*
		 * Draw a color rectangle
		 */
			if (i < Ncolors)
			{
				XSetForeground (XtDisplay (Graphics),Gcontext, 
					Colors[Ncolors - i - 1].pixel);
				XFillRectangle (XtDisplay (Graphics), 
					GWFrame (Graphics), Gcontext, left, 
					(int)(top + i * bar_height), 10, 
					(int)(bar_height + 1));
			}
		/*
		 * Numeric label
		 */
			cval = center + (Ncolors/2 - i - 1) * step;
			sprintf (string, "%.1f", cval);
	
			XSetForeground (XtDisplay (Graphics), Gcontext, White);
			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				left + 15, (int)(top + i * bar_height), string,
				 0.0, Sascale, JustifyLeft, JustifyCenter);
		}
	}
}




void
CAP_LineContour (c, update)
char	*c;
Boolean	update;
/*
 * Line contour CAP plot for the given component
 */
{
	float	center, step, cval;
	char	fname[20], string[10];
	int	top, bottom, left, right, wheight, i;
	int	tacmatch = 0;
/* 
 * Use the common CAP contouring routine to do a color line contour plot
 */
	CAP_Contour (c, LineContour, fname, &center, &step);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	if (pda_Search (Pd, c, "ta-color-match", NULL, &tacmatch, SYMT_BOOL) &&
			tacmatch && Monocolor)
		An_TopAnnot (px_FldDesc (c, fname), Ctclr.pixel);
	else 
		An_TopAnnot (px_FldDesc (c, fname), Tadefclr.pixel);
	An_TopAnnot (" contour", Tadefclr.pixel);
	An_TopAnnot (".  ", Tadefclr.pixel);
/*
 * Side annotation
 */
	if(Sashow)
	{	
		An_AnnotLimits (&top, &bottom, &left, &right);
		left += 10;
		top += 5;

		wheight = USABLE_HEIGHT;

		if(! Monocolor)
		{
			XSetForeground (XtDisplay (Graphics), Gcontext, 
				Tadefclr.pixel);
			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
					left, top, fname, 0.0, Sascale, 
					JustifyLeft, JustifyTop);
			top += Sascale * wheight;
			for (i = 0; i <= Ncolors; i += Ctlimit)
			{
			/*
			 * Numeric label
			 */
				cval = center + (i - Ncolors / 2) * step;
				sprintf (string, "%.1f", cval);
	
		
				XSetForeground (XtDisplay (Graphics), Gcontext, 
					Colors[i].pixel);
				DrawText (Graphics, GWFrame (Graphics), 
					Gcontext, left, top, string, 0.0, 
					Sascale, JustifyLeft, JustifyTop);
				top += (int)(1.2 * Sascale * wheight);
			}
		}
	}
}




void
CAP_Contour (c, type, fname, center, step)
char	*c, *fname;
contour_type	type;
float	*center, *step;
/*
 * Execute a CAP contour plot, based on the given plot
 * description, specified component, and contour type.
 * Return the field name, contour center, and step from the plot
 * description.
 */
{
	char	ctname[40], platform[40], ctcolor[40];
	int	xdim, ydim;
	float	*rgrid, *grid, x0, x1, y0, y1, alt;
	int	pix_x0, pix_x1, pix_y0, pix_y1, dolabels, linewidth;
	int 	labelflag;
	time	t;
	Boolean	ok;
	XColor	black;
	XRectangle	clip;
/*
 * Get necessary parameters from the plot description
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, fname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "contour-center", fname, (char *) center, 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "contour-step", fname, (char *) step, 
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
	else ok &= pda_ReqSearch (Pd, c, "color-table", "contour", ctname, 
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
		ct_LoadTable (ctname, &Colors, &Ncolors);
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	alt = Alt;
	/* msg_ELog (EF_INFO, "Get grid at %.2f km", alt); */
	t = PlotTime;
	rgrid = ga_GetGrid (&t, platform, fname, &xdim, &ydim, &x0, &y0,
			&x1, &y1, &alt);
	if (Comp_index == 0)
		Alt = alt;
	if (! rgrid)
	{
		msg_ELog (EF_PROBLEM, "Unable to get grid");
		return;
	}
/*
 * Kludge: rotate the grid into the right ordering.
 */
	grid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, grid, xdim, ydim);
	free (rgrid);
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
			BADVAL);
		FillContour (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1, *center, *step);
		break;
	    case LineContour:
		if(! Monocolor)
			CO_Init (Colors, Ncolors, Ncolors / 2, black, clip, 
				TRUE, BADVAL);
		else CO_InitMono (Ctclr, clip, TRUE, BADVAL);
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
	lw_TimeStatus (c, &t);
	free (grid);
}




void
CAP_Vector (c, update)
char	*c;
Boolean	update;
/*
 * Execute a CAP vector plot, based on the given plot
 * description, specified component, and plot time
 */
{
	char	uname[20], vname[20], cname[30], platform[40], annot[120];
	int	xdim, ydim;
	float	*rgrid, *ugrid, *vgrid;
	float	vscale, x0, x1, y0, y1, alt;
	int	pix_x0, pix_x1, pix_y0, pix_y1;
	int	top, bottom, left, right, xannot, yannot;
	Boolean	ok;
	int	tacmatch = 0;
	XColor	color;
	time 	t;
/*
 * Get necessary parameters from the plot description
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "u-field", NULL, uname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "v-field", NULL, vname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "arrow-scale", NULL, CPTR (vscale), 
		SYMT_FLOAT);
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
 * Allocate the chosen arrow color
 */
	if (! ct_GetColorByName (cname, &color))
	{
		msg_ELog (EF_PROBLEM, "Can't get arrow color '%s'!", cname);
		return;
	}
/*
 * Get the data (pass in plot time, get back actual data time)
 */
	alt = Alt;
	t = PlotTime;
	rgrid = ga_GetGrid (&t, platform, uname, &xdim, &ydim, &x0, &y0,
			&x1, &y1, &alt);
	if (Comp_index == 0)
		Alt = alt;
	if (! rgrid)
	{
		msg_ELog (EF_PROBLEM, "Unable to get U grid");
		return;
	}
	ugrid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, ugrid, xdim, ydim);
	free (rgrid);

	rgrid = ga_GetGrid (&t, platform, vname, &xdim, &ydim, &x0, &y0,
			&x1, &y1, &alt);
	if (! rgrid)
	{
		msg_ELog (EF_PROBLEM, "Unable to get V grid");
		return;
	}
	vgrid = (float *) malloc (xdim * ydim * sizeof (float));
	ga_RotateGrid (rgrid, vgrid, xdim, ydim);
	free (rgrid);
/*
 * Convert the grid limits to pixel values
 */
	pix_x0 = XPIX (x0);	pix_x1 = XPIX (x1);
	pix_y0 = YPIX (y0);	pix_y1 = YPIX (y1);
/*
 * Draw the vectors
 */
	VectorGrid (Graphics, GWFrame (Graphics), ugrid, vgrid, xdim, ydim, 
		pix_x0, pix_y0, pix_x1, pix_y1, vscale, BADVAL, color);
/*
 * Free the data arrays
 */
	free (ugrid);
	free (vgrid);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
/*
 * Top annotation
 */
	sprintf (annot, " plot (%s).  ", platform);
	if (pda_Search (Pd, c, "ta-color-match", NULL, &tacmatch, SYMT_BOOL) &&
			tacmatch)
		An_TopAnnot ("Vector winds", color.pixel);
	else
		An_TopAnnot ("Vector winds", Tadefclr.pixel);
	An_TopAnnot (annot, Tadefclr.pixel);
/*
 * Side annotation (scale vectors)
 */
	An_AnnotLimits (&top, &bottom, &left, &right);

	if(tacmatch)
		XSetForeground (XtDisplay (Graphics), Gcontext, color.pixel);
	else
		XSetForeground (XtDisplay (Graphics), Gcontext, Tadefclr.pixel);

	xannot = (left + right) / 2;
	yannot = top + 0.04 * USABLE_HEIGHT;
	DrawText (Graphics, GWFrame (Graphics), Gcontext, xannot, yannot, 
		"10 m/sec", 0.0, Sascale, JustifyCenter, JustifyBottom);
	xannot = left;
	yannot += Sascale * USABLE_HEIGHT;
	if(tacmatch)
		VG_AnnotVector (xannot, yannot + 4, 10.0, 0.0, color.pixel);
	else
		VG_AnnotVector (xannot, yannot + 4, 10.0, 0.0, Tadefclr.pixel);
	lw_TimeStatus (c, &t);
}




void
CAP_Raster (c, update)
char	*c;
Boolean	update;
/*
 * Execute a CAP raster plot, based on the given plot
 * description, specified conent, and plot time
 */
{
	char	name[20], string[10], ctname[40], platform[40];
	int	xdim, ydim;
	int	top, bottom, left, right, i, newrp, fastloop;
	Boolean	ok;
	float	*grid, x0, x1, y0, y1, alt;
	float	min, max, bar_height, val, frac;
	int	pix_x0, pix_x1, pix_y0, pix_y1;
	XRectangle	clip;
	XColor	black;
	time	t;
/*
 * Get necessary parameters from the plot description
 */
	strcpy (name, "none");
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "field", NULL, name, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "minval", name, CPTR (min), SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "maxval", name, CPTR (max), SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "color-table", "raster", ctname, 
		SYMT_STRING);

	if (! ok)
		return;
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
	t = PlotTime;
	grid = ga_GetGrid (&t, platform, name, &xdim, &ydim, &x0, &y0,
			&x1, &y1, &alt);
	if (Comp_index == 0)
		Alt = alt;
	if (! grid)
	{
		msg_ELog (EF_INFO, "Unable to get grid for %s at %d %d",
			platform, PlotTime.ds_yymmdd, PlotTime.ds_hhmmss);
		return;
	}
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
	ct_GetColorByName ("black", &black);
	RP_Init (Colors, Ncolors, black, clip, min, max);
	if (! newrp)
		RasterPlot (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1);
	else
		RasterXIPlot (Graphics, GWFrame (Graphics), grid, xdim, ydim, 
			pix_x0, pix_y0, pix_x1, pix_y1, fastloop);
/*
 * Free the data array
 */
	free (grid);
/*
 * If it's just an update, return now since we don't want
 * to re-annotate
 */
	if (update)
		return;
	lw_TimeStatus (c, &t);
/*
 * Top annotation
 */
	An_TopAnnot (px_FldDesc (c, name), Tadefclr.pixel);
	An_TopAnnot ("plot", Tadefclr.pixel);
	An_TopAnnot (".  ", Tadefclr.pixel);
/*
 * Side annotation (color bar)
 */
	An_AnnotLimits (&top, &bottom, &left, &right);

	bottom -= 5;
	top += 5;
	bar_height = (bottom - top) / (float) Ncolors;
/*
 * Throw in the field name.
 */
	XSetForeground (XtDisplay (Graphics), Gcontext, Tadefclr.pixel);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, 
		top, name, 0.0, Sascale, JustifyLeft, 
		JustifyCenter);
	top += Sascale * USABLE_HEIGHT;
/*
 * Add all the colors.
 */
	for (i = 0; i < Ncolors; i++)
	{
	/*
	 * Draw a color rectangle
	 */
		XSetForeground (XtDisplay (Graphics), Gcontext, 
			Colors[Ncolors - i - 1].pixel);
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, left, (int)(top + i * bar_height), 10, 
			(int)(bar_height + 1));
	}
/*
 * Do the numeric labels.
 */
	for (i = 0; i < 9; i++)
	{
		frac = (float) i/8.0;
		val = min + (1.0 - frac) * (max - min);
		sprintf (string, "%.1f", val);

		XSetForeground (XtDisplay (Graphics), Gcontext,Tadefclr.pixel);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, left + 15, 
			(int)(top + frac * (bottom - top)),string,0.0,Sascale, 
			JustifyLeft, JustifyCenter);
	}
}
