/*
 * Herein lies all the Constant Altitude Plot code, carved from PlotExec.
 */
static char *rcsid = "$Id: ConstAltPlot.c,v 2.6 1991-11-14 17:48:57 kris Exp $";
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
static char Ctname[40];
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
	void	CAP_Contour (char *, contour_type, char *, float *, 
		float *, char *);
	static float * CAP_ImageGrid (char *, time *, PlatformId, char *,
		int *, int *, float *, float *, float *, float *, ScaleInfo *,
		float *);
	void	CAP_RasterSideAnnot (char *, char *, int, int, int);
	void	CAP_StaPltSideAnnot (char *, char *, int, int, int);
# else
	void	CAP_FContour ();
	void	CAP_Vector (), CAP_Raster (), CAP_LineContour ();
	void	CAP_Contour ();
	void	CAP_RasterSideAnnot ();
	void	CAP_StaPltSideAnnot ();
# endif

extern void An_ColorNumber ();
extern void An_ColorBar ();
extern void An_ColorVector ();
extern int An_GetLeft ();


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
	char	string[10], fname[20], ctable[40], data[100];
/*
 * Use the common CAP contouring routine to do a filled contour plot
 */
	CAP_Contour (c, FilledContour, fname, &center, &step, ctable);
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
Boolean	update;
/*
 * Line contour CAP plot for the given component
 */
{
	float	center, step, cval;
	char	fname[20], string[10], data[100], ctable[40];
	int	top, bottom, left, right, wheight, i;
	int	tacmatch = 0;
/* 
 * Use the common CAP contouring routine to do a color line contour plot
 */
	CAP_Contour (c, LineContour, fname, &center, &step, ctable);
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
			(char *) &tacmatch, SYMT_BOOL) && tacmatch && Monocolor)
		An_TopAnnot (px_FldDesc (c, fname), Ctclr.pixel);
	else 
		An_TopAnnot (px_FldDesc (c, fname), Tadefclr.pixel);
	An_TopAnnot (" contour", Tadefclr.pixel);
	An_TopAnnot (".  ", Tadefclr.pixel);
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
CAP_Contour (c, type, fname, center, step, ctable)
char	*c, *fname, *ctable;
contour_type	type;
float	*center, *step;
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
	t = PlotTime;
	rgrid = ga_GetGrid (&t, platform, fname, &xdim, &ydim, &x0, &y0,
			&x1, &y1, &alt);
	if (Comp_index == 0)
		Alt = alt;
	if (! rgrid)
	{
		msg_ELog (EF_INFO, "Unable to get grid");
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
	int	i, j, xdim, ydim;
	float	*rgrid, *ugrid, *vgrid;
	float	vscale, x0, x1, y0, y1, alt;
	int	pix_x0, pix_x1, pix_y0, pix_y1;
	int	top, bottom, left, right, xannot, yannot;
	Boolean	ok;
	int	tacmatch = 0, grid, linewidth;
	XColor	color, qcolor;
	time 	t;
	PlatformId pid;
	char	*fields[6];
	DataObject *dobj;
	float	unitlen;
	char	quadrants[120], *quads[6], quadclr[30], string[10];
	int	numquads = 0, offset_x[] = {-15, -15, 15, 15};
	int	offset_y[] = {-15, 15, -15, 15};
	char	data[100];
/*
 * Get necessary parameters from the plot description
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "u-field", NULL, uname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "v-field", NULL, vname, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "arrow-scale", NULL, CPTR (vscale), 
		SYMT_FLOAT);
	unitlen = USABLE_HEIGHT * vscale;
	if (! ok)
		return;
/*
 * Should we grid the data.
 */
	if (! pda_Search (Pd, c, "grid", NULL, (char *) &grid, SYMT_BOOL))
		grid = TRUE;
	msg_ELog (EF_DEBUG, "grid %s", grid ? "true" : "false");
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
	if (! pda_Search (Pd, c, "line-width", "vector", (char *) &linewidth,
		SYMT_INT))
		linewidth = 0;
	if (linewidth == 1) linewidth = 0;
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, linewidth,
		LineSolid, CapButt, JoinMiter);
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

	if (grid)
	{
		rgrid = ga_GetGrid (&t, platform, uname, &xdim, &ydim, 
				&x0, &y0, &x1, &y1, &alt);
		if (Comp_index == 0)
			Alt = alt;
		if (! rgrid)
		{
			msg_ELog (EF_INFO, "Unable to get U grid");
			return;
		}
		ugrid = (float *) malloc (xdim * ydim * sizeof (float));
		ga_RotateGrid (rgrid, ugrid, xdim, ydim);
		free (rgrid);

		rgrid = ga_GetGrid (&t, platform, vname, &xdim, &ydim, 
				&x0, &y0, &x1, &y1, &alt);
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
		VectorGrid (Graphics, GWFrame (Graphics), Gcontext, ugrid, 
			vgrid, xdim, ydim, pix_x0, pix_y0, pix_x1, pix_y1, 
			vscale, BADVAL, color);
	/*
	 * Free the data arrays
	 */
		free (ugrid);
		free (vgrid);
	}
	else
	/*
	 * Do the ROBOT style winds plot.
	 */
	{
		if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Bad platform '%s'", platform);
			return;
		}

		if (! ds_DataTimes (pid, &PlotTime, 1, DsBefore, &t))
		{
			msg_ELog(EF_INFO,"No data available at all for '%s'",
				platform);
			return;
		}
		if (pd_Retrieve (Pd, c, "quadrants", quadrants, SYMT_STRING))
		{
			if (!pd_Retrieve(Pd,c,"quad-color",quadclr,SYMT_STRING))
			{
				strcpy (quadclr, cname);
				qcolor = color;
			}
			else if(! ct_GetColorByName(quadclr, &qcolor))
			{
				strcpy (quadclr, cname);
				qcolor = color;
			}
			numquads = CommaParse (quadrants, quads);
			if (numquads > 4) numquads = 4;
		}

		fields[0] = uname;
		fields[1] = vname;
		for (i = 0; i < numquads; i++)
			fields[i + 2] = quads[i];

		if ((dobj = ds_GetData (pid, fields, 2 + numquads, &t, &t, 
			Org2dGrid, alt, BADVAL)) == 0)
		{
			msg_ELog (EF_INFO, "Get failed on '%s'", platform);
			return;
		}
	/*
	 * Graphics context stuff.
	 */
		XSetForeground (XtDisplay (Graphics), Gcontext, color.pixel);
	/*
	 * Draw the vectors.
	 */
		for (i = 0; i < dobj->do_desc.d_irgrid.ir_npoint; i++)
		{
			cvt_ToXY (dobj->do_desc.d_irgrid.ir_loc[i].l_lat, 
				dobj->do_desc.d_irgrid.ir_loc[i].l_lon, 
				&x0, &y0);
			pix_x0 = XPIX (x0);
			pix_y0 = YPIX (y0);
			ov_PositionIcon ("pam-loc", pix_x0, pix_y0, 
				color.pixel);
			XSetLineAttributes (XtDisplay (Graphics), Gcontext, 
				linewidth, LineSolid, CapButt, JoinMiter);
			if ((dobj->do_data[0][i] != BADVAL) && 
			    (dobj->do_data[1][i] != BADVAL))
				draw_vector (XtDisplay (Graphics),
				GWFrame (Graphics), Gcontext, pix_x0, pix_y0, 
				dobj->do_data[0][i], dobj->do_data[1][i], 
				unitlen);
			XSetForeground (XtDisplay (Graphics), Gcontext,
					qcolor.pixel);
		/*
 		 * Do quadrants if necessary.
		 */
			for (j = 0; j < numquads; j++)
			{
				if (dobj->do_data[j+2][i] == BADVAL)
					continue;
				sprintf(string, "%.1f", dobj->do_data[j+2][i]); 
				DrawText (Graphics, GWFrame (Graphics), 
					Gcontext, pix_x0 + offset_x[j], 
					pix_y0 + offset_y[j], string, 0.0, 
					Sascale, JustifyCenter, JustifyCenter);
			}
			XSetForeground (XtDisplay (Graphics), Gcontext,
					color.pixel);
		}
	}
	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, LineSolid, 
		CapButt, JoinMiter);
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
	if (pda_Search (Pd, c, "ta-color-match", NULL, (char *) &tacmatch,
			SYMT_BOOL) && tacmatch)
		An_TopAnnot ("Vector winds", color.pixel);
	else
		An_TopAnnot ("Vector winds", Tadefclr.pixel);
	An_TopAnnot (annot, Tadefclr.pixel);
/*
 * Side annotation (scale vectors)
 */

	if (grid)
	{
		sprintf (data, "%s %s %f %f %f", "10m/sec", cname,
			10.0, 0.0, unitlen); 
		An_AddAnnotProc (An_ColorVector, c, data, strlen (data),
			40, FALSE, FALSE);
	}
	else
	{
		if (numquads > 0)
		{
			sprintf (data, "%s %s %s %f %d ", "10m/sec", 
				cname, quadclr, unitlen, numquads);
			for (i = 0; i < 4; i++)
				if (i < numquads)
				{
					strcat (data, dobj->do_fields[i + 2]);
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
	}
	lw_TimeStatus (c, &t);
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
	int offset_x[] = {-15, -15, 15, 15};
	int offset_y[] = {-15, 15, -15, 15};
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
Boolean	update;
/*
 * Execute a CAP raster plot, based on the given plot
 * description, specified conent, and plot time
 */
{
	char	name[20], ctname[40], platform[40], data[100], hcolor[40];
	char	param[50];
	int	xdim, ydim;
	int	fastloop, newrp, nsteps;
	Boolean	ok, highlight;
	float	*grid, x0, x1, y0, y1, alt;
	float	min, max, center, step, hvalue, hrange;
	int	pix_x0, pix_x1, pix_y0, pix_y1, image;
	XRectangle	clip;
	XColor	black, xc;
	time	t;
	PlatformId pid;
	DataOrganization org;
	ScaleInfo scale;
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
	t = PlotTime;
	if (image)
		grid = CAP_ImageGrid (c, &t, pid, name, &xdim, &ydim, &x0, &y0,
			&x1, &y1, &scale, &alt);
	else
		grid = ga_GetGrid (&t, platform, name, &xdim, &ydim, &x0, &y0,
			&x1, &y1, &alt);
	if (! grid)
	{
		msg_ELog (EF_INFO, "Unable to get grid for %s at %d %d",
			platform, PlotTime.ds_yymmdd, PlotTime.ds_hhmmss);
		return;
	}
	if (Comp_index == 0)
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
	ct_GetColorByName ("black", &black);
	ct_GetColorByName (hcolor, &xc);
	max = center + (nsteps/2) * step;
	min = center - (nsteps/2) * step;
	RP_Init (Colors, Ncolors, black, clip, min, max, highlight, hvalue, 
		xc, hrange);
	if (image)
		RasterImagePlot (Graphics, DrawFrame, grid, xdim,
			ydim, pix_x0, pix_y0, pix_x1, pix_y1, scale.s_Scale,
			scale.s_Offset);
	else if (! newrp)
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
	An_TopAnnot (" plot", Tadefclr.pixel);
	An_TopAnnot (".  ", Tadefclr.pixel);
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


static void
CAP_RasterSideAnnot (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
{
	char string[40], ctable[40], color[40];
	float center, step, val, used, scale, value, range, max;
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






static float *
CAP_ImageGrid (c, when, pid, field, xdim, ydim, x0, y0, x1, y1, scale, alt)
char *c, *field;
time *when;
PlatformId pid;
int *xdim, *ydim;
float *x0, *y0, *x1, *y1, *alt;
ScaleInfo *scale;
/*
 * Fetch an image grid from this platform.
 */
{
	time realtime, stimes[60], obstimes[2];
	DataObject *dobj;
	RGrid *rg;
	float *ret, cdiff;
	Location slocs[60];
	int nsample, samp, csamp, all = 0, ntime;
/*
 * Find out when we can really get data.
 */
	if (! (ntime = ds_DataTimes (pid, when, 1, DsBefore, &realtime)))
	{
		msg_ELog (EF_INFO, "No data available at all for %s",
			ds_PlatformName (pid));
		return (0);
	}
	msg_ELog (EF_DEBUG, "Plot time %d %d -> %d %d", when->ds_yymmdd,
		when->ds_hhmmss, realtime.ds_yymmdd, realtime.ds_hhmmss);
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
		msg_ELog (EF_DEBUG, "Ptime %d, obs %d -- %d", 
				when->ds_hhmmss, obstimes[0].ds_hhmmss,
				obstimes[1].ds_hhmmss);
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
		msg_ELog (EF_DEBUG, "First, %d, diff %.1f", realtime.ds_hhmmss,
			cdiff);
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
			msg_ELog (EF_DEBUG, "Second, %d, diff %.1f",
				realtime.ds_hhmmss, cdiff);
		}
	}
/*
 * Snarf it.
 */
	if ((dobj = ds_GetData (pid, &field, 1, &realtime, &realtime,
				OrgImage, *alt, 0)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s/%s at %d %06d",
			ds_PlatformName (pid), field, realtime.ds_yymmdd, 
			realtime.ds_hhmmss);
		return (0);
	}
	*alt = dobj->do_aloc->l_alt;
/*
 * Return the various pieces of info.
 */
	rg = dobj->do_desc.d_img.ri_rg;
	*xdim = rg->rg_nX;
	*ydim = rg->rg_nY;
	cvt_ToXY (dobj->do_aloc->l_lat, dobj->do_aloc->l_lon, x0, y0);
	*x1 = *x0 + (rg->rg_nX - 1)*rg->rg_Xspacing;
	*y1 = *y0 + (rg->rg_nY - 1)*rg->rg_Yspacing;
	*scale = *dobj->do_desc.d_img.ri_scale;
/*
 * Save the pointer to the data, tell DS not to free it, and dump the 
 * data object.  The grid will be freed explicitly later.
 */
	ret = dobj->do_data[0];
	dobj->do_flags &= ~(DOF_FREEDATA | DOF_FREEALLDATA);
	ds_FreeDataObject (dobj);
	*when = realtime;
	return (ret);
}





void
CAP_Finish (alt)
float alt;
/*
 * Finish out CAP plots.
 */
{
	char string[80];
	int deg = 0;
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
