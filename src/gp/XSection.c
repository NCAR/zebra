/*
 * Vertical cross-sectioning
 */
static char *rcsid = "$Id: XSection.c,v 2.4 1991-11-22 20:52:28 kris Exp $";
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
# define BADVAL	-999.0
# define BUFLEN	1024

/*
 * Action to take when we hit a bad sounding
 */
# define BAD_SOUNDING	{ \
				sprintf (string, " (bad)"); \
				An_TopAnnot (string, White.pixel); \
				continue; \
			}

/*
 * The cross-section plane array, its length, its height, 
 * its dimensions, and a macro to reference it two-dimensionally
 */
static float	*Plane, *P_wgt, P_len, P_hgt = 12.0, P_bot = 0.0;

# define HDIM	50
# define VDIM	50

# define PLANE(i,j)	(Plane[((i)*VDIM)+(j)])
# define P_WGT(i,j)	(P_wgt[((i)*VDIM)+(j)])

/*
 * Floor and ceiling arrays and their associated weight arrays, so we 
 * know where data should not be placed
 */
static float	*Floor, *Ceiling;
static float	*F_wgt, *C_wgt;

/*
 * Cross-section endpoints
 */
float	X0 = 0.0, X1 = 0.0, Y0 = 0.0, Y1 = 0.0;
time	T0;

/*
 * Pixel limits for the plot
 */
int	Pix_left, Pix_right, Pix_bottom, Pix_top;

/*
 * Altitude or pressure on the vertical scale?
 */
int	Use_alt = TRUE;

/*
 * Time-height plot?
 */
int	Time_height;

/*
 * Use filled contours?
 */
int	Fill_contour;

/*
 * Maximum acceptable time difference between sounding time and plot time
 */
int	Maxdiff = -1;

/*
 * Points for a sounding trace
 */
XPoint	Trace[BUFLEN];
int	Tracelen = 0;

/*
 * Color array and indices
 */
XColor	*Colors;
int	Ncolors;
XColor	White, Black;

/*
 * Clip and unclip rectangles
 */
XRectangle	Clip, Unclip;

/*
 * Forward declarations
 */
void	xs_XSect (), xs_Background ();
void	xs_TimeHeight (), xs_Spatial (), xs_ExtendTrace (), xs_PutData ();
void	xs_DrawTrace (), xs_AddToLevel (), xs_BuildLimits ();
int	xs_Pos (), xs_TimePos (), xs_ZIndex ();




void
xs_LineContour (c, update)
char	*c;
bool	update;
{
	Fill_contour = FALSE;
	xs_XSect (c, update);
}




void
xs_FilledContour (c, update)
char	*c;
bool	update;
{
	Fill_contour = TRUE;
	xs_XSect (c, update);
}




void
xs_XSect (c, update)
char	*c;
bool	update;
/*
 * Draw a cross-section based on the given PD component.
 */
{
	bool	ok;
	char	platforms[120], fldname[20], ctname[20], string[10];
	char	param[50];
	int	i, j, dolabels, linewidth, top, bottom, left, right, wheight;
	float	center, step, cval, bar_height;
/*
 * Get the platforms, field, contour limits, and color table
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok = pda_ReqSearch (Pd, c, "field", NULL, fldname, SYMT_STRING);
	sprintf (param, "%s-center", fldname);
	ok &= pda_ReqSearch (Pd, c, param, "contour", (char *) &center, 
		SYMT_FLOAT);
	sprintf (param, "%s-step", fldname);
	ok &= pda_ReqSearch (Pd, c, param, "contour", (char *) &step, 
		SYMT_FLOAT);
	ok &= pda_ReqSearch (Pd, c, "color-table", "xsect", ctname, 
		SYMT_STRING);

	if (! ok)
		return;
/*
 * User-specified maximum time difference
 */
	if (! pda_Search (Pd, c, "max-time-diff", "xsect", (char *) &Maxdiff,
		SYMT_INT))
		Maxdiff = -1;
	else
		Maxdiff *= 60;	/* Convert to seconds */
/*
 * Special stuff for line contours
 */
	if (! pda_Search (Pd, c, "do-labels", "xsect", (char *) &dolabels,
		SYMT_BOOL))
		dolabels = TRUE;

	if (! pda_Search (Pd, c, "line-width", "xsect", (char *) &linewidth,
		SYMT_INT))
		linewidth = 0;
/*
 * Get the color table
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);
/*
 * Get black and white
 */
	ct_GetColorByName ("black", &Black);
	ct_GetColorByName ("white", &White);
/*
 * Different handling for spatial and time-height cross-sections
 */
	if (! pda_Search (Pd, c, "time-height", "xsect", (char *) &Time_height,
		SYMT_BOOL))
		Time_height = FALSE;

	if (Time_height)	
		xs_TimeHeight (c);
	else
		xs_Spatial (c);
/*
 * Pixel limits
 */
	Pix_left = XPIX (0.0);
	Pix_right = XPIX (P_len);
	Pix_bottom = YPIX (0.0);
	Pix_top = YPIX (P_hgt);
/*
 * Clip and unclip rectangles
 */
	Clip.x = Pix_left + 1;
	Clip.y = Pix_top + 1;
	Clip.width = Pix_right - Pix_left;
	Clip.height = Pix_bottom - Pix_top;

	Unclip.x = 0;
	Unclip.y = 0;
	Unclip.width = GWWidth (Graphics);
	Unclip.height = GWHeight (Graphics);
/*
 * Make sure everything necessary has been specified
 */
	if (! Time_height && X0 == X1 && Y0 == Y1)
	{
		msg_ELog (EF_PROBLEM, "Endpoints must be different!");
		return;
	}
/*
 * Allocate space for the plane and weight arrays
 */
	Plane = (float *) malloc (HDIM * VDIM * sizeof (float));
	P_wgt = (float *) malloc (HDIM * VDIM * sizeof (float));
/*
 * Get the floor and ceiling arrays and their associated weight arrays
 */
	Floor = (float *) malloc (HDIM * sizeof (float));
	F_wgt = (float *) malloc (HDIM * sizeof (float));
	Ceiling = (float *) malloc (HDIM * sizeof (float));
	C_wgt = (float *) malloc (HDIM * sizeof (float));
/*
 * Draw the background
 */
	xs_Background (platforms, fldname);
/*
 * Fill the plane with BADVALs and set the weights to zero.
 * Initialize the floor and ceiling arrays also.
 */
	for (i = 0; i < HDIM; i++)
	{
		for (j = 0; j < VDIM; j++)
		{
			PLANE (i, j) = BADVAL;
			P_WGT (i, j) = 0.0;
		}

		Floor[i] = P_bot;
		F_wgt[i] = 0.0;
		Ceiling[i] = P_bot + P_hgt;
		C_wgt[i] = 0.0;
	}
/*
 * Fill the data plane
 */
	xs_PutData (platforms, fldname);
/*
 * Draw the contours
 */
	if (Fill_contour)
	{
		FC_Init (Colors, Ncolors, Ncolors / 2, Black, Clip, 
			TRUE, BADVAL);
		FillContour (Graphics, GWFrame (Graphics), Plane, HDIM, VDIM, 
			Pix_left, Pix_bottom, Pix_right, Pix_top, center, 
			step);
	}
	else
	{
		CO_Init (Colors, Ncolors, Ncolors / 2, Black, Clip, 
			TRUE, BADVAL);
		Contour (Graphics, GWFrame (Graphics), Plane, HDIM, VDIM, 
			Pix_left, Pix_bottom, Pix_right, Pix_top, center, 
			step, dolabels, linewidth);
	}
/*
 * Side annotation (color scales)
 */
	An_AnnotLimits (&top, &bottom, &left, &right);

	if (Fill_contour)
	{
		left += 5;
		top += 5;
		bottom -= 5;

		bar_height = (float)(bottom - top) / (float) Ncolors;

		for (i = 0; i <= Ncolors; i++)
		{
		/*
		 * Draw a color rectangle
		 */
			if (i < Ncolors)
			{
				XSetForeground (XtDisplay (Graphics), Gcontext,
					Colors[i].pixel);
				XFillRectangle (XtDisplay (Graphics), 
					GWFrame (Graphics), Gcontext, left, 
					(int)(top + i * bar_height), 10, 
					(int)(bar_height + 1.5));
			}
		/*
		 * Numeric label
		 */
			cval = center + (i - Ncolors / 2) * step;
			sprintf (string, "%.1f", cval);

			XSetForeground (XtDisplay (Graphics), Gcontext, 
				White.pixel);
			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				left + 15, (int)(top + i * bar_height), string,
				0.0, 0.02, JustifyLeft, JustifyCenter);
		}
	}
	else
	{
		left += 10;
		top += 5;

		wheight = GWHeight (Graphics);

		for (i = 0; i < Ncolors; i++)
		{
		/*
		 * Numeric label
		 */
			cval = center + (i - Ncolors / 2) * step;
			sprintf (string, "%.1f", cval);

			XSetForeground (XtDisplay (Graphics), Gcontext, 
				Colors[i].pixel);
			DrawText (Graphics, GWFrame (Graphics), Gcontext,
				left, top, string, 0.0, 0.02, JustifyLeft, 
				JustifyTop);
			top += (int)(1.2 * 0.02 * wheight);
		}
	}
/*
 * Release the arrays and exit
 */
	free (Plane);
	free (P_wgt);
	free (Floor);
	free (F_wgt);
	free (Ceiling);
	free (C_wgt);
	return;
}




void
xs_PutData (platforms, fldname)
char	*platforms, *fldname;
/*
 * Fill the cross-section array with data from the chosen soundings
 */
{
	int	plat, nplat, pt, npts, iz, zndx, zndx_prev, status, ih, iv;
	int	yymmdd, hhmmss, diff;
	float	val, x, y, z, t, val_prev, x_prev, y_prev, z_prev, t_prev;
	float	zstep, hlen, frac;
	float	*fdata, *xpos, *ypos, *zpos, *tpos;
	float	xhighest, yhighest, zhighest;
	char	*pnames[20], zfld[20], *zf, string[20];
	time	ptime, dtime;
	PlatformId	pid;
	DataObject	*z_dobj = NULL, *f_dobj = NULL, *t_dobj = NULL;
/*
 * Parse out platform names
 */
	nplat = CommaParse (platforms, pnames);
/*
 * Vertical grid spacing
 */
	zstep = P_hgt / (float)(VDIM - 1);
/*
 * Loop through the platforms
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Add this platform to the top annotation
	 */
		if (plat != 0)
			An_TopAnnot (", ", White.pixel);

		An_TopAnnot (pnames[plat], White.pixel);
	/*
	 * Initialize for keeping track of the highest point
	 */
		if (Use_alt)
			zhighest = -9999.0;
		else
			zhighest = 9999.0;
	/*
	 * Get the ID of this platform
	 */
		pid = ds_LookupPlatform (pnames[plat]);
		if (pid == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Bad platform '%s'", 
				pnames[plat]);
			BAD_SOUNDING;
		}
	/*
	 * Find the closest data time before PlotTime
	 */
		ptime = PlotTime;
		if (! ds_DataTimes (pid, &ptime, 1, DsBefore, &ptime))
		{
			msg_ELog (EF_PROBLEM, "No data for '%s' at %d %d",
				pnames[plat], ptime.ds_yymmdd, 
				ptime.ds_hhmmss);
			BAD_SOUNDING;
		}
	/*
	 * Get the vertical position (altitude or pressure) data
	 */
		if (Use_alt)
			strcpy (zfld, "alt");
		else
			strcpy (zfld, "pres");

		zf = zfld;
		z_dobj = ds_GetObservation (pid, &zf, 1, &ptime, OrgScalar, 
			0.0, BADVAL);

		if (! z_dobj)
		{
			msg_ELog (EF_PROBLEM, "No '%s' data for '%s' at %d %d",
				zfld, pnames[plat], ptime.ds_yymmdd, 
				ptime.ds_hhmmss);
			BAD_SOUNDING;
		}

		zpos = z_dobj->do_data[0];
		npts = z_dobj->do_npoint;
	/*
	 * Get the data for the requested field
	 */
		f_dobj = ds_GetObservation (pid, &fldname, 1, &ptime, 
			OrgScalar, 0.0, BADVAL);

		if (! f_dobj)
		{
			msg_ELog (EF_PROBLEM, "No '%s' data for '%s' at %d %d",
				fldname, pnames[plat], ptime.ds_yymmdd, 
				ptime.ds_hhmmss);
			BAD_SOUNDING;
		}

		fdata = f_dobj->do_data[0];
	/*
	 * Put together the x,y position data
	 */
		if (Time_height && ! xs_TimePos (pid, &ptime, &tpos))
		{
			msg_ELog (EF_PROBLEM, "No '%s' time data at %d %06d",
				pnames[plat], ptime.ds_yymmdd, 
				ptime.ds_hhmmss);
			BAD_SOUNDING;
		}
		else if (! Time_height && ! xs_Pos (pid, &ptime, &xpos, &ypos))
		{
			msg_ELog (EF_PROBLEM, 
				"No '%s' position data at %d %06d", 
				pnames[plat], ptime.ds_yymmdd, 
				ptime.ds_hhmmss);
			BAD_SOUNDING;
		}
	/*
	 * Make sure the sounding start time is within the user-specified
	 * limit (if any)
	 */
		if (Maxdiff > 0)
		{
			ud_sub_date (&ptime, f_dobj->do_times, &dtime);
			diff = (dtime.ds_yymmdd % 100) * 86400 + 
				(dtime.ds_hhmmss / 10000) * 3600 + 
				((dtime.ds_hhmmss / 100) % 100) * 60 +
				(dtime.ds_hhmmss % 100);

			if (diff > Maxdiff)
				BAD_SOUNDING;
		}
	/*
	 * Loop through the points
	 */
		val_prev = BADVAL;

		for (pt = 0; pt < npts; pt++)
		{
		/*
		 * Bag this point if the datum or position is bad
		 */
			if (Time_height)
			{
				if (tpos[pt] == BADVAL)
					continue;
			}
			else
			{
				if (xpos[pt] == BADVAL || ypos[pt] == BADVAL)
					continue;
			}

			if (fdata[pt] == BADVAL || zpos[pt] == BADVAL)
				continue;
		/*
		 * Update the ceiling if this point is higher
		 */
			if ((Use_alt && zpos[pt] > zhighest) ||
				(!Use_alt && zpos[pt] < zhighest))
			{
				xhighest = xpos[pt];
				yhighest = ypos[pt];
				zhighest = zpos[pt];
			}
		/*
		 * Special treatment for the first good point
		 */
			if (val_prev == BADVAL)
			{
			/*
			 * Assign the previous point values
			 */
				val_prev = fdata[pt];
				z_prev = zpos[pt];
				zndx_prev = xs_ZIndex (zpos[pt]);

				if (Time_height)
					t_prev = tpos[pt];
				else
				{
					x_prev = xpos[pt];
					y_prev = ypos[pt];
				}
			/*
			 * Use the point to help build the floor array
			 */
				xs_BuildLimits (Floor, F_wgt, xpos[pt], 
					ypos[pt], zpos[pt]);
			/*
			 * Go on to the next point
			 */
				continue;
			}
		/*
		 * Quit when we get above the grid
		 */
			if (zndx_prev >= VDIM)
				break;
		/*
		 * Find the index of the next grid height at or above zpos[pt]
		 */
			zndx = xs_ZIndex (zpos[pt]);
		/*
		 * Assign values at grid levels between this point and the
		 * previous one
		 */
			for (iz = zndx_prev; iz < zndx && iz < VDIM; iz++)
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
				frac = (z - z_prev) / (zpos[pt] - z_prev);

				val = val_prev + frac * (fdata[pt] - val_prev);

				if (Time_height)
					t = (t_prev + frac * 
						(tpos[pt] - t_prev)) / 3600.0;
				else
				{
					x = x_prev + frac * (xpos[pt]-x_prev);
					y = y_prev + frac * (ypos[pt]-y_prev);
				}
			/*
			 * Add this datum in at the current height index
			 */
				if (Time_height)
					xs_AddToLevel (iz, t, 0.0, val);
				else
					xs_AddToLevel (iz, x, y, val);
			}
		/*
		 * Project this point onto the plane, and add a point to 
		 * the trace for this sounding
		 */
			if (Time_height)
				hlen = tpos[pt] / 3600.0;	/* to hours */
			else
				hlen = hypot (xpos[pt] - X0, ypos[pt] - Y0) *
					cos (atan2 (ypos[pt]-Y0, xpos[pt]-X0) -
					atan2 (Y1-Y0, X1-X0));

			xs_ExtendTrace (hlen, zpos[pt] - P_bot);
		/*
		 * Make this the previous point
		 */
			val_prev = fdata[pt];
			z_prev = zpos[pt];
			zndx_prev = zndx;
			if (Time_height)
				t_prev = tpos[pt];
			else
			{
				x_prev = xpos[pt];
				y_prev = ypos[pt];
			}
		}
	/*
	 * Free the position arrays returned by xs_Pos () or xs_TimePos ()
	 */
		if (xpos)
			free (xpos);

		if (ypos)
			free (ypos);

		if (tpos)
			free (tpos);
	/*
	 * Only go on if we had at least one good data value
	 */
		if (val_prev == BADVAL)
			continue;
	/*
	 * Use the highest point of this sounding to help build the 
	 * ceiling array
	 */
		xs_BuildLimits (Ceiling, C_wgt, xhighest, yhighest, zhighest);
	/*
	 * Draw the trace for this sounding
	 */
		xs_DrawTrace (pnames[plat]);
	/*
	 * Annotate the sounding with a time
	 */
		yymmdd = f_dobj->do_times[0].ds_yymmdd;
		hhmmss = f_dobj->do_times[0].ds_hhmmss;

		if (yymmdd != PlotTime.ds_yymmdd)
			sprintf (string, " (%d/%d/%d,%02d:%02d)", 
				yymmdd / 10000, (yymmdd / 100) % 100,
				yymmdd % 100, hhmmss / 10000, 
				(hhmmss / 100) % 100);
		else
			sprintf (string, " (%02d:%02d)", hhmmss / 10000, 
				(hhmmss / 100) % 100);

		An_TopAnnot (string, White.pixel);
	}
/*
 * Finish the top annotation with a period
 */
	An_TopAnnot (".  ", White.pixel);
/*
 * Remove data below the floor and above the ceiling
 */
	for (ih = 0; ih < HDIM; ih++)
	{
		for (iv = 0; iv < xs_ZIndex (Floor[ih]); iv++)
			PLANE(ih,iv) = BADVAL;

		for (iv = xs_ZIndex (Ceiling[ih]); iv < VDIM; iv++)
			PLANE(ih,iv) = BADVAL;
	}
/*
 * The plane array is populated, free the allocated memory and return
 */
	if (f_dobj)
		ds_FreeDataObject (f_dobj);

	if (z_dobj)
		ds_FreeDataObject (z_dobj);

	return;
}




int
xs_ZIndex (z)
float	z;
/*
 * Return the index of the first grid level at or above z
 */
{
	float	fndx = (z - P_bot) / P_hgt * (VDIM - 1);

	if ((float)((int) fndx) == fndx)
		return ((int) fndx);	
	else
		return ((int) fndx + 1);
}




void
xs_AddToLevel (iz, xdat, ydat, vdat)
int	iz;
float	vdat, xdat, ydat;
/*
 * Apply the point with value vdat located at (xdat,ydat) to the grid 
 * at height index iz.  (For time-height plots, xdat should be the time 
 * position and ydat should be zero)
 */
{
	int	ih;
	float	x, y, d, xstep, ystep, tstep, wgt;
/*
 * Sanity check
 */
	if (iz < 0 || iz >= VDIM)
		ui_error ("*BUG* Bad vertical index in xs_AddToLevel");
/*
 * Step through the grid horizontally at height index iz and use a distance
 * weighting scheme to apply the given point
 */
	if (Time_height)
		tstep = P_len / (HDIM - 1);
	else
	{
		xstep = (X1 - X0) / (HDIM - 1);
		ystep = (Y1 - Y0) / (HDIM - 1);
	}


	for (ih = 0; ih < HDIM; ih++)
	{
		if (Time_height)
		{
			x = ih * tstep;
			y = 0.0;
		}
		else
		{
			x = X0 + ih * xstep;
			y = Y0 + ih * ystep;
		}

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
		PLANE(ih,iz) = (PLANE(ih,iz) * P_WGT(ih,iz) + vdat * wgt) /
			(P_WGT(ih,iz) + wgt);
		P_WGT(ih,iz) = P_WGT(ih,iz) + wgt;
	}
}




void
xs_BuildLimits (array, weight, xdat, ydat, zdat)
float	*array, *weight;
float	xdat, ydat, zdat;
/*
 * Use the given point to help build either the given ceiling or floor
 * array.  For time-height plots, xdat should be the time position and 
 * ydat should be zero.
 */
{
	int	ih;
	float	pos, d, hstep, wgt;
/*
 * Horizontal step length in the plane
 */
	hstep = P_len / (HDIM - 1);
/*
 * Find the distance from the left endpoint of the plane to the projection
 * of the given point onto the plane.
 */
	if (Time_height)
		pos = xdat;
	else
		pos = hypot (xdat - X0, ydat - Y0) * 
			cos (atan2 (ydat-Y0, xdat-X0) - atan2 (Y1-Y0, X1-X0));
/*
 * Step through the array and use a distance weighting scheme to apply 
 * the given point.  The point is projected onto the plane before distances
 * are calculated. 
 */
	for (ih = 0; ih < HDIM; ih++)
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




void
xs_Background (platforms, fldname)
char	*platforms, *fldname;
/*
 * Draw the background for this cross-section
 */
{
	float	tick, tickinc, lat, lon, lolim, hilim;
	char	string[80], ctime0[20], ctime1[20];
	int	i, dolabel, seconds;
	char	*snd_site ();
	date	sdate, del_time, end_time, snd_time ();
	XPoint	pts[5];
/*
 * Title
 */
	sprintf (string, "Cross-section of %s ", fldname);
	An_TopAnnot (string, White.pixel);

# ifdef notdef
	if (Time_height)
	{
	/*
	 * Get the end time in date struct format
	 */
		seconds = (int) (P_len * 3600);

		del_time.ds_yymmdd = seconds / 86400;
		seconds %= 86400;

		del_time.ds_hhmmss = (seconds / 3600) * 10000 + 
			((seconds / 60) % 60) * 100 +
			(seconds % 60);

		ud_add_date (&T0, &del_time, &end_time);
	/*
	 * Write the begin and end times into string
	 */
		ud_format_date (ctime0, &T0, UDF_FULL);
		ud_format_date (ctime1, &end_time, UDF_FULL);
		sprintf (string, "from %s to %s", ctime0, ctime1);
	}
	else
	/*
	 * Write the start and end points into string
	 */
		sprintf (string, "from (%.1f,%.1f) km to (%.1f,%.1f) km ", 
			X0, Y0, X1, Y1);

	An_TopAnnot (string, White.pixel);
# endif

	sprintf (string, "using platforms: ");
	An_TopAnnot (string, White.pixel);
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
 * Figure out where to put ticks on horizontal axes
 */
	if (Time_height)
	{
		tickinc = pow (10.0, floor (log10 (P_len)));

		if ((P_len / tickinc) < 1.5)
			tickinc *= 0.1;
		else if ((P_len / tickinc) < 3.0)
			tickinc *= 0.2;
		else if ((P_len / tickinc) < 8.0)
			tickinc *= 0.5;
	/*
	 * Don't make tick increment finer than 1/2 hour
	 */
		if (Time_height && tickinc < 0.5)
			tickinc = 0.5;
	}
	else
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
		pts[1].y = YPIX (0.025 * P_hgt);
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);

		pts[0].y = YPIX (0.975 * P_hgt);
		pts[1].y = Pix_top;
		XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, pts, 2, CoordModeOrigin);
	/*
	 * Label every other tick
	 */
		if (dolabel)
		{
			if (Time_height)
				sprintf (string, "%d", (int) tick);
			else
				sprintf (string, "(%.1f,%.1f)", 
					X0 + tick / P_len * (X1 - X0),
					Y0 + tick / P_len * (Y1 - Y0));

			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				XPIX (tick), YPIX (-0.005 * P_hgt), string, 
				0.0, 0.02, JustifyCenter, JustifyTop);
		}
		dolabel = ! dolabel;
	}

	if (Time_height)
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			XPIX (0.5 * P_len), YPIX (-0.04 * P_hgt), 
			"Time from start (hours)", 0.0, 0.02, 
			JustifyCenter, JustifyTop);
	else
		DrawText (Graphics, GWFrame (Graphics), Gcontext,
			XPIX (0.5 * P_len), YPIX (-0.04 * P_hgt), 
			"Position (km)", 0.0, 0.02, JustifyCenter, 
			JustifyTop);
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
		pts[0].y = pts[1].y = YPIX (tick - P_bot);
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
			sprintf (string, "%d", (int) tick);
			DrawText (Graphics, GWFrame (Graphics), Gcontext, 
				XPIX (-0.005 * P_len), YPIX (tick - P_bot), 
				string, 0.0, 0.02, JustifyRight, 
				JustifyCenter);
		}
		dolabel = ! dolabel;
	}

	if (Use_alt)
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			XPIX (-0.04 * P_len), YPIX (0.5 * P_hgt), 
			"Altitude (km MSL)", 90.0, 0.02, JustifyCenter, 
			JustifyBottom);
	else
		DrawText (Graphics, GWFrame (Graphics), Gcontext, 
			XPIX (-0.04 * P_len), YPIX (0.5 * P_hgt), 
			"Pressure (mb)", 90.0, 0.02, JustifyCenter, 
			JustifyBottom);
}




void
xs_Spatial (c)
char	*c;
/*
 * Initialize for a spatial cross-section plot
 */
{
	bool	ok;
	int	nvals;
	char	string[30], *vals[2];
/*
 * Grab the left endpoint from the PD
 */
	ok = pda_ReqSearch (Pd, c, "left-endpoint", NULL, string, 
		SYMT_STRING);

	if (! ok)
		return;

	if ((nvals = CommaParse (string, vals)) != 2)
		msg_ELog (EF_PROBLEM, "Endpoints must be in x,y format");


	if (! sscanf (vals[0], "%f", &X0) || ! sscanf (vals[1], "%f", &Y0))
		msg_ELog (EF_PROBLEM, "Bad left endpoint (%s,%s)", 
			vals[0], vals[1]);
/*
 * Grab the right endpoint from the PD
 */
	ok = pda_ReqSearch (Pd, c, "right-endpoint", NULL, string, 
		SYMT_STRING);

	if (! ok)
		return;

	if ((nvals = CommaParse (string, vals)) != 2)
		msg_ELog (EF_PROBLEM, "Endpoints must be in x,y format");


	if (! sscanf (vals[0], "%f", &X1) || ! sscanf (vals[1], "%f", &Y1))
		msg_ELog (EF_PROBLEM, "Bad right endpoint (%s,%s)",
			vals[0], vals[1]);
/*
 * Find the plane length and indicate this isn't a time-height plot
 */
	P_len = hypot (X1 - X0, Y1 - Y0);
	Time_height = FALSE;
/*
 * Set the user coordinates of the graphics window.  Note that Xlo and
 * Xhi are x coordinates of the window, NOT of "real" space.  The x 
 * direction of the window is parallel to the direction of the plane.
 * Similarly, Ylo and Yhi are window y coordinates, which correspond to
 * the z coordinate of "real" space.
 */
	Xlo = 0.0;
	Xhi = P_len;
	Ylo = 0.0;
	Yhi = P_hgt;
}




void
xs_TimeHeight ()
/*
 * Initialize for a time-height plot
 */
{
# ifdef notdef
	int	snd, sec_delta, pt, npts;
	float	*tdata;
	date	stime, diff, latest, earliest;
	date	snd_time ();
/*
 * Find the earliest and latest times from all of the soundings used
 */
	earliest.ds_yymmdd = 99991231;
	earliest.ds_hhmmss = 235959;
	latest.ds_yymmdd = 0;
	latest.ds_hhmmss = 0;

	for (snd = 0; snd < Nsnd; snd++)
	{
		stime = snd_time (S_id[snd]);
	/*
	 * See if this is the latest sounding by start time.
	 * If so, put the length of the sounding into sec_delta
	 */
		if (stime.ds_yymmdd > latest.ds_yymmdd || 
			(stime.ds_yymmdd == latest.ds_yymmdd && 
			 stime.ds_hhmmss > latest.ds_hhmmss))
		{
			latest = stime;

			if (snd_has_field (S_id[snd], f_time))
			{
				tdata = (float *) 
					malloc (BUFLEN * sizeof (float));
				npts = snd_get_data (S_id[snd], tdata, BUFLEN, 
					f_time, BADVAL);
				for (pt = npts-1; pt >= 0; pt--)
					if (tdata[pt] != BADVAL)
					{
						sec_delta = 
							(int)(tdata[pt] + 0.5);
						break;
					}

				free (tdata);
			}
			else
				sec_delta = 0;
		}
					
	/*
	 * See if this is the earliest sounding
	 */
		if (stime.ds_yymmdd < earliest.ds_yymmdd ||
			(stime.ds_yymmdd == earliest.ds_yymmdd &&
			 stime.ds_hhmmss < earliest.ds_hhmmss))
			earliest = stime;
	}
/*
 * Make sure we have a time range
 */
	if (earliest.ds_yymmdd == latest.ds_yymmdd && 
		earliest.ds_hhmmss == latest.ds_hhmmss)
		ui_error ("All soundings used are at the same time!");
/*
 * Set the plot start time
 */
	T0 = earliest;
/*
 * Find the length of the plane (the total time range in hours)
 */
	ud_sub_date (&latest, &earliest, &diff);
	P_len = (diff.ds_yymmdd % 100) * 86400 + 	/* days    */
		(diff.ds_hhmmss / 10000) * 3600 + 	/* hours   */
		((diff.ds_hhmmss / 100) % 100) * 60 + 	/* minutes */
		(diff.ds_hhmmss % 100) + sec_delta;	/* seconds */

	P_len /= 3600.0;
/*
 * Set the time-height flag
 */
	Time_height = TRUE;
# endif
}

	



int
xs_Pos (pid, ptime, xpp, ypp)
PlatformId	pid;
time		*ptime;
float		**xpp, **ypp;
/*
 * Return the x and y positions of the data for platform 'pid'
 * The xpp and ypp arrays are malloc'ed by xs_Pos () and must
 * be freed by the caller.
 */
{
	float	*xpos, *ypos, *lat, *lon, *wspd, *wdir, *wtime, *dummy;
	float	ws, wd, t, dt, site_x, site_y;
	int	pt, npts;
	float	snd_s_lat (), snd_s_lon ();
	char	fname[20], *fn;
	DataObject	*lat_dobj = NULL, *lon_dobj = NULL;
	DataObject	*wspd_dobj = NULL, *wdir_dobj = NULL;
	DataObject	*t_dobj = NULL;
/*
 * Derive the (x,y) positions from (lat,lon) if possible
 */
# ifdef notdef
	if (snd_has_field (sid, f_lat) && snd_has_field (sid, f_lon))
# endif
	if (TRUE)
	{
	/*
	 * Lat and lon are available (can be converted directly to x,y)
	 */
		fn = fname;

		strcpy (fname, "lat");
		lat_dobj = ds_GetObservation (pid, &fn, 1, ptime, OrgScalar, 
			0.0, BADVAL);

		if (! lat_dobj)
			return (FALSE);

		strcpy (fname, "lon");
		lon_dobj = ds_GetObservation (pid, &fn, 1, ptime, OrgScalar, 
			0.0, BADVAL);

		if (! lon_dobj)
			return (FALSE);

		lat = lat_dobj->do_data[0];
		lon = lon_dobj->do_data[0];
		npts = lat_dobj->do_npoint;
	/*
	 * Get the (x,y) site location
	 */
		cvt_ToXY (lat_dobj->do_loc.l_lat, lat_dobj->do_loc.l_lon, 
			&site_x, &site_y);
	/*
	 * Allocate the x and y position arrays
	 */
		*xpp = xpos = (float *) malloc (npts * sizeof (float));
		*ypp = ypos = (float *) malloc (npts * sizeof (float));
	/*
	 * Convert the (lat,lon) points to (x,y) and put them in the arrays
	 */
		for (pt = 0; pt < npts; pt++)
			if (lat[pt] == BADVAL || lon[pt] == BADVAL)
				xpos[pt] = ypos[pt] = BADVAL;
			else
				cvt_ToXY (lat[pt], lon[pt], &(xpos[pt]), 
					&(ypos[pt]));
	/*
	 * Free the data objects
	 */
		ds_FreeDataObject (lat_dobj);
		ds_FreeDataObject (lon_dobj);
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
	else
	{
	/*
	 * We have no position data, just assume the sounding
	 * goes straight up
	 *
	 * Find out how many data points we need to do, then fill the 
	 * position arrays with site_x and site_y
	 */
		dummy = (float *) malloc (BUFLEN * sizeof (float));
		npts = snd_get_data (sid, dummy, BUFLEN, Fld, BADVAL);
		free (dummy);

		for (pt = 0; pt < npts; pt++)
		{
			xpos[pt] = site_x;
			ypos[pt] = site_y;
		}
	}
# endif

	return (TRUE);
}




int
xs_TimePos (pid, ptime, tpos)
char	*pid;
time	*ptime;
float	**tpos;
/*
 * Return the time positions of the data points for 'pid' relative to T0
 */
{
# ifdef notdef
	float	*time;
	int	npts, pt, start_sec;
	date	stime, diff, snd_time ();
/*
 * Get the sounding start time and the difference from the start time
 * of the plot
 */
	stime = snd_time (sid);
	ud_sub_date (&stime, &T0, &diff);
/*
 * Convert the time difference into seconds
 */
	start_sec = (diff.ds_yymmdd % 100) * 86400 +
		(diff.ds_hhmmss / 10000) * 3600 +
		((diff.ds_hhmmss / 100) % 100) * 60 + 
		(diff.ds_hhmmss % 100);
/*
 * If there is no time data for this sounding, fill the array with the
 * start time and return
 */
	if (! snd_has_field (sid, f_time))
	{
		for (pt = 0; pt < BUFLEN; pt++)
			tpos[pt] = start_sec;

		return;
	}
/*
 * Get the time data for the sounding
 */
	time = (float *) malloc (BUFLEN * sizeof (float));	

	npts = snd_get_data (sid, time, BUFLEN, f_time, BADVAL);

	for (pt = 0; pt < npts; pt++)
		if (time[pt] != BADVAL)
			tpos[pt] = start_sec + time[pt];
		else
			tpos[pt] = BADVAL;

	free (time);

	return;
# endif
}




void
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




void
xs_DrawTrace (name)
char	*name;
/*
 * Draw the current platform's trace
 */
{
	char	*string, dash[2];
	int	i, label_x, label_y;
	XColor	wht;
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
	string = (char *) malloc ((2 + strlen (name)) * sizeof (char));
	string[0] = ' ';
	strcpyUC (string + 1, name);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, label_x, label_y, 
		string, -90.0, 0.015, JustifyLeft, JustifyCenter);
	free (string);
/*
 * Set up to start a new trace
 */
	Tracelen = 0;
}


# endif  /* C_PT_XSECT */
