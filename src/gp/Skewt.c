/*
 * Skew-t plotting module
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

# if C_PT_SKEWT

# include <math.h>
# include <X11/Intrinsic.h>
# include <ui.h>
# include <ui_error.h>
# include <defs.h>
# include <draw.h>
# include <pd.h>
# include <message.h>
# include <GraphicsW.h>
# include <DataStore.h>
# include <DataChunk.h>
# include "derive.h"
# include "GraphProc.h"
# include "GC.h"
# include "PixelCoord.h"
# include "DrawText.h"

RCSID ("$Id: Skewt.c,v 2.23 1996-02-22 20:16:52 burghart Exp $")

/*
 * General definitions
 */
# define T_K	273.15
# define DEG_TO_RAD(x)	((x) * 0.017453292)
# define RAD_TO_DEG(x)	((x) * 57.29577951)
# ifndef PI
# define PI	3.141592654
# endif
# define BUFLEN	1024

/*
 * Plot limits (initialized in sk_Init below)
 */
static float	Pmin, Pmax;
static int	Pstep = 100;
static float	Tmin, Tmax;
static int	Tstep = 10;

/*
 * Slope of the isotherms
 */
# define SKEWSLOPE	1.0

/*
 * Mapping functions from pres to y and from (temp,y) to x
 */
# define YPOS(p)	(log((p)/Pmax) / log(Pmin/Pmax))
# define XPOS(t,y)	(((t)-Tmin)/(Tmax-Tmin) + (y)/SKEWSLOPE)

/*
 * Winds stuff
 */
static float	W_scale = 25.0;

/*
 * Line style
 */
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;

/*
 * Color array and indices
 */
static XColor	*Colors;
static int	Ncolors;
static bool 	Tacmatch = TRUE;
static XColor 	Tadefclr;
/*
 * Feet vs. Kilometer flag.
 */
static bool	DoFeet = FALSE;

/*
 * Wind barbs or wind vectors?
 */
static bool	Do_vectors;

# define C_BLACK	0
# define C_WHITE	1
# define C_BG1		2
# define C_BG2		3
# define C_BG3		4
# define C_BG4		5
# define C_DATA(i)	(6 + (i))


/*
 * Forward declarations
 */
void		sk_Skewt FP ((char *, int)); 
static void	sk_Background FP (()); 
static void	sk_Lift FP ((int, float*, float*, float*, double)); 
static void	sk_Thermo FP ((char *, char *, XColor, int));
static void	sk_Winds FP ((char *, char *, XColor, int, int, int, int, 
			      int));
static void	sk_Polyline FP ((float *, float*, int, LineStyle, XColor)); 
static void	sk_DrawText FP ((char *, double, double, double, XColor, 
				 double, int, int)); 
static void	sk_Clip FP ((double, double, double, double)); 
static void	sk_Surface FP ((float *, float *, float *, int, float*,
				float *, float *, double));
static int	sk_700mb FP ((float *, float *, float*, int, double, double,
			      float *, float *, int *, double));


void
sk_Skewt (c, update)
char	*c;
bool	update;
/*
 * Draw a skew-t, log p plot based on the given PD component.
 */
{
	bool		ok;
	int		plat, nplat, nwplat;
	char		ctname[24], tadefcolor[32], style[16];
	char		platforms[PlatformListLen];
	char		windplats[PlatformListLen];
	char		*pnames[MaxPlatforms], *wpnames[MaxPlatforms];
	XColor		color;
	int		skip;
/*
 * Get the platform list and color table
 */
	ok = pda_ReqSearch (Pd, c, "platform", NULL, platforms, SYMT_STRING);
	ok &= pda_ReqSearch (Pd, c, "color-table", "skewt", ctname, 
		SYMT_STRING);

	if (! ok)
		return;
/*
 * Break apart the platform names
 */
	nplat = CommaParse (platforms, pnames);
	if (nplat > 3)
		nplat = 3;
/*
 * Allow for separate platform(s) for winds data
 */
	nwplat = 0;

	if (pda_Search (Pd, c, "wind-platform", NULL, windplats, SYMT_STRING))
	{
		nwplat = CommaParse (windplats, wpnames);
		if (nwplat < nplat)
		{
			msg_ELog (EF_PROBLEM, 
				  "Not enough names in 'wind-platform'");
			nwplat = 0;
		}
		else
			nwplat = nplat;
	}
/*
 * Look for data limits; set them to defaults if necessary
 */
	if (! pda_Search (Pd, c, "pres-minval", "skewt", (char *) &Pmin, 
		SYMT_FLOAT))
		Pmin = 100.0;
	if (! pda_Search (Pd, c, "pres-maxval", "skewt", (char *) &Pmax, 
		SYMT_FLOAT))
		Pmax = 1050.0;
	if (! pda_Search (Pd, c, "temp-minval", "skewt", (char *) &Tmin, 
		SYMT_FLOAT))
		Tmin = -40.0;

	if (! pda_Search (Pd, c, "temp-maxval", "skewt", (char *) &Tmax, 
		SYMT_FLOAT))
		Tmax = 35.0;

	if (! pda_Search (Pd, "global", "ta-color", NULL, tadefcolor, 
		SYMT_STRING))
		strcpy(tadefcolor, "white");
	if (! ct_GetColorByName(tadefcolor, &Tadefclr))
	{
		msg_ELog(EF_PROBLEM, "Can't get default color: '%s'.",
			tadefcolor);
		strcpy(tadefcolor, "white");
		ct_GetColorByName(tadefcolor, &Tadefclr);
	}
	Tacmatch = TRUE;
	pda_Search (Pd, "global", "ta-color-match", NULL, (char *) &Tacmatch,
		SYMT_BOOL);
/*
 * Do feet or kilometers.
 */
	if (! pda_Search (Pd, c, "do-feet", "skewt", (char *) &DoFeet,
		SYMT_BOOL))
		DoFeet = FALSE;
	msg_ELog (EF_DEBUG, "DoFeet %s", DoFeet ? "true" : "false");
/*
 * Vectors (default) or barbs?
 */
        Do_vectors = TRUE;
        if (pda_Search (Pd, c, "wind-style", NULL, style, SYMT_STRING))
                Do_vectors = strncmp (style, "barb", 4);
/*
 * Plot every "skip" points
 */
	skip = 1;
	pda_Search (Pd, c, "data-skip", "skewt", (char *) &skip, SYMT_INT);
/*
 * Get the color table
 */
	ct_LoadTable (ctname, &Colors, &Ncolors);

	if (Ncolors < 8)
	{
		msg_ELog (EF_PROBLEM, "Skew-t color table too small");
		return;
	}
/*
 * Set the plot limits
 */
	Xlo = -0.2;
	Ylo = -0.1;
	Xhi = 1.4;
	Yhi = 1.1;
/*
 * Plot the background and top annotation
 */
	if (! update)
	{
		sk_Background ();
		An_TopAnnot ("Skew-t plot for ", Tadefclr.pixel);
		ot_SetString ("PLATFORM            TIME\n");
	}
/*
 * Loop through the platforms
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Determine the color for this platform
	 */
		color = Tacmatch ? Colors[C_DATA(plat)] : Tadefclr;
	/*
	 * Add a comma to the annotation (after the first platform)
	 */
		if (plat > 0)
			An_TopAnnot (", ", Tadefclr.pixel);
	/*
	 * Do the thermo bit, then the winds
	 */
		sk_Thermo (c, pnames[plat], color, update);
		sk_Winds (c, (nwplat > 0) ? wpnames[plat] : pnames[plat],
			  color, plat, nplat, (bool)(nwplat > 0), update, 
			  skip);
	}
/*
 * Add a period to the top annotation
 */
        if (! update) 
	    An_TopAnnot (".  ", Tadefclr.pixel);
/*
 * Unclip since we want to return the shared GC in clean condition
 */
	sk_Clip (Xlo, Ylo, Xhi, Yhi);
}




static void
sk_Background ()
/*
 * Draw the background for a skew T, log p diagram
 */
{
	int	i;
	float	x[128], y[128];
	char	string[64];
	float	t, pt, p, annot_angle, slope, intercept, xloc, yloc;
	static float	mr[] = {0.1, 0.2, 0.4, 1.0, 2.0, 3.0, 5.0, 8.0, 
			12.0, 20.0, 30.0, 40.0, 50.0, 60.0, 0.0};
	float step = Pstep;
/*
 * ICAO standard atmosphere pressures for altitudes from 0 km to 20 km
 * every 1 km
 */
	static float	alt_pres[] = {1013.2, 898.8, 795.0, 701.2, 616.5,
				540.3, 471.9, 410.7, 356.1, 307.6, 264.5,
				226.3, 193.3, 165.1, 141.0, 120.4, 102.9,
			        87.86, 75.05, 64.10, 54.75};
/*
 * Draw the outside rectangle
 */
	x[0] = 0.0; y[0] = 0.0;
	x[1] = 1.0; y[1] = 0.0;
	x[2] = 1.0; y[2] = 1.0;
	x[3] = 0.0; y[3] = 1.0;
	x[4] = 0.0; y[4] = 0.0;
	sk_Polyline (x, y, 5, L_solid, Colors[C_BG2]);
/*
 * Isotherms
 */
	y[0] = 0.0;
	y[1] = 1.0;

	annot_angle = RAD_TO_DEG (atan (SKEWSLOPE));

	for (t = -120; t <= 50; t += Tstep)
	{
	/*
	 * Draw the isotherm
	 */
		sk_Clip (0.0, 0.0, 1.0, 1.0);

		x[0] = XPOS (t, y[0]);
		x[1] = XPOS (t, y[1]);
		if (x[1] < 0.0)
			continue;

		sk_Polyline (x, y, 2, L_solid, Colors[C_BG2]);
	/*
	 * Unclip so we can annotate outside the border
	 */
		sk_Clip (Xlo, Ylo, Xhi, Yhi);
	/*
	 * Write the number either on the top or on the right side depending
	 * on the isotherm
	 */
		sprintf (string, "%d", (int) t);

		if (x[1] <= 1.0)
			sk_DrawText (string, x[1] + 0.01 * SKEWSLOPE, 1.01,
				annot_angle, Tadefclr, 0.025, JustifyLeft, 
				JustifyCenter);
		else
		{
			float	intercept = SKEWSLOPE * (1.01 - x[0]);
			if (intercept > 0.0)
				sk_DrawText (string, 1.01, intercept, 
					annot_angle, Tadefclr, 0.025, 
					JustifyLeft, JustifyCenter);
		}
	}
/*
 * Isobars
 */
	sk_Clip (Xlo, Ylo, Xhi, Yhi);
	x[0] = 0.0;
	x[1] = 1.0;
	for (p = Pmax; p >= Pmin; 
		p -= ((int) p % (int) step) ? ((int) p % (int) step) : step)
	{
	/*
	 * Draw the isobar
	 */
		y[0] = y[1] = YPOS (p);

		sk_Polyline (x, y, 2, L_solid, Colors[C_BG2]);
	/*
	 * Annotate along the left side
	 */
		sprintf (string, "%d", (int) p);
		sk_DrawText (string, -0.01, y[0], 0.0, Tadefclr, 0.025, 
			JustifyRight, JustifyCenter);
	/*
	 * Kludge: if we get below the step value, decrease it so we continue
	 * to get lines.
	 */
		if (p <= step)
			step /= 10;
	}
/*
 * Standard atmosphere altitude scale
 */
	sk_Clip (Xlo, 0.0, 0.0, 1.0);

	x[0] = x[1] = Xlo / 2.0;
	y[0] = 0.0;
	y[1] = 1.0;
	sk_Polyline (x, y, 2, L_solid, Colors[C_BG2]);

	x[0] = Xlo / 2.0;
	x[1] = x[0] - 0.01;
	for (i = 0; alt_pres[i] > Pmin; i++)
	{
	/*
	 * Tick mark
	 */
		y[0] = y[1] = YPOS (alt_pres[i]);
		sk_Polyline (x, y, 2, L_solid, Colors[C_BG2]);
	/*
	 * Label
	 */
		if (DoFeet)
			sprintf (string, "%.1f ", i*1000.0/0.30480);
		else
			sprintf (string, "%d ", i);
		sk_DrawText (string, x[1], y[1], 0.0, Colors[C_BG2], 0.02, 
			JustifyRight, JustifyCenter);
	}
/*
 * Saturation mixing ratio lines
 */
	sk_Clip (0.0, 0.0, 1.0, 1.0);
	y[0] = 0.0;
	y[1] = YPOS ((Pmin + Pmax) / 2.0);
	for (i = 0; mr[i] > 0.0; i++)
	{
	/*
	 * The lines go from Pmax to (Pmax + Pmin) / 2
	 */
		x[0] = XPOS (t_mr (Pmax, mr[i]) - T_K, y[0]);
		x[1] = XPOS (t_mr ((Pmin + Pmax) / 2.0, mr[i]) - T_K, y[1]);
	/*
	 * Find the angle for the annotation
	 */
		annot_angle = RAD_TO_DEG (atan ((y[1] - y[0])/(x[1] - x[0])));
	/*
	 * Plot the line and annotate just above the top of the line
	 */
		sk_Polyline (x, y, 2, L_dashed, Colors[C_BG2]);
		sprintf (string, "%03.1f", mr[i]);
		sk_DrawText (string, x[1], y[1] + 0.01, annot_angle, 
			Colors[C_BG2], 0.02, JustifyLeft, JustifyCenter);
	}
/*
 * Saturated adiabats
 */
	for (t = 0; t <= 36; t += 4)
	{
		int	npts = 0;
		float	ept;
	/*
	 * Find the equivalent potential temperature which corresponds
	 * to a saturated parcel at the surface at temperature t
	 */
		ept = theta_e (t + T_K, t + T_K, 1000.);
	/*
	 * Build the "iso-ept" curve
	 */
		for (p = Pmin + 100; p < Pmax + 15; p += 15)
		{
			float	temp = t_sat (ept, p) - T_K;
			y[npts] = YPOS (p);
			x[npts] = XPOS (temp, y[npts]);
			npts++;
		}
	/*
	 * Plot the curve and annotate just above the (Pmin + 100) isobar
	 */
		sk_Polyline (x, y, npts, L_dotted, Colors[C_BG3]);

		if (x[0] > 0.0 && x[0] < 1.0)
		{
			annot_angle = 
				RAD_TO_DEG (ATAN2 (y[1] - y[0], x[1] - x[0]));
			if (annot_angle > 0.0)
				annot_angle -= 180.0;

			sprintf (string, "%d", (int) t);
			sk_DrawText (string, x[0], y[0] + 0.01, annot_angle, 
				Colors[C_BG3], 0.02, JustifyRight, 
				JustifyCenter);
		}
	}
/*
 * Dry adiabats
 */
 	for (pt = Tmin - (int) Tmin % 10; pt < Tmax + 200; pt += 10)
	{
		int	npts = 0;
	/*
	 * Build a constant potential temperature curve, assuming a dry parcel
	 */
		for (p = Pmin; p < Pmax + 15; p += 15)
		{
			float	temp = theta_to_t (pt + T_K, p) - T_K;
			y[npts] = YPOS (p);
			x[npts] = XPOS (temp, y[npts]);
			npts++;
		}
	/*
	 * Plot the curve and annotate just inside either the left or top
	 * boundary
	 */
		sk_Polyline (x, y, npts, L_dotted, Colors[C_BG4]);

		sprintf (string, "%d", (int) (pt + 273));

		if (x[0] > 0.0 && x[0] <= 1.0)
		{
			slope = (y[1] - y[0]) / (x[1] - x[0]);
			annot_angle = RAD_TO_DEG (atan (slope));
		/*
		 * Nasty stuff for annotation positioning
		 */
			xloc = x[0] - 0.02 / slope;
			xloc -= 0.005 * slope / sqrt (1.0 + slope * slope);

			yloc = 0.98;
			yloc += 0.005 / sqrt (1.0 + slope * slope);
		/*
		 * Write the number
		 */
			sk_DrawText (string, xloc, yloc, annot_angle, 
				Colors[C_BG4], 0.02, JustifyLeft, 
				JustifyBottom);
		}
		else if (x[0] <= 0.0)
		{
			for (i = 0; i < npts; i++)
				if (x[i] > 0.0)
					break;

			if (i != npts)
			{
			/*
			 * Nasty stuff for the annotation positioning
			 */
				slope = (y[i] - y[i-1]) / (x[i] - x[i-1]);
				annot_angle = RAD_TO_DEG (atan (slope));

				intercept = y[i] - slope * x[i];
				xloc = 0.0;
				xloc -= 0.005 * 
					slope / sqrt (1.0 + slope * slope);

				yloc = intercept;
				yloc += 0.005 / sqrt (1.0 + slope * slope);
			/*
			 * Write the number
			 */
				sk_DrawText (string, xloc, yloc, annot_angle,
					Colors[C_BG4], 0.02, JustifyLeft, 
					JustifyBottom);
			}
		}
	}
}




static void
sk_Lift (ndata, pres, temp, dp, badvalue)
int	ndata;
float	*pres, *temp, *dp, badvalue;
/*
 * Draw the lines for a lifted parcel for the given pressure, temperature,
 * and dp data
 */
{
	int	npts, ndx_700, do_700;
	float	x[200], y[200];
	float	p_lcl, t_lcl, pt, w, t_sfc, p_sfc, dp_sfc, ept;
	float	t_700, dp_700, p_lcl700, t_lcl700, ept_700, pt_700;
	float	p, pstep = 10, t, min_lcl;
/*
 * Find the first good point and use it as the surface point
 */
ERRORCATCH
	sk_Surface (temp, pres, dp, ndata, &t_sfc, &p_sfc, &dp_sfc, badvalue);
	sk_700mb (temp, pres, dp, ndata, p_sfc, dp_sfc, &t_700, &dp_700, 
		&ndx_700, badvalue);
ON_ERROR
	msg_ELog (EF_INFO, "About to return from sk_Lift");
	ui_epop ();
	return;
ENDCATCH

	t_sfc += T_K;	t_700 += T_K;
	dp_sfc += T_K;	dp_700 += T_K;
/*
 * Find the mixing ratio for our surface point
 */
	w = w_sat (dp_sfc, p_sfc);
/*
 * Get the potential temperature, LCL pressure and temperature, and the 
 * eqivalent potential temperature at the surface
 */
	pt = theta_dry (t_sfc, p_sfc);
	p_lcl = lcl_pres (t_sfc, dp_sfc, p_sfc);
	t_lcl = lcl_temp (t_sfc, dp_sfc);
	ept = theta_e (t_lcl, t_lcl, p_lcl);
/*
 * Get the forecasted (700 mb) LCL pressure, temp, and ept
 */
	if (t_700 > dp_700)
	{
		pt_700 = theta_dry (t_700, 700.0);
		p_lcl700 = lcl_pres (t_700, dp_700, 700.0);
		t_lcl700 = lcl_temp (t_700, dp_700);
		ept_700 = theta_e (t_lcl700, t_lcl700, p_lcl700);
		do_700 = TRUE;
	}
	else
	{
		do_700 = FALSE;
		msg_ELog (EF_INFO, 
			"Unable to calculate forecasted lifted parcel");
	}
/*
 * Draw the saturated adiabat from the LCL up
 */
	npts = 0;

	for (p = p_lcl; p >= Pmin - pstep; p -= pstep)
	{
		t = t_sat (ept, p) - T_K;
		y[npts] = YPOS (p);
		x[npts] = XPOS ((float) t, y[npts]);
		npts++;
	}

	sk_Polyline (x, y, npts, L_dotted, Colors[C_BG1]);
/*
 * Draw the saturated adiabat from the forecasted LCL up
 */
	if (do_700)
	{
		npts = 0;

		for (p = p_lcl700; p >= Pmin - pstep; p -= pstep)
		{
			t = t_sat (ept_700, p) - T_K;
			y[npts] = YPOS (p);
			x[npts] = XPOS ((float) t, y[npts]);
			npts++;
		}

		sk_Polyline (x, y, npts, L_dotted, Colors[C_BG1]);
	}
/*
 * Draw the dry adiabat from the LCL down
 */
	npts = 0;

	for (p = p_lcl; p < p_sfc + pstep; p += pstep)
	{
	/*
	 * Stop at the surface pressure
	 */
		if (p > p_sfc)
			p = p_sfc;
	/*
	 * Get the temp corresponding to our theta at this pressure
	 */
		t = theta_to_t (pt, p) - T_K;
	/*
	 * Translate into overlay coordinates
	 */
		y[npts] = YPOS (p);
		x[npts] = XPOS ((float) t, y[npts]);
		npts++;
	}

	sk_Polyline (x, y, npts, L_dotted, Colors[C_BG1]);
/*
 * Draw the dry adiabat from the forecasted LCL down
 */
	if (do_700)
	{
		npts = 0;

		for (p = p_lcl700; p < 700.0 + pstep; p += pstep)
		{
		/*
		 * Stop at 700 mb
		 */
			if (p > 700.0)
				p = 700.0;
		/*
		 * Get the temp corresponding to our theta at this pressure
		 */
			t = theta_to_t (pt_700, p) - T_K;
		/*
		 * Translate into overlay coordinates
		 */
			y[npts] = YPOS (p);
			x[npts] = XPOS ((float) t, y[npts]);
			npts++;
		}

		sk_Polyline (x, y, npts, L_dotted, Colors[C_BG1]);
	}
/*
 * Draw the saturation mixing ratio line from the surface up to the LCL
 * with the lower pressure
 */
	if (do_700)
		min_lcl = (p_lcl < p_lcl700 ? p_lcl : p_lcl700);
	else
		min_lcl = p_lcl;

	t = t_mr (min_lcl, w) - T_K;
	y[0] = YPOS ((float) min_lcl);
	x[0] = XPOS ((float) t, y[0]);

	t = t_mr (p_sfc, w) - T_K;
	y[1] = YPOS ((float) p_sfc);
	x[1] = XPOS ((float) t, y[1]);

	npts = 2;

	sk_Polyline (x, y, npts, L_dotted, Colors[C_BG1]);
/*
 * Done
 */
	return;
}




static void
sk_Thermo (c, pname, color, update)
char	*c, *pname;
XColor	color;
bool    update;
/*
 * Plot the thermo data using the given component, platform name, and color.  
 * The boolean "update" tells whether this is just an update of a previous
 * plot.
 */
{
	float	*xt, *xd, *yt, *yd, *pres, *temp, *dp, badvalue;
	float	y;
	int	i, npts, nprev, good_d = 0, good_t = 0, antime = 0, nflds;
	char	string[40];
	FieldId	flist[3], *platflds;
	ZebTime	ptime;
	PlatformId	pid;
	DataChunk	*dc;
	Pixel tacolor = Tacmatch ? color.pixel : Tadefclr.pixel;
	dsDetail details[5];
	int ndetail = 0;
/*
 * Get the platform id and obtain a good data time
 */
	pid = ds_LookupPlatform (pname);
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", pname);
		return;
	}
	
	ptime = PlotTime;
	if (! ds_GetObsTimes (pid, &ptime, &ptime, 1, NULL))
	{
		char	tstring[20];

		TC_EncodeTime (&ptime, TC_Full, tstring);
		msg_ELog (EF_INFO, "No data for '%s' at %s", pname, tstring);
		return;
	}
/*
 * If this is an update, find out how many points we got last time
 */
	nprev = 0;
	
	if (update)
		pda_Search (Pd, c, "skPrivate-thermo-npts", NULL, 
			    (char *) &nprev, SYMT_INT);
/*	
 * Build the field list, with an ugly kluge to find either "temp" or "tdry"
 * for the temperature field.  At some point, the field names should be 
 * specified through plot description parameters...
 */
	flist[0] = F_Lookup ("pres");
	flist[1] = BadField;
	flist[2] = F_Lookup ("dp");

	platflds = (FieldId *) malloc (50 * sizeof (FieldId));
	ds_GetFields (pid, &ptime, &nflds, platflds);
	for (i = 0; i < nflds; i++)
		if (! strcmp (F_GetName (platflds[i]), "temp") ||
		    ! strcmp (F_GetName (platflds[i]), "tdry"))
			flist[1] = platflds[i];
	free (platflds);
/*
 * Check for a request for a particular bad value
 */
	if (pda_Search (Pd, c, "bad-value", pname, (char *)&badvalue,
			SYMT_FLOAT))
	{
		details[ndetail].dd_V.us_v_float = badvalue;
		details[ndetail++].dd_Name = DD_FETCH_BADVAL;
	}
/*
 * Get the data
 */
	if (! (dc = ds_FetchObs (pid, DCC_Scalar, &ptime, flist, 3, 
				 details, ndetail)))
	{
		msg_ELog (EF_PROBLEM, "Unable to get data for '%s'.", pname);
		return;
	}
	if (ndetail)
		dc_SetBadval (dc, badvalue);
/*
 * Add this platform to the annotation
 */
        if (! update)
	{
		int active = FALSE;
		pda_Search (Pd, c, "top-annot-active", "skewt",
				(char *) &active, SYMT_BOOL);
		An_DoTopAnnot (pname, tacolor, active ? c : 0, pname);
	}
/*
 * See if they want to hear about the time.
 */
	if (! pda_Search (Pd, c, "annot-time", "skewt", (char *) &antime,
			SYMT_BOOL) || antime)
	{
		char atime[40];
		TC_EncodeTime (&ptime, TC_Full, atime + 2);
		atime[0] = ' '; atime[1] = '(';
		strcat (atime, ")");
		An_TopAnnot (atime, tacolor);
	}
/*
 * Find the number of points in the observation, and stash it away
 */
	badvalue = dc_GetBadval (dc);
	npts = dc_GetNSample (dc);

	if (nprev > npts)
	{
	/*
	 * We're starting a new sounding, so trigger a global update
	 */
		TriggerGlobal = TRUE;
		nprev = npts = 0;
	}
		
	pd_Store (Pd, c, "skPrivate-thermo-npts", (char *) &npts, SYMT_INT);
/*
 * Get the data from the chunk, skipping points we've already plotted (if any)
 */
	npts -= nprev;

	pres = (float *) malloc (npts * sizeof (float));
	temp = (float *) malloc (npts * sizeof (float));
	dp = (float *) malloc (npts * sizeof (float));

	for (i = 0; i < npts; i++)
	{
		pres[i] = dc_GetScalar (dc, i + nprev, flist[0]);
		temp[i] = dc_GetScalar (dc, i + nprev, flist[1]);
		dp[i] = dc_GetScalar (dc, i + nprev, flist[2]);
	}

	dc_DestroyDC (dc);
/*
 * Put a line in the overlay times widget
 */
	if (! update)
	{
		sprintf (string, "%-19.19s ", pname);
		TC_EncodeTime (&ptime, TC_Full, string + 20);
		strcat (string, "\n");
		ot_Append (string);
	}
	
/*
 * Clip
 */
	sk_Clip (0.0, 0.0, 1.0, 1.0);
/*
 * Allocate pixel coordinate arrays
 */
	xt = (float *) malloc (npts * sizeof (float));
	xd = (float *) malloc (npts * sizeof (float));
	yt = (float *) malloc (npts * sizeof (float));
	yd = (float *) malloc (npts * sizeof (float));
/*
 * Translate to skew-t coordinates
 */
	for (i = 0; i < npts; i++)
	{
		if (pres[i] == badvalue)
			continue;
		else
		{
			y = YPOS (pres[i]);
			yt[good_t] = y;
			yd[good_d] = y;
		}

		if (temp[i] != badvalue)
			xt[good_t++] = XPOS (temp[i], y);

		if (dp[i] != badvalue)
			xd[good_d++] = XPOS (dp[i], y);
	}
/*
 * Draw the lines
 */
	sk_Polyline (xt, yt, good_t, L_solid, color);
	sk_Polyline (xd, yd, good_d, L_solid, color);
/*
 * Draw the lifted parcel lines
 */
	if (! update)
		sk_Lift (npts, pres, temp, dp, badvalue);
/*
 * Free the allocated space and return
 */

	free (pres);
	free (temp);
	free (dp);

	free (xt);
	free (xd);
	free (yt);
	free (yd);

	return;
}




static void
sk_Winds (c, pname, color, plot_ndx, nplots, annot, update, skip)
char	*c, *pname;
XColor	color;
int	plot_ndx, nplots;
bool	annot, update;
int	skip;
/*
 * Plot sounding winds given:
 *	c:	pd component
 *	pname:	the platform name
 *	color:	the color to use
 *	plot_ndx:	the index for this winds plot
 *	nplots:	the total number of winds plots
 *	annot:	write annotation for this plot?
 *	update:	is this an update plot?
 *	skip:	plot every skip'th point
 */
{
	float	xstart, xscale, yscale, xov[2], yov[2], badvalue;
	float	*pres, *u, *v, wspd, wdir;
	int	i, nfld, npts, nprev, ipts, shaftlen;
	char	string[40];
	bool	have_u, have_v, have_uv;
	ZebTime	ptime;
	FieldId	flist[3], favail[30];
	DataChunk	*dc;
	PlatformId	pid;
	dsDetail details[5];
	int ndetail = 0;
/*
 * Add this platform to the annotation
 */
	if (annot && ! update)
	{
		sprintf (string, " (winds: %s)", pname);
		An_TopAnnot (string, Tacmatch ? color.pixel : Tadefclr.pixel);
	}
/*
 * Get the platform id and obtain a good data time
 */
	pid = ds_LookupPlatform (pname);
	if (pid == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad winds platform '%s'", pname);
		return;
	}
	
	ptime = PlotTime;
	if (! ds_GetObsTimes (pid, &ptime, &ptime, 1, NULL))
	{
		char	tstring[20];

		TC_EncodeTime (&ptime, TC_Full, tstring);
		msg_ELog (EF_INFO, "No winds data for '%s' at %s", pname, 
			  tstring);
		return;
	}
/*
 * Get u_wind and v_wind if they exist, otherwise get wspd and wdir 
 * and we'll do the derivation.  (The necessity for this will 
 * disappear when the data store has some derivation capability of 
 * its own)
 */
	have_u = have_v = FALSE;
	nfld = sizeof (favail) / sizeof (FieldId); /* max we accept */
	ds_GetFields (pid, &ptime, &nfld, favail);
	for (i = 0; i < nfld; i++)
	{
		have_u |= (favail[i] == F_Lookup ("u_wind"));
		have_v |= (favail[i] == F_Lookup ("v_wind"));
	}
	
	have_uv = have_u && have_v;

	flist[0] = F_Lookup ("pres");
	flist[1] = have_uv ? F_Lookup ("u_wind") : F_Lookup ("wspd");
	flist[2] = have_uv ? F_Lookup ("v_wind") : F_Lookup ("wdir");
/*
 * If this is an update, find out how many points we got last time
 */
	nprev = 0;
	if (update)
		pda_Search (Pd, c, "skPrivate-wind-npts", NULL, 
			    (char *) &nprev, SYMT_INT);
/*
 * Check for a request for a particular bad value
 */
	if (pda_Search (Pd, c, "bad-value", pname, (char *)&badvalue,
			SYMT_FLOAT))
	{
		details[ndetail].dd_V.us_v_float = badvalue;
		details[ndetail++].dd_Name = DD_FETCH_BADVAL;
	}
/*
 * Get the data
 */
	if (! (dc = ds_FetchObs (pid, DCC_Scalar, &ptime, flist, 3, 
				 details, ndetail)))
	{
		msg_ELog (EF_PROBLEM, "Unable to get winds data for '%s'", 
			  pname);
		return;
	}
	if (ndetail)
		dc_SetBadval (dc, badvalue);
	else
		badvalue = dc_GetBadval (dc);
/*
 * Find the number of points in the observation and stash it in the pd
 */
	npts = dc_GetNSample (dc);

	if (nprev > npts)
	{
	/*
	 * We're starting a new sounding, so trigger a global update
	 */
		TriggerGlobal = TRUE;
		nprev = npts = 0;
	}
		
	pd_Store (Pd, c, "skPrivate-wind-npts", (char *) &npts, SYMT_INT);
/*
 * Get the data from the chunk, deriving u_wind and v_wind if necessary, 
 * and skipping points we've already plotted (if any)
 */
	npts -= nprev;

	pres = (float *) malloc (npts * sizeof (float));
	u = (float *) malloc (npts * sizeof (float));
	v = (float *) malloc (npts * sizeof (float));

        ipts = 0;
	for (i = 0; i < npts; i++)
	{
		if ((i + nprev) % skip)
			continue;

		pres[ipts] = dc_GetScalar (dc, i + nprev, flist[0]);

		if (have_uv)
		{
			u[ipts] = dc_GetScalar (dc, i + nprev, flist[1]);
			v[ipts] = dc_GetScalar (dc, i + nprev, flist[2]);
		}
		else
		{
			wspd = dc_GetScalar (dc, i + nprev, flist[1]);
			wdir = dc_GetScalar (dc, i + nprev, flist[2]);
			u[ipts] = wspd == badvalue || wdir == badvalue ?
				badvalue :
                                wspd * cos (DEG_TO_RAD (270.0 - wdir));
			v[ipts] = wspd == badvalue || wdir == badvalue ?
				badvalue :
                                wspd * sin (DEG_TO_RAD (270.0 - wdir));
		}
		ipts++;
	}
        npts = ipts;

	dc_DestroyDC (dc);
/*
 * Put a line in the overlay times widget
 */
	if (annot && ! update)
	{
		sprintf (string, "%-11.11s (winds) ", pname);
		TC_EncodeTime (&ptime, TC_Full, string + 20);
		strcat (string, "\n");
		ot_Append (string);
	}
/*
 * Calculate the x starting position for the wind vectors and the x and y 
 * scaling factors
 */
#	define WINDSX0 1.1	/* leave space from 1.0 to 1.1 for labels */
	xstart = WINDSX0 + 
		(Xhi - WINDSX0) * (float) (plot_ndx + 0.5) / (float) (nplots);
	xscale = (Xhi - 1.0) / (W_scale * 2 * nplots);
	yscale = fabs (xscale * 
		(YPIX (1.0) - YPIX (0.0)) / (XPIX (1.0) - XPIX (0.0)));
/*
 * Shaft length (in pixels) for barbs
 */
	shaftlen = (XPIX (1.0 + (Xhi - 1.0) / 6.0) - XPIX (1.0));
/*
 * Plot the winds
 */
	sk_Clip (Xlo, Ylo, Xhi, Yhi);

	for (i = 0; i < npts; i++)
	{
		if (pres[i] == badvalue || pres[i] < Pmin || pres[i] > Pmax)
			continue;
	/*
	 * Do vectors or barbs as requested
	 */
		if (Do_vectors)
		{
		/*
		 * Get the starting point
		 */
			xov[0] = xstart;
			yov[0] = YPOS (pres[i]);
		/*
		 * Convert to the overlay coordinates
		 */
			if (u[i] == badvalue || v[i] == badvalue)
				continue;
			
			xov[1] = xov[0] + u[i] * xscale;
			yov[1] = yov[0] + v[i] * yscale;
		/*
		 * Draw the wind line
		 */
			sk_Polyline (xov, yov, 2, L_solid, color);
		}
		else
		{
			double	wspd, wdir;

			if (u[i] == badvalue || v[i] == badvalue)
				continue;
		/*
		 * Hacked in barb drawing
		 */
			wspd = hypot (u[i], v[i]);
			wdir = ATAN2 (-v[i], -u[i]);
			XSetForeground (XtDisplay (Graphics), Gcontext, 
					color.pixel);
			draw_barb (XtDisplay (Graphics), GWFrame (Graphics), 
				   Gcontext, XPIX (xstart), 
				   YPIX (YPOS (pres[i])), wdir, wspd, shaftlen,
				   FALSE);
		}
	}
/*
 * Draw the staff
 */
	xov[0] = xstart;	xov[1] = xstart;
	yov[0] = 0.0;		yov[1] = 1.0;

	sk_Polyline (xov, yov, 2, L_solid, Colors[C_BG2]);
/*
 * Scaling annotation (draw for the first one only)
 */
	if (plot_ndx == 0 && ! update)
	{
		sk_DrawText ("WINDS PROFILE", 1.0 + 0.5 * (Xhi - 1.0), -0.01, 
			0.0, Tadefclr, 0.025, JustifyCenter, JustifyTop);
		if (Do_vectors)
		{
			sk_DrawText (" = 10 M/S", 1.0 + 0.5 * (Xhi - 1.0), 
				     -0.06, 0.0, Tadefclr, 0.02, JustifyLeft, 
				     JustifyCenter); 

			xov[0] = 1.0 + 0.5 * (Xhi - 1.0) - (10.0 * xscale);
			xov[1] = 1.0 + 0.5 * (Xhi - 1.0);
			yov[0] = -0.06;
			yov[1] = -0.06;
			sk_Polyline (xov, yov, 2, L_solid, Tadefclr);
		}
		else
		{
			sk_Clip (-9.0, -9.0, 9.0, 9.0);	/* Ugly kluge */
		/*
		 * Barb legend: 50 m/s, 10 m/s, 5 m/s
		 */
			XSetForeground (XtDisplay (Graphics), Gcontext,
					Tadefclr.pixel);

			sk_DrawText (" = 50 M/S", 1.0 + 0.5 * (Xhi - 1.0), 
				     -0.08, 0.0, Tadefclr, 0.02, JustifyLeft, 
				     JustifyCenter); 
			draw_barb (XtDisplay (Graphics), GWFrame (Graphics),
				   Gcontext, XPIX (1.0 + 0.5 * (Xhi - 1.0)),
				   YPIX (-0.08), 3.1416, 50.0, shaftlen, 
				   FALSE);
			
			sk_DrawText (" = 10 M/S", 1.0 + 0.5 * (Xhi - 1.0), 
				     -0.13, 0.0, Tadefclr, 0.02, JustifyLeft, 
				     JustifyCenter); 
			draw_barb (XtDisplay (Graphics), GWFrame (Graphics),
				   Gcontext, XPIX (1.0 + 0.5 * (Xhi - 1.0)),
				   YPIX (-0.13), 3.1416, 10.0, shaftlen, 
				   FALSE);
			
			sk_DrawText (" = 5 M/S", 1.0 + 0.5 * (Xhi - 1.0), 
				     -0.18, 0.0, Tadefclr, 0.02, JustifyLeft, 
				     JustifyCenter); 
			draw_barb (XtDisplay (Graphics), GWFrame (Graphics),
				   Gcontext, XPIX (1.0 + 0.5 * (Xhi - 1.0)),
				   YPIX (-0.18), 3.1416, 5.0, shaftlen, 
				   FALSE);
			
		}
	}
/*
 * Done
 */
	free (pres);
	free (u);
	free (v);

	return;
}




static void
sk_DrawText (text, x, y, rot, color_ndx, cheight, hjust, vjust)
char	*text;
float	x, y, rot, cheight;
XColor	color_ndx;
int	hjust, vjust;
/*
 * ENTRY:
 *	text	string to write
 *	x,y	where to write (the lower left corner of the skew-t box
 *		is at (0.0,0.0) and the upper right corner is at (1.0,1.0))
 *	rot	rotation in degrees counter-clockwise from horizontal
 *	color_ndx	index of the color to be used
 *	cheight	text height w.r.t. skew-t box height
 *	hjust	horizontal justification (see DrawText.h)
 *	vjust	vertical justification
 * EXIT:
 *	The string has been written
 */
{
	int	xpix, ypix;
	float	scale;
	Pixel	color;
/*
 * Find the pixel location
 */
	xpix = XPIX (x);
	ypix = YPIX (y);
/*
 * Set up a graphics context with the correct foreground color
 */
	color = color_ndx.pixel;
	XSetForeground (XtDisplay (Graphics), Gcontext, color);
/*
 * Draw the text
 */
	scale = cheight / (Yhi - Ylo);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, xpix, ypix, text, 
		rot, scale, hjust, vjust);
}




static void
sk_Clip (xlo, ylo, xhi, yhi)
float	xlo, ylo, xhi, yhi;
/*
 * Set the clipping window
 */
{
	XRectangle	r;
/*
 * Build the clip rectangle (XRectangle (x,y) is its upper left corner)
 */
	r.x = XPIX (xlo);
	r.y = YPIX (yhi);

	r.width = XPIX (xhi) - r.x + 1;
	r.height = YPIX (ylo) - r.y + 1;
/*
 * Put the clip rectangle into the GC
 */
	XSetClipRectangles (XtDisplay (Graphics), Gcontext, 0, 0, &r, 1, 
		Unsorted);
}




static void
sk_Polyline (x, y, npts, style, color_ndx)
float		*x, *y;
int		npts;
LineStyle	style;
XColor		color_ndx;
/*
 * ENTRY:
 *	x,y	location arrays (the lower left corner of the skew-t box
 *		is at (0.0,0.0) and the upper right corner is at (1.0,1.0))
 *	npts	the number of points
 *	style	line type (solid, dashed, or dotted)
 *	color_ndx	index of the color to use
 * EXIT:
 *	The polyline has been drawn
 */
{
	int	i, line_style;
	XPoint	*pts;
	char	dash[2];
	Pixel	color;

	if (npts == 0)
		return;
/*
 * Allocate the XPoint array
 */
	pts = (XPoint*) malloc (npts * sizeof (XPoint));
/*
 * Fill the pixel location arrays
 */
	for (i = 0; i < npts; i++)
	{
		pts[i].x = XPIX (x[i]);
		pts[i].y = YPIX (y[i]);
	}
/*
 * Set up the correct foreground color and line style
 */
	color = color_ndx.pixel;
	XSetForeground (XtDisplay (Graphics), Gcontext, color);

	switch (style)
	{
	    case L_solid:
		line_style = LineSolid;
		break;
	    case L_dashed:
		line_style = LineOnOffDash;
		dash[0] = 6;
		dash[1] = 6;
		break;
	    case L_dotted:
		line_style = LineOnOffDash;
		dash[0] = 2;
		dash[1] = 4;
		break;
	    default:
		msg_ELog (EF_PROBLEM, "Unknown line style %d in sk_Polyline\n",
			style);
		line_style = LineSolid;
	}

	XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, line_style, 
		CapButt, JoinMiter);
	if (line_style != LineSolid)
		XSetDashes (XtDisplay (Graphics), Gcontext, 0, dash, 2);
/*
 * Draw the line
 */
	XDrawLines (XtDisplay (Graphics), GWFrame (Graphics), Gcontext, pts, 
		npts, CoordModeOrigin);
/*
 * Make the GC use LineSolid again
 */
	if (line_style != LineSolid)
		XSetLineAttributes (XtDisplay (Graphics), Gcontext, 0, 
			LineSolid, CapButt, JoinMiter);
/*
 * Free the allocated points
 */
	free (pts);
}




static void
sk_Surface (t, p, dp, npts, t_sfc, p_sfc, dp_sfc, badvalue)
float	*t, *p, *dp, *t_sfc, *p_sfc, *dp_sfc, badvalue;
int	npts;
/*
 * Find the surface values.  If the MLI flag is set (i.e., we're using
 * the modified lifted index), the surface dewpoint returned is actually
 * the dewpoint corresponding to the surface pressure and the mean mixing
 * ratio for the lowest 50 mb of the sounding; the surface temperature
 * returned is the temperature corresponding to the surface pressure and
 * the mean potential temperature for the lowest 50 mb of the sounding.
 */
{
	int	i = 0, mr_count = 0, theta_count = 0;
	float	mr_sum = 0.0, mr, theta_sum = 0.0, theta, p_top;
/*
 * Sanity check
 */
	if (npts <= 0)
		ui_error ("%d points in sounding!", npts);
/*
 * Find the lowest point with good values in all three fields; this
 * will be our surface point.
 */
	while (t[i] == badvalue || p[i] == badvalue || dp[i] == badvalue)
		if (++i == npts)
			ui_error ("No surface point for analysis");

	*t_sfc = t[i];
	*p_sfc = p[i];
	*dp_sfc = dp[i];
# ifdef notdef
/*
 * If we're not using the modified lifted index, return now
 */
	if (! Flg_mli)
		return;
# endif
/*
 * Using MLI.  Average the mixing ratio and theta over the lowest 50 mb
 */
	p_top = *p_sfc - 50.0;

	for (; (p[i] > p_top || p[i] == badvalue) && i < npts; i++)
	{
	/*
	 * Don't try to use bad values
	 */
		if (p[i] == badvalue || dp[i] == badvalue || t[i] == badvalue)
			continue;
	/*
	 * Find the mixing ratio and theta and increment our sums
	 */
		if (dp[i] != badvalue)
		{
			mr_sum += w_sat (dp[i] + T_K, p[i]);
			mr_count++;
		}
		if (t[i] != badvalue)
		{
			theta_sum += theta_dry (t[i] + T_K, p[i]);
			theta_count++;
		}
	}
/*
 * Make sure we spanned 50 mb
 */
	if (i == npts)
		ui_error ("The sounding does not span 50 mb");

/*
 * Find the mean mixing ratio, then get the corresponding dewpoint.
 */
	mr = mr_sum / mr_count;
	*dp_sfc = t_mr (*p_sfc, mr) - T_K;
/*
 * Find the mean theta and the corresponding surface temperature
 */
	theta = theta_sum / theta_count;
	*t_sfc = theta_to_t (theta, *p_sfc) - T_K;
/*
 * Done
 */
	return;
}




static int
sk_700mb (t, p, dp, npts, p_sfc, dp_sfc, temp700, dp700, ndx700, badvalue)
float	*t, *p, *dp, p_sfc, dp_sfc, *temp700, *dp700, badvalue;
int	npts, *ndx700;
/*
 * Find the 700 mb temperature, the 700 mb dewpoint corresponding to the
 * surface mixing ratio, and the index of the last pressure > 700 mb in 
 * the data arrays.
 */
{
	int	i;
	float	t_prev, p_prev, w;
/*
 * Find the first pressure < 700.0
 */
	for (i = 0; i < npts; i++)
	{
		if (p[i] == badvalue || t[i] == badvalue)
			continue;

		if (p[i] < 700.0)
			break;

		p_prev = p[i];
		t_prev = t[i];
	}

	*ndx700 = i - 1;
/*
 * Make sure we have two good points to interpolate between
 */
	if (p_prev == badvalue || i == npts)
		return (FALSE);
/*
 * Interpolate the 700 mb temperature from the point we just found and the
 * previous good point
 */
	*temp700 = t_prev + (700.0 - p_prev) / (p[i] - p_prev) * 
		(t[i] - t_prev);
/*
 * Find the mixing ratio of our surface dewpoint, then find the
 * corresponding 700 mb dewpoint
 */
	w = w_sat (dp_sfc + T_K, p_sfc);
	*dp700 = t_mr (700.0, w) - T_K;
	return (TRUE);
}




# endif /* C_PT_SKEWT */
