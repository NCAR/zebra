/*
 * Skew-t plotting module
 */
# include <math.h>
# include <X11/Intrinsic.h>
# include <ui.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <DataStore.h>
# include "derive.h"
# include "GraphProc.h"
# include "GC.h"
# include "PixelCoord.h"
# include "DrawText.h"

/*
 * General definitions
 */
# define T_K	273.15
# define DEG_TO_RAD(x)	((x) * 0.017453292)
# define RAD_TO_DEG(x)	((x) * 57.29577951)
# define BADVAL	-999.0
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
 * Forward declarations
 */
void	sk_Skewt (), sk_Background (), sk_Lift (), sk_Thermo (), sk_Winds ();
void	sk_Polyline (), sk_DrawText (), sk_Clip (), sk_Surface ();
int	sk_700mb ();

/*
 * Line style
 */
typedef enum {L_solid, L_dashed, L_dotted} LineStyle;

/*
 * Color array and indices
 */
static XColor	*Colors;
static int	Ncolors;
static int 	Tacmatch = TRUE;
static XColor 	Tadefclr;

# define C_BLACK	0
# define C_WHITE	1
# define C_BG1		2
# define C_BG2		3
# define C_BG3		4
# define C_BG4		5
# define C_DATA(i)	(6 + (i))




void
sk_Skewt (c, update)
char	*c;
bool	update;
/*
 * Draw a skew-t, log p plot based on the given PD component.
 */
{
	bool	ok;
	int	status, i, npts, plat, nplat;
	char	platforms[80], annot[80], ctname[20], tadefcolor[30];
	time	ptime;
	char	*pnames[5];
	char	*flist[] = { "pres", "tdry", "dp", "u_wind", "v_wind" };
	PlatformId	pid;
	DataObject	*dobj = NULL;
	float		*pres, *temp, *dp, *u_wind, *v_wind;
/*
 * Get the platform and color table
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
		lw_OvInit ("PLATFORM    TIME\n");
	}
/*
 * Loop through the platforms
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Add this platform to the annotation
	 */
		if (! update)
		{
			if (plat > 0)
				An_TopAnnot (", ", Tadefclr.pixel);

			if (Tacmatch)
				An_TopAnnot (pnames[plat], 
					Colors[C_DATA(plat)].pixel);
			else
				An_TopAnnot (pnames[plat], 
					Tadefclr.pixel);
		}
	/*
	 * Get the data
	 */
		pid = ds_LookupPlatform (pnames[plat]);
		if (pid == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Bad platform '%s'", 
				pnames[plat]);
			continue;
		}

		ptime = PlotTime;
		if (! ds_DataTimes (pid, &ptime, 1, DsBefore, &ptime))
		{
			msg_ELog (EF_PROBLEM, "No data for '%s' at %d %d",
				pnames[plat], ptime.ds_yymmdd, 
				ptime.ds_hhmmss);
			continue;
		}
		dobj = ds_GetObservation (pid, flist, 5, &ptime, OrgScalar,
			0.0, BADVAL);
		if (! dobj)
		{
			msg_ELog (EF_PROBLEM, 
				"Unable to get data for '%s' at %d %06d", 
				pnames[plat], PlotTime.ds_yymmdd, 
				PlotTime.ds_hhmmss);
			continue;
		}

		pres = dobj->do_data[0];
		temp = dobj->do_data[1];
		dp = dobj->do_data[2];
		u_wind = dobj->do_data[3];
		v_wind = dobj->do_data[4];

		npts = dobj->do_npoint;
	/*
	 * Plot the thermo data and then the winds data
	 */
		sk_Thermo (pres, temp, dp, npts, plat);
		sk_Winds (pres, u_wind, v_wind, npts, plat, nplat);
	/*
	 * Fill in the time info.
	 */
		sprintf (annot, "%-12s%2d:%02d\n", pnames[plat],
			dobj->do_begin.ds_hhmmss/10000,
			(dobj->do_begin.ds_hhmmss/100) % 100);
		lw_OvAddString (annot);
	/*
	 * Free the data object
	 */
		ds_FreeDataObject (dobj);
	}
/*
 * Add a period to the top annotation
 */
	An_TopAnnot (".  ", Tadefclr.pixel);
/*
 * Unclip since we want to return the shared GC in clean condition
 */
	sk_Clip (Xlo, Ylo, Xhi, Yhi);
}




void
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
			12.0, 20.0, 0.0};
/*
 * ICAO standard atmosphere pressures for altitudes from 0 km to 16 km
 * every 1 km
 */
	static float	alt_pres[] = {1013.2, 898.8, 795.0, 701.2, 616.5,
				540.3, 471.9, 410.7, 356.1, 307.6, 264.5,
				226.3, 193.3, 165.1, 141.0, 120.4, 102.9};
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
		p -= ((int) p % Pstep) ? ((int) p % Pstep) : Pstep)
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
	for (i = 0; i <= 16; i++)
	{
	/*
	 * Tick mark
	 */
		y[0] = y[1] = YPOS (alt_pres[i]);
		sk_Polyline (x, y, 2, L_solid, Colors[C_BG2]);
	/*
	 * Label
	 */
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
	for (t = 0; t <= 32; t += 4)
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
				RAD_TO_DEG (atan2 (y[1] - y[0], x[1] - x[0]));
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

		sprintf (string, "%d", (int) pt);

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




void
sk_Lift (ndata, pres, temp, dp)
int	ndata;
float	*pres, *temp, *dp;
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
	sk_Surface (temp, pres, dp, ndata, &t_sfc, &p_sfc, &dp_sfc);
	sk_700mb (temp, pres, dp, ndata, p_sfc, dp_sfc, &t_700, &dp_700, 
		&ndx_700);

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




void
sk_Thermo (p, t, d, npts, plot_ndx)
float	*p, *t, *d;
int	npts, plot_ndx;
/*
 * Plot the thermo data for the given sounding
 */
{
	float	*xt, *xd, *yt, *yd;
	float	y;
	int	i, good_d = 0, good_t = 0;
	DataObject	dobjs[3];
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
		if (p[i] == BADVAL)
			continue;
		else
		{
			y = YPOS (p[i]);
			yt[good_t] = y;
			yd[good_d] = y;
		}

		if (t[i] != BADVAL)
			xt[good_t++] = XPOS (t[i], y);

		if (d[i] != BADVAL)
			xd[good_d++] = XPOS (d[i], y);
	}
/*
 * Draw the lines
 */
	sk_Polyline (xt, yt, good_t, L_solid, Colors[C_DATA (plot_ndx)]);
	sk_Polyline (xd, yd, good_d, L_solid, Colors[C_DATA (plot_ndx)]);
/*
 * Draw the lifted parcel lines
 */
	sk_Lift (npts, p, t, d);
/*
 * Free the allocated space and return
 */
	free (xt);
	free (xd);
	free (yt);
	free (yd);

	return;
}




void
sk_Winds (p, u, v, npts, plot_ndx, nplots)
float	*p, *u, *v;
int	npts;
int	plot_ndx, nplots;
/*
 * Plot the winds for the given sounding
 */
{
	float	xstart, xscale, yscale, xov[2], yov[2], w_aspect;
	int	i;
/*
 * Calculate the x starting position and the x and y scaling factors
 */
	xstart = 1.0 + 
		(Xhi - 1.0) * (float) (plot_ndx + 0.5) / (float) (nplots);
	xscale = (Xhi - 1.0) / (W_scale * 2 * nplots);

	w_aspect = (Yhi - Ylo) / (Xhi - 1.0) * GWWidth (Graphics) /
		GWHeight (Graphics);
	yscale = w_aspect * xscale;
/*
 * Plot the winds
 */
	sk_Clip (Xlo, Ylo, Xhi, Yhi);

	for (i = 0; i < npts; i++)
	{
		if (p[i] == BADVAL || p[i] < Pmin || p[i] > Pmax)
			continue;
	/*
	 * Get the starting point
	 */
		xov[0] = xstart;
		yov[0] = YPOS (p[i]);
	/*
	 * Convert to the overlay coordinates
	 */
		if (u[i] != BADVAL)
			xov[1] = xov[0] + u[i] * xscale;
		else
			continue;

		if (v[i] != BADVAL)
			yov[1] = yov[0] + v[i] * yscale;
		else
			continue;
	/*
	 * Draw the wind line
	 */
		sk_Polyline (xov, yov, 2, L_solid, Colors[C_DATA (plot_ndx)]);
	}
/*
 * Draw the staff
 */
	xov[0] = xstart;	xov[1] = xstart;
	yov[0] = 0.0;		yov[1] = 1.0;

	sk_Polyline (xov, yov, 2, L_solid, Colors[C_BG2]);
/*
 * Annotate on the first one
 */
	if (plot_ndx == 0)
	{
		sk_DrawText ("WINDS PROFILE", 1.0 + 0.5 * (Xhi - 1.0), -0.01, 
			0.0, Tadefclr, 0.025, JustifyCenter, JustifyTop);
		sk_DrawText (" = 10 M/S", 1.0 + 0.5 * (Xhi - 1.0), -0.06, 0.0, 
			Tadefclr, 0.02, JustifyLeft, JustifyCenter); 

		xov[0] = 1.0 + 0.5 * (Xhi - 1.0) - (10.0 * xscale);
		xov[1] = 1.0 + 0.5 * (Xhi - 1.0);
		yov[0] = -0.06;
		yov[1] = -0.06;
		sk_Polyline (xov, yov, 2, L_solid, Tadefclr);
	}
/*
 * Done
 */
	return;
}




void
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




void
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




void
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




void
sk_Surface (t, p, dp, npts, t_sfc, p_sfc, dp_sfc)
float	*t, *p, *dp, *t_sfc, *p_sfc, *dp_sfc;
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
 * Find the lowest point with good values in all three fields; this
 * will be our surface point.
 */
	while (t[i] == BADVAL || p[i] == BADVAL || dp[i] == BADVAL)
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

	for (; (p[i] > p_top || p[i] == BADVAL) && i < npts; i++)
	{
	/*
	 * Don't try to use bad values
	 */
		if (p[i] == BADVAL || dp[i] == BADVAL || t[i] == BADVAL)
			continue;
	/*
	 * Find the mixing ratio and theta and increment our sums
	 */
		if (dp[i] != BADVAL)
		{
			mr_sum += w_sat (dp[i] + T_K, p[i]);
			mr_count++;
		}
		if (t[i] != BADVAL)
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




int
sk_700mb (t, p, dp, npts, p_sfc, dp_sfc, temp700, dp700, ndx700)
float	*t, *p, *dp, p_sfc, dp_sfc, *temp700, *dp700;
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
		if (p[i] == BADVAL || t[i] == BADVAL)
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
	if (p_prev == BADVAL || i == npts)
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
