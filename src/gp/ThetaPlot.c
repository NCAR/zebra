/*
 * Produce the morgan-style theta plot skew-t variant.
 */

/*		Copyright (C) 1987-97 by UCAR
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

# if C_PT_THETAPLOT


# include <math.h>
# include <X11/Intrinsic.h>

# include <defs.h>
# include <draw.h>
# include <pd.h>
# include <message.h>
# include <GraphicsW.h>
# include <DataStore.h>
# include <met_formulas.h>
# include "GraphProc.h"
# include "GC.h"
# include "PixelCoord.h"
# include "DrawText.h"
# include "Skewt.h"



RCSID ("$Id: ThetaPlot.c,v 2.6 2002-08-15 22:37:55 burghart Exp $")


/*
 * General definitions (from skewt)
 */
# define T_K	273.15
# define DEG_TO_RAD(x)	((x) * 0.017453292)
# define RAD_TO_DEG(x)	((x) * 57.29577951)
# ifndef PI
# define PI	3.141592654
# endif
#define EPSILON 622	/* from das */
/* # define BUFLEN	1024 */

# define MINUTES(v) ((int) (60*(v - (int) v) + 0.5))

/*
 * Implement the log scale on Y.
 */
# define YPOS(p, pres)	(log((pres)/p->tp_PMax) / log(p->tp_PMin/p->tp_PMax))
# define TPOS(p, theta) (((theta) - p->tp_ThMin)/(p->tp_ThMax - p->tp_ThMin))

/*
 * This is a container for the theta plot parameters.
 */
typedef struct _TPParams
{
	PlatformId 	tp_Plat;	/* Platform to plot */
	char		tp_CTable[40];	/* Color table to use */
	float		tp_PMin, tp_PMax; /* Min/max pressure values */
	float		tp_ThMin, tp_ThMax; /* Min/max theta values */
	float		tp_TeMin, tp_TeMax; /* Min/max theta values */
	XColor		*tp_Colors;	/* Color table		*/
	int		tp_NColor;	/* How many colors	*/
	int		tp_PStep;	/* Pressure step (grid)		*/
	int		tp_ThStep;	/* Theta step (grid)		*/
	int		tp_TeStep;	/* Temperature step (grid)	*/
	int		tp_TwPlot;	/* This is a theta-w plot	*/
	int		tp_AStep;	/* Adiabat theta step		*/
	int		tp_MaxDA;	/* Maximum # of dry adiabats	*/
	int		tp_SMRTop;	/* Top of mr lines		*/
	WindInfo	tp_WInfo;	/* Wind info cookie		*/
	float		tp_WScale;	/* Winds scale			*/
	int		tp_WBarb;	/* Plot winds as barbs		*/
	int		tp_WSkip;	/* Data skip			*/
} TPParams;


/*
 * Here's where we put the fields in the list.
 */
static const int FP_Pres = 0, FP_Tdry = 1, FP_Dp = 2, FP_Uw = 3, FP_Vw = 4;


/*
 * Forwards.
 */
static zbool TP_GetPlotParams (char *, TPParams *);
static void TP_DrawBox (TPParams *);
static float TP_Theta (const TPParams *, float, float, float);
static void TP_AltitudeScale (TPParams *p);
static void TP_DoPlot (TPParams *, DataChunk *, FieldId *);
static void TP_AddTrace (TPParams *, int, float *, float *, float *, float *,
		XColor);
static void TP_DoWinds (TPParams *p, DataChunk *dc, FieldId *fids);

static float das_vapor_pressure (float);
static float das_th_w (float);
static float das_th_e (float, float, float, float);
static float das_mr (float, float);
static float das_temp_from_p_and_mr (float press, float ws);






void
TP_ThetaPlot (c, update)
char *c;
zbool update;
/*
 * Do one of these here theta plots.
 */
{
	TPParams params;
	DataChunk *dc;
	ZebTime when;
	FieldId flist[5];
	char string[100];
/*
 * Get the various plotting parameters.
 */
	if (! TP_GetPlotParams (c, &params))
		return;
/*
 * Set up plotting and do our background box.
 */
	sk_InitPlotLimits ();
	TP_DrawBox (&params);
/*
 * See if there is any data to plot.
 */
	when = PlotTime;
	if (! ds_GetObsTimes (params.tp_Plat, &when, &when, 1, NULL))
	{
		msg_ELog (EF_INFO, "No data for %s",
				ds_PlatformName (params.tp_Plat));
		return;	/* and after all that work too */
	}
/*
 * Put together the field list, and do the fetch.
 */
	flist[FP_Pres] = F_Lookup ("pres");
	flist[FP_Tdry] = F_Lookup ("tdry");
	flist[FP_Dp] = F_Lookup ("dp");
	FindWindsFields (c, params.tp_Plat, &when, flist + FP_Uw,
			&params.tp_WInfo);
	if (! (dc = ds_FetchObs (params.tp_Plat, DCC_Scalar, &when, flist, 5,
			NULL, 0)))
	{
		msg_ELog (EF_PROBLEM, "Unable to get data for %s",
				ds_PlatformName (params.tp_Plat));
		return;
	}
/*
 * Make the overlay times widget happy.
 */
	ot_SetString ("PLATFORM            TIME\n");
	sprintf (string, "%-19.19s ", ds_PlatformName (params.tp_Plat));
	TC_EncodeTime (&when, TC_Full, string + 20);
	strcat (string, "\n");
	ot_Append (string);
/*
 * Do the plot.
 */
	TP_DoPlot (&params, dc, flist);
	dc_DestroyDC (dc);
	An_DoTopAnnot ("Theta plot (", params.tp_Colors[C_WHITE].pixel, 0, 0);
	An_DoTopAnnot (ds_PlatformName (params.tp_Plat),
			params.tp_Colors[C_WHITE].pixel, 0, 0);
	An_DoTopAnnot (").", params.tp_Colors[C_WHITE].pixel, 0, 0);
	
}






static zbool
TP_GetPlotParams (char *c, TPParams *params)
/*
 * Get the plot parameters from the PD.
 */
{
	char platform[CFG_PLATNAME_LEN], ctable[40], style[40];
/*
 * Get the platform.  Only one, for now.
 */
	if (! pda_ReqSearch (Pd, c, "platform", NULL, platform, SYMT_STRING))
		return (FALSE);
	if ((params->tp_Plat = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad thetaplot platform %s", platform);
		return (FALSE);
	}
/*
 * See what representation they asked for.
 */
	pd_Retrieve (Pd, c, "representation", platform, SYMT_STRING);
	params->tp_TwPlot = ! strcmp (platform, "thetaw");
/*
 * Color table.
 */
	if (! pda_ReqSearch (Pd, c, "color-table", "thetaplot", ctable,
			SYMT_STRING))
		return (FALSE);
	if (! ct_LoadTable (ctable, &params->tp_Colors, &params->tp_NColor))
	{
		msg_ELog (EF_PROBLEM, "Unable to load ctable %s", ctable);
		return (FALSE);
	}
/*
 * Steps for the background grid.
 */
	if (! pda_Search (Pd, c, "pres-step", "thetaplot",
			(char *) &params->tp_PStep, SYMT_INT))
		params->tp_PStep = 100;
	if (! pda_Search (Pd, c, "theta-step", "thetaplot",
			(char *) &params->tp_ThStep, SYMT_INT))
		params->tp_ThStep = 20;
	if (! pda_Search (Pd, c, "temp-step", "thetaplot",
			(char *) &params->tp_TeStep, SYMT_INT))
		params->tp_TeStep = 10;
	if (! pda_Search (Pd, c, "adiabat-step", "thetaplot",
			(char *) &params->tp_AStep, SYMT_INT))
		params->tp_AStep = 20;
	if (! pda_Search (Pd, c, "max-dry-adiabats", "thetaplot",
			(char *) &params->tp_MaxDA, SYMT_INT))
		params->tp_MaxDA = 6;
	if (! pda_Search (Pd, c, "smr-top", "thetaplot",
			(char *) &params->tp_SMRTop, SYMT_INT))
		params->tp_SMRTop = 400;
/*
 * Pressure limits.
 */
	if (! pda_Search (Pd, c, "pres-minval", "thetaplot",
			(char *) &params->tp_PMin, SYMT_FLOAT))
		params->tp_PMin = 100.0;
	if (! pda_Search (Pd, c, "pres-maxval", "thetaplot",
			(char *) &params->tp_PMax, SYMT_FLOAT))
		params->tp_PMax = 1000.0;
/*
 * Theta limits.
 */
	if (! pda_Search (Pd, c, "theta-minval", "thetaplot",
			(char *) &params->tp_ThMin, SYMT_FLOAT))
		params->tp_ThMin = 270;
	if (! pda_Search (Pd, c, "theta-maxval", "thetaplot",
			(char *) &params->tp_ThMax, SYMT_FLOAT))
		params->tp_ThMax = 470.0;
/*
 * Temperature limits.
 */
	if (! pda_Search (Pd, c, "temp-minval", "thetaplot",
			(char *) &params->tp_TeMin, SYMT_FLOAT))
		params->tp_TeMin = -70.0;
	if (! pda_Search (Pd, c, "temp-maxval", "thetaplot",
			(char *) &params->tp_TeMax, SYMT_FLOAT))
		params->tp_TeMax = 40.0;
/*
 * Winds info.
 */
	if (! pda_Search (Pd, c, "wind-scale", "thetaplot",
			(char *) &params->tp_WScale, SYMT_FLOAT))
		params->tp_WScale = 25.0;		/* old default */
	params->tp_WBarb = pda_Search (Pd, c, "wind-style", "thetaplot",
			style, SYMT_STRING) && ! strcmp (style, "barb");
	if (! pda_Search (Pd, c, "data-skip", "thetaplot",
			(char *) &params->tp_WSkip, SYMT_INT))
		params->tp_WSkip = 0;
	return (1);
}






static void
TP_DrawBox (TPParams *p)
/*
 * Draw the box containing the theta plot.  Abandon all hope if you enter
 * here, just remember that the original DAS source was really just
 * infinitely worse.
 */
{
	float x[128], y[128], ax, ay, th, otheta, theta;
	int pres, np, temp, pstep, dacount, m;
	char astring[80];
/*
 * This is the array of mixing ratio lines to use in the display.
 */
# define N_SMR 11
	static float smr[N_SMR] = {0.1,  1.0,  5.0,  10.0, 15.0, 20.0, 25.0,
				   30.0, 35.0, 40.0, 45.0};
/*
 * Draw the outside rectangle
 */
	x[0] = 0.0; y[0] = 0.0;
	x[1] = 1.0; y[1] = 0.0;
	x[2] = 1.0; y[2] = 1.0;
	x[3] = 0.0; y[3] = 1.0;
	x[4] = 0.0; y[4] = 0.0;
	sk_Polyline (x, y, 5, L_solid, 0, p->tp_Colors[C_BG1]);
/*
 * Let's get isobaric.
 */
	x[0] = 0;
	x[1] = 1;
	for (pres = p->tp_PMin; pres <= p->tp_PMax; pres += p->tp_PStep)
	{
	/*
	 * Draw the line.
	 */
		y[0] = y[1] = YPOS (p, pres);
		sk_Polyline (x, y, 2, L_solid, 0, p->tp_Colors[C_BG1]);
	/*
	 * Annotate.
	 */
		sprintf (astring, "%d", pres);
		sk_DrawText (astring, -0.01, y[0], 0.0,	p->tp_Colors[C_BG1],
				0.025, JustifyRight, JustifyCenter);
	}
/*
 * Put a pressure label there too.
 */
	sk_DrawText ("P", -0.045, YPOS (p, 150), 0.0, p->tp_Colors[C_BG1],
			0.025, JustifyCenter, JustifyCenter);
	sk_DrawText ("(mb)", -0.045, YPOS (p, 150) - 0.03, 0.0,
			p->tp_Colors[C_BG1], 0.025, JustifyCenter,
			JustifyCenter);
/*
 * Time to do the theta lines.
 */
	y[0] = 0;
	y[1] = 1;
	for (theta = p->tp_ThMin; theta <= p->tp_ThMax; theta += p->tp_ThStep)
	{
	/*
	 * Draw the line and annotate it.
	 */
		x[0] = x[1] = TPOS (p, theta);
		sk_Polyline (x, y, 2, L_solid, 0, p->tp_Colors[C_BG1]);
		sprintf (astring, "%d", (int) theta);
		sk_DrawText (astring, x[0], y[0] - 0.01, 0.0,
				p->tp_Colors[C_BG1], 0.025, JustifyCenter,
				JustifyTop);
	}
	sk_DrawText ("Equivalent Potential Temperature (K)", 0.5, -0.045,
			0.0, p->tp_Colors[C_BG1], 0.03, JustifyCenter,
			JustifyTop);
/*
 * OK, now we do the constant temperature lines.
 */
	pstep = p->tp_PStep/10;
	for (temp = p->tp_TeMax; temp >= p->tp_TeMin; temp -= p->tp_TeStep)
	{
		theta = TP_Theta (p, p->tp_PMax, temp, temp);
	/*
	 * Find the first pressure value that yields a point within the box.
	 */
		for (pres = p->tp_PMax; pres > p->tp_PMin; pres -= pstep)
			if (TP_Theta (p, pres, temp, temp) >= p->tp_ThMin)
				break;
		theta = TP_Theta (p, pres, temp, temp);
	/*
	 * Initialize the first point.
	 */
		np = 1;
		x[0] = TPOS (p, theta);
		y[0] = YPOS (p, pres);
	/*
	 * Now plow along the pressure axis.
	 */
		for ( ; pres > p->tp_PMin; pres -= pstep)
		{
		/*
		 * Get the theta limits.
		 */
			otheta = theta;
			theta = TP_Theta (p, pres - pstep, temp, temp);
		/*
		 * See if we cross the max theta axis here; if so, draw
		 * the remainder of the line and bail the loop.
		 */
			if (otheta < p->tp_ThMax && theta > p->tp_ThMax)
			{
				float y1 = pres -
					pstep*(p->tp_ThMax - otheta)/
					(theta - otheta);
				x[np] = TPOS (p, p->tp_ThMax);
				y[np] = YPOS (p, y1);
				np++;
				break;
			}
		/*
		 * If we out of the box don't do anything.
		 */
			if (theta < p->tp_ThMin || theta > p->tp_ThMax ||
					otheta < p->tp_ThMin ||
					otheta > p->tp_ThMax)
				continue;
		/*
		 * Figure out where to plot.
		 */
			x[np] = TPOS (p, theta);
			y[np] = YPOS (p, pres - pstep);
			np++;
		/* DAS CROPPING CODE IS HERE */
		}
	/*
	 * Put out the points and annotate the line.
	 */
		if (np < 2)
			continue;
		sk_Polyline (x, y, np, L_solid, 0, p->tp_Colors[C_BG2]);
		if (pres <= p->tp_PMin)		/* got all the way through */
		{
			ax = TPOS (p, theta) - 0.02;
			ay = YPOS (p, p->tp_PMin) + 0.02;
		}
		else /* Bailed */
		{
			ax = TPOS (p, p->tp_ThMax) + 0.01;
			ay = YPOS (p, pres - pstep);
		}
		sprintf (astring, "%d", temp);
		sk_DrawText (astring, ax, ay, 0.0, p->tp_Colors[C_BG2],
				0.025, JustifyLeft, JustifyCenter);
	}
/*
 * Temperature annotation.
 */
	sk_DrawText ("T (C)", 1.05, 0.82, 0.0,p->tp_Colors[C_BG2], 0.025,
			JustifyLeft, JustifyTop);
/*
 * To the sound of serious girding of loins, we now jump into the dry
 * adiabat lines.
 */
	dacount = 0;
	for (th = p->tp_ThMin; th <= p->tp_ThMax && dacount < p->tp_MaxDA;
	     th += p->tp_AStep)
	{
		float t, mr;
		np = 0;
		dacount++;
	/*
	 * Loop until we get within range.
	 */
		for (pres = p->tp_PMax; pres > p->tp_PMin; pres -= pstep)
		{
			t = theta_to_t (th, pres) - T_K;
			mr = das_mr (pres, t);
			if (mr < 200 && mr > 0 && t < 43)
				break;
		}
		theta = TP_Theta (p, pres, t, t);
	/*
	 * Now draw the line.
	 */
		for (; pres > p->tp_PMin; pres -= pstep)
		{
		/*
		 * Calculate initial parameters.
		 */
			t = theta_to_t (th, pres - pstep) - T_K;
			mr = das_mr (pres - pstep, t);
			if (mr > 200 || mr < 0 || t > 43)
				continue;	/* why again?  not sure */
			otheta = theta;
			theta = TP_Theta (p, pres - pstep, t, t);
		/*
		 * If we're crossing the boundary here do the chop.
		 */
			if (otheta > p->tp_ThMax && theta < p->tp_ThMax)
			{
				float y1 = pres -
					pstep*(p->tp_ThMax - otheta)/
					(theta - otheta);
				x[np] = TPOS (p, p->tp_ThMax);
				y[np] = YPOS (p, y1);
				np++;
			}
		/*
		 * Otherwise just accumulate points if we are within
		 * the box.
		 */
			else if (theta >= p->tp_ThMin && theta <= p->tp_ThMax
					&& otheta >= p->tp_ThMin 
					&& otheta <= p->tp_ThMax)
			{
				x[np] = TPOS (p, theta);
				y[np] = YPOS (p, pres);
				np++;
			}
		}
	/*
	 * OK, time to blast out the line.
	 */
		if (np > 1)
			sk_Polyline(x,y, np, L_dashed, 0, p->tp_Colors[C_BG3]);
	}
/*
 * And, as if that mess weren't enough, we throw on "saturation mixing
 * ratio lines" as well.  This is one ugly display.
 */
	for (m = 0; m < N_SMR; m++)
	{
		np = 0;
		for (pres = p->tp_PMax; pres >= p->tp_SMRTop; pres -= pstep)
		{
		/*
		 * Calculate starting values.
		 */
			float t = das_temp_from_p_and_mr (pres, smr[m]);
			otheta = theta;
			theta = TP_Theta (p, pres, t, t);
			if (pres == p->tp_PMax)
				continue;
		/*
		 * See if we're on the edge.
		 */
			if (otheta < p->tp_ThMax && theta > p->tp_ThMax)
			{
				float y1 = pres -
					pstep*(p->tp_ThMax - otheta)/
					(theta - otheta);
				x[np] = TPOS (p, p->tp_ThMax);
				y[np] = YPOS (p, y1);
				np++;
			}
		/*
		 * Maybe we're completely off the edge.
		 */
			if (otheta < p->tp_ThMin || otheta > p->tp_ThMax ||
					theta < p->tp_ThMin ||
					theta > p->tp_ThMax)
				continue;
		/*
		 * OK, add a point.
		 */
			x[np] = TPOS (p, theta);
			y[np] = YPOS (p, pres);
			np++;
		}
	/*
	 * Draw a line if we have one.
	 */
		if (np > 1)
		{
			sk_Polyline (x, y, np, L_dotted, 0, 
				     p->tp_Colors[C_BG4]);
			if (smr[m] < 5)
				sprintf (astring, "%.1f", smr[m]);
			else
				sprintf (astring, "%d", (int) smr[m]);
			sk_DrawText (astring, x[3], y[3], -45.0,
					p->tp_Colors[C_BG4], 0.025,
					JustifyCenter, JustifyBottom);
		}
	}
/*
 * Now the altitude scale.
 */
	TP_AltitudeScale (p);
}





static float
TP_Theta (const TPParams *p, float pres, float temp, float dp)
/*
 * Get the right theta value for this plot from the given parameters.
 */
{
	float mr = das_mr (pres, temp);
	float theta = das_th_e (pres, temp, temp, mr); /* DAS ignores dp! */
	return (p->tp_TwPlot ? das_th_w (theta) : theta);
}





static void
TP_AltitudeScale (TPParams *p)
/*
 * Draw the altitude scale.  This is carved out of skewt.
 */
{
/*
 * ICAO standard atmosphere pressures for altitudes from 0 km to 20 km
 * every 1 km
 */
	static float	alt_pres[] = {1013.2, 898.8, 795.0, 701.2, 616.5,
				540.3, 471.9, 410.7, 356.1, 307.6, 264.5,
				226.3, 193.3, 165.1, 141.0, 120.4, 102.9,
			        87.86, 75.05, 64.10, 54.75};
	float x[2], y[2];
	int i;
	char string[20];
/*
 * Set up.
 */
/*	sk_Clip (Xlo, 0.0, 0.0, 1.0); */

	x[0] = x[1] = Xlo / 2.0;
	y[0] = 0.0;
	y[1] = 1.0;
	sk_Polyline (x, y, 2, L_solid, 0, p->tp_Colors[C_WHITE]);

	x[0] = Xlo / 2.0;
	x[1] = x[0] - 0.01;
	for (i = 0; alt_pres[i] > p->tp_PMin; i++)
	{
	/*
	 * Tick mark
	 */
		y[0] = y[1] = YPOS (p, alt_pres[i]);
		sk_Polyline (x, y, 2, L_solid, 0, p->tp_Colors[C_WHITE]);
	/*
	 * Label
	 */
# ifdef notdef
		if (DoFeet)
			sprintf (string, "%.1f ", i*1000.0/0.30480);
		else
# endif
		sprintf (string, "%d ", i);
		sk_DrawText (string, x[1], y[1], 0.0, p->tp_Colors[C_WHITE],
				0.025, JustifyRight, JustifyCenter);
	}
}





static void
TP_DoPlot (TPParams *p, DataChunk *dc, FieldId *flist)
/*
 * Plot the actual data traces.
 */
{
	float *theta, *thetae, *thetaed, *x, *y, *pr, badval, mr;
	int np, nsamp, samp, nuse;
/*
 * Figure out how big the data chunk is, and allocate one big pile of arrays.
 */
	nsamp = dc_GetNSample (dc);
	theta = (float *) malloc (6*nsamp*sizeof (float));
	thetae = theta + nsamp;
	thetaed = thetae + nsamp;
	x = thetaed + nsamp;
	y = x + nsamp;
	pr = y + nsamp;
/*
 * Calculate the various types of theta.
 */
	nuse = 0;
	badval = dc_GetBadval (dc);
	for (samp = 0; samp < nsamp; samp++)
	{
	/*
	 * Pull out the basic parameters and make sure they are good.
	 */
		float pres = dc_GetScalar (dc, samp, flist[FP_Pres]);
		float temp = dc_GetScalar (dc, samp, flist[FP_Tdry]);
		float dp = dc_GetScalar (dc, samp, flist[FP_Dp]);
		if (pres == badval || temp == badval || dp == badval)
			continue;
		pr[nuse] = pres;
	/*
	 * Calculate the fancy stuff.
	 */
		mr = das_mr (pres, temp);
		theta[nuse] = das_th_e (pres, temp, temp, mr);
		mr = das_mr (pres, dp);
		thetae[nuse] = das_th_e (pres, temp, dp, mr);
		thetaed[nuse] = das_th_e (pres, dp, dp, mr);
		nuse++;
	}
/*
 * Do the plots.
 */
	sk_Clip (0, 0, 1, 1);
	TP_AddTrace (p, nuse, pr, theta, x, y, p->tp_Colors[C_DATA(0)]);
	TP_AddTrace (p, nuse, pr, thetae, x, y, p->tp_Colors[C_DATA(1)]);
	TP_AddTrace (p, nuse, pr, thetaed, x, y, p->tp_Colors[C_DATA(2)]);
/*
 * Do the winds plot to.
 */
	TP_DoWinds (p, dc, flist);
/*
 * Clean up and go home.
 */
	free (theta);
}





static void
TP_AddTrace (TPParams *p, int np, float *pres, float *theta, float *x,
		float *y, XColor color)
/*
 * Draw a trace on the screen.
 */
{
	int i;

	for (i = 0; i < np; i++)
	{
		x[i] = TPOS (p, theta[i]);
		y[i] = YPOS (p, pres[i]);
	}
	sk_Polyline (x, y, np, L_solid, 2, color);
}






static void
TP_DoWinds (TPParams *p, DataChunk *dc, FieldId *fids)
/*
 * Put up the winds plots.  This is stolen and brutally hacked from
 * skewt; I couldn't use it in place 'cuz it depends on too many globals
 * from over there.
 */
{
	float badval = dc_GetBadval (dc), xstart, xscale, yscale;
	float x[2], y[2];
	int npt = dc_GetNSample (dc), shaftlen, pt;
	XColor color = p->tp_Colors[C_DATA(3)];
	XColor acolor = p->tp_Colors[C_WHITE];
/*
 * Calculate the x starting position for the wind vectors and the x and y 
 * scaling factors
 */
#	define WINDSX0 1.1	/* leave space from 1.0 to 1.1 for labels */
	xstart = WINDSX0 + (Xhi - WINDSX0)/2.0;
	xscale = (Xhi - 1.0) / (p->tp_WScale * 2);
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
	for (pt = 0; pt < npt; pt += (p->tp_WSkip > 0 ? p->tp_WSkip : 1))
	{
		float pres, u, v;
	/*
	 * No good pressure?  We can't plot anything.
	 */
		pres = dc_GetScalar (dc, pt, fids[FP_Pres]);
		if (pres == badval || pres < p->tp_PMin || pres > p->tp_PMax)
			continue;
	/*
	 * Pull out the winds.
	 */
		u = dc_GetScalar (dc, pt, fids[FP_Uw]);
		v = dc_GetScalar (dc, pt, fids[FP_Vw]);
		GetWindData (&p->tp_WInfo, &u, &v, badval);
		if (u == badval || v == badval)
			continue;
	/*
	 * Did they want barbs?
	 */
		if (p->tp_WBarb)
		{
			float wspd, wdir;
			wspd = hypot (u, v);
			wdir = ATAN2 (-v, -u);
			XSetForeground (XtDisplay (Graphics), Gcontext, 
					color.pixel);
			draw_barb (XtDisplay (Graphics), GWFrame (Graphics), 
				   Gcontext, XPIX (xstart), 
				   YPIX (YPOS (p, pres)), wdir, wspd, shaftlen,
				   FALSE);			
		}
	/*
	 * OK, give them vectors.
	 */
		else
		{
			x[0] = xstart;
			y[0] = YPOS (p, pres);
			x[1] = x[0] + u*xscale;
			y[1] = y[0] + v*yscale;
			sk_Polyline (x, y, 2, L_solid, 0, color);
		}
	}
/*
 * OK, add the staff.
 */
	x[0] = xstart;	x[1] = xstart;
	y[0] = 0.0;	y[1] = 1.0;
	sk_Polyline (x, y, 2, L_solid, 0, p->tp_Colors[C_BG2]);
/*
 * Annotation.
 */
	sk_DrawText ("WINDS PROFILE", 1.0 + 0.5 * (Xhi - 1.0), -0.01, 
			0.0, acolor, 0.025, JustifyCenter, JustifyTop);
	if (! p->tp_WBarb)
	{
		sk_DrawText (" = 10 M/S", 1 + (Xhi - 1.0)/2, -0.06, 0.0,
				acolor, 0.02, JustifyLeft, JustifyCenter); 
		x[0] = 1 + (Xhi - 1.0)/2 - (10.0 * xscale);
		x[1] = 1 + (Xhi - 1.0)/2;
		y[0] = -0.06;
		y[1] = -0.06;
		sk_Polyline (x, y, 2, L_solid, 0, acolor);
	}
/*
 * Barb annotation.
 */
	else
	{
		sk_Clip (-9.0, -9.0, 9.0, 9.0);	/* Ugly kludge */
	/*
	 * Barb legend: 50 m/s, 10 m/s, 5 m/s
	 */
		XSetForeground (XtDisplay (Graphics), Gcontext, acolor.pixel);
		sk_DrawText (" = 50 M/S", 1 + (Xhi - 1.0)/2, -0.08, 0.0,
				acolor, 0.02, JustifyLeft, JustifyCenter); 
		draw_barb (XtDisplay (Graphics), GWFrame (Graphics),
				Gcontext, XPIX (1.0 + 0.5 * (Xhi - 1.0)),
				YPIX (-0.08), 3.1416, 50.0, shaftlen, FALSE);
			
		sk_DrawText (" = 10 M/S", 1 + (Xhi - 1.0)/2, -0.13, 0.0,
				acolor, 0.02, JustifyLeft, JustifyCenter); 
		draw_barb (XtDisplay (Graphics), GWFrame (Graphics),
				Gcontext, XPIX (1 + (Xhi - 1.0)/2),
				YPIX (-0.13), 3.1416, 10.0, shaftlen, FALSE);
	
		sk_DrawText (" = 5 M/S", 1 + (Xhi - 1.0)/2, -0.18, 0.0,
				acolor, 0.02, JustifyLeft, JustifyCenter); 
		draw_barb (XtDisplay (Graphics), GWFrame (Graphics),
				Gcontext, XPIX (1.0 + 0.5 * (Xhi - 1.0)),
				YPIX (-0.18), 3.1416, 5.0, shaftlen, FALSE);
	}	
}





/*
 * Some derivations stolen from the DAS code.
 */


static float
das_mr (float press, float tempc)
/*
 * Calculate mixing ratio, their way.
 */
{
	float vapor_p;
	if(tempc > -50.0)
	{
		vapor_p = 6.112*exp (17.67*tempc/(tempc + 243.5));
		return ((622.0*vapor_p)/(press - vapor_p));
	}
	else
		return (0.0000001);
}
	



static float
das_th_e (float press, float tempc, float dewc, float mr)
/*
 * Theta E calculation.
 */
{
	float term2, tempk, dewk, theta_d, tlcl;

	tempk = tempc + T_K;
	dewk = dewc + T_K;
/* COMPUTE POTENTIAL TEMPERATURE (FIRST TERM IN EQUATION) */
	theta_d = tempk*pow (1000.0/press, (0.2854*(1.0 - 0.28E-3*mr)));
/* COMPUTE TEMPERATURE IN K AT LCL */
	tlcl = 1.0/(1.0/(dewk - 56.0) + (log(tempk/dewk)/800.0)) + 56.0;
/* COMPUTE SECOND TERM IN EQUATION */
	term2 = exp ((3.376/tlcl - 0.00254)*mr*(1.0 + 0.81E-3*mr));
/* COMPUTE THETA-E */
	return (theta_d*term2);
}





static float
das_th_w (float the)
/*
 * Theta W from Theta E
 */
{
	float es, d, p, a, tq, tq1, x, w;
	int i;

	a = the;
	tq = 253.16;
	tq1 = tq - T_K;
	d = 120.;
	p = 1000.;
	for (i = 0; i < 12; i++)
	{
		d /= 2.;
		es = das_vapor_pressure (tq1);
		w = (622.*es)/(p - es);
	/*	x = a*exp(-2.6518986*w/tq) - tq*(pow ((1000./p), 0.286)); */
		x = a*exp (-2.6518986*w/tq) - tq;
		if (fabs (x) < 0.01)
			return (tq);
		tq = tq + ((x >= 0) ? d : -d);
		tq1 = tq - T_K;
	}
	return (tq);
}




static float
das_vapor_pressure (float tempc)
/*
 * Get vapor pressure.
 */
{
	return ((tempc > -50) ?
			(6.112*exp ((17.67*tempc)/(tempc+243.5))) : 0.0000001);
}




static float
das_temp_from_p_and_mr (float press, float ws)
{
	float term_a, term_b;
	term_a = log ((ws*press)/(6.112*EPSILON));
	term_b = (1.0/((1.0/term_a) - (1.0/17.67)));
	return ((243.5/17.67)*term_b);
}


# endif /* C_PT_THETAPLOT */
