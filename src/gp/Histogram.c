/*
 * The Histogram code, what fun.
 */
/*		Copyright (C) 1995 by UCAR
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
# include <X11/Intrinsic.h>
# include <config.h>
# include <defs.h>
# include <message.h>
# include <pd.h>
# include <DataStore.h>
# include <GraphicsW.h>
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"

MAKE_RCSID ("$Id: Histogram.c,v 2.10 2001-06-19 23:48:30 granger Exp $")

# if C_PT_HISTOGRAM

/*
 * A couple of globals set up so that each histogram component knows what
 * to do when plot time comes up.
 */
static int NComponents;		/* How many components we have	*/
static int NPlotted;		/* How many plotted so far	*/
static int PlotHeight;		/* How high each can be		*/

/*
 * How much space for plotting?  These numbers should not be wired in
 * quite this way, but the LayoutControl stuff is nuts and I don't want
 * to try to deal with it now.  The TOPBORDER assumes minimal top annotation.
 */
# define TOPBORDER 25
# define BOTTOMBORDER 50	/* Just above the icons 		*/
# define MIDDLEBORDER 5		/* Space between plots. 		*/
# define BAR_ANNOT_SPACE 15	/* Annotation below bars 		*/
# define COUNT_ANNOT_SPACE 30	/* Space for counts, above 		*/
# define MIN_LINE_SPACE	30	/* Min pixels between grid lines 	*/
# define BAR_LEFT 50		/* Where the first bar starts		*/
/*
 * Limits, since config.h doesn't currently seem to have equivalents.
 */
# define HG_MAX_PLATFORMS	10
# define HG_MAX_FIELDS		10
# define HG_MAX_BINS		32

/*
 * Forwards go here.
 */
static int HG_BCParams FP ((char *, PlatformId *, int *, FieldId *, int *,
		float *, int *, zbool *, int *, XColor **, int *, zbool *,
		zbool *, zbool *, int *, int *, float *, float *));
static DataChunk *HG_GetBCData FP ((PlatformId, FieldId *, int, int));
static void HG_FillBins FP ((DataChunk *, FieldId, float *, int, int *,int *));
static void HG_GetLimits FP ((int **, int, int, int, int *, int, int *,int *));
static void HG_DrawBar FP ((int, int, int, int, int, int, int, Pixel, int,
		double));
static void HG_SideAnnot FP ((char *, int *, int, PlatformId, FieldId *, int,
		XColor *, int, int, double));
static void HG_ChartLimits FP ((int *, int *));
static void HG_BinAnnot FP ((float *, int, int, double, int, int, int));
static void HG_DividerBar FP ((char *));
static int HG_FigureInterval FP ((int, int, int));
static void HG_CountGrid FP ((char *, int, int, int, int));
static void HG_NextPlot FP ((char *));
static void HG_SetOT (char *c, DataChunk *dc);






void
HG_Init (t)
ZebTime *t;
/*
 * Set things up.
 */
{
	char **comps = pd_CompList (Pd);
	int comp;
	zbool disabled;
/*
 * Count the components, but not the disabled ones.
 */
	NComponents = 0;
	for (comp = 1; comps[comp]; comp++)
		if (! pd_Retrieve (Pd, comps[comp], "disable",
				(char *)&disabled, SYMT_BOOL) || ! disabled)
			NComponents++;
/*
 * You never know what they might do.
 */
	if (NComponents <= 0)
	{
		msg_ELog (EF_INFO, "No components in histogram plot?!?");
		return;
	}
/*
 * Figure available height for each plot.
 */
	PlotHeight = GWHeight (Graphics) - (TOPBORDER + BOTTOMBORDER);
	PlotHeight -= (NComponents - 1)*MIDDLEBORDER;
	PlotHeight /= NComponents;
/*
 * There's not much useful we can put into the top annotation, but there
 * might as well be something there...
 */
	An_TopAnnot ("Histogram");
/*
 * That's about it.
 */
	ot_SetString ("Component     Platform   NPoints Time period\n");
	NPlotted = 0;
}




static void
HG_NextPlot (c)
char *c;
/*
 * Time for the next histogram plot.
 */
{
	if (++NPlotted > 1)
		HG_DividerBar (c);
}





void
HG_CountBarChart (c, update)
char *c;
int update;
/*
 * Do our basic, barchart display.
 */
{
	PlatformId pids[HG_MAX_PLATFORMS];
	FieldId fids[HG_MAX_FIELDS];
	float bins[HG_MAX_BINS], sscale, bscale;
	int nplat, nfield, nbin, tperiod, ncolor, cmin, cmax, bwidth, ngroup;
	int plat, *badcounts, field, barpos, y0, y1, bbase, maxbh, nok = 0;
	int **counts, bin, pbegin, binwidth, sapos, junk, lheight;
	zbool bvbin, anncounts, autoscale, zanchor;
	XColor *ctable;
/*
 * Onward.
 */
	HG_NextPlot (c);
/*
 * Grab our info.
 */
	if (! HG_BCParams (c, pids, &nplat, fids, &nfield, bins, &nbin,
			&bvbin, &tperiod, &ctable, &ncolor, &anncounts,
			&autoscale, &zanchor, &cmin, &cmax, &sscale, &bscale))
		return;
/*
 * Figure out what our bar width will be.
 */
	ngroup = nbin + (bvbin ? 2 : 1);
	bwidth = (GWWidth (Graphics)*(F_X1) - BAR_LEFT)/
		(nplat*nfield*ngroup + ngroup - 1);
	binwidth = (nplat*nfield + 1)*bwidth;
	pbegin = BAR_LEFT;
/*
 * Allocate the bin table.
 */
	counts = (int **) malloc (nplat*nfield*sizeof (int *));
	memset (counts, 0, nplat*nfield*sizeof (int *));
	badcounts = (int *) malloc (nplat*nfield*sizeof (int));
	memset (badcounts, 0, nplat*nfield*sizeof (int));
/*
 * Now we work our way through the platforms.
 */
	for (plat = 0; plat < nplat; plat++)
	{
		DataChunk *dc;
	/*
	 * Get the data.
	 */
		if (! (dc = HG_GetBCData (pids[plat], fids, nfield, tperiod)))
			continue;
		nok++;		/* We have something */
	/*
	 * Now go through each field.
	 */
		for (field = 0; field < nfield; field++)
		{
			int offset = plat*nfield + field;
		/*
		 * Fill up the bins.
		 */
			counts[offset]=(int *) malloc((nbin + 1)*sizeof(int));
			memset (counts[offset], 0, (nbin + 1)*sizeof (int));
			HG_FillBins (dc, fids[field], bins, nbin,
					counts[offset], badcounts + offset);
		}
	/*
	 * Fix up the overlay times and get rid of the DC.
	 */
		HG_SetOT (c, dc);
		dc_DestroyDC (dc);
	}
/*
 * Did we maybe not get any data at all?  If so, to hell with the rest of
 * that stuff.
 */
	if (! nok)
		goto cleanup;
/*
 * Now we can figure out our plot limits, if need be.  Save them for the
 * adjuster widget.
 */
	if (autoscale)
	{
		HG_GetLimits (counts, nplat, nfield, nbin, badcounts, zanchor,
				&cmin, &cmax);
		pd_Store (Pd, c, "count-min", (char *) &cmin, SYMT_INT);
		pd_Store (Pd, c, "count-max", (char *) &cmax, SYMT_INT);
	}
/*
 * Figure out the bar limits.
 */
	HG_ChartLimits (&y0, &y1);
	bbase = y1 - BAR_ANNOT_SPACE;
	maxbh = bbase - y0 - 2;
	if (anncounts)
		maxbh -= COUNT_ANNOT_SPACE;
/*
 * Count annotation and grid.
 */
	HG_CountGrid (c, cmin, cmax, bbase, maxbh);
/*
 * Side annotation stuff.
 */
	sapos = y0 + BAR_ANNOT_SPACE;
	DT_TextBox (Graphics, GWFrame (Graphics), 0, 0, "Fungus", 0.0,
			sscale, JustifyLeft, JustifyBottom, &junk, &junk,
			&junk, &lheight);
	lheight *= -1;	/* grumble */
/*
 * Plotting time.
 */
	for (plat = 0; plat < nplat; plat++)
	{
	/*
	 * Plot up the bars.
	 */
		for (field = 0; field < nfield; field++)
		{
			int off = plat*nfield + field;
			if (counts[off])
			{
				for (bin = 0; bin <= nbin; bin++)
					HG_DrawBar (pbegin + bin*binwidth +
						off*bwidth, bbase, maxbh,
						bwidth, counts[off][bin],
						cmin, cmax,
						ctable[off % ncolor].pixel,
						anncounts, bscale);
				free (counts[off]);
			}
			if (bvbin)
				HG_DrawBar(pbegin+(nbin+1)*binwidth+off*bwidth,
						bbase, maxbh, bwidth,
						badcounts[off],	cmin, cmax,
						ctable[off % ncolor].pixel,
						anncounts, bscale);
		}
	/*
	 * Add annotation on the side.
	 */
		HG_SideAnnot (c, &sapos, lheight, pids[plat], fids, nfield,
				ctable, ncolor, plat*nfield, sscale);
	}
/*
 * Now we gots to annotate the bins.
 */
	HG_BinAnnot (bins, nbin, bvbin, bscale, pbegin -bwidth/2, y1,binwidth);
/*
 * Cleanup and done.
 */
cleanup:
	free (counts);
	free (badcounts);
}







static int
HG_BCParams (c, pids, nplat, fids, nfield, bins, nbin, bvbin, tperiod,
		ctable, ncolor, anncounts, autoscale, zanchor, cmin, cmax,
		sscale, bscale)
char *c;
PlatformId *pids;
int *nplat, *nfield, *nbin, *tperiod, *ncolor, *cmin, *cmax;
FieldId *fids;
float *bins, *sscale, *bscale;
XColor **ctable;
zbool *bvbin, *anncounts, *autoscale, *zanchor;
/*
 * Get the parameters that control our plot.
 */
{
	char s[CFG_PLATNAME_LEN], *names[32];
	int np, i, nf;
/*
 * Platforms?
 */
	if (! pda_ReqSearch (Pd, c, "platform", NULL, s, SYMT_STRING))
		return (FALSE);
	np = CommaParse (s, names);
	*nplat = 0;
	for (i = 0; i < np; i++)
	{
		if ((pids[*nplat] = ds_LookupPlatform (names[i])) ==
				BadPlatform)
			msg_ELog (EF_PROBLEM, "Bad platform %s", names[i]);
		else
			(*nplat)++;
	}
	if (*nplat == 0)
	{
		msg_ELog (EF_PROBLEM, "No good platforms! :-(");
		return (FALSE);
	}
/*
 * Fields.
 */
	if (! pda_ReqSearch (Pd, c, "field", NULL, s, SYMT_STRING))
		return (FALSE);
	nf = ParseFieldList (s, names);
	for (*nfield = 0; *nfield < nf; (*nfield)++)
		fids[*nfield] = F_Lookup (names[*nfield]);
/*
 * Bins.
 */
	if (! pda_ReqSearch (Pd, c, "bins", NULL, s, SYMT_STRING))
		return (FALSE);
	*nbin = CommaParse (s, names);
	for (i = 0; i < *nbin; i++)
		if (! sscanf (names[i], "%f", bins + i))
		{
			msg_ELog (EF_PROBLEM, "Bad bin: '%s'", names[i]);
			return (FALSE);
		}
/*
 * Misc boolean parameters
 */
	*bvbin = FALSE;
	(void) pda_Search (Pd, c, "badvalue-bin", "histogram", (char *) bvbin,
			SYMT_BOOL);
	*anncounts = FALSE;
	(void) pda_Search (Pd, c, "annotate-counts", "histogram",
			(char *) anncounts, SYMT_BOOL);
	*zanchor = TRUE;
	(void) pda_Search (Pd, c, "zero-anchor", "histogram", (char *) zanchor,
			SYMT_BOOL);
/*
 * What is the time period?
 */
	if (! pda_ReqSearch (Pd, c, "time-period", "histogram", s,SYMT_STRING))
		return (FALSE);
	if (! strcmp (s, "observation"))
		*tperiod = 0;
	else if ((*tperiod = pc_TimeTrigger (s)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Bad histogram time period: %s", s);
		return (FALSE);
	}
/*
 * Colors.
 */
	if (! pda_Search (Pd, c, "color-table", "histogram", s, SYMT_STRING))
		strcpy (s, "hotcold");
	if (! ct_LoadTable (s, ctable, ncolor))
	{
		msg_ELog (EF_PROBLEM, "Unknown color table: %s", s);
		ct_LoadTable ("hotcold", ctable, ncolor); /* Hope...*/
	}
/*
 * Bar scaling.
 */
	if (! pda_Search (Pd, c, "scale-mode", "histogram", s, SYMT_STRING))
		*autoscale = TRUE;
	else
		*autoscale = (strcmp (s, "manual") != 0);
	if (! *autoscale)
	{
		if (! pda_ReqSearch (Pd, c, "count-min", "histogram",
				(char *) cmin, SYMT_INT))
			*autoscale = TRUE;
		if (! pda_ReqSearch (Pd, c, "count-max", "histogram",
				(char *) cmax, SYMT_INT))
			*autoscale = TRUE;
		if (*cmin >= *cmax)
		{
			msg_ELog (EF_PROBLEM, "count-min must be < count-max");
			*autoscale = TRUE;
		}
	}
/*
 * Annotation scaling.
 */
	if (! pda_Search (Pd, c, "sa-scale", "histogram", (char *) sscale,
			SYMT_FLOAT))
		*sscale = 14;
	if (! pda_Search (Pd, c, "bar-annot-scale", "histogram",
			(char *) bscale, SYMT_FLOAT))
		*bscale = 10;
/*
 * Looks like we made it.
 */
	return (TRUE);
}




static DataChunk *
HG_GetBCData (pid, fids, nfield, tperiod)
PlatformId pid;
FieldId *fids;
int nfield, tperiod;
/*
 * Get the data for this subset of the plot.
 */
{
	ZebTime begin, end;
/*
 * Figure out times.
 */
	if (tperiod > 0)
	{
		begin = end = PlotTime;
		begin.zt_Sec -= tperiod;
	}
	else
	{
		if (! ds_GetObsTimes (pid, &PlotTime, &end, 1, NULL))
			return (0);
	}
/*
 * Organization check.  For now we enforce scalar.  Such is not the
 * final intent, however.
 */
	if (ds_PlatformDataOrg (pid) != OrgScalar)
	{
		msg_ELog (EF_PROBLEM,"Scalar organization required for histo");
		return (0);
	}
/*
 * Time to do a fetch and we're done.
 */
	if (tperiod > 0)
		return (ds_Fetch (pid, DCC_Scalar, &begin, &end, fids,
				nfield, 0, 0));
	else
		return (ds_FetchObs (pid, DCC_Scalar, &end, fids, nfield,0,0));
}





static void
HG_FillBins (dc, fid, bins, nbin, counts, badcount)
DataChunk *dc;
FieldId fid;
float *bins;
int nbin, *counts, *badcount;
/*
 * Calculate up the bins here.
 */
{
	float badval = dc_GetBadval (dc), dv;
	int  sample, nsample = dc_GetNSample (dc), bin;

	for (sample = 0; sample < nsample; sample++)
	{
		dv = dc_GetScalar (dc, sample, fid);
	/*
	 * Catch bad data flags separately.
	 */
		if (dv == badval)
			(*badcount)++;
	/*
	 * We have to search the bins until we find the right thing.  If
	 * we assume the number of bins is small, it's not worthwhile to
	 * set up and run a fancy search here.
	 */
		else
		{
			for (bin = 0; bin < nbin; bin++)
			{
				if (dv < bins[bin])
				{
					counts[bin]++;
					break;
				}
			}
			if (bin == nbin)
				counts[nbin]++;
		}
	}
}






static void
HG_GetLimits (counts, nplat, nfield, nbin, badcounts, zanchor, cmin, cmax)
int **counts, nplat, nfield, nbin, *badcounts, zanchor, *cmin, *cmax;
/*
 * Figure the max and min counts.
 */
{
	int pf, bin;

	*cmin = 9999;
	*cmax = 0;
	for (pf = 0; pf < nplat*nfield; pf++)
	{
		int *bcounts = counts[pf];
	/*
	 * Make really, really sure there is data here.  Otherwise things
	 * get ugly.
	 */
		if (! bcounts)
			continue;
	/*
	 * Count up the regular bins.
	 */
		for (bin = 0; bin <= nbin; bin++)
		{
			if (bcounts[bin] < *cmin)
				*cmin = bcounts[bin];
			if (bcounts[bin] > *cmax)
				*cmax = bcounts[bin];
		}
	/*
	 * Do the bad value flags too, if warranted.
	 */
		if (badcounts)
		{
			if (badcounts[pf] < *cmin)
				*cmin = badcounts[pf];
			if (badcounts[pf] > *cmax)
				*cmax = badcounts[pf];
		}
	}
/*
 * All that work, and we still throw away the min value if they want that.
 */
	if (zanchor)
		*cmin = 0;
}
	       








static void
HG_DrawBar (pos, bbase, maxbh, bwidth, counts, min, max, color, annot, bscale)
int pos, bbase, maxbh, bwidth, counts, min, max, annot;
Pixel color;
double bscale;
/*
 * Actually draw a bar for this thing.
 */
{
	int y0, y1, bheight;
/*
 * Figure out where we want to be.
 */
	bheight = (maxbh * (counts - min))/(max - min);
/*
 * Just draw a bar.
 */
	FixForeground (color);
	if (bheight > 0)
		XFillRectangle (Disp, GWFrame (Graphics), Gcontext, pos,
				bbase - bheight, bwidth - 1, bheight);
	else
		bheight = 0;
/*
 * If they want, we should throw in the counts too.
 */
	if (annot)
	{
		char scounts[12];

		sprintf (scounts, "%d", counts);
# ifdef notdef
		FixForeground (WhitePixelOfScreen (XtScreen (Graphics)));
		DrawText (Graphics, GWFrame (Graphics), Gcontext,
				pos + bwidth/2, bbase - bheight - 1, scounts,
				0.0, bscale, JustifyCenter, JustifyBottom);
# endif
		DrawText (Graphics, GWFrame (Graphics), Gcontext,
				pos + bwidth/2, bbase - bheight - 1, scounts,
				90.0, bscale, JustifyLeft, JustifyCenter);
	}
}






static void
HG_SideAnnot (c, ypos, lheight, pid, fids, nfield, ctable, ncolor, offset,
		scale)
char *c;
int *ypos, lheight, nfield, ncolor, offset;
PlatformId pid;
FieldId *fids;
XColor *ctable;
double scale;
/*
 * Do the side annotation for this platform.
 */
{
	int xpos = GWWidth (Graphics)*F_X1 + 5, field, sx, sy, ex, ey;
	char s[40];
	Pixel white = WhitePixelOfScreen (XtScreen (Graphics));
	const char *platname = ds_PlatformName (pid);
/*
 * Platform name.
 */
	sprintf (s, "%s:", platname);
	FixForeground (white);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, xpos, *ypos, s,
			0.0, scale, JustifyLeft, JustifyTop);
	*ypos += lheight;
/*
 * Add a little bar for each field.
 */
	for (field = 0; field < nfield; field++)
	{
		char *fname = SimpleFieldName (fids[field]);
	/*
	 * Fill in the bar.
	 */
		FixForeground (ctable[offset % ncolor].pixel);
		XFillRectangle (Disp, GWFrame (Graphics), Gcontext,
				xpos + 10, *ypos + 2, 20, lheight - 4);
	/*
	 * Add the field name and make it active.
	 */
		FixForeground (white);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, xpos + 35,
				*ypos, fname, 0.0, scale, JustifyLeft,
				JustifyTop);
		DT_TextBox (Graphics, GWFrame (Graphics), xpos + 35,
				*ypos, fname, 0, scale, JustifyLeft,
				JustifyTop, &sx, &sy, &ex, &ey);
		I_ActivateArea (sx - 2, ey + 1, ex - sx + 4, sy - ey + 2,
				"field", c, platname, 0);
		*ypos += lheight;
		offset++;
	}
}




static void
HG_ChartLimits (y0, y1)
int *y0, *y1;
/*
 * Return the space that we have to play with here.
 */
{
	*y1 = GWHeight (Graphics) - (NPlotted - 1)*PlotHeight - BOTTOMBORDER;
	*y0 = *y1 - PlotHeight;
}





static void
HG_BinAnnot (bins, nbin, bvbin, bscale, left, bottom, binwidth)
float *bins;
double bscale;
int nbin, bvbin, left, bottom, binwidth;
/*
 * Annotate our bins.
 */
{
	int bin;
	char s[80];
/*
 * The first bin is special.
 */
	bottom -= BAR_ANNOT_SPACE - 1;
	FixForeground (WhitePixelOfScreen (XtScreen (Graphics)));
	left += binwidth/2;
	sprintf (s, "< %.2f", bins[0]);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, bottom, s,
			0.0, bscale, JustifyCenter, JustifyTop);
/*
 * All the middle bins.
 */
	for (bin = 1; bin < nbin; bin++)
	{
		left += binwidth;
		sprintf (s, "%.2f>%.2f", bins[bin - 1], bins[bin]);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, left, bottom,
				s, 0.0, bscale, JustifyCenter, JustifyTop);
	}
/*
 * The max bin.
 */
	left += binwidth;
	sprintf (s, " >= %.2f", bins[nbin - 1]);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, bottom, s,
			0.0, bscale, JustifyCenter, JustifyTop);
/*
 * And maybe a bad value flag.
 */
	if (bvbin)
	{
		left += binwidth;
		DrawText (Graphics, GWFrame (Graphics), Gcontext, left, bottom,
				"Bad flag", 0.0, bscale, JustifyCenter,
				JustifyTop);
	}
}
		



static void
HG_DividerBar (c)
char *c;
/*
 * Draw up the divider bar.
 */
{
	char cname[32];
	XColor color;
	int y0, y1;
/*
 * What color do they want it?
 */
	if (! pda_Search (Pd, c, "divider-color", "histogram", cname,
			SYMT_STRING))
		strcpy (cname, "gray50");
	if (! ct_GetColorByName (cname, &color))
	{
		msg_ELog (EF_PROBLEM, "Bad color: %s", cname);
		ct_GetColorByName ("white", &color);
	}
/*
 * Draw it.
 */
	HG_ChartLimits (&y0, &y1);
	FixForeground (color.pixel);
	XDrawLine (Disp, GWFrame (Graphics), Gcontext, 0, y1 - 1,
			GWWidth(Graphics)*F_X1, y1 - 1);
}




static void
HG_SetOT (char *c, DataChunk *dc)
/*
 * Add a line for this platform into the overlay times widget.
 */
{
	ZebTime zt;
	char s[120];
/*
 * Assemble the string.
 */
	sprintf (s, "%-13s %-10s %7d ", c, ds_PlatformName (dc->dc_Platform),
			dc_GetNSample (dc));
	dc_GetTime (dc, 0, &zt);
	TC_EncodeTime (&zt, TC_Full, s + strlen (s));
	strcat (s, " -> ");
	dc_GetTime (dc, dc_GetNSample (dc) - 1, &zt);
	TC_EncodeTime (&zt, TC_Full, s + strlen (s));
	strcat (s, "\n");
/*
 * Add it in.
 */
	ot_Append (s);
}




static void
HG_CountGrid (c, cmin, cmax, base, maxbh)
char *c;
int cmin, cmax, base, maxbh;
/*
 * Throw on a count grid.
 */
{
	zbool dogrid;
	float scale;
	char s[32];
	XColor gridc, annotc;
	int lineint, linepos, lleft = BAR_LEFT - 10;
	int lright = GWWidth (Graphics)*F_X1;
/*
 * Make sure they really want a grid.
 */
	if (pda_Search (Pd, c, "count-grid", "histogram", (char *) &dogrid,
			SYMT_BOOL) && ! dogrid)
		return;
/*
 * Control parameters.
 */
	scale = 12;
	pda_Search (Pd, c, "count-grid-scale", "histogram", (char *) &scale,
			SYMT_FLOAT);
	if (! pda_Search (Pd, c, "count-grid-color", "histogram", s,
			SYMT_STRING))
		strcpy (s, "gray60");
	ct_GetColorByName (s, &gridc);
	if (! pda_Search (Pd, c, "count-grid-annot-color", "histogram", s,
			SYMT_STRING))
		strcpy (s, "white");
	ct_GetColorByName (s, &annotc);
/*
 * Figure out the line interval.
 */
	lineint = HG_FigureInterval (cmin, cmax, maxbh);
/*
 * Now we do some drawing.
 */
	for (linepos = cmin; linepos <= cmax; /*see below*/)
	{
		int ypos = base - ((linepos - cmin)*maxbh)/(cmax - cmin);
	/*
	 * Annotate the counts here.
	 */
		sprintf (s, "%5d", linepos);
		FixForeground (annotc.pixel);
		if ((base - ypos) > 15 || ypos == base)
			DrawText (Graphics, GWFrame (Graphics), Gcontext,
					BAR_LEFT - 12, ypos, s, 0.0, scale,
					JustifyRight, JustifyCenter);
	/*
	 * And a grid line.
	 */
		FixForeground (gridc.pixel);
		XDrawLine (Disp, GWFrame (Graphics), Gcontext, lleft, ypos,
				lright, ypos);
	/*
	 * Move on to the next one.
	 */
		linepos += lineint;
		linepos -= linepos % lineint;
	}
}





static int
HG_FigureInterval (cmin, cmax, maxbh)
int cmin, cmax, maxbh;
/*
 * Figure an optimal line interval for this count range.
 */
{
	int maxlines = maxbh/MIN_LINE_SPACE;
	int minint, interval;
/*
 * Figure the smallest interval we're willing to draw.
 */
	minint = (cmax - cmin)/maxlines;
	if (minint <= 0)
		return (1);
/*
 * The above gives us the smallest interval that we're prepared to draw
 * in the space we have.  Now we try to smartly round it up to something
 * useful.
 */
	interval = (int) (pow (10.0, floor (log10 ((float) (cmax - cmin)))));
	if (interval/minint >= 5)
		return ((int) (interval/5.0));
	else if (interval/minint >= 2)
		return ((int) (interval/2.0));
	return ((int) interval);
}



# endif /* C_PT_HISTOGRAM -- whole file */
