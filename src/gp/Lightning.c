/*
 * Locaton display routine.
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
# if C_CAP_LIGHTNING

# include <math.h>
# include <X11/Intrinsic.h>

# include <defs.h>
# include <pd.h>
# include <message.h>
# include <GraphicsW.h>
# include <DataStore.h>
# include "GC.h"
# include "GraphProc.h"
# include "PixelCoord.h"
# include "DrawText.h"

RCSID("$Id: Lightning.c,v 2.16 2001-04-20 08:26:27 granger Exp $")

extern Pixel	White;

/*
 * Forwards.
 */
void	li_CAPLight FP ((char *, int));
void	li_SideAnnot FP ((char *, char *, int, int, int));

void
li_CAPLight (comp, update)
char *comp;
zbool update;
{
	char	platform[30], step[30], ctable[30], field[40], color[40];
	char	tadefcolor[30], data[100], temp[50], iconname[40];
	char	dannot[32];
	int	period, x, y, numcolor, pid, istep;
	int	i, index, nsamp, showicon, doannot = FALSE;
	ZebTime	begin, when;
	float	fx, fy, sascale;
	XColor	*colors, tadefclr, acolor;
	DataChunk	*dc;
	Location	loc;
	FieldId datafld;
/*
 * Get our platform first, since that's what is of interest to us.
 */
	if (! pda_ReqSearch (Pd,comp, "platform", NULL, platform, SYMT_STRING))
		return;
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Unknown platform '%s'", platform);
		return;
	}
/*
 * Make sure that we'll get the right sort of stuff.
 */
	if (! ds_IsMobile (pid))
	{
		msg_ELog (EF_PROBLEM, 
			"Location display on static platform %s", platform);
		return;
	}
/*
 * Pull out other parameters of interest.
 */
	if (! pda_Search (Pd, comp, "time-step", platform, step, SYMT_STRING))
		period = 300;
	else if ((period = pc_TimeTrigger (step)) == 0)
	{
		msg_ELog (EF_PROBLEM, 
			"Unparsable time-step: '%s'", step);
		period = 300;
	}
/*
 * Get the color table.
 */
	if (! pda_ReqSearch (Pd, comp, "color-table", platform, ctable, 
		SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, 
			"No color table specified in component %s", comp);
		return;
	}
	if (! ct_LoadTable (ctable, &colors, &numcolor))
	{
		msg_ELog (EF_PROBLEM, "Unable to load color table %s", ctable);
		return;
	}
/*
 * Read in annotation information.
 */
	if(! pd_Retrieve (Pd, "global", "ta-color", tadefcolor, SYMT_STRING))
		strcpy (tadefcolor, "white");
	if(! ct_GetColorByName (tadefcolor, &tadefclr))
	{
		msg_ELog (EF_PROBLEM, 
			"Can't get default color: '%s'.", tadefcolor);
		strcpy (tadefcolor, "white");
		ct_GetColorByName (tadefcolor, &tadefclr);
	}
	if (! pda_Search (Pd, comp, "sa-scale", platform, (char *) &sascale,
		SYMT_FLOAT))
		sascale = 0.02;
/*
 * Get the name of the icon to use.
 */
	showicon = TRUE;
	pda_Search (Pd, comp, "show-icon", platform, (char *) &showicon, 
		SYMT_INT);
	if (! pda_ReqSearch (Pd, comp, "icon", platform, iconname, SYMT_STRING))
	{
		msg_ELog (EF_PROBLEM, "Can't find icon.");
		return;
	}
/*
 * See if they want a data field annotated with the locations.
 */
	if (pda_Search (Pd, comp, "annot-field", platform, field, SYMT_STRING))
	{
		datafld = F_Lookup (field);
		doannot = TRUE;
		if (! pda_Search (Pd, comp, "annot-color", platform, color,
				SYMT_STRING))
			strcpy (color, "white");
		ct_GetColorByName (color, &acolor);
	}
/*
 * Figure the begin time.
 */
	begin = PlotTime;
	istep = period / 60;

	period *= numcolor;
	begin.zt_Sec -= period;
/*
 * Get the data.
 */
	dc = doannot ? ds_Fetch (pid, DCC_Scalar, &begin, &PlotTime, &datafld,
			1, NULL, 0) :
		ds_Fetch (pid, DCC_Location, &begin, &PlotTime, NULL, 0,
				NULL, 0);
	if (! dc)
	{
		msg_ELog (EF_INFO, "No %s data available", platform);
		return;
	}
	nsamp = dc_GetNSample (dc);
/*
 * Work through the data.
 */
	for (i = 0; i < nsamp; i++)
	{
		dc_GetLoc (dc, i, &loc);
	/*
	 * Locate this point.
	 */
		prj_Project (loc.l_lat, loc.l_lon, &fx, &fy);
		x = XPIX (fx); 
		y = YPIX (fy);
	/*
	 * Place the icon. 
	 */
		dc_GetTime (dc, i, &when);
		index = (int) (((float) PlotTime.zt_Sec - 
			(float) when.zt_Sec) / (float) period * 
			(float) (numcolor - 1));
		if ((index < 0) || (index >= numcolor))
			index = 0;
		if (showicon)
			I_PositionIcon (comp, platform, &when, iconname, x, y,
				colors[index].pixel);
		else
			ov_PositionIcon (iconname, x, y, colors[index].pixel);
	/*
	 * Also annotation.
	 */
		if (! doannot)
			continue;
		XSetForeground (Disp, Gcontext, acolor.pixel);
		sprintf (dannot, "%.1f", dc_GetScalar (dc, i, datafld));
		DrawText (Graphics, GWFrame (Graphics), Gcontext, x + 10, y,
				dannot, 0.0, sascale, JustifyLeft,
				JustifyCenter);
	}
/*
 * Annotate if necessary.
 */
 	if (! update)
	{
	/*
	 * On the top.
	 */
		sprintf (temp, "%s location.", platform);
		An_TopAnnot (temp);
	/*
	 * Down the side too.
	 */
        	sprintf (data, "%s %d %d", ctable, nsamp, istep);
		An_AddAnnotProc (li_SideAnnot, comp, data, strlen (data),
			75, TRUE, FALSE);
	}
/*
 * Put in the status line.
 */
	dc_GetTime (dc, nsamp - 1, &when);
	ot_AddStatusLine (comp, platform, "(lightning)", &when);
/*
 * Free the data.
 */
	dc_DestroyDC (dc);
}



void
li_SideAnnot (comp, data, datalen, begin, space)
char *comp, *data;
int datalen, begin, space;
/*
 * Do side annotation for a location plot.
 */
{
        char string[40], ctable[40];
        float scale;
        int i, left, limit, ncolors, bar_height, match;
	int npoint, istep;
        XColor *colors, xc;
/*
 * Get the data.
 */
	An_GetSideParams (comp, &scale, &limit);
	An_GetTopParams (&xc, &match);
        sscanf (data, "%s %d %d", ctable, &npoint, &istep);
        ct_LoadTable (ctable, &colors, &ncolors);
/*
 * Put in the colored bars.
 */
	left = An_GetLeft ();
	bar_height = (float) space / (float) (ncolors + 1);
	for (i = 0; i < ncolors; i++)
	{
		XSetForeground(XtDisplay (Graphics), Gcontext, 
			colors[i].pixel);
		XFillRectangle (XtDisplay (Graphics), GWFrame (Graphics), 
			Gcontext, left, begin, 10, bar_height);
		sprintf (string, "%d - %d", i*(istep),(i+1)*istep); 
		XSetForeground (XtDisplay (Graphics), Gcontext, xc.pixel);
		DrawText (Graphics, GWFrame (Graphics), Gcontext, left + 15, 
			begin, string, 0.0, scale, JustifyLeft, JustifyTop);
		begin += bar_height;	
		space -= bar_height;
	}
	sprintf (string, "Total = %d", npoint); 
	DrawText (Graphics, GWFrame (Graphics), Gcontext, left, 
		begin, string, 0.0, scale, JustifyLeft, JustifyTop);
}


# endif   /* C_CAP_LIGHTNING */
