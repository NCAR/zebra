/*
 * Widget for getting position of cursor.
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
# include <math.h>

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/AsciiText.h>

# include <defs.h>
# include <message.h>
# include <pd.h>
# include <GraphicsW.h>
# include <bitmaps.h>
# include "PixelCoord.h"
# include "GraphProc.h"

RCSID ("$Id: PositionWidget.c,v 1.25 2001-11-30 21:29:28 granger Exp $")

# ifndef PI
# define PI 3.141592654
# endif
# define MAXORG 20
# define KM_TO_NM (0.62137119 * 5280.0 / 6080.0)
# define KM_TO_MI (0.62137119)
# define NM_TO_FT (6080.0)

static Widget 	PosLabel = NULL, DMSButton, OrgText, OrgLabel;
static Widget	KNButton;

static char	GPOrigin[40];
# define NOWHERE	"(none)"

static int 	PWMade = FALSE, DegMinSec = TRUE, DoNm = TRUE;
static int	CursorX = 0, 
	        CursorY = 0; 		/* Location of last XQueryPointer  */
static int	CursorValid = FALSE;	/* X,Y have been set and are valid */

/*
 * Forward declarations
 */
#ifdef notdef
static void pw_PosPopdown FP((Widget, int, int));
#endif
void pw_PosStatus ();
static void pw_PosDisplay ();
void pw_InitPos();
Widget pw_PosCreate FP((char *, Widget, XtAppContext));
void ChangeType ();
void ChangeUnit ();


/*
 * Actions for the GetPosition widget
 */
static XtActionsRec pw_Actions[] =
{
	{ "new-origin",		pw_PosDisplay	},
	{ "change-type",	ChangeType	},
	{ "change-unit",	ChangeUnit	}
};




void
pw_InitPos ()
/*
 * Tell UI about the position widget, and register our actions
 */
{
	uw_def_widget ("position", "Get Position", pw_PosCreate, 0, 0);
}


Widget
pw_PosCreate (junk, parent, actx)
char 	*junk;
Widget 	parent;
XtAppContext 	actx;
/*
 * Actually create the position widget.
 */
{
	Widget	hbutton;
	Arg	args[20];
	char	units[16];
	int	n;

	XtAppAddActions (Actx, pw_Actions, XtNumber(pw_Actions));
/*
 * Determine the distance units we'll start with.  We default to "km", and
 * will accept "km" or "nm" (any case)
 */
	DoNm = FALSE;

	if (pda_Search (Pd, "global", "getpos-default-units", NULL, 
			units, SYMT_STRING))
	{
		if (! strcasecmp (units, "km"))
			DoNm = FALSE;
		else if (! strcasecmp (units, "nm"))
			DoNm = TRUE;
		else
		{
			msg_ELog (EF_PROBLEM, 
				  "Bad getpos-default-units: '%s', using km",
				  units);
			DoNm = FALSE;
		}
	}
/*
 * Get our starting origin
 */
	strcpy (GPOrigin, NOWHERE);
	pda_Search (Pd, "global", "getpos-default-origin", NULL, GPOrigin, 
		    SYMT_STRING);
/*
 * Make the bitmaps for the left and right arrow buttons.
 */
	bm_BuildBitmaps (parent);
/*
 * The text window which displays position.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "");		n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
	XtSetArg (args[n], XtNresize, True);		n++;
	XtSetArg (args[n], XtNwidth, 300);		n++;
	XtSetArg (args[n], XtNheight, 120);		n++;
	PosLabel = XtCreateManagedWidget ("GetPosition", labelWidgetClass,
		parent, args, n);
/*
 * The button to switch between deg/min/sec and decimal degrees
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, DegMinSec ? "Deg/Min/Sec" : 
		"Decimal Deg"); n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
	XtSetArg (args[n], XtNfromVert, PosLabel);		n++;
	XtSetArg (args[n], XtNbottom, XtChainBottom); n++;
	XtSetArg (args[n], XtNtop, XtChainBottom); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
	DMSButton = XtCreateManagedWidget ("degminsec", commandWidgetClass,
		parent, args, n);
	XtAddCallback (DMSButton, XtNcallback, ChangeType, 0);
/*
 * The button to switch between km and nm.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, DoNm ? "Nm" : "km");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
	XtSetArg (args[n], XtNfromVert, DMSButton);		n++;
	XtSetArg (args[n], XtNbottom, XtChainBottom); n++;
	XtSetArg (args[n], XtNtop, XtChainBottom); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
	KNButton = XtCreateManagedWidget ("nmkm", commandWidgetClass,
		parent, args, n);
	XtAddCallback (KNButton, XtNcallback, ChangeUnit, 0);
/*
 * Add the help mechanism
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Help");			n++;
	XtSetArg (args[n], XtNfromHoriz, KNButton);		n++;
	XtSetArg (args[n], XtNfromVert, DMSButton);		n++;
	XtSetArg (args[n], XtNbottom, XtChainBottom); n++;
	XtSetArg (args[n], XtNtop, XtChainBottom); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
	hbutton = XtCreateManagedWidget ("help", commandWidgetClass,
					 parent, args, n);
	XtAddCallback (hbutton, XtNcallback, HelpCallback, 
		       (XtPointer)GP_HELP_GPOSITION);
/*
 * The text widget for entering the origin.
 */
        n = 0;
        XtSetArg (args[n], XtNfromHoriz, DMSButton); n++;
        XtSetArg (args[n], XtNfromVert, PosLabel); n++;
	XtSetArg (args[n], XtNbottom, XtChainBottom); n++;
	XtSetArg (args[n], XtNtop, XtChainBottom); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
        XtSetArg (args[n], XtNdisplayPosition, 0); n++;
        XtSetArg (args[n], XtNinsertPosition, 0); n++;
        XtSetArg (args[n], XtNresize, XawtextResizeNever); n++;
        XtSetArg (args[n], XtNlength, 40); n++;
        XtSetArg (args[n], XtNwidth, 70); n++;
        XtSetArg (args[n], XtNstring, GPOrigin); n++;
        XtSetArg (args[n], XtNtype, XawAsciiString); n++;
        XtSetArg (args[n], XtNuseStringInPlace, True); n++; 
	XtSetArg (args[n], XtNleftMargin, 5); n++;
        XtSetArg (args[n], XtNeditType, XawtextEdit); n++;
        OrgText = XtCreateManagedWidget ("origintext", asciiTextWidgetClass,
                parent, args, n);
/*
 * The status line.
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 1);			n++;
	XtSetArg (args[n], XtNlabel, "Enter an origin.");	n++;
        XtSetArg (args[n], XtNwidth, 200); n++;
	XtSetArg (args[n], XtNfromHoriz, DMSButton);		n++;
	XtSetArg (args[n], XtNfromVert, OrgText);		n++;
	XtSetArg (args[n], XtNbottom, XtChainBottom); n++;
	XtSetArg (args[n], XtNtop, XtChainBottom); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
	OrgLabel = XtCreateManagedWidget ("orglabel", labelWidgetClass,
		parent, args, n);

	XtInstallAllAccelerators (parent, parent);
	PWMade = TRUE;
	return (PosLabel);
}


static void
SetStatus (string)
char *string;
{
	Arg arg;

	XtSetArg (arg, XtNlabel, string);
	XtSetValues (OrgLabel, &arg, 1);
}


void
ChangeType ()
/*
 * Change the type of the position display between Lat/Lon and Range/Azimuth.
 */
{
	Arg	args[2];

	DegMinSec = ! DegMinSec;
	XtSetArg (args[0], XtNlabel, DegMinSec ? "Deg/Min/Sec" : 
		"Decimal Deg");
	XtSetValues (DMSButton, args, 1);

	pw_PosDisplay ();	/* Convert any previously set point */
}


void
ChangeUnit ()
/*
 * Change the position display between km and nm.
 */
{
	Arg	args[2];

	DoNm = ! DoNm;
	XtSetArg (args[0], XtNlabel, DoNm ? "Nm" : "km");
	XtSetValues (KNButton, args, 1);

	pw_PosDisplay ();	/* Convert any previously set point */
}



void
pw_PosStatus ()
/*
 * Query X server for cursor location, set our global x,y pixel coords,
 * then call pw_PosDisplay() to actually calculate and the display
 * the new coordinates.
 */
{
	Window	wjunk;
	int	junk, x, y;

	if (! PWMade) return;
/*
 * Find out where the pointer is.
 */
	XQueryPointer (XtDisplay (Graphics), XtWindow (Graphics), &wjunk, 
		&wjunk, &junk, &junk, &x, &y, (unsigned int *) &junk);
/*
 * Save the point for immediate and future reference
 */
	CursorX = x;
	CursorY = y;
	CursorValid = TRUE;

	pw_PosDisplay ();
}



static void
pw_PosDisplay ()
/*
 * Create the position text line and display it in the widget.
 * Entered via pw_PosStatus() and the new-origin() action.
 */
{
	char	string[100], label[300], offstring[50], statusstr[100];
	char	units[3];
	int	x, y;
	float	offset, lat, lon, ox, oy; 
	double	range, azimuth, subx, suby;
	Location	loc;
	Arg	args[5];

	if (! CursorValid) return;	/* No cursor point set yet */
/*
 * Empty the label
 */
	strcpy (label, "");
/*
 * Get the most recent pointer location
 */
	x = CursorX;
	y = CursorY;
/*
 * Calculate the lat/lon
 */
	prj_Reverse (XUSER(x), YUSER(y), &lat, &lon);
	if (DegMinSec)
	{
		int	latdeg, latmin, latsec, londeg, lonmin, lonsec;
		char	ewdir, nsdir;

		ewdir = (lon > 0) ? 'E' : 'W';
		nsdir = (lat > 0) ? 'N' : 'S';

		lat = fabs (lat);
		lon = fabs (lon);

		latdeg = (int) lat;
		latmin = (int)((lat - latdeg) * 60);
		latsec = (int)((lat - latdeg - latmin / 60.0) * 3600);

		londeg = (int) lon;
		lonmin = (int)((lon - londeg) * 60);
		lonsec = (int)((lon - londeg - lonmin / 60.0) * 3600);

		sprintf (string,
			 "Lat: %d %2d'%2d'' %c  Lon: %d %2d'%2d'' %c\n", 
			 latdeg, latmin, latsec, nsdir, londeg, lonmin, 
			 lonsec, ewdir);
		strcat (label, string);
		sprintf (string, "     %d %5.2f'  %c       %d %5.2f'  %c\n\n", 
			 latdeg, (lat - latdeg)*60, nsdir, 
			 londeg, (lon - londeg)*60, ewdir);
		strcat (label, string);
	}
	else
	{
	    sprintf (string, "Lat: %.2f Lon: %.2f\n\n\n", lat, lon);
	    strcat (label, string);
	}


/*
 * Get the location and the azimuth offset of the origin.
 */
	if (! GetLocation (GPOrigin, &PlotTime, &loc))
	{
		if (strcmp (GPOrigin, NOWHERE))
		{
			msg_ELog (EF_PROBLEM, "Unable to locate origin '%s'.",
				  GPOrigin);
			sprintf (statusstr, "Unable to locate origin: '%s'.",
				 GPOrigin);
			SetStatus (statusstr);
		}
        /*
	 * At least display lat/lon since we've already gone to the trouble
	 */
		strcat (label, "No origin for relative coordinates\n");
		XtSetArg (args[0], XtNlabel, label);
		XtSetValues (PosLabel, args, 1);
		return;
	}
	sprintf (statusstr, "Origin set to: %s.", GPOrigin);
	SetStatus (statusstr);
/*
 * Get the azimuth offset.
 */
	sprintf (offstring, "%s-azimuth-offset", GPOrigin);
	if (! pda_Search (Pd, "global", offstring, NULL, 
		(char *) &offset, SYMT_FLOAT))
		offset = 0.0;			
/*
 * Calculate range and azimuth
 */
	prj_Project (loc.l_lat, loc.l_lon, &ox, &oy);
	subx = (double) (XUSER(x) - ox);
	suby = (double) (YUSER(y) - oy);

	range = hypot (subx, suby);

	strcpy (units, "km");

	if (DoNm)
	{
		range *= KM_TO_NM;
		subx *= KM_TO_NM;
		suby *= KM_TO_NM;
		strcpy (units, "Nm");
	}
		
	azimuth =  ATAN2 (suby, subx);
	azimuth = 90.0 - (azimuth * 180.0 / PI) - offset;
	if (azimuth < 0.0)
		azimuth += 360.0;

	sprintf (string, "Az: %.0f  Range: %.1f %s\n\n", azimuth, range, 
		 units);
	
	strcat (label, string);
/*
 * Calculate x and y.
 */
	
	sprintf (string, "x: %.1f %s  y: %.1f %s", subx, units, suby, units);
	if (DoNm)
		sprintf (string + strlen (string), 
			 " (%.0f ft)", suby * NM_TO_FT);
	
	sprintf (string + strlen (string), "\n\n");

	strcat (label, string);
/*
 * Say what its relative to.
 */
	sprintf (string, "Relative to %s", GPOrigin);
	strcat (label, string);

	XtSetArg (args[0], XtNlabel, label);
	XtSetValues (PosLabel, args, 1);
}
