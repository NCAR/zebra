/*
 * The graphics window annotation widget for making pretty pictures.
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

# include "PixelCoord.h"
# include "DrawText.h"
# include "GraphProc.h"

RCSID("$Id: AnnotWidget.c,v 1.4 2001-01-16 22:27:35 granger Exp $")

# define STRLEN		1000

extern Pixel	White;		/* The color white.			*/
static Widget 	LocLabel;	/* Displays currently selected location	*/
static Widget	Text;		/* Window for user typed comments	*/
static char	TextStr[STRLEN];/* User typed comments			*/
static int 	AWMade = FALSE;	/* Is the widget made yet?		*/
static int	X, Y;		/* Location of annotation.		*/

# ifdef __STDC__
	void		aw_SetLoc ();
	void		aw_InitAnnot();
	Widget		aw_AnnotCreate (char *, Widget, XtAppContext);
	static void	DoAnnotate ();
	static void	Cancel ();
# else
	void		aw_SetLoc ();
	void		aw_InitAnnot();
	Widget		aw_AnnotCreate ();
	static void	DoAnnotate ();
	static void	Cancel ();
# endif


void
aw_InitAnnot ()
/*
 * Tell UI about the annotation widget.
 */
{
	uw_def_widget ("annotate", "Annotate", aw_AnnotCreate, 0, 0);
}


Widget
aw_AnnotCreate (junk, parent, actx)
char 	*junk;
Widget 	parent;
XtAppContext 	actx;
/*
 * Actually create the annotation widget.
 */
{
	Widget	left;
	Arg	args[10];
	int	n;
/*
 * The label which displays location.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "");		n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft);	n++;
	XtSetArg (args[n], XtNresize, True);		n++;
	XtSetArg (args[n], XtNwidth, 300);		n++;
	XtSetArg (args[n], XtNheight, 40);		n++;
	LocLabel = XtCreateManagedWidget ("location", labelWidgetClass,
		parent, args, n);
/*
 * The text area for user comments.
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL);			n++;
	XtSetArg (args[n], XtNfromVert, LocLabel);		n++;
	XtSetArg (args[n], XtNdisplayPosition, 0);		n++;
	XtSetArg (args[n], XtNinsertPosition, 0);		n++;
	XtSetArg (args[n], XtNlength, STRLEN);			n++;
	XtSetArg (args[n], XtNresize, XawtextResizeNever);	n++;
	XtSetArg (args[n], XtNwidth, 300);			n++;
	XtSetArg (args[n], XtNheight, 120);			n++;
	XtSetArg (args[n], XtNstring, TextStr);			n++;
	XtSetArg (args[n], XtNtype, XawAsciiString);		n++;
	XtSetArg (args[n], XtNuseStringInPlace, True);		n++;
	XtSetArg (args[n], XtNleftMargin, 5);			n++;
	XtSetArg (args[n], XtNeditType, XawtextEdit);		n++;
	Text = XtCreateManagedWidget ("usertext", asciiTextWidgetClass,
		parent, args, n);
/*
 * The button to do the annotation.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Annotate");	n++;
	XtSetArg (args[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (args[n], XtNfromVert, Text);		n++;
	left = XtCreateManagedWidget ("annotbutton", commandWidgetClass,
		parent, args, n);
	XtAddCallback (left, XtNcallback, DoAnnotate, 0);
/*
 * The button to cancel this annotation.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, "Cancel");		n++;
	XtSetArg (args[n], XtNfromHoriz, left);		n++;
	XtSetArg (args[n], XtNfromVert, Text);		n++;
	left = XtCreateManagedWidget ("cancelbutton", commandWidgetClass,
		parent, args, n);
	XtAddCallback (left, XtNcallback, Cancel, 0);

	AWMade = TRUE;
	return (LocLabel);
}



static void
DoAnnotate ()
/*
 * Draw the annotation in the graphics window. 
 */
{
	Arg	args[2];
/*
 * Check to see where we'll be drawing this.
 */
	if (DrawFrame != DisplayFrame)
		DrawFrame = DisplayFrame;
	GWDrawInFrame (Graphics, DrawFrame);	
/*
 * Place a circle on the exact point chosen.
 */
	ov_PositionIcon ("circle", X, Y, White);
/*
 * Draw the text next to the circle.
 */
	XSetForeground (XtDisplay (Graphics), Gcontext, White);
	DrawText (Graphics, GWFrame (Graphics), Gcontext, X + 5, Y,
		TextStr, 0.0, 0.04, JustifyLeft, JustifyTop);
/*
 * Clear everything.
 */
	XtSetArg (args[0], XtNlabel, "");
	XtSetValues (LocLabel, args, 1);
/*
 * Display the frame.
 */
	GWDisplayFrame (Graphics, DisplayFrame);
}


static void
Cancel ()
/*
 * Cancel the current annotation.
 */
{
	Arg	args[2];
/*
 * Blank out the location label.
 */
	XtSetArg (args[0], XtNlabel, "");
	XtSetValues (LocLabel, args, 1);
}


void
aw_SetLoc ()
/*
 * Create the location text line and display it in the widget.
 */
{
	char	string[100];
	Window	wjunk;
	int	junk, x, y;
	float	lat, lon; 
	Arg	args[5];

	if (! AWMade) return;
/*
 * Empty the label
 */
	string[0] = '\0';
/*
 * Find out where the pointer is.
 */
	XQueryPointer (XtDisplay (Graphics), XtWindow (Graphics), &wjunk, 
		&wjunk, &junk, &junk, &x, &y, (unsigned int *) &junk);
/*
 * Save the location in globals.
 */
	X = x;	
	Y = y;	
/*
 * Calculate the lat/lon
 */
	prj_Reverse (XUSER(x), YUSER(y), &lat, &lon);
	sprintf (string, "Annotating at - Lat:  %.2f Lon:  %.2f\n\n", lat, lon);
/*
 * Set the LocLabel.
 */
	XtSetArg (args[0], XtNlabel, string);
	XtSetValues (LocLabel, args, 1);
}
