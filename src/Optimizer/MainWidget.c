/*
 * Main optimizer widget
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

static char *rcsid = "$Id: MainWidget.c,v 1.3 1991-09-17 16:00:12 burghart Exp $";

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Box.h>

# include "prototypes.h"
# include "globals.h"

/*
 * Private prototypes
 */
# ifdef __STDC__
	static Widget	mw_MWCreate (int, Widget, XtAppContext);
# else
	static Widget	mw_MWCreate ();
# endif




void
mw_DefineMainWidget ()
/*
 * Hook the main widget into UI
 */
{
	uw_def_widget ("Optimizer", "Optimizer", mw_MWCreate, 0, 0);
}




static Widget
mw_MWCreate (junk, parent, appc)
int	junk;
Widget	parent;
XtAppContext	appc;
/*
 * UI widget create routine
 */
{
	return (mw_MainWidget (parent));
}



Widget
mw_MainWidget (parent)
Widget	parent;
/*
 * Create the main optimizer widget, which holds the others
 */
{
	Widget	outer_form, cform, rform, sform, soform;
	int	n;
	Arg	args[20];
/*
 * Create the outer Box widget to hold everything
 */
	n = 0;
	XtSetArg (args[n], XtNborderWidth, 3); n++;
	XtSetArg (args[n], XtNhSpace, 1); n++;
	XtSetArg (args[n], XtNvSpace, 1); n++;
	outer_form = XtCreateManagedWidget ("outerForm", boxWidgetClass, 
		parent, args, n);
/*
 * Make the command widget, radar widget, send widget, and scan option widget 
 * children of the outer box
 */
	cform = cw_CWidget (outer_form);
	rform = rw_RWidget (outer_form);
	sform = sw_SWidget (outer_form);
	soform = so_SOWidget (outer_form);
/*
 * Done 
 */
	return (outer_form);
}
