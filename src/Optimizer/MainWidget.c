/*
 * Main optimizer widget
 * $Id: MainWidget.c,v 1.2 1991-07-05 19:51:23 burghart Exp $
 */
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
