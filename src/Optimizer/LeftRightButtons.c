/*
 * Left-right button widget
 * $Id: LeftRightButtons.c,v 1.1 1991-06-16 17:02:25 burghart Exp $
 */
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Command.h>
# include "bitmaps.h"




Widget
LeftRightButtons (parent, callback)
Widget	parent;
void	(*callback)();
/*
 * Create a pair of buttons with left and right pointing arrows.  The
 * callback function is assigned to both buttons and is called with -1 for
 * the left button and +1 for the right button.
 */
{
	Arg	args[10];
	int	n;
	Widget	holder, left, right;
/*
 * Create a form widget to hold the buttons
 */
	n = 0;
 	XtSetArg (args[n], XtNdefaultDistance, 0); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNleft, XtChainLeft); n++;
	XtSetArg (args[n], XtNright, XtChainLeft); n++;
	holder = XtCreateManagedWidget ("lrButtons", formWidgetClass, parent,
		args, n);
/*
 * Make the buttons
 */
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, NULL); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNbitmap, LeftArrow); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNinternalWidth, 2); n++;
	left = XtCreateManagedWidget ("leftbutton", commandWidgetClass, 
		holder, args, n);
	XtAddCallback (left, XtNcallback, callback, -1);

	n = 0;
	XtSetArg (args[n], XtNfromHoriz, left); n++;
	XtSetArg (args[n], XtNfromVert, NULL); n++;
	XtSetArg (args[n], XtNbitmap, RightArrow); n++;
	XtSetArg (args[n], XtNborderWidth, 0); n++;
	XtSetArg (args[n], XtNinternalWidth, 2); n++;
	right = XtCreateManagedWidget ("rightbutton", commandWidgetClass, 
		holder, args, n);
	XtAddCallback (right, XtNcallback, callback, 1);

	return (holder);
}
