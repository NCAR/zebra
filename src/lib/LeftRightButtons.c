/*
 * Left-right button widget
 */
static char *rcsid = "$Id: LeftRightButtons.c,v 1.1 1991-09-13 15:01:58 corbet Exp $";
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
