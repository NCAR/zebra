//
// Definitions of window utilities.
//
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

//
// Hold our display info.
//
class dsDisplay
{
	XtAppContext dd_Appc;	// Application context.
public:
	Widget	dd_Top;		// Top level widget.
	dsDisplay (int *, char **);
	void run () { XtAppMainLoop (dd_Appc); }
	void sync ();
//	~dsDisplay ();
};




//
// The base class of all our windows -- this thing just holds the info
// together.
//
class dsWindow
{
protected:
	Widget	dw_shell;		// The shell widget that holds it all
	Widget	dw_form;		// The form that holds everything.
public:
	dsWindow (char *, const dsDisplay &, int top=0); // Create with name
	virtual ~dsWindow ();
	virtual void popup ();			// Put on screen
	virtual void popdown ();		// Take off screen
};





//
// A subclass for popups -- adds title and a zap button.
//

class dsPopupWindow : public dsWindow
{
protected:
	Widget corner;		// Upper left widget -- for forms
public:
	dsPopupWindow (const dsDisplay &disp, char *title, int zapspace = 50);
	void SetTitle (const char *);
};






//
// The main window.
//
class dsMainWindow : private dsWindow
{
	Widget	spaceLabel;		// The free space label
public:
	dsMainWindow (const dsDisplay &);
	~dsMainWindow () {};
	void UpdateSpace ();
};



# ifdef XtNleft
//
// A useful little utility to add the "normal" restraints.
//
static inline void
AddConstraints (Arg *args, int *n)
//
// Constrain things so they don't get mangled.
//
{
	XtSetArg (args[*n], XtNleft, XtChainLeft);	(*n)++;
	XtSetArg (args[*n], XtNright, XtChainLeft);	(*n)++;
	XtSetArg (args[*n], XtNtop, XtChainTop);	(*n)++;
	XtSetArg (args[*n], XtNbottom, XtChainTop);	(*n)++;
}




static inline void
AddBotConstraints (Arg *args, int *n)
//
// Constrain things so they don't get mangled.
//
{
	XtSetArg (args[*n], XtNleft, XtChainLeft);	(*n)++;
	XtSetArg (args[*n], XtNright, XtChainLeft);	(*n)++;
	XtSetArg (args[*n], XtNtop, XtChainBottom);	(*n)++;
	XtSetArg (args[*n], XtNbottom, XtChainBottom);	(*n)++;
}

# endif


//
// A couple of globals.
//
extern dsDisplay *Disp;
extern dsMainWindow *Main;
