static char *rcsid = "$Id: dm_pick.c,v 2.4 1993-02-18 22:18:27 burghart Exp $";
/*
 * Handle the window picking operation.
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
# include "dm_vars.h"
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/cursorfont.h>



/*
 * This structure is used to communicate through usy_traverse.
 */
struct wpick
{
	Window	wp_id;		/* The id of the picked window	*/
	char	wp_name[40];	/* The window name. */
};


static int dm_CmpPickWin ();


void
PickWin (winname)
char *winname;
/*
 * Choose a window and store its name into "winname".
 */
{
	int status, done = False;
	Cursor cursor;
	XEvent event;
	Window win, root = RootWindow (Dm_Display, 0);	/* xxx */
	struct wpick wp;
/*
 * Make the special cursor.
 */
	cursor = XCreateFontCursor (Dm_Display, XC_crosshair);
/*
 * Reach out and snarf up the pointer.
 */
	status = XGrabPointer (Dm_Display, root, False,
		ButtonPressMask | ButtonReleaseMask, GrabModeSync,
		GrabModeAsync, None, cursor, CurrentTime);
	if (status != GrabSuccess)
	{
		msg_ELog (EF_PROBLEM, "Unable to grab pointer for pick");
		return;
	}
/*
 * Now wait until we get something.
 */
	while (! done)
	{
	/*
	 * Allow some events through and get the next one.
	 */
		XAllowEvents (Dm_Display, SyncPointer, CurrentTime);
		XWindowEvent (Dm_Display, root,
			ButtonPressMask | ButtonReleaseMask, &event);
	/*
	 * See what we got.
	 */
		switch (event.type)
		{
		/*
		 * On a button press, we've selected a window.
		 */
		    case ButtonPress:
			win = (event.xbutton.subwindow == None) ? root :
				event.xbutton.subwindow;

			break;
		/*
		 * On a button release, we're done.
		 */
		   case ButtonRelease:
		   	done = True;
			break;
		}
	}
/*
 * Let loose of the pointer.
 */
	XUngrabPointer (Dm_Display, CurrentTime);
	XSync (Dm_Display, False);
/*
 * Pass through the current config, trying to find the window.
 */
	if (! win)
		return;
	wp.wp_id = win;
	usy_traverse (Current, dm_CmpPickWin, (long) &wp, FALSE);
	strcpy (winname, wp.wp_name);
}





static int
dm_CmpPickWin (name, type, v, wp)
char *name;
int type;
union usy_value *v;
struct wpick *wp;
/*
 * Check out this window.
 */
{
	struct cf_window *win = (struct cf_window *) v->us_v_ptr;
	Window root, parent, *children;
	unsigned int nchild;
	XWindowAttributes attr;
	
	if (! (win->cfw_flags & CF_WIDGET) && (win->cfw_win != 0))
	{
		msg_ELog (EF_DEBUG, "Window name = %s", win->cfw_name);
		msg_ELog (EF_DEBUG, "Window id = %X", win->cfw_win);
		XGetWindowAttributes (Dm_Display, win->cfw_win, &attr);
		if (attr.override_redirect == True)
		{
		/*
		 * The shell is in override redirect mode, i.e it has
		 * NOT been reparented.  Compare the picked window with
		 * the windows we know about.
		 */
			if (wp->wp_id == win->cfw_win)
			{
				strcpy (wp->wp_name, win->cfw_name);
				return (FALSE);
			}
		}
		else
		{
		/*
		 * The shell is not in override redirect mode, i.e it has
		 * been reparented.  Compare the picked window with
		 * the parent window of the windows we know about.
		 */
			XQueryTree (Dm_Display, win->cfw_win, &root, &parent, 
				&children, &nchild);	
			if (wp->wp_id == parent)
			{
				strcpy (wp->wp_name, win->cfw_name);
				return (FALSE);
			}
		}
	}
	return (TRUE);
}
