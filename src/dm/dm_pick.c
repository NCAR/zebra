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
# include <string.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/cursorfont.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>

# include <ui.h>
# include <defs.h>
# include <dm.h>
# include "dm_vars.h"

RCSID("$Id: dm_pick.c,v 2.17 2001-01-08 22:16:50 granger Exp $")

/*
 * This structure is used to communicate through usy_traverse.
 */
struct wpick
{
	int	wp_found;	/* A dm window was found */
	Window	wp_id;		/* The id of the picked window	*/
	char	wp_name[40];	/* The window name. */
};


static int dm_CmpPickWin ();


static Cursor PickCursor ()
{
	Pixel fg;
	Pixel bg;
	Pixmap pmap = None;
	XColor fgcolor, bgcolor;
	Cursor cursor = None;
	Arg args[10];
	int n;

	/* See if we have a widget context from ui. */
	Widget button = uw_get_menubutton();
	if (button)
	{
	    n = 0;
	    XtSetArg (args[n], XtNforeground, &fg); n++;
	    XtSetArg (args[n], XtNbackground, &bg); n++;
	    XtSetArg (args[n], XtNbitmap, &pmap); n++;
	    XtGetValues (button, args, n);
	}

	if (pmap != None)
	{
	    Colormap cmap = DefaultColormap (Dm_Display, 0);
	    fgcolor.pixel = fg;
	    bgcolor.pixel = bg;
	    XQueryColor (Dm_Display, cmap, &fgcolor);
	    XQueryColor (Dm_Display, cmap, &bgcolor);
	    cursor = XCreatePixmapCursor (Dm_Display, pmap, None,
					  &fgcolor, &bgcolor, 0, 0);
	}

	if (cursor == None)
	{
	    cursor = XCreateFontCursor (Dm_Display, XC_crosshair);
	}
	return cursor;
}



int
PickWin (winname)
char *winname;
/*
 * Choose a window and store its name into "winname".  Return nonzero
 * if successful, otherwise return zero and leave winname unchanged.
 */
{
	int status, done = False;
	Cursor cursor;
	XEvent event;
	Window win = None;
	Window root = RootWindow (Dm_Display, 0);	/* xxx */
	struct wpick wp;
/*
 * Make the special cursor.
 */
	cursor = PickCursor();
/*
 * Reach out and snarf up the pointer.
 */
	status = XGrabPointer (Dm_Display, root, False,
		ButtonPressMask | ButtonReleaseMask, GrabModeSync,
		GrabModeAsync, None, cursor, CurrentTime);
	if (status != GrabSuccess)
	{
		msg_ELog (EF_PROBLEM, "Unable to grab pointer for pick");
		return (FALSE);
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
			win = event.xbutton.subwindow;
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
	msg_ELog (EF_DEBUG, "Pick got win 0x%x", win);
/* 
 * Make sure the window chosen was not the root window,
 * in which case there is no subwindow and we just return.
 */
	if (win == None)
		return (FALSE);
/*
 * Initialize the search
 */
	wp.wp_found = FALSE;
/*
 * Pass through the current config, trying to find the window.  Don't copy
 * the returned window name unless the search was successful.
 */
	wp.wp_id = win;
	usy_traverse (Current, dm_CmpPickWin, (long) &wp, FALSE);
	if (wp.wp_found)
	{
		strcpy (winname, wp.wp_name);
		return (TRUE);
	}
	return (FALSE);
}





static int
dm_CmpPickWin (name, type, v, wp)
char *name;
int type;
union usy_value *v;
struct wpick *wp;
/*
 * Check out this window.  The *wp structure must NOT be modified
 * unless a match is found, since it contains a default value for PickWin()
 */
{
	struct cf_window *win = (struct cf_window *) v->us_v_ptr;
	Process *proc = win->cfw_process;
	Window root, parent, *children = NULL;
	unsigned int nchild = 0;
	
	if (! (IsWidget (win)) && (proc) && (proc->p_win != 0))
	{
		msg_ELog (EF_DEBUG, "Window name = %s", win->cfw_name);
		msg_ELog (EF_DEBUG, "Window id = %X", proc->p_win);
	/*
	 * We used to check for the shell being in override redirect mode, 
	 * i.e it has NOT been reparented, but this seems not to be necessary.
	 * Do compare the picked window with the windows we know about.
	 */
		msg_ELog (EF_DEBUG, "OR case");
		if (wp->wp_id == proc->p_win)
		{
			strcpy (wp->wp_name, win->cfw_name);
			wp->wp_found = TRUE;
			return (FALSE);
		}
	/*
	 * In most window managers the window has been reparented. 
	 * Compare the picked window with the parent window of the windows 
	 * we know about.
	 */
		XQueryTree (Dm_Display, proc->p_win, &root, &parent, 
			&children, &nchild);
		msg_ELog (EF_DEBUG, "NOR, parent is 0x%x", parent);
	/*
	 * Don't need the children, so free them, if any
	 */
		if (nchild)
			XFree((void *)children);
		if (wp->wp_id == parent)
		{
			strcpy (wp->wp_name, win->cfw_name);
			wp->wp_found = TRUE;
			return (FALSE);
		}
	/*
	 * failing that, do it all one more time, since mwm likes
	 * to put two levels of reparenting in...
	 */
		XQueryTree (Dm_Display, parent, &root, &parent, 
			&children, &nchild);
		msg_ELog (EF_DEBUG, "NOR, parent2 is 0x%x", parent);
		if (nchild)
			XFree((void *)children);
		if (wp->wp_id == parent && parent != root)
		{
			strcpy (wp->wp_name, win->cfw_name);
			wp->wp_found = TRUE;
			return (FALSE);
		}

	}
	return (TRUE);
}
