static char *rcsid = "$Id: dm_pick.c,v 1.2 1990-12-04 16:06:09 corbet Exp $";
/*
 * Handle the window picking operation.
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
	char	*wp_sym;	/* The symbol to assign the name to. */
};


static int dm_CmpPickWin ();


void
PickWin (var)
char *var;
/*
 * Choose a window and store it into "var".
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
		ui_error ("Unable to grab pointer for pickwin");
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
/*
 * Pass through the current config, trying to find the window.
 */
	wp.wp_id = win;
	wp.wp_sym = var;
	usy_traverse (Current, dm_CmpPickWin, (long) &wp, FALSE);
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
	
	if (! (win->cfw_flags & CF_WIDGET) && wp->wp_id == win->cfw_win)
	{
		union usy_value nv;
		nv.us_v_ptr = win->cfw_name;
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), wp->wp_sym,
			SYMT_STRING, &nv);
		return (FALSE);
	}
	return (TRUE);
}
