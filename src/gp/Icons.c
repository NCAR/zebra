/*
 * Deal with the icons on the bottom of the window.
 */
static char *rcsid = "$id$";
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
# include <X11/Xaw/Cardinals.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/Label.h>	/* For now */
# include "../include/defs.h"
# include "../include/pd.h"
# include "../include/message.h"
# include "../include/DataStore.h"
# include "GraphProc.h"


# ifdef notdef
/*
 * The translations for our menubuttons.  They don't seem to work in the
 * resources file, so we'll try it here.
 *
 * (Currently not used).
 */
static char Translations[] =
	"<EnterWindow>:		highlight() \n\
	<LeaveWindow>:		reset() \n\
	<Btn1Down>:		reset()PopupMenu() \n\
	<Btn2Down>:		reset()PopupMenu() \n\
	<Btn3Down>:		reset()PopupMenu()";
XtTranslations MBTranslations;	/* The parsed version.	*/

# endif


/*
 * Icons are really MenuButton widgets, which we keep around so as to avoid
 * constantly deleting and recreating them.
 */
# define MAXNAME 40
struct IconList
{
	Widget	il_icon;		/* The actual icon	*/
	struct IconList *il_next;	/* Next list entry	*/
	/* Used in button callbacks */
	char	il_component[MAXNAME]; /* The component with this icon */
	char	il_menus[3][MAXNAME];	/* Menus to pop up. 	*/
};
static struct IconList *AvailIcons = 0;	/* Which icons are available	*/
static struct IconList *UsedIcons = 0;	/* Which are in use		*/
static struct IconList *AvailPos = 0;	/* Which position icons are available*/
static struct IconList *UsedPos = 0;	/* Which position icons are in use*/

/*
 * We want to keep the icon pixmaps around, since there aren't a whole
 * lot of them, and we'll have to load them a lot otherwise.  They live
 * in this symbol table, indexed by name.
 */
static stbl IconTable = 0;

/*
 * Forwards.
 */
# ifdef __STDC__
	static Pixmap I_GetIcon (char *, int *, int *, int);
	static struct IconList *I_PutIcon (Pixmap, int *, int, int);
	static struct IconList *I_GetWidget (struct IconList **, 
			struct IconList **);
	static void I_MenuPopup (Widget, XEvent *);
# else
	static Pixmap I_GetIcon ();
	static struct IconList *I_PutIcon ();
	static struct Iconlist *I_GetWidget ();
	static void I_MenuPopup ();
# endif


/*
 * The action procedure to attach to the menu buttons.
 */
static XtActionsRec Actions[] =
{
	{ "IconMenuPopup",	I_MenuPopup	},
};

/*
 * The height above the bottom for the icons.
 */
# define ICONHEIGHT 45


void
I_init ()
/*
 * Initialize.
 */
{
/*
 * Create the symbol table to hold the icon pixmaps.
 */
	IconTable = usy_c_stbl ("Icons");
/*
 * Add our actions.
 */
	XtAppAddActions (Actx, Actions, ONE);
# ifdef notdef
/* 
 * Set up our translations.
 */
	MBTranslations = XtParseTranslationTable (Translations);
# endif
}




static void
I_clear ()
/*
 * Clear out all of the icons that are currently in use.
 */
{
/*
 * Go through and make each one free.
 */
	while (UsedIcons)
	{
		struct IconList *ilp;
	/*
	 * Unmanage this one.
	 */
	 	XtUnmanageChild (UsedIcons->il_icon);
	/*
	 * Move it to the available list.
	 */
	 	ilp = UsedIcons;
		UsedIcons = UsedIcons->il_next;
		ilp->il_next = AvailIcons;
		AvailIcons = ilp;
	}
}


void
I_PositionIcon (name, x, y, fg)
char	*name;
int	x, y, fg;
{
	union usy_value	v;
	int		type, xh, yh, junk;
	unsigned int	w, h, ujunk;
	char		fname[120];
	Pixmap		pmap;
	Display		*disp = XtDisplay (Graphics);
	Window		root = RootWindow (disp, 0);
	Arg		args[5];
	struct IconList	*ilp;

	msg_ELog (EF_DEBUG, "Position Icon %s %d %d", name, x, y);
	if (! usy_g_symbol (IconTable, name, &type, &v))
	{
		strcpy (fname, "../lib/icons");
		strcat (fname, name);
		if (XReadBitmapFile(disp, root, fname, &w, &h, &pmap, &xh, &yh)
			!= BitmapSuccess)
		{
			msg_ELog (EF_PROBLEM, "Unable to read icon file '%s'",
				fname);
			return; 
		}
		v.us_v_ptr = (char *) pmap;
		usy_s_symbol (IconTable, name, SYMT_POINTER, &v);
	}

	pmap = (Pixmap) v.us_v_ptr;
	ilp = I_GetWidget (&AvailPos, &UsedPos);

	XGetGeometry (disp, pmap, &root, &junk, &junk, &w, &h, &ujunk, &ujunk);

	XtSetArg (args[0], XtNbitmap, pmap);
	XtSetArg (args[1], XtNx, x);
	XtSetArg (args[2], XtNy, y);
	XtSetArg (args[3], XtNforeground, fg);
	XtSetValues (ilp->il_icon, args, 4);
}


void
I_DoIcons ()
/*
 * Put in the icons.
 */
{
	int xpos = 5, comp, disable = 0;
	int width, height, fg, bg;
	char **comps = pd_CompList (Pd), platform[40], *qual = NULL;
	Pixmap icon;
	struct IconList *ilp;
/* 
 * Clear out the old ones.
 */
	msg_ELog (EF_DEBUG, "Update icons");
	I_clear ();
/*
 * Go through each component.
 */
	for (comp = 0; comps[comp]; comp++)
	{
	/*
	 * Is this one disabled?	
	 */
	 	if (! pda_Search (Pd, comps[comp], "disable", comps[comp],
				(char *) &disable, SYMT_BOOL))
			disable = FALSE;
	/*
	 * Dig up an icon.
	 */
		if (! (icon = I_GetIcon (comps[comp], &fg, &bg, disable)))
			continue;
	/*
	 * Put it onto the display
	 */
		ilp = I_PutIcon (icon, &xpos, fg, bg);
	/*
	 * If this component has a platform, we use it to qualify the menu
	 * searches.
	 */
		if (comp == 0)
			strcpy (qual = platform, "global");
	 	else if (pd_Retrieve (Pd, comps[comp], "platform", platform,
				SYMT_STRING))
			qual = platform;
	/*
	 * Now fill in the menu info.
	 */
		strcpy (ilp->il_component, comps[comp]);
		if (! pda_Search (Pd, comps[comp], "icon-left-menu",
				qual, ilp->il_menus[0], SYMT_STRING))
			strcpy (ilp->il_menus[0], "");
		if (! pda_Search (Pd, comps[comp], "icon-middle-menu",
				qual, ilp->il_menus[1], SYMT_STRING))
			strcpy (ilp->il_menus[1], "");
		if (! pda_Search (Pd, comps[comp], "icon-right-menu",
				qual, ilp->il_menus[2], SYMT_STRING))
			strcpy (ilp->il_menus[2], "");
# ifdef notdef
		msg_ELog (EF_DEBUG, "Menus: '%s', '%s', '%s'",
			ilp->il_menus[0], ilp->il_menus[1], ilp->il_menus[2]);
# endif
	}
}






static Pixmap
I_GetIcon (comp, fg, bg, disable)
char *comp;
int *fg, *bg, disable;
/*
 * Try to get the icon for this component.
 */
{
	char platform[80], iname[40], fname[120], color[40]; 
	char agelimit[10], agecolor[40], agebackground[40];
	char repr[40];
	union usy_value v;
	int type, yh, xh;
	int seconds, ntime;
	unsigned int h, w;
	time dtime, timenow;
	PlatformId pid;
	Pixmap pmap;
	Display *disp = XtDisplay (Graphics);
	Window root = RootWindow (disp, 0);
	XColor xc;
/*
 * Figure out the name of the icon to use.
 */
	if (! strcmp (comp, "global"))
		strcpy (platform, "global");
	else
		pd_Retrieve (Pd, comp, "platform", platform, SYMT_STRING);
	if (! pda_Search (Pd, comp, "icon", platform, iname, SYMT_STRING))
		return (NULL);
	if (! strcmp (iname, "none"))
		return (NULL);
/*
 * Figure out colors.
 */
	if (! pda_Search (Pd, comp,
		disable ? "disabled-icon-color" : "icon-color",
		platform, color,SYMT_STRING)) strcpy (color, "white");
	ct_GetColorByName (color, &xc);
	*fg = xc.pixel;
	if (! pda_Search (Pd, comp,
			disable? "disabled-icon-background" :"icon-background",
			 platform, color, SYMT_STRING))
		strcpy (color, "black");
	ct_GetColorByName (color, &xc);
	*bg = xc.pixel;
/*
 * If the data is too old, then change those colors.
 */
	pd_Retrieve (Pd, comp, "representation", repr, SYMT_STRING);
	if (pda_Search (Pd, comp, "icon-age-limit", platform, agelimit,
		SYMT_STRING) && (strcmp (repr, "overlay") != 0))
	{
		seconds = pc_TimeTrigger (agelimit);
		pid = ds_LookupPlatform (platform);
		if (! PlotTime.ds_hhmmss)
			tl_GetTime (&timenow);
		else
			timenow = PlotTime;
		ntime = ds_DataTimes (pid, &timenow, 1, DsBefore, &dtime);
		if ((ntime == 0) || ((TC_FccToSys (&timenow) - 
			TC_FccToSys (&dtime)) > seconds))
		{
			if (pda_Search (Pd, comp, "icon-age-color", NULL,
				agecolor, SYMT_STRING))
			{
				ct_GetColorByName (agecolor, &xc);
				*fg = xc.pixel;
			}
			if (pda_Search (Pd, comp, "icon-age-background", 
				NULL, agebackground, SYMT_STRING))
			{
				ct_GetColorByName (agebackground, &xc);
				*bg = xc.pixel;
			}
		}
	}
/*
 * If this icon already exists in our table, we can just return it.
 */
	if (usy_g_symbol (IconTable, iname, &type, &v))
		return ((Pixmap) v.us_v_ptr);
/*
 * Otherwise we gotta go dig it up.
 */
	strcpy (fname, "../lib/icons/");	/* XXX */
	strcat (fname, iname);
	if (XReadBitmapFile (disp, root, fname, &w, &h, &pmap, &xh, &yh) !=
		BitmapSuccess)
	{
		msg_ELog (EF_PROBLEM, "Unable to read icon file '%s'", fname);
		return (NULL);
	}
/*
 * Remember this one.
 */
	v.us_v_ptr = (char *) pmap;
	usy_s_symbol (IconTable, iname, SYMT_POINTER, &v);
	return (pmap);
}






static struct IconList *
I_PutIcon (icon, xpos, fg, bg)
Pixmap icon;
int *xpos, fg, bg;
/*
 * Add this icon to the display.
 */
{
	int gh = GWHeight (Graphics), junk;
	unsigned int width, height;
	Arg args[5];
	struct IconList *ilp;
	Window root;
/*
 * We need a label widget to put this in.
 */
	ilp = I_GetWidget (&AvailIcons, &UsedIcons);
/*
 * Figure out the dimensions of this pixmap.
 */
	XGetGeometry (XtDisplay (Graphics), icon, &root, &junk, &junk,
		&width, &height, (unsigned int *) &junk,
		(unsigned int *) &junk);
/*
 * Store all the necessary values.
 */
	XtSetArg (args[0], XtNbitmap, icon);
	XtSetArg (args[1], XtNx, *xpos);
	XtSetArg (args[2], XtNy, gh - height - 10);
	XtSetArg (args[3], XtNforeground, fg);
	XtSetArg (args[4], XtNbackground, bg);
	XtSetValues (ilp->il_icon, args, FIVE);
/*
 * Fix the X position.
 */
	*xpos += 7 + width;
	return (ilp);
}





static struct IconList *
I_GetWidget (avail, used)
struct IconList	**avail, **used;
/*
 * Get a widget.
 */
{
	Arg args[5];
	struct IconList *ilp;
	int n;
/*
 * Return one off the available list if possible.
 */
	if (*avail)
	{
		ilp = *avail;
		*avail = ilp->il_next;
		XtManageChild (ilp->il_icon);
	}
/*
 * Otherwise allocate a new one.
 */
	else
	{
		ilp = ALLOC (struct IconList);
		n = 0;
		XtSetArg (args[n], XtNinternalHeight, 3);	n++;
		XtSetArg (args[n], XtNinternalWidth, 2);	n++;
		XtSetArg (args[n], XtNborderWidth, 1);		n++;
		XtSetArg (args[n], XtNmenuName, "iconmenu");	n++;
		XtSetArg (args[n], XtNsensitive, True);		n++;
		ilp->il_icon = XtCreateManagedWidget ("icon",
			menuButtonWidgetClass, Graphics, args, n);
		XtRealizeWidget (ilp->il_icon);
	}
/*
 * Put it onto the used list, and return it.
 */
	/* XtOverrideTranslations (ilp->il_icon, MBTranslations); */
	ilp->il_next = *used;
	*used = ilp;
	return (ilp);
}





static void
I_MenuPopup (w, ev)
Widget w;
XEvent *ev;
/*
 * Deal with a button event.
 */
{
	char *menu;
	struct IconList *ilp;
	union usy_value v;
/*
 * First, we need to find the icon that generated this event.
 */
	for (ilp = UsedIcons; ilp; ilp = ilp->il_next)
		if (ilp->il_icon == w)
			break;
	if (! ilp)
	{
		msg_ELog (EF_PROBLEM, "Weird button event on 0x%x", w);
		return;
	}
/*
 * Find the menu to pop up.
 */
	menu = ilp->il_menus[ev->xbutton.button - 1];
	if (! *menu)
		return;
/*
 * Store the component name where the action procedures can find it.
 */
	v.us_v_ptr = ilp->il_component;
	usy_s_symbol (Vtable, "icon_component", SYMT_STRING, &v);
/*
 * Throw it up onto the screen, and let it handle things from here.
 */
	if (strcmp (menu, "DataAvailable"))
		uw_IWRealize (menu, Graphics);
	XtCallActionProc (w, "XawPositionSimpleMenu", ev, &menu, 1);
	XtCallActionProc (w, "MenuPopup", ev, &menu, 1);
}
