/*
 * Deal with the icons on the bottom of the window.
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
# include <X11/Intrinsic.h>
# include <X11/IntrinsicP.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/Label.h>	/* For now */
# include <X11/extensions/shape.h>
# include <ui_error.h>

# include <config.h>
# include <defs.h>
# include <pd.h>
# include <message.h>
# include <timer.h>
# include <DataStore.h>
# include <GraphicsW.h>

RCSID("$Id: Icons.c,v 2.26 1995-06-29 23:28:47 granger Exp $")

# include "GraphProc.h"
# include "ActiveArea.h"


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
struct IconData
{
	char	id_platform[40];		/* Platform name	*/
	date	id_time;			/* Data time		*/
};

struct IconList
{
	Widget	il_icon;		/* The actual icon		*/
	struct IconList *il_next;	/* Next list entry		*/
	/* Used in button callbacks */
	char	il_component[MAXNAME];	/* The component with this icon */
	char	il_menus[3][MAXNAME];	/* Menus to pop up. 		*/
	struct IconData	*il_data;	/* Pointer to IconData struct	*/
};
static struct IconList *AvailIcons = NULL; /* Which icons are available	*/
static struct IconList *UsedIcons = NULL; /* Which are in use		*/
static struct IconList *AvailPos = NULL; /*Which position icons are available*/
static struct IconList *UsedPos = NULL;	/* Which position icons are in use*/

/*
 * We want to keep the icon pixmaps around, since there aren't a whole
 * lot of them, and we'll have to load them a lot otherwise.  They live
 * in this symbol table, indexed by name.
 */
static stbl IconTable = 0;

/*
 * Forwards.
 */
static Pixmap 	I_GetIcon FP ((char *, int *, int *, int));
static struct 	IconList *I_PutIcon FP ((Pixmap, int *, int, int));
static struct 	IconList *I_GetWidget FP ((struct IconList **,
			struct IconList **));
static void 	I_MenuPopup FP ((Widget, XEvent *, String *, Cardinal *));
static void	I_Clear FP ((struct IconList **, struct IconList **));
void		I_ClearPosIcons FP (());
static void	I_AAButton FP ((ActiveArea *, XEvent *));


/*
 * The action procedure to attach to the menu buttons.
 */
static XtActionsRec Actions[] =
{
	{ "IconMenuPopup",	I_MenuPopup	},
	{ "IconMenuSetup",	I_MenuPopup	},
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
	XtAppAddActions (Actx, Actions, XtNumber(Actions));
	XtRegisterGrabAction (I_MenuPopup, True,
		ButtonPressMask|ButtonReleaseMask, GrabModeAsync,
		GrabModeAsync);
}




static void
I_Clear (avail, used)
struct IconList	**avail, **used;
/*
 * Clear out all of the icons that are currently in use.
 */
{
/*
 * Go through and make each one free.
 */
	while (*used)
	{
		struct IconList *ilp;
	/*
	 * Unmanage this one.
	 */
	 	XtUnmanageChild ((*used)->il_icon);
	/*
	 * Move it to the available list.
	 */
	 	ilp = *used;
		*used = (*used)->il_next;
		ilp->il_next = *avail;
		*avail = ilp;
	}
}


void
I_ClearPosIcons ()
/*
 * Clear the position icons.
 */
{
	I_Clear (&AvailPos, &UsedPos);
}





void
I_PositionIcon (comp, platform, zt, name, x, y, fg)
char	*comp, *platform;
ZebTime	*zt;
char	*name;
int	x, y, fg;
/*
 * Place a position icon named 'name' at the location (x,y), color fg.
 */
{
	int xh, yh, w, h;
	Pixmap pmap = I_GetPMap (name, &xh, &yh, &w, &h);
	XGCValues vals;
	bool active = FALSE;
/*
 * Bail out if we didn't get a good pixmap
 */
	if (! pmap)
		return;
/*
 * Back off to put the hotspot on the requested place.
 */
	x -= xh;
	y -= yh;
/*
 * Write the icon into the display frame.
 */
	SetClip (TRUE);
	vals.foreground = fg;
	vals.fill_style = FillStippled;
	vals.stipple = pmap;
	XChangeGC (Disp, Gcontext, GCForeground|GCFillStyle|GCStipple, &vals); 
	XSetTSOrigin (Disp, Gcontext, x, y);
	XFillRectangle (Disp, GWFrame (Graphics), Gcontext, x, y, w, h);
	ResetGC ();
/*
 * See if they want this to be an active area, and set it up if so.
 */
	if (pda_Search (Pd, comp, "active-icon", platform, &active, SYMT_BOOL)
	    && active)
		I_ActivateArea (x, y, w, h, "posicon", comp, platform, 0);
}




void
I_ActivateArea (x, y, w, h, type, comp, plat, something)
int x, y, w, h;
char *type, *comp, *plat, *something;
/*
 * Create an active area under management of the icon module.  The SOMETHING
 * parameter is reserved for future use (will be probably some sort of
 * action type) and should be passed as zero for now.
 */
{
	aa_AddArea (x, y, w, h, type, comp, plat, 0, I_AAButton);
}




static void
I_AAButton (area, ev)
ActiveArea *area;
XEvent *ev;
/*
 * Somebody has poked on this area.
 */
{
	static char *bnames[3] = { "left", "middle", "right" };
	char pname[32], menu[64];
	char *mp = menu; /* kludge */
	SValue v;
	Arg arg;
/*
 * Look up a menu for this button.
 */
	sprintf (pname, "%s-%s-menu", area->aa_type,
		 bnames[ev->xbutton.button - 1]);
	if (! pda_Search (Pd, area->aa_comp, pname, area->aa_plat, menu,
			  SYMT_STRING))
		return;
/*
 * Tweak variable values.
 */
	v.us_v_ptr = area->aa_comp;
	usy_s_symbol (Vtable, "icon_component", SYMT_STRING, &v);
	v.us_v_ptr = area->aa_type;
	usy_s_symbol (Vtable, "area_type", SYMT_STRING, &v);
	v.us_v_int = ! strcmp (area->aa_type, "posicon"); /* back compat */
	usy_s_symbol (Vtable, "position_icon", SYMT_BOOL, &v);
	v.us_v_ptr = area->aa_plat;
	usy_s_symbol (Vtable, "icon_platform", SYMT_STRING, &v);
/*
 * Throw it up onto the screen, and let it handle things from here.
 */
	if (strcmp (menu, "DataAvailable") && strcmp (menu, "FieldMenu"))
		uw_IWRealize (menu, Graphics);
/*
 * Specify the menu name in the MenuButton's menuName resource so that
 * further actions can find it
 */
	XtSetArg (arg, XtNmenuName, menu);
	XtSetValues (Graphics, &arg, (Cardinal)1 );
/*
 * We have to explicitly call the actions from here since attempting to
 * popup the menu is conditional on whether a menu was found for this
 * menu for the particular button.  Hence the actions cannot be 
 * unconditionally called via the <BtnDn> translation.
 */
	XtCallActionProc (Graphics, "PositionAndPopupRdssMenu", ev, &mp, 1);
}








void
I_DoIcons ()
/*
 * Put in the icons.
 */
{
	int xpos = 5, comp;
	int fg, bg;
	char **comps = pd_CompList (Pd), *qual = NULL;
	char platform[PlatformListLen];
	bool disable = FALSE;
	Pixmap icon;
	struct IconList *ilp;
/* 
 * Clear out the old ones.
 */
	msg_ELog (EF_DEBUG, "Update icons");
	I_Clear (&AvailIcons, &UsedIcons);
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
	char platform[PlatformListLen];
	char iname[40], color[40]; 
	XColor xc;
	int xh, yh, w, h;
/*
 * Figure out the name of the icon to use.
 */
	if (! strcmp (comp, "global"))
		strcpy (platform, "global");
	else
		pd_Retrieve (Pd, comp, "platform", platform, SYMT_STRING);
	if (! pda_Search (Pd, comp, "icon", platform, iname, SYMT_STRING))
		return (0);
	if (! strcmp (iname, "none"))
		return (0);
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
 * Finally, dig up the pixmap.
 */
	return I_GetPMap (iname, &xh, &yh, &w, &h);
}



Pixmap
I_GetPMap (iname, xh, yh, w, h)
char *iname;
int *xh, *yh, *w, *h;
/*
 * Return the pixmap corresponding to this name.
 */
{
	union usy_value v;
	int type;
	Pixmap pmap;
	Window root = RootWindow (Disp, 0);
	char fname[120];
	struct iconinfo
	{
		Pixmap ic_pmap;
		int ic_xh, ic_yh, ic_w, ic_h;
	} *icinfo;
/*
 * If this icon already exists in our table, we can just return it.
 */
	if (usy_g_symbol (IconTable, iname, &type, &v))
	{
		icinfo = (struct iconinfo *) v.us_v_ptr;
		*xh = icinfo->ic_xh;
		*yh = icinfo->ic_yh;
		*w = icinfo->ic_w;
		*h = icinfo->ic_h;
		return (icinfo->ic_pmap);
	}
/*
 * Otherwise we gotta go dig it up.
 */
	if (! FindFile (iname, IconPath, fname) || XReadBitmapFile (Disp,
		 root, fname, (unsigned int *) w, (unsigned int *) h, &pmap,
		  xh, yh) != BitmapSuccess)
	{
		msg_ELog (EF_PROBLEM, "Unable to load icon  '%s'", iname);
		return (0);
	}
/*
 * Remember this one.
 */
	icinfo = ALLOC (struct iconinfo);
	icinfo->ic_xh = *xh;
	icinfo->ic_yh = *yh;
	icinfo->ic_w = *w;
	icinfo->ic_h = *h;
	icinfo->ic_pmap = pmap;
	v.us_v_ptr = (char *) icinfo;
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
	ilp->il_data = NULL;
	ilp->il_next = *used;
	*used = ilp;
	return (ilp);
}




void
I_RedirectButton (win, ev)
Window win;
XEvent *ev;
/*
 * Try to redirect an event to an icon.
 */
{
	struct IconList *ilp;
/*
 * Search the lists to see what we can find.  This is inefficient in that
 * I_MenuPopup will simply repeat the search, but we can worry about that
 * after this kludge works.
 */
	for (ilp = UsedIcons; ilp; ilp = ilp->il_next)
		if (XtWindow (ilp->il_icon) == win)
		{
			I_MenuPopup (ilp->il_icon, ev, 0, 0);
			return;
		}
	for (ilp = UsedPos; ilp; ilp = ilp->il_next)
		if (XtWindow (ilp->il_icon) == win)
		{
			I_MenuPopup (ilp->il_icon, ev, 0, 0);
			return;
		}
}





/* ARGSUSED */
static void
I_MenuPopup (w, ev, stringjunk, cardjunk)
Widget w;				/* Should be the MenuButton 	*/
XEvent *ev;				/* The button down event	*/
String *stringjunk;
Cardinal *cardjunk;
/*
 * Deal with a button event.
 */
{
	char *menu;
	struct IconList *ilp;
	union usy_value v;
	Arg arg;
	bool posicon = FALSE;
/*
 * First, we need to find the icon that generated this event.
 */
	for (ilp = UsedIcons; ilp; ilp = ilp->il_next)
		if (ilp->il_icon == w)
			break;

	if (! ilp)
		for (ilp = UsedPos; ilp; ilp = ilp->il_next)
			if (ilp->il_icon == w)
			{
				posicon = TRUE;
				break;
			}
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
	v.us_v_int = posicon;
	usy_s_symbol (Vtable, "position_icon", SYMT_BOOL, &v);
	v.us_v_ptr = posicon ? "posicon" : "icon";
	usy_s_symbol (Vtable, "area_type", SYMT_STRING, &v);
	if (ilp->il_data != NULL)
	{
		v.us_v_ptr = ilp->il_data->id_platform;
		usy_s_symbol (Vtable, "icon_platform", SYMT_STRING, &v);
		v.us_v_date = ilp->il_data->id_time;
		usy_s_symbol (Vtable, "icon_time", SYMT_DATE, &v);
	}
/*
 * Throw it up onto the screen, and let it handle things from here.
 */
	ERRORCATCH
	if (strcmp (menu, "DataAvailable") && strcmp (menu, "FieldMenu"))
		uw_IWRealize (menu, Graphics);
	ENDCATCH
/*
 * Specify the menu name in the MenuButton's menuName resource so that
 * further actions can find it
 */
	XtSetArg (arg, XtNmenuName, menu);
	XtSetValues (w, &arg, (Cardinal)1 );
/*
 * We have to explicitly call the actions from here since attempting to
 * popup the menu is conditional on whether a menu was found for this
 * particular button.  Hence the actions cannot be unconditionally called
 * via the <BtnDn> translation.
 */
	XtCallActionProc (w, "PositionAndPopupRdssMenu", ev, &menu, 1);

#ifdef notdef
	XtCallActionProc (w, "XawPositionSimpleMenu", ev, &menu, 1);
	XtCallActionProc (w, "MenuPopup", ev, &menu, 1);
#endif
}



void
I_RepositionMenu (w)
Widget w;
/*
 * Get the current size and location of the popup shell widget and make
 * sure the entire widget appears on the screen (or as much as possible
 * anyway).  Since we could have already been moved based on an old size,
 * we have to start all over with positioning the menu based on the
 * pointer location.  This is a utility function called by popup callbacks
 * for menus like FieldMenu and DataMenu.  
 *
 * This fixes the old bug of FieldMenus not popping up completely on the
 * screen the first time. This could also be done by calling the popup
 * callbacks before calling the popup-and-position action in I_MenuPopup(),
 * so that the menu shell figures its new size before getting positioned.
 * That approach might produce less flashing on the screen when the window
 * resizes before realizing the shell.  However, using this function allows
 * the FieldMenu and DataMenu source to be more self-contained and generic.
 */
{
	int root_x, root_y, win_x, win_y;
	Window root, child;
	unsigned int mask;
	Position x, y;
	Dimension width, height, sw, sh;
	Dimension border;
	Arg args[10];
	Cardinal n;

	XQueryPointer (XtDisplay(w), XtWindow(w), &root, &child,
		       &root_x, &root_y, &win_x, &win_y, &mask);
	/*
	 * Default to popping up with pointer over top left corner
	 */
	x = root_x - 5;
	y = root_y - 5;

	n = 0;
	XtSetArg (args[n], XtNwidth, &width); ++n;
	XtSetArg (args[n], XtNheight, &height); ++n;
	XtSetArg (args[n], XtNborderWidth, &border); ++n;
	XtGetValues (w, args, n);

	sw = WidthOfScreen (XtScreen(w));
	sh = HeightOfScreen (XtScreen(w));
	if (x - 5 + width + 2*border > (unsigned) sw)
		x -= width + 2*border;
	if (y - 5 + height + 2*border > (unsigned) sh)
		y = sh - height - 2*border;

	/*
	 * Send the widget its new location.  If it hasn't changed,
	 * it will ignore this.
	 */
	n = 0;
	XtSetArg (args[n], XtNx, x); ++n;
	XtSetArg (args[n], XtNy, y); ++n;
	XtSetValues (w, args, n);
}



void
I_ColorIcons (colorcomp)
char *colorcomp;
/*
 * Put in the icons.
 */
{
        int fg, bg, seconds, ntime, update;
	bool disable;
	ZebTime timenow, datatime;
        char comp[40], repr[40];
	char platform[PlatformListLen];
	char agelimit[40], color[40];
	PlatformId pid;
	Arg args[2];
	XColor xc;
        struct IconList *ilp;
/*
 * Get default colors just in case.
 */
	ct_GetColorByName ("white", &xc);
	fg = xc.pixel;
	ct_GetColorByName ("black", &xc);
	bg = xc.pixel;
/*
 * Is this an update or global plot.
 */
	if (strcmp (colorcomp, "global") == 0)
		update = FALSE;
	else update = TRUE;
/*
 * Go through each icon.
 */
	ilp = UsedIcons;	
	while (ilp != NULL)
        {
	/*
	 * Don't bother with the global component.
	 */
		strcpy (comp, ilp->il_component);
		if (strcmp (comp, "global") == 0)
		{
			ilp = ilp->il_next;
			continue;
		}
	/*
	 * If its an update check for matching components, otherwise
	 * don't bother.
	 */
		if (update && (strcmp (comp, colorcomp) != 0))
		{
			ilp = ilp->il_next;
			continue;
		}
        /*
         * Is this one disabled?  Then don't bother.
         */
		disable = FALSE;
                pda_Search (Pd, comp, "disable", comp, (char *) &disable, 
			SYMT_BOOL);
		if (disable)
		{
			ilp = ilp->il_next;
			continue;
		}
	/*
	 * Get the platform name and representation.
	 */
	 	pda_ReqSearch (Pd, comp, "platform", NULL, platform,
			SYMT_STRING);
        	pda_ReqSearch (Pd, comp, "representation", NULL, repr,
			SYMT_STRING);
	/*
	 * See if this platform has an age limit and convert it to seconds. 
	 */
	        if (pda_Search (Pd, comp, "icon-age-limit", platform, agelimit,
                	SYMT_STRING) && (strcmp (repr, "overlay") != 0))
        	{
			seconds = pc_TimeTrigger (agelimit); 
		/*
		 * Plot time
		 */
			if ((pid = ds_LookupPlatform(platform)) == BadPlatform)
			{
				ilp = ilp->il_next;
				continue;	
			}		
			if (! PlotTime.zt_Sec) 
				tl_Time (&timenow); 
			else
                       		timenow = PlotTime;
		/*
		 * Model nastiness: For model data in validation mode, we
		 * need to tweak the data request time.
		 */
			if (ds_IsModelPlatform (pid) && ValidationMode)
				timenow.zt_Sec -= ForecastOffset;
		/*
		 * Get the time for available data
		 */
               		ntime = ds_DataTimes (pid, &timenow, 1, DsBefore, 
				&datatime);
		/*
		 * If the data is old then color it.
		 */
               		if ((ntime == 0) || ((timenow.zt_Sec - datatime.zt_Sec)					 > seconds))
               		{
                       		if (pda_Search (Pd, comp, 
						"icon-age-foreground",
						NULL, color, SYMT_STRING))
                       		{
                               		ct_GetColorByName (color, &xc);
					fg = xc.pixel;
                       		}
        			else if (pda_Search (Pd, comp, "icon-color",
						     platform, color, 
						     SYMT_STRING)) 
				{
        				ct_GetColorByName (color, &xc);
        				fg = xc.pixel;
				}

                       		if (pda_Search (Pd, comp, 
						"icon-age-background",
						NULL, color, SYMT_STRING))
                       		{
                          		ct_GetColorByName (color, &xc);
					bg = xc.pixel;
                       		}
        			else if (pda_Search (Pd, comp, 
						     "icon-background", 
						     platform, color, 
						     SYMT_STRING))
				{
        				ct_GetColorByName (color, &xc);
        				bg = xc.pixel;
				}
               		}
		/*
		 * If the data isn't old color it the regular colors.
		 */
			else
			{
        			if (pda_Search (Pd, comp, "icon-color",
						platform, color, SYMT_STRING)) 
				{
        				ct_GetColorByName (color, &xc);
        				fg = xc.pixel;
				}
        			if (pda_Search (Pd, comp, "icon-background",
						platform, color, SYMT_STRING))
				{
        				ct_GetColorByName (color, &xc);
        				bg = xc.pixel;
				}
			}
		/*
		 * Set the resoures in the widget.
		 */
			XtSetArg (args[0], XtNforeground, fg);
			XtSetArg (args[1], XtNbackground, bg);
			XtSetValues (ilp->il_icon, args, TWO);
       		}
		ilp = ilp->il_next;
	}
}

