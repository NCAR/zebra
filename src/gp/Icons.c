static char *rcsid = "$id$";
/*
 * Deal with the icons on the bottom of the window.
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
	static Pixmap I_GetIcon (char *, int *, int *);
	static struct IconList *I_PutIcon (Pixmap, int *, int, int);
	static struct IconList *I_GetWidget (void);
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
	 * No icon for disabled components.
	 */
	 	if (pda_Search (Pd, comps[comp], "disable", comps[comp],
				(char *) &disable, SYMT_BOOL) && disable)
			continue;
	/*
	 * Dig up an icon.
	 */
		if (! (icon = I_GetIcon (comps[comp], &fg, &bg)))
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
I_GetIcon (comp, fg, bg)
char *comp;
int *fg, *bg;
/*
 * Try to get the icon for this component.
 */
{
	char platform[80], iname[40], fname[120], color[40];
	union usy_value v;
	int type, yh, xh;
	unsigned int h, w;
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
	if (! pda_Search (Pd, comp, "icon-color", platform, color,SYMT_STRING))
		strcpy (color, "white");
	ct_GetColorByName (color, &xc);
	*fg = xc.pixel;
	if (! pda_Search (Pd, comp, "icon-background", platform, color,
			SYMT_STRING))
		strcpy (color, "black");
	ct_GetColorByName (color, &xc);
	*bg = xc.pixel;
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
	ilp = I_GetWidget ();
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
I_GetWidget ()
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
	if (AvailIcons)
	{
		ilp = AvailIcons;
		AvailIcons = ilp->il_next;
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
	ilp->il_next = UsedIcons;
	UsedIcons = ilp;
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
	uw_IWRealize (menu, Graphics);
	XtCallActionProc (w, "XawPositionSimpleMenu", ev, &menu, 1);
	XtCallActionProc (w, "MenuPopup", ev, &menu, 1);
}
