/*
 * Windowing code for pulldown menu widgets.
 */

# ifdef XSUPPORT


static char *rcsid = "$Id: ui_wPulldown.c,v 1.4 1990-08-28 08:57:47 corbet Exp $";

# ifndef X11R3		/* This stuff don't work under R3.	*/
/* 
 * Window system code.
 */
# include <X11/X.h>
# include <X11/Xlib.h>
# include <X11/cursorfont.h>
# include <X11/Intrinsic.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>

# include <X11/Xaw/Box.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/SmeLine.h>

# include "ui.h"
# include "ui_param.h"
# include "ui_globals.h"
# include "ui_expr.h"
# include "ui_mode.h"
# include "ui_commands.h"
# include "ui_window.h"
# include "ui_error.h"
# include "ui_loadfile.h"

/*
 * The "menubar" widget.
 */
struct menubar_widget
{
	int	mw_type;	/* = WT_MENUBAR				*/
	struct gen_widget *mw_next;	/* Next widget in the chain	*/
	void (*mw_create)();	/* The routine to create this thing	*/
	void (*mw_popup) ();	/* Popup routine			*/
	void (*mw_destroy) ();	/* The destroy method			*/
	struct frame_widget *mw_frame;	/* The associated frame		*/
	Widget mw_w;		/* The actual popup widget structure	*/
	/* -- end of gen_widget stuff */
	int	mw_nmenus;	/* The number of menus in this bar	*/
	struct mb_menu *mw_menus; /* The actual menus			*/
};


/*
 * This structure holds an actual pulldown menu.
 */
# define MAX_ENTRY	60	/* XXX Maximum # of entries		*/
struct mb_menu
{
	int	mbm_type;		/* = WT_INTPOPUP		*/
	struct gen_widget *mbm_nxt;	/* Next widget in the chain	*/
	void (*mbm_create)();	/* The routine to create this thing	*/
	void (*mbm_popup) ();	/* Popup routine			*/
	void (*mbm_destroy) ();	/* The destroy method			*/
	struct frame_widget *mw_frame;	/* The associated frame		*/
	Widget	mbm_pulldown;		/* The actual pulldown		*/
	/* -- end of gen_widget stuff */
	char	mbm_name[MAXTITLE];	/* The name of the menu		*/
	Widget	mbm_button;		/* The menu button		*/
	int	mbm_nentries;		/* The number of entries.	*/
	char	*mbm_etext[MAX_ENTRY];	/* The text of each entry	*/
	char	*mbm_eact[MAX_ENTRY];	/* The associated action command */
	Widget	mbm_ewidget[MAX_ENTRY];	/* The smeBSB widget		*/
	char	*mbm_eexpr[MAX_ENTRY];	/* Mark expression		*/
	bool	mbm_eval[MAX_ENTRY];	/* Last expr value		*/
	struct mb_menu *mbm_next;	/* The next entry in the chain	*/
	struct map_table *mbm_map;	/* The mapping table		*/
	int	mbm_nmap;		/* Number of map table entries	*/
	char	*mbm_selector;		/* Selector variable		*/
	int	mbm_esel;		/* Currently selected entry	*/
	bool	mbm_oom;		/* one-of-many selection	*/
	bool	mbm_expr;		/* There are mark expressions	*/
	bool	mbm_pixmap;		/* There is a pixmap 		*/
};

/*
 * Forward routines.
 */
# ifdef __STDC__
	static void uw_mbmcreate (struct mb_menu *menu, Widget parent);
	static Pixmap uw_mbmPixmap (char *);
# else
	static void uw_mbmcreate ();
	static Pixmap uw_mbmPixmap ();
# endif




struct gen_widget *
uw_mb_def ()
/*
 * Interactively define a new menubar widget.
 */
{
	struct menubar_widget *new;
	int uw_in_menubar ();
	void uw_mbcreate (), uw_mbdestroy ();
/*
 * Put together a widget structure.
 */
	new = NEW (struct menubar_widget);
	new->mw_type = WT_MENUBAR;
	new->mw_nmenus = 0;
	new->mw_menus = (struct mb_menu *) 0;
	new->mw_create = uw_mbcreate;
	new->mw_destroy = uw_mbdestroy;
	new->mw_popup = 0;
/*
 * Read in each pulldown.
 */
	ERRORCATCH
		ui_subcommand ("ust$in-menubar", "Menubar>", uw_in_menubar,
				new);
	ON_ERROR
		relvm (new);	/* What about pulldowns? */
		RESIGNAL;
	ENDCATCH
/*
 * Done.
 */
	return ((struct gen_widget *) new);
}






int
uw_in_menubar (mb, cmds)
struct menubar_widget *mb;
struct ui_command *cmds;
/*
 * Define a pulldown menu.
 */
{
	struct mb_menu *menu = NEW (struct mb_menu), *list;
	int uw_in_mbentry ();
/*
 * If this is an ENDDEF, we're done.
 */
	if (UKEY (cmds[0]) == UIC_ENDDEF)
	{
		relvm (menu);
		return (FALSE);
	}
	cmds++;
/*
 * If there is a keyword here, we want a pixmap.
 */
	if ((menu->mbm_pixmap = cmds[0].uc_ctype) == UTT_KW)
		cmds++;
/*
 * Initialize the menu structure, and add it to the list.
 */
 	menu->mbm_nmap = menu->mbm_nentries = 0;
	menu->mbm_expr = FALSE;
	menu->mbm_next = 0;
	menu->mbm_type = WT_MENUBUTTON;
	strcpy (menu->mbm_name, UPTR (*cmds));
	if (mb->mw_nmenus++ == 0)
		mb->mw_menus = menu;
	else
	{
		for (list = mb->mw_menus; list->mbm_next;
				list = list->mbm_next)
			;
		list->mbm_next = menu;
	}
	cmds++;
/*
 * If this is a one-of-many menu, grab also the selector.
 */
	if (menu->mbm_oom = (cmds[2].uc_ctype != UTT_END))
		menu->mbm_selector = usy_pstring (UPTR (cmds[2]));
/*
 * Deal with the actual entries now.
 */
	/* errorcatch? */
	ui_subcommand ("ust$in-mb-entry", "MenuEntry>", uw_in_mbentry, menu);
	return (TRUE);
}





struct gen_widget *
uw_DefIPU (name)
char *name;
/*
 * Define an internal popup menu widget.
 */
{
	struct mb_menu *menu = NEW (struct mb_menu);
	int uw_in_mbentry ();
/*
 * Initialize the menu structure.
 */
 	menu->mbm_nmap = menu->mbm_nentries = 0;
	menu->mbm_expr = menu->mbm_oom = FALSE;
	menu->mbm_next = 0;
	menu->mbm_type = WT_INTPOPUP;
	menu->mbm_create = uw_mbmcreate;
	strcpy (menu->mbm_name, name);
/*
 * Deal with the actual entries now.
 */
	/* errorcatch? */
	ui_subcommand ("ust$in-mb-entry", "MenuEntry>", uw_in_mbentry, menu);
	return ((struct gen_widget *) menu);
}




int
uw_in_mbentry (menu, cmds)
struct mb_menu *menu;
struct ui_command *cmds;
/*
 * Deal with the actual menubar entries.
 */
{
	struct mtemp mt;

	switch (UKEY (*cmds))
	{
	/*
	 * ENDMENU means we're done.
	 */
	   case UIC_ENDMENU:
	   	return (FALSE);

	/*
	 * Most things are ENTRYs.
	 */
	   case UIC_ENTRY:
	   	menu->mbm_etext[menu->mbm_nentries] = 
				usy_pstring (UPTR (cmds[1]));
		menu->mbm_eact[menu->mbm_nentries] =
				usy_pstring (UPTR (cmds[2]));
		if (cmds[3].uc_ctype != UTT_END)
		{
			menu->mbm_expr = TRUE;
			menu->mbm_eexpr[menu->mbm_nentries] =
				usy_pstring (UPTR (cmds[3]));
		}
		else
			menu->mbm_eexpr[menu->mbm_nentries] = 0;
		menu->mbm_nentries++;
		break;
	/*
	 * A LINE is represented by a null entry.
	 */
	   case UIC_LINE:
		menu->mbm_etext[menu->mbm_nentries++] = (char *) 0;
		break;
	/*
	 * Maybe we have a map table.
	 */
	   case UIC_MAPPING:
	   	if (! menu->mbm_oom)
			ui_warning ("Map table ignored -- no selector");
	    /*
	     * Start by simply allocating a big map table.
	     */
		menu->mbm_nmap = 0;
		menu->mbm_map = (struct map_table *)
			getvm (MAXMAP * sizeof (struct map_table));
	    /*
	     * Now pull in the stuff.
	     */
		mt.mtm_nmap = &menu->mbm_nmap;
		mt.mtm_t = menu->mbm_map;
	    	ui_subcommand ("ust$in-map", "  Map>", uw_in_map, &mt);
		break;
	/*
	 * Anything else is weird.
	 */
	   default:
	   	ui_error ("(BUG): Funky kw %d %s in mb entry", UKEY (*cmds),
			cmds->uc_text);
		break;
	}
	return (TRUE);
}





void
uw_mbcreate (mw, parent)
struct menubar_widget *mw;
Widget parent;
/*
 * Realize this widget.
 */
{
	static Arg mbargs[10] = {
		{ XtNorientation,	(XtArgVal) XtorientHorizontal}
	};
	struct mb_menu *menu;
/*
 * Create the box widget that will contain the menu buttons.
 */
	mw->mw_w = XtCreateWidget ("menubar", boxWidgetClass, parent,
		mbargs, 1);
/*
 * Now go through and create each popup, and add it.
 */
 	for (menu = mw->mw_menus; menu; menu = menu->mbm_next)
		uw_mbmcreate (menu, mw->mw_w);
}




static void
uw_mbmcreate (menu, parent)
struct mb_menu *menu;
Widget parent;
/*
 * Actually create this menubar widget.
 */
{
	int i, uw_mb_cb ();
	static Arg margs[10];
	int uw_mb_popup ();
/*
 * Create the menubutton and the shell for the menu.
 */
	if (menu->mbm_type == WT_MENUBUTTON)
	{
	/*
	 * Put in the menu name.  If there is a pixmap with this
	 * menu, now is when we load it.
	 */
		XtSetArg (margs[0], XtNmenuName, menu->mbm_name); i = 1;
		if (menu->mbm_pixmap)
		{
			Pixmap pm = uw_mbmPixmap (menu->mbm_name);
			if (pm)
			{
				XtSetArg (margs[i], XtNbitmap, pm);
				i++;
			}
		}
	/*
	 * Create the menu button.
	 */
		menu->mbm_button = XtCreateManagedWidget (menu->mbm_name,
			menuButtonWidgetClass, parent, margs, i);
		parent = menu->mbm_button;
	}
	menu->mbm_pulldown = XtCreatePopupShell (menu->mbm_name,
		simpleMenuWidgetClass, parent, NULL, ZERO);
/*
 * Go through and add each entry.
 */
	XtSetArg (margs[0], XtNleftMargin, (menu->mbm_oom || menu->mbm_expr) ?
			20 : 4);
	for (i = 0; i < menu->mbm_nentries; i++)
	{
		if (menu->mbm_etext[i])
		{
			menu->mbm_ewidget[i] = XtCreateManagedWidget (
				menu->mbm_etext[i], smeBSBObjectClass,
				menu->mbm_pulldown, margs, ONE);
			XtAddCallback (menu->mbm_ewidget[i], XtNcallback,
				uw_mb_cb, menu->mbm_eact[i]);
		}
		else
			menu->mbm_ewidget[i] = XtCreateManagedWidget ("line",
				smeLineObjectClass, menu->mbm_pulldown,
				NULL, ZERO);
	}
/*
 * If this menu has a selector, put in a popup callback to insure that it
 * is always right.
 */
	if (menu->mbm_oom || menu->mbm_expr)
	{
		menu->mbm_esel = -1;
		XtAddCallback (menu->mbm_pulldown, XtNpopupCallback,
			uw_mb_popup, menu);
	}
# ifdef notdef
/*
 * For menus with mark expressions, go through and set the initial values.
 */
	if (menu->mbm_expr)
		uw_mb_set_marks (menu, TRUE);
# endif
}





static Pixmap
uw_mbmPixmap (name)
char *name;
/*
 * Try to pull in this file as a pixmap.
 */
{
	Pixmap ret;
	unsigned int w, h;
	int xh, yh;

	if (XReadBitmapFile (XtDisplay (Top), RootWindow (XtDisplay (Top), 0),
		name, &w, &h, &ret, &xh, &yh) == BitmapSuccess)
		return (ret);
	return (NULL);
}





uw_mb_cb (w, action, junk)
Widget w;
XtPointer action, junk;
/*
 * The menubar callback.
 */
{
	ui_perform (action);
}


uw_mb_popup (wgt, xpmenu, junk)
Widget wgt;
XtPointer xpmenu, junk;
/*
 * The menu popup callback routine.
 */
{
	struct mb_menu *menu = (struct mb_menu *) xpmenu;
	int type, index, xh, yh;
	unsigned int h, w;
	union usy_value v;
	Arg args[5];
/*
 * Do we have our pixmap?
 */
	if (! Mb_mark && XReadBitmapFile (XtDisplay (Top),
		XtWindow (menu->mbm_pulldown), Mb_mark_file, &w, &h,
		&Mb_mark, &xh, &yh) != BitmapSuccess)
	{
		ui_warning ("Can't read bitmap file '%s'", Mb_mark_file);
		return;
	}
/*
 * If this menu has individual item marks, deal with it separately.
 */
	if (menu->mbm_expr)
	{
		uw_mb_set_marks (menu, FALSE);
		return;
	}
/*
 * Look up the selector variable.
 */
	if (! usy_g_symbol (Ui_variable_table, menu->mbm_selector, &type, &v))
		return;
	index = uw_map_lookup (type, &v, menu->mbm_nmap, menu->mbm_map);
/*
 * If it's the same is the current value, we're done.
 */
 	if (index == menu->mbm_esel)
		return;
/*
 * Clear the old setting.
 */
	if (menu->mbm_esel >= 0)
	{
		XtSetArg (args[0], XtNleftBitmap, None);
		XtSetValues (menu->mbm_ewidget[menu->mbm_esel], args, ONE);
	}
/*
 * Set the new one.
 */
	if ((menu->mbm_esel = index) < 0 || index >= menu->mbm_nentries)
		return;
	XtSetArg (args[0], XtNleftBitmap, Mb_mark);
	XtSetValues (menu->mbm_ewidget[index], args, ONE);
}





uw_mb_set_marks (menu, all)
struct mb_menu *menu;
bool all;
/*
 * Set the individual item marks for this menu.
 */
{
	int i, type;
	struct parse_tree *pt;
	union usy_value v;
	Arg args[2];

	for (i = 0; i < menu->mbm_nentries; i++)
	{
	/*
	 * If no expression, no mark.
	 */
	 	if (! menu->mbm_eexpr[i])
			continue;
	/*
	 * Evaluate the expression.
	 */
		if ((pt = ue_parse (menu->mbm_eexpr[i], 0, FALSE)) == 0)
		{
			ui_warning ("Unable to parse '%s'",menu->mbm_eexpr[i]);
			continue;
		}
		ue_eval (pt, &v, &type);
		ue_rel_tree (pt);
	/*
	 * Make it boolean, and see if things have changed.
	 */
	 	if (type != SYMT_BOOL)
			uit_coerce (&v, type, SYMT_BOOL);
		if (v.us_v_int == menu->mbm_eval[i] && ! all)
			continue;
	/*
	 * They have.  Set the new map.
	 */
		XtSetArg (args[0], XtNleftBitmap, v.us_v_int ? Mb_mark : None);
		XtSetValues (menu->mbm_ewidget[i], args, ONE);
		menu->mbm_eval[i] = v.us_v_int;
	}
}




void uw_mbdestroy (gw, realized)
struct gen_widget *gw;
bool realized;
/*
 * Destroy a menubar widget.
 */
{
	struct menubar_widget *mw = (struct menubar_widget *) gw;

/*
 * Get rid of each individual pulldown.
 */
	while (mw->mw_menus)
	{
		struct mb_menu *zap = mw->mw_menus;
		mw->mw_menus = zap->mbm_next;
		uw_MenuDestroy (zap, realized);
	}
/*
 * Clean up here.
 */
	if (realized)
		XtDestroyWidget (mw->mw_w);
	relvm (mw);
}



uw_MenuDestroy (doomed, realized)
struct mb_menu *doomed;
bool realized;
/*
 * Get rid of one pulldown menu.
 */
{
	int i;
/*
 * Get rid of the widgets themselves, if necessary.
 */
	if (realized)
	{
		for (i = 0; i < doomed->mbm_nentries; i++)
			XtDestroyWidget (doomed->mbm_ewidget[i]);
		XtDestroyWidget (doomed->mbm_pulldown);
		XtDestroyWidget (doomed->mbm_button);
	}
/*
 * Zap the per-entry strings.
 */
	for (i = 0; i < doomed->mbm_nentries; i++)
	{
		if (doomed->mbm_etext[i])
		{
			usy_rel_string (doomed->mbm_etext[i]);
			usy_rel_string (doomed->mbm_eact[i]);
		}
		if (doomed->mbm_eexpr[i])
			usy_rel_string (doomed->mbm_eexpr[i]);
	}
/*
 * If there is a selector, get rid of it.
 */
	if (doomed->mbm_oom)
		usy_rel_string (doomed->mbm_selector);
	if (doomed->mbm_nmap)
		relvm (doomed->mbm_map);
/*
 * I think that's all.
 */
 	relvm (doomed);
}
		


uw_SaveMenubar (lun, mw)
int lun;
struct menubar_widget *mw;
/*
 * Save a menubar widget.
 */
{
	int i;
	struct mb_menu *mb;
/*
 * The only thing of real interest in the menubar widget is the number
 * of pulldowns it contains.
 */
 	bfput (lun, &mw->mw_nmenus, sizeof (int));
/*
 * Save all of the pulldowns separately.
 */
	mb = mw->mw_menus;
	for (i = 0; i < mw->mw_nmenus; i++)
	{
		uw_SavePulldown (lun, mb);
		mb = mb->mbm_next;
	}
}




uw_SavePulldown (lun, mb)
int lun;
struct mb_menu *mb;
/*
 * Save a single pulldown menu to the file.
 */
{
	int i;
/*
 * Start by saving the whole damn structure.
 */
	bfput (lun, mb, sizeof (struct mb_menu));
/*
 * Now save the additional stuff for each entry.
 */
	for (i = 0; i < mb->mbm_nentries; i++)
	{
		if (mb->mbm_etext[i])
		{
			bfput (lun, mb->mbm_etext[i],
				strlen (mb->mbm_etext[i]) + 1);
			bfput (lun, mb->mbm_eact[i],
				strlen (mb->mbm_eact[i]) + 1);
		}
		if (mb->mbm_eexpr[i])
			bfput (lun, mb->mbm_eexpr[i],
				strlen (mb->mbm_eexpr[i]) + 1);
	}
/*
 * Save the selector and map table, if there is one.
 */
	if (mb->mbm_oom)
		bfput (lun, mb->mbm_selector, strlen (mb->mbm_selector) + 1);
 	if (mb->mbm_nmap)
		uw_s_map (lun, mb->mbm_nmap, mb->mbm_map);
}





struct gen_widget *
uw_LoadMenubar (lun)
int lun;
/*
 * Load a menubar widget from this file.
 */
{
	struct menubar_widget *mw = NEW (struct menubar_widget);
	struct mb_menu *pull, *uw_LoadPulldown (), *end;
	int i;
/*
 * Initialize the fields in the menubar structure.
 */
	mw->mw_type = WT_MENUBAR;
	mw->mw_create = uw_mbcreate;
	mw->mw_destroy = uw_mbdestroy;
	mw->mw_popup = 0;
	bfget (lun, &mw->mw_nmenus, sizeof (int));
/*
 * Now we just pull in the menus themselves.
 */
	for (i = 0; i < mw->mw_nmenus; i++)
	{
		pull = uw_LoadPulldown (lun);
		if (i == 0)
			mw->mw_menus = pull;
		else
			end->mbm_next = pull;
		end = pull;
		end->mbm_next = 0;
	}
/*
 * Done.
 */
 	return ((struct gen_widget *) mw);
}




struct mb_menu *
uw_LoadPulldown (lun)
int lun;
/*
 * Load one pulldown menu from this file.
 */
{
	struct mb_menu *new = NEW (struct mb_menu);
	int i;
/*
 * Load the structure itself from the file.
 */
	bfget (lun, new, sizeof (struct mb_menu));
/*
 * Get each of the entries.
 */
	for (i = 0; i < new->mbm_nentries; i++)
	{
		if (new->mbm_etext[i])
		{
			new->mbm_etext[i] = uw_LoadString (lun);
			new->mbm_eact[i] = uw_LoadString (lun);
		}
		if (new->mbm_eexpr[i])
			new->mbm_eexpr[i] = uw_LoadString (lun);
		new->mbm_eval[i] = FALSE;
	}
/*
 * Selector and map table, if necessary.
 */
	if (new->mbm_oom)
		new->mbm_selector = uw_LoadString (lun);
	if (new->mbm_nmap)
		new->mbm_map = uw_LoadMap (lun, new->mbm_nmap);
	return (new);
}




# endif /* X11R3 */
# endif /* XSUPPORT */
