/*
 * Windowing code for pulldown menu widgets.
 */

# ifdef XSUPPORT


static char *rcsid = "$Id: ui_wPulldown.c,v 1.22 2002-07-11 22:50:44 burghart Exp $";

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
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/SmeLine.h>
# ifdef SMEMENU
/* #  include <X11/Xaw/SmeMenu.h> */
#  include "SmeMenu.h"
#  include "submenu.h"
#  include "RdssMenu.h"
#  define MenuWidgetClass rdssMenuWidgetClass
# else
#  include <X11/Xaw/SimpleMenu.h>
#  define MenuWidgetClass simpleMenuWidgetClass
# endif

# include "ui.h"
# include "ui_param.h"
# include "ui_globals.h"
# include "ui_expr.h"
# include "ui_mode.h"
# include "ui_commands.h"
# include "ui_window.h"
# include "ui_error.h"
# include "ui_loadfile.h"
# include "LiteClue/LiteClue.h"

# ifdef SMEMENU
static Pixmap SubMenuPixmap = 0;
# endif

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
	GenWidget *(*mw_cl) ();	/* (unused)				*/
	struct frame_widget *mw_frame;	/* The associated frame		*/
	Widget mw_w;		/* The actual popup widget structure	*/
	/* -- end of gen_widget stuff */
	int	mw_nmenus;	/* The number of menus in this bar	*/
	struct mb_menu *mw_menus; /* The actual menus			*/
	bool	mw_vert;	/* Is this a vertical menubar?		*/
};


typedef enum {
	CommandEntry,
	LineEntry
# ifdef SMEMENU
	, MenuEntry
# endif
} EntryType;

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
	char	mbm_title[MAXTITLE];	/* Title to appear on the menu	*/
	Widget	mbm_button;		/* The menu button		*/
	int	mbm_nentries;		/* The number of entries.	*/
	EntryType mbm_EType[MAX_ENTRY];	/* Type of each entry		*/
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
	bool	mbm_SubMarks;		/* There are submenus to mark	*/
};

/*
 * Prototypes
 */
static void uw_mbmcreate (struct mb_menu *menu, Widget parent);
static void uw_MenuDestroy (struct mb_menu *, int);
static void uw_mb_cb (Widget w, XtPointer action, XtPointer junk);
static void uw_mb_popup (Widget wgt, XtPointer xpmenu, XtPointer junk);
static void uw_mb_submenu_marks (struct mb_menu *menu);
static void uw_mb_set_marks (struct mb_menu *menu, int all);
void uw_SavePulldown (int lun, struct mb_menu *mb);




struct gen_widget *
uw_mb_def (frame)
FrameWidget *frame;
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
	new->mw_vert = FALSE;
	new->mw_frame = frame;
/*
 * Read in each pulldown.
 */
	ERRORCATCH
		ui_subcommand ("ust$in-menubar", "Menubar>", uw_in_menubar,
				(long) new);
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
	struct mb_menu *menu, *list;
	int uw_in_mbentry ();
/*
 * See what we've got here.
 */
	switch (UKEY (*cmds))
	{
	/*
	 * If this is an ENDDEF, we're done.
	 */
	   case UIC_ENDDEF:
		return (FALSE);
	/*
	 * Otherwise maybe it says "vertical"
	 */
	   case UIC_VERTICAL:
		mb->mw_vert = TRUE;
		return (TRUE);
	/*
	 * Could be frame stuff.
	 */
	   case UIC_NOHEADER:
	   case UIC_LOCATION:
	   case UIC_SIZE:
	   case UIC_OVERRIDE:
	   	uw_DoFrameParam (mb->mw_frame, cmds);
		return (TRUE);
	}
/*
 * If it's none of those, we're dealing with a pulldown menu.
 */
	cmds++;
	menu = NEW (struct mb_menu);
/*
 * If there is a keyword here, we want a pixmap.
 */
	if (menu->mbm_pixmap = (cmds[0].uc_ctype == UTT_KW))
		cmds++;
/*
 * Initialize the menu structure, and add it to the list.
 */
 	menu->mbm_nmap = menu->mbm_nentries = 0;
	menu->mbm_SubMarks = menu->mbm_expr = FALSE;
	menu->mbm_next = 0;
	menu->mbm_type = WT_MENUBUTTON;
	menu->mbm_title[0] = '\0';
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
	if (menu->mbm_oom = (cmds->uc_ctype != UTT_END))
		menu->mbm_selector = usy_pstring (UPTR (*cmds));
	else
		menu->mbm_selector = NULL;
/*
 * Deal with the actual entries now.
 */
	/* errorcatch? */
	ui_subcommand ("ust$in-mb-entry", "MenuEntry>", uw_in_mbentry, 
		(long) menu);
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
	menu->mbm_SubMarks = menu->mbm_expr = menu->mbm_oom = FALSE;
	menu->mbm_next = 0;
	menu->mbm_type = WT_INTPOPUP;
	menu->mbm_create = uw_mbmcreate;
	menu->mbm_destroy = uw_MenuDestroy;
	menu->mbm_title[0] = '\0';
	strcpy (menu->mbm_name, name);
	menu->mbm_button = 0;
	menu->mbm_selector = 0;
/*
 * Deal with the actual entries now.
 */
	/* errorcatch? */
	ui_subcommand ("ust$in-mb-entry", "MenuEntry>", uw_in_mbentry,
		(long) menu);
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
		menu->mbm_EType[menu->mbm_nentries] = CommandEntry;
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
		menu->mbm_eval[menu->mbm_nentries] = FALSE;
		menu->mbm_nentries++;
		break;
	/*
	 * A LINE is represented by a null entry.
	 */
	   case UIC_LINE:
		menu->mbm_EType[menu->mbm_nentries] = LineEntry;
	   	menu->mbm_eexpr[menu->mbm_nentries] = 0;
		menu->mbm_etext[menu->mbm_nentries++] = (char *) 0;
		break;
# ifdef SMEMENU
	/*
	 * Maybe a submenu?
	 */
	  case UIC_SUBMENU:
		menu->mbm_EType[menu->mbm_nentries] = MenuEntry;
		menu->mbm_eexpr[menu->mbm_nentries] = 0;
	   	menu->mbm_etext[menu->mbm_nentries] = 
				usy_pstring (UPTR (cmds[1]));
		menu->mbm_eact[menu->mbm_nentries] = 
				usy_pstring (UPTR (cmds[2]));
		menu->mbm_nentries++;
		menu->mbm_SubMarks = TRUE;
		break;
# else
	   case UIC_SUBMENU:
	   	ui_warning ("No submenus supported in this version");
		break;
# endif
	/*
	 * It could be a title.
	 */
	   case UIC_TITLE:
	   	strcpy (menu->mbm_title, UPTR (cmds[1]));
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
	    	ui_subcommand ("ust$in-map", "  Map>", uw_in_map, (long) &mt);
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
	Arg mbargs[2];
	struct mb_menu *menu;
/*
 * Create the box widget that will contain the menu buttons.
 */
	XtSetArg (mbargs[0], XtNorientation,
		mw->mw_vert ? XtorientVertical : XtorientHorizontal);
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
	int i;
	Arg margs[10];
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
			Pixmap pm = uw_GetPixmap (menu->mbm_name);
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
	/*
	 * Add a tooltip to the button.
	 */
		{
		    static Widget liteClue = 0;
		    if (! liteClue)
		    {
		        liteClue = XtVaCreatePopupShell( "LiteClue_shell",
					 xcgLiteClueWidgetClass, Top, NULL);
		    }
		    XcgLiteClueAddWidget(liteClue, menu->mbm_button,
					 menu->mbm_title, 0, 0);
		}
	}
/*
 * Create the pulldown.  Add the title if necessary.
 */
	if (menu->mbm_title[0])
		XtSetArg (margs[0], XtNlabel, menu->mbm_title);
	menu->mbm_pulldown = XtCreatePopupShell (menu->mbm_name,
		MenuWidgetClass, parent, margs,
		menu->mbm_title[0] ? ONE : ZERO);
/*
 * Go through and add each entry.
 */
# ifdef SMEMENU
	XtSetArg (margs[0], XtNleftMargin, 20);
# else
	XtSetArg (margs[0], XtNleftMargin, (menu->mbm_oom || menu->mbm_expr) ?
			20 : 4);
# endif
	for (i = 0; i < menu->mbm_nentries; i++)
	{
	/*
	 * Create a subwidget based on the entry type.
	 */
	 	switch (menu->mbm_EType[i])
		{
		/*
		 * Basic command entries.
		 */
		   case CommandEntry:
			menu->mbm_ewidget[i] = XtCreateManagedWidget (
				menu->mbm_etext[i], smeBSBObjectClass,
				menu->mbm_pulldown, margs, ONE);
			XtAddCallback (menu->mbm_ewidget[i], XtNcallback,
				(XtCallbackProc) uw_mb_cb, menu->mbm_eact[i]);
			break;
		/*
		 * Lines.
		 */
		   case LineEntry:
			menu->mbm_ewidget[i] = XtCreateManagedWidget ("line",
				smeLineObjectClass, menu->mbm_pulldown,
				NULL, ZERO);
			break;
# ifdef SMEMENU
		/*
		 * Submenus.
		 */
		   case MenuEntry:
		   	XtSetArg (margs[1], XtNmenu, menu->mbm_eact[i]);
			uw_IWRealize (menu->mbm_eact[i], parent);
		   	menu->mbm_ewidget[i] = XtCreateManagedWidget
				(menu->mbm_etext[i], smeMenuObjectClass,
				 menu->mbm_pulldown, margs, TWO);
			break;
# endif
		}
	}
/*
 * If this menu has a selector, put in a popup callback to insure that it
 * is always right.
 */
	if (menu->mbm_oom || menu->mbm_expr || menu->mbm_SubMarks)
	{
		menu->mbm_esel = -1;
		XtAddCallback (menu->mbm_pulldown, XtNpopupCallback,
			(XtCallbackProc) uw_mb_popup, menu);
	}
}



# ifdef notdef

static Pixmap
uw_mbmPixmap (name)
char *name;
/*
 * Try to pull in this file as a pixmap.
 */
{
	Pixmap ret;
	unsigned int w, h;
	int xh, yh, type;
	union usy_value v;
	char fname[120];
/*
 * If they've defined ui$bitmap_directory, we'll use it.
 */
	if (! usy_g_symbol (Ui_variable_table, "ui$bitmap_directory",&type,&v))
		strcpy (fname, name);
	else if (type != SYMT_STRING)
	{
		ui_warning ("ui$bitmap_directory is not a string -- ignored");
		strcpy (fname, name);
	}
	else
	{
		strcpy (fname, v.us_v_ptr);
		strcat (fname, "/");
		strcat (fname, name);
	}
/*
 * Now try to pull in the file.
 */
	if (XReadBitmapFile (XtDisplay (Top), RootWindow (XtDisplay (Top), 0),
		fname, &w, &h, &ret, &xh, &yh) == BitmapSuccess)
		return (ret);
	return (NULL);
}

# endif


/*
 * Look up a menu by this name and return its title.  If not found or
 * the found widget is not a menu, return null.  A menu without a title
 * will return an empty string.  We assume the menu will be the first
 * child of a frame widget.  If we only get the frame, revert to its title.
 */
char *
uw_menu_title (char *menu)
{
    struct gen_widget *gw = uw_g_widget (menu);
    struct frame_widget *frame = (struct frame_widget *) gw;
    char *title = 0;

    if (gw)
    {
	struct mb_menu *mbm = (struct mb_menu *) frame->fw_next;
	if (mbm && mbm->mbm_type == WT_INTPOPUP)
	{
	    title = mbm->mbm_title;
	}
	else
	{
	    title = frame->fw_title;
	}
    }
    return title;
}



static Widget uw_popup_menubutton;

Widget uw_get_menubutton()
{
	return uw_popup_menubutton;
}


static void
uw_mb_cb (w, action, junk)
Widget w;
XtPointer action, junk;
/*
 * The menubar callback.
 */
{
	/* Set the pixmap context for this callback, which we get from 
	 * the menubutton through which we were called. */
	Widget shell, button;
	uw_popup_menubutton = 0;
	if ((shell = XtParent(w)) &&
	    (button = XtParent(shell)) &&
	    XtIsSubclass (button, menuButtonWidgetClass))
	{
		uw_popup_menubutton = button;
	}
	ui_perform (action);
	uw_popup_menubutton = 0;
}


static void
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

	int	star_width = 16, star_height = 16;
	static unsigned char star_bits[] = {
		0x00, 0x00, 0x80, 0x00, 0x80, 0x00, 0x88, 0x08, 0x90, 0x04, 
		0xa0, 0x02, 0x40, 0x01, 0x3e, 0x3e, 0x40, 0x01, 0xa0, 0x02, 
		0x90, 0x04, 0x88, 0x08, 0x80, 0x00, 0x80, 0x00, 0x00, 0x00, 
		0x00, 0x00};
/*
 * Do we have the pixmap for the marker?
 */
	if (! Mb_mark)
	{
	/*
	 * Start with our default bitmap (a star)
	 */
		Mb_mark = XCreateBitmapFromData (XtDisplay (Top),
			XtWindow (menu->mbm_pulldown), (char *)star_bits, 
			star_width, star_height);
		if (! Mb_mark)
			ui_warning ("Uh-oh!  Failed to make menumark pixmap!");
	/*
	 * If the user specified a bitmap file, try for that
	 */
		if ((unsigned int) strlen (Mb_mark_file) > (unsigned) 0 && 
			XReadBitmapFile (XtDisplay (Top), 
			XtWindow (menu->mbm_pulldown), Mb_mark_file, &w, &h, 
			&Mb_mark, &xh, &yh) != BitmapSuccess)
		{
			ui_warning ("Bad bitmap file '%s'", Mb_mark_file);
			return;
		}
	}
/*
 * Deal with submenus.
 */
# ifdef SMEMENU
	if (! SubMenuPixmap)
	{
		SubMenuPixmap = XCreateBitmapFromData ( XtDisplay (Top),
			XtWindow (menu->mbm_pulldown), (char *)submenu_bits,
			submenu_width, submenu_height);

		if (! SubMenuPixmap)
			ui_warning ("Uh-oh!  Couldn't create submenu pixmap!");
	}
	if (menu->mbm_SubMarks)
		uw_mb_submenu_marks (menu);
# endif
/*
 * If this menu has individual item marks, deal with it separately.
 */
	if (menu->mbm_expr)
	{
		uw_mb_set_marks (menu, FALSE);
		return;
	}
	if (! menu->mbm_selector)
		return;
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




# ifdef SMEMENU
static void
uw_mb_submenu_marks (menu)
struct mb_menu *menu;
/*
 * Add marks for the submenus.
 */
{
	int i;
	Arg args[2];
/*
 * Set the submenu marks.
 */
	XtSetArg (args[0], XtNleftBitmap, SubMenuPixmap);
	for (i = 0; i < menu->mbm_nentries; i++)
		if (menu->mbm_EType[i] == MenuEntry)
			XtSetValues (menu->mbm_ewidget[i], args, ONE);
	menu->mbm_SubMarks = FALSE;
/*
 * If there is no longer any need for this callback, remove it.
 */
	if (! menu->mbm_oom && ! menu->mbm_expr)
		XtRemoveCallback (menu->mbm_pulldown, XtNpopupCallback,
			(XtCallbackProc) uw_mb_popup, menu);
}
# endif



static void
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
	 	if (menu->mbm_EType[i] != CommandEntry || ! menu->mbm_eexpr[i])
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




static void
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
		if (doomed->mbm_button)
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
		

void
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



void
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
	new->mbm_create = uw_mbmcreate;
	new->mbm_destroy = uw_MenuDestroy;
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
		if (new->mbm_EType[i] == MenuEntry)
			new->mbm_SubMarks = TRUE;
	}
/*
 * Selector and map table, if necessary.
 */
	if (new->mbm_oom)
		new->mbm_selector = uw_LoadString (lun);
	else
		new->mbm_selector = NULL;

	if (new->mbm_nmap)
		new->mbm_map = uw_LoadMap (lun, new->mbm_nmap);
	return (new);
}




# endif /* X11R3 */
# endif /* XSUPPORT */
