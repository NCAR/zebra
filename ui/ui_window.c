/* 3/89 jc */

# ifdef XSUPPORT
/* 
 * Window system code.
 */
# include <X11/X.h>
# include <X11/Xlib.h>
# include <X11/cursorfont.h>
# include <X11/Intrinsic.h>
# include <X11/Cardinals.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
/*
 * X11.3 backward compatibility.
 */
# ifdef X11R3
# include <X11/Box.h>
# include <X11/Command.h>
# include <X11/List.h>
# include <X11/Form.h>
# include <X11/Label.h>
# include <X11/VPaned.h>
# define XawListHighlight XtListHighlight
# define XawListUnhighlight XtListUnhighlight

# else
# include <X11/Xaw/Box.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/List.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/VPaned.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include <X11/Xaw/SmeLine.h>
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

static char *Rcsid = "$Id: ui_window.c,v 1.8 1990-03-02 18:31:34 corbet Exp $";

static bool Initialized = FALSE;
static bool Active = FALSE;	/* Is window mode active??	*/

/*
 * Window-system related stuff.
 */
static Widget Top;		/* Our (unrealized) top widget		*/
static XtAppContext Appc;	/* The application context		*/
static XFontStruct *Labelfont;	/* The font for labels.			*/
static Cursor Zapcursor;	/* The cursor for zap buttons		*/

/*
 * Fonts.
 */
# define TITLE_FONT_LEN 100
static char Title_font_name[TITLE_FONT_LEN];/* Name of the label font	*/
# define DEFAULT_TITLE_FONT \
	"-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1"

/*
 * We have two symbol tables.
 */
static stbl Widget_table = 0;	/* Where the actual widgets live	*/
static stbl Widget_vars;	/* Variables used by commands and such  */

/*
 * This pixmap holds the little mark used in pulldown menus.
 */
static Pixmap Mb_mark = 0;
static char Mb_mark_file[200];
# define DEFAULT_MARK_FILE "/usr/include/X11/bitmaps/star"

/*
 * An entry in a mapping table, which is used to map selector variable
 * values onto highlighted entries.
 */
struct map_table
{
	int mt_type;		/* Domain value type		*/
	union usy_value mt_v;	/* The domain value		*/
	int mt_target;		/* The target value.		*/
};
# define MAXMAP	40	/* Maximum entries in a map table	*/

/*
 * Temporary structure used when parsing map tables.
 */
struct mtemp
{
	int *mtm_nmap;			/* Pointer to # of entries	*/
	struct map_table *mtm_t;	/* The actual table		*/
};



# define MAXTITLE 40
# define MAXCB	256
/*
 * The "generic" widget, which contains the fields that every widget 
 * structure must have.
 */
struct gen_widget
{
	int gw_type;		/* The type of this widget		*/
	struct gen_widget *gw_next;	/* The next widget in the chain */
	void (*gw_create)();	/* The routine to create this thing	*/
	void (*gw_popup) ();	/* Routine to be called on popups	*/
	void (*gw_destroy) ();	/* The destroy method			*/
	struct frame_widget *gw_frame;	/* The associated frame		*/
	Widget gw_w;		/* The actual X widget			*/
};

/*
 * The "frame" which is the outermost structure for all ui widgets.
 */
struct frame_widget
{
	int	fw_type;	/* = WT_FRAME				*/
	struct gen_widget *fw_next;	/* Next widget in the chain	*/
	void (*fw_create)();	/* The routine to create this thing	*/
	void (*fw_popup) ();	/* Popup routine			*/
	void (*fw_destroy) ();	/* The destroy method			*/
	struct frame_widget *fw_frame;	/* The associated frame		*/
	Widget fw_w;		/* The actual popup widget structure	*/
	/* -- end of gen_widget stuff */
	char fw_name[MAXTITLE];	/* The name of this widget		*/
	char fw_title[MAXTITLE];/* The title of this widget		*/
	int fw_flags;		/* Status flags				*/
	int fw_nchild;		/* Number of children widgets		*/
	int fw_x, fw_y;		/* Override position			*/
	int fw_width, fw_height;/* Override size			*/
	Widget fw_vp;		/* The internal vpaned widget		*/
	Widget fw_form;		/* The internal form widget		*/
	Widget fw_bottom;	/* Last widget on the stack.		*/
};

/*
 * Frame widget flags.
 */
# define WF_INTERNAL	0x0001	/* Internally-defined widget		*/
# define WF_CREATED	0x0002	/* The X widget has been created	*/
# define WF_POPPED	0x0004	/* This widget is actually displayed	*/
# define WF_INIT	0x0008	/* Was this widget created during init? */
# define WF_OVERRIDE	0x0010	/* This is an OVERRIDE widget		*/
# define WF_NODEC	0x0020	/* Do not decorate this widget		*/

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
	char	mbm_name[MAXTITLE];	/* The name of the menu		*/
	Widget	mbm_button;		/* The menu button		*/
	Widget	mbm_pulldown;		/* The actual pulldown		*/
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
};




/*
 * List widgets with a single callback, as well as "cmenu" widgets.
 */
struct list_widget
{
	int	lw_type;	/* = WT_LIST or WT_CMENU		*/
	struct gen_widget *lw_next;	/* Next widget in the chain	*/
	void (*lw_create)();	/* The routine to create this thing	*/
	void (*lw_popup) ();	/* Popup routine			*/
	void (*lw_destroy) ();	/* The destroy method			*/
	struct frame_widget *lw_frame;	/* The associated frame		*/
	Widget lw_list;		/* The actual list widget		*/
	/* -- end of gen_widget stuff */
	short lw_flags;		/* Flag field				*/
	short lw_nitem;		/* The number of entries		*/
	short lw_ilen;		/* Length of items field		*/
	short lw_clen;		/* Length of commands field.		*/
	int lw_nmap;		/* Number of map table entries		*/
	struct map_table *lw_map; /* The selector mapping table		*/
	void (*lw_cb) ();	/* The callback function		*/
	char *lw_cbdata;	/* Data for the callback function	*/
	char *lw_items;		/* The actual item list			*/
	char **lw_cptr;		/* Indices into w_cbdata for cmenu	*/
	char *lw_select;	/* Name of selector variable		*/
};

/*
 * List and CMENU widget flags.
 */
# define LWF_SELECTOR	0x0001	/* Is there a selector variable?	*/
# define LWF_HORIZONTAL	0x0002	/* Horizontal orientation		*/


/*
 * This is a temporary structure used in the definition of cmenu widgets.
 */
# define CM_MAX	40
struct cmenu_temp
{
	struct list_widget *c_lw;
	char *c_items[CM_MAX];
	char *c_cmds[CM_MAX];
	char *c_select;
	int c_nentry;
};

/*
 * Kludge flag used during save operations.
 */
static bool Save_all = 0;
	

/*
 * Forward routines.
 */
struct frame_widget *uw_make_frame ();
void uw_lselect ();
struct gen_widget *uw_list_def (), *uw_cm_def (), *uw_mb_def ();






uw_init ()
/*
 * Initialize.  This is stuff which can (and should) be done whether we
 * will ever be running in window mode or not.
 */
{
	union usy_value v;

	Widget_table = usy_c_stbl ("ui$widget_table");
	Widget_vars = usy_c_stbl ("ui$widget_vars");
	v.us_v_ptr = (char *) Widget_vars;
	usy_s_symbol (usy_g_stbl ("usym$master_table"), "w", SYMT_SYMBOL, &v);
	usy_c_indirect (Ui_variable_table, "ui$title_font", Title_font_name,
		SYMT_STRING, TITLE_FONT_LEN);
	strcpy (Title_font_name, DEFAULT_TITLE_FONT);
	usy_c_indirect (Ui_variable_table, "ui$menu_mark_file", Mb_mark_file,
		SYMT_STRING, 200);
	strcpy (Mb_mark_file, DEFAULT_MARK_FILE);
}





uw_mode (cmds)
struct ui_command *cmds;
/*
 * perform a shift into Window mode.
 */
{
	Arg args[10];
	void uw_quit (), uw_sel (), uw_do_pop ();
	static char *argv[] = { "ui", 0 };
	static int one = 1;
	int uw_xevent (), h, w;
/*
 * Throw the mode onto the stack.
 */
	ui_push_mode (M_WINDOW);
/*
 * Initialize the toolkit if necessary.  We create a top level application
 * shell, but never realize it -- we don't need it.  All actual interaction
 * is handled with popups.
 */
	if (! Initialized)
	{
		Top = XtAppInitialize (&Appc, "UI", NULL, ZERO, &one, argv,
			NULL, NULL, ZERO);
		Labelfont = XLoadQueryFont (XtDisplay (Top), Title_font_name);
		Zapcursor = XCreateFontCursor (XtDisplay (Top), XC_pirate);
		Initialized = TRUE;
	}
/*
 * Get our file descriptor number, and tell tty to watch for it.
 */
	uw_sync ();
	tty_watch (XConnectionNumber (XtDisplay (Top)), uw_xevent);
	Active = TRUE;
	return (TRUE);
}





uw_endmode ()
/*
 * Drop out of window mode.
 */
{
	int uw_t_popdown ();
/*
 * Pop down any widgets which are on the screen.
 */
	usy_traverse (Widget_table, uw_t_popdown, 0, FALSE);
	uw_sync ();
/*
 * Turn off the watch process.
 */
	tty_nowatch (XConnectionNumber (XtDisplay (Top)));
	Active = FALSE;
}
				  




uw_t_popdown (symbol, type, v, junk)
char *symbol;
int type, junk;
union usy_value *v;
/*
 * Make sure this widget is popped down.
 */
{
	struct frame_widget *fw = (struct frame_widget *) v->us_v_ptr;

	if (fw->fw_type == WT_FRAME && fw->fw_flags & WF_POPPED)
	{
		XtPopdown (fw->fw_w);
		fw->fw_flags &= ~WF_POPPED;
	}
	return (TRUE);
}







uw_xevent ()
/*
 * Called when an X event is pending.
 */
{
	XEvent event;
/*
 * Deal with any queued events.  The Active test has been taken out on the
 * theory that (1) the stream of events will soon stop since we pop down all
 * of our widgets, and (2) we might as well deal with the ones that are
 * there.
 */
/* 	for (; Active && XtAppPending (Appc); ) */
 	while (XtAppPending (Appc))
	{
		XtAppNextEvent (Appc, &event);
		XtDispatchEvent (&event);
	}
/*
 * Done.
 */
	return (1);
}








/*
 * Widget definition.
 */
struct gen_widget *
uw_g_widget (name)
char *name;
/*
 * Look for a widget by this name.
 */
{
	union usy_value v;
	int type;
/*
 * Try a lookup.
 */
 	if (! usy_g_symbol (Widget_table, name, &type, &v))
		return (NULL);
	return ((struct gen_widget *) v.us_v_ptr);
}





char **
uw_nt_to_array (strings)
char *strings;
/*
 * Return a pointer to a (dynamically allocated) list of character pointers,
 * each of which points to one of the null-terminated strings in STRINGS.
 */
{
	int nstr = 0, i;
	char *cp, **ret;
/*
 * First, go through and count the strings.
 */
 	for (cp = strings; *cp; cp += strlen (cp) + 1)
		nstr++;
/*
 * Allocate the memory, and fill it in.
 */
	ret = (char **) getvm ((nstr+1)*sizeof (char *));
	cp = strings;
	for (i = 0; i < nstr; i++)
	{
		ret[i] = cp;
		cp += strlen (cp) + 1;
	}
	ret[nstr] = (char *) 0;
	return (ret);
}







uw_define (cmds)
struct ui_command *cmds;
{
	char *name = UPTR (cmds[0]), *title = UPTR (cmds[2]);
	int type = UINT (cmds[1]);
	struct gen_widget *gw;
	struct frame_widget *frame;
/*
 * Here we just split apart, depending on the various widget types.
 */
	switch (type)
	{
	/*
	 * Simple list widgets.
	 */
	   case WT_LIST:
	   	gw = uw_list_def ();
		break;
	/*
	 * Command menu widgets.
	 */
	   case WT_CMENU:
	   	gw = uw_cm_def ();
		break;
	/*
	 * Stack widgets -- these ones are special.
	 */
	   case WT_STACK:
	   	uw_dstack (name, title);
		return (TRUE);

	/*
	 * Menubar widgets.
	 */
	   case WT_MENUBAR:
	   	gw = uw_mb_def ();
		break;

	   default:
	   	ui_error ("(BUG): unknown widget type: %d\n", type);
	}
/*
 * Now really define our new widget.
 */
	frame = uw_make_frame (name, title);
	frame->fw_flags = WF_INTERNAL;
	uw_add_child (frame, (struct gen_widget *) gw);
	uw_wdef (frame);
	return (TRUE);
}





uw_wdef (frame)
struct frame_widget *frame;
/*
 * Define this is an official internal widget.
 */
{
	struct frame_widget *old;
	union usy_value v;
/*
 * If the widget already exists, get rid of it.  Sometime it would be nice
 * to do this better.
 */
	if (old = (struct frame_widget *) uw_g_widget (frame->fw_name))
		uw_zap_widget (old);
/*
 * Define the new widget.
 */
 	v.us_v_ptr = (char *) frame;
	usy_s_symbol (Widget_table, frame->fw_name, SYMT_POINTER, &v);
}




uw_dstack (name, title)
char *name, *title;
/*
 * Define a stack widget.
 */
{
	struct frame_widget *frame = uw_make_frame (name, title);
	int uw_in_stack ();
/*
 * Simply branch out and read subwidget definitions.
 */
	frame->fw_flags = WF_INTERNAL;
	ERRORCATCH
		ui_subcommand ("ust$in-stack", "Stack>", uw_in_stack, frame);
	ON_ERROR
		/* uw_destroy (frame) */
		RESIGNAL;
	ENDCATCH
/*
 * Actually define this widget.
 */
	uw_wdef (frame);
	return (TRUE);
}






uw_in_stack (frame, cmds)
struct frame_widget *frame;
struct ui_command *cmds;
/*
 * Deal with a stack widget command.
 */
{
	struct gen_widget *gw;
/*
 * Get the specified widget.
 */
	switch (UKEY (*cmds))
	{
	   case WT_LIST:
	   	gw = uw_list_def ();
		break;

	   case WT_CMENU:
		gw = uw_cm_def ();
		break;

	   case UIC_ENDDEF:
	   	return (FALSE);
	}
/*
 * Add it to the list.
 */
	uw_add_child (frame, gw);
	return (TRUE);
}





uw_mk_list (name, title, nitem, items, callback, cbdata)
char *name, *title, *cbdata;
int nitem;
char **items;
void (*callback) ();
/*
 * Create an application-defined list widget.
 * Entry:
 *	NAME	Is the name to be assigned to this widget.
 *	TITLE	Is the title to put on the widget.
 *	NITEM	is the number of list items.
 *	ITEMS	is an array of pointers to the actual item text.
 *	CALLBACK is the callback function.
 *	CBDATA	is a data value to be given to the callback function.
 * Exit:
 *	The widget has been (re)defined.
 */
{
	struct list_widget *new;
	struct frame_widget *frame;
	void uw_lcreate (), uw_ldestroy ();
	char *cp;
	int i, len;
/*
 * Get a list widget structure, and fill it in.
 */
 	new = NEW (struct list_widget);
	new->lw_cbdata = cbdata;
	new->lw_cb = callback;
	new->lw_type = WT_LIST;
	new->lw_create = uw_lcreate;
	new->lw_destroy = uw_ldestroy;
	new->lw_flags = 0;
	new->lw_nmap = 0;
	new->lw_select = 0;
	new->lw_flags = 0;
	new->lw_popup = 0;
/*
 * Turn the item list into the extended string that uw_lcreate wants.
 */
	len = 0;
	for (i = 0; i < nitem; i++)
		len += strlen (items[i]) + 1;
	len++;
	new->lw_items = cp = getvm (len);
	for (i = 0; i < nitem; i++)
	{
		strcpy (cp, items[i]);
		cp += strlen (cp) + 1;
	}
	*cp = 0;
	new->lw_ilen = len;
	new->lw_nitem = nitem;
/*
 * Create a frame to hold this thing.
 */
 	frame = uw_make_frame (name, title);
	frame->fw_flags = 0;
/*
 * Put it all together.
 */
 	uw_add_child (frame, (struct gen_widget *) new);
	uw_wdef (frame);
}








struct gen_widget *
uw_list_def ()
{
/*
 * Interactively define a list widget.
 */
	struct list_widget *new;
	int uw_in_list ();
	void uw_lcreate (), ui_perform (), uw_ldestroy ();
/*
 * Make a new list widget structure.
 */
	new = NEW (struct list_widget);
	new->lw_cbdata = 0;
	new->lw_type = WT_LIST;
	new->lw_create = uw_lcreate;
	new->lw_destroy = uw_ldestroy;
	new->lw_cb = ui_perform;
	new->lw_nmap = 0;
/*
 * Now read in the info.
 */
	ERRORCATCH
		ui_subcommand ("ust$in-list", "List>", uw_in_list, new);
	ON_ERROR
		if (new->lw_nmap)
			relvm (new->lw_map);
		relvm (new);
		RESIGNAL;
	ENDCATCH
	new->lw_popup = (new->lw_flags & LWF_SELECTOR) ? uw_lselect : 0;
/*
 * Done.
 */
 	return ((struct gen_widget *) new);
}






uw_in_list (lw, cmds)
struct list_widget *lw;
struct ui_command *cmds;
/*
 * Handle one of the "within widget" commands.
 */
{
	int i, len;
	char *cp;
	int uw_in_map ();
	struct mtemp mt;

	switch (UKEY (cmds[0]))
	{
	/*
	 * Done.
	 */
	   case UIC_ENDDEF:
	   	if (lw->lw_nmap && (lw->lw_flags & LWF_SELECTOR) == 0)
			ui_warning ("Map table ignored without selector");
	   	if (! lw->lw_items)
			ui_cl_error (FALSE, cmds->uc_col, "No items given");
		else if (! lw->lw_cbdata)
			ui_cl_error (FALSE, cmds->uc_col, "No command given");
		else
			return (FALSE);
		break;
	/*
	 * Add a command.
	 */
	   case UIC_COMMAND:
		if (lw->lw_cbdata)
		{
			ui_warning ("Old command (%s) superseded",
				lw->lw_cbdata);
			usy_rel_string (lw->lw_cbdata);
		}
	   	lw->lw_cbdata = usy_string (UPTR (cmds[1]));
		break;
# ifdef notdef
	/*
	 * The title.
	 */
	   case UIC_TITLE:
	   	strcpy (lw->lw_title, UPTR (cmds[1]));
		break;
# endif
	/*
	 * A selector variable.
	 */
	   case UIC_SELECTOR:
		if (lw->lw_flags & LWF_SELECTOR)
		{
			ui_warning ("Old selector (%s) superseded",
				lw->lw_select);
			usy_rel_string (lw->lw_select);
		}
	   	lw->lw_select = usy_string (UPTR (cmds[1]));
		lw->lw_flags |= LWF_SELECTOR;
		break;
	/*
	 * The item list.
	 */
	    case UIC_ITEMS:
		lw->lw_nitem = len = 0;
		for (i = 1; cmds[i].uc_ctype != UTT_END; i++)
			len += strlen (UPTR (cmds[i])) + 1;
		cp = lw->lw_items = getvm (len + 1);
		for (i = 1; cmds[i].uc_ctype != UTT_END; i++)
		{
			strcpy (cp, UPTR (cmds[i]));
			cp += strlen (cp) + 1;
			lw->lw_nitem++;
		}
		*cp = '\0';
		lw->lw_ilen = len + 1;
		break;
	/*
	 * A mapping table.
	 */
	    case UIC_MAPPING:
	    /*
	     * Start by simply allocating a big map table.
	     */
		lw->lw_nmap = 0;
		lw->lw_map = (struct map_table *)
		getvm (MAXMAP * sizeof (struct map_table));
	    /*
	     * Now pull in the stuff.
	     */
		mt.mtm_nmap = &lw->lw_nmap;
		mt.mtm_t = lw->lw_map;
	    	ui_subcommand ("ust$in-map", "  Map>", uw_in_map, &mt);
		break;
	/*
	 * Horizontal orientation.
	 */
	    case UIC_HORIZONTAL:
	    	lw->lw_flags |= LWF_HORIZONTAL;
		break;
	}
	return (TRUE);
}




uw_in_map (mt, cmds)
struct mtemp *mt;
struct ui_command *cmds;
/*
 * Deal with a map table.
 */
{
	char *usy_pstring ();
	int nmap = *mt->mtm_nmap;
/*
 * See if we're done.
 */
	if (cmds->uc_ctype == UTT_KW && UKEY (*cmds) == UIC_ENDMAPPING)
		return (FALSE);
/*
 * No overflows, please.
 */
 	if (nmap >= MAXMAP)
	{
		ui_warning ("Maximum map table size (%d) exceeded", MAXMAP);
		return (TRUE);
	}
/*
 * OK, move over the info.
 */
	mt->mtm_t[nmap].mt_type = cmds[0].uc_vptype;
	mt->mtm_t[nmap].mt_target = UINT (cmds[1]);
	switch (cmds[0].uc_vptype)
	{
	   case SYMT_INT:
	   case SYMT_BOOL:
	   	mt->mtm_t[nmap].mt_v.us_v_int = UINT (cmds[0]);
		break;

	   case SYMT_FLOAT:
	   	mt->mtm_t[nmap].mt_v.us_v_float = UFLOAT (cmds[0]);
		break;

	   case SYMT_STRING:
	   	mt->mtm_t[nmap].mt_v.us_v_ptr =
					usy_pstring (UPTR (cmds[0]));
		break;

	   default:
	   	ui_warning ("BUG: Funky type (%d)", cmds[0].uc_vptype);
		break;
	}
	(*mt->mtm_nmap)++;
	return (TRUE);
}
	
			




uw_popup (name)
char *name;
/*
 * Pop this particular widget up.
 */
{
	struct gen_widget *gw = uw_g_widget (name);
	struct frame_widget *frame;
/*
 * Make sure the widget exists.
 */
	if (! Active)
		ui_error ("Window mode is not currently active");
	if (! gw)
		ui_error ("Unknown widget: %s", name);
	if (gw->gw_type != WT_FRAME)
		ui_error ("Funky widget type: %d", gw->gw_type);
	frame = (struct frame_widget *) gw;
/*
 * If the widget is already on screen, we do nothing.
 */
 	if (frame->fw_flags & WF_POPPED)
		return;
/*
 * Make sure it has been created.
 */
 	if (! (frame->fw_flags & WF_CREATED))
		uw_cr_frame (frame);
/*
 * Now put it up on the screen.
 */
	XtPopup (frame->fw_w, XtGrabNone);
	uw_sync ();
	frame->fw_flags |= WF_POPPED;
/*
 * Pass through the child widgets, doing whatever work is needed.
 */
	for (gw = frame->fw_next; gw; gw = gw->gw_next)
		if (gw->gw_popup)
			(*gw->gw_popup) (gw);
# ifdef notdef
 	if (frame->fw_flags & LWF_SELECTOR)
	{
		struct list_widget *w = (struct list_widget *) gw;
		if (! usy_defined (Ui_variable_table, w->w_select))
			uw_setselector (w->w_select, 0);
		uw_lselect (w);
	}
# endif
}



uw_popdown (name)
char *name;
/*
 * Pop this particular widget down.
 */
{
	struct gen_widget *gw = uw_g_widget (name);
	struct frame_widget *frame = (struct frame_widget *) gw;
/*
 * Make sure the widget exists.
 */
	if (! gw)
		ui_error ("Unknown widget: %s\n", name);
/*
 * If the widget is not already on screen, we do nothing.
 */
 	if (! (frame->fw_flags & WF_POPPED))
		return;
/*
 * Now bring it down.
 */
	XtPopdown (frame->fw_w);
	uw_sync ();
	frame->fw_flags &= ~WF_POPPED;
}




void
uw_lcb (xw, lw, item)
Widget xw;
struct list_widget *lw;
char *item;
/*
 * The general list widget callback.
 */
{
	union usy_value v;
# ifdef X11R3
	XtListReturnStruct *ritem = (XtListReturnStruct *) item;
# else
	XawListReturnStruct *ritem = (XawListReturnStruct *) item;
# endif
/*
 * Set up the "selection" variable.
 */
	v.us_v_ptr = ritem->string;
	usy_s_symbol (Widget_vars, "selection", SYMT_STRING, &v);
/*
 * Set the selector variable, if appropriate.
 */
	if (lw->lw_flags & LWF_SELECTOR && ! lw->lw_nmap)
		uw_setselector (lw->lw_select, ritem->list_index);
/*
 * Perform the callback.
 */
	ERRORCATCH
		(*lw->lw_cb) (lw->lw_cbdata);
	ENDCATCH
/*
 * Deal with highlighting.
 */
 	if (lw->lw_flags & LWF_SELECTOR)
		uw_lselect (lw);
	else
	 	XawListUnhighlight (lw->lw_list);
}





void
uw_zapbutton (xw, fw, junk)
Widget xw;
struct frame_widget *fw;
char *junk;
/*
 * Somebody has hit the zap button on this widget.
 */
{
	uw_popdown (fw->fw_name);
}





struct frame_widget *
uw_make_frame (name, title)
char *name, *title;
/*
 * Make the standard widget shell.
 * Entry:
 *	NAME	is the name of the frame to create.
 * Exit:
 *	The return value is the top shell widget.
 */
{
	struct frame_widget *w = NEW (struct frame_widget);
/*
 * Initialize the frame structure.
 */
	w->fw_type = WT_FRAME;
	w->fw_next = 0;
	w->fw_create = 0;
	w->fw_flags = 0;
	w->fw_nchild = 0;
	strcpy (w->fw_name, name);
	strcpy (w->fw_title, title);
	return (w);
}




uw_cr_frame (w)
struct frame_widget *w;
/*
 * Create this frame widget.
 */
{
	static Arg arglist[] = {
		{XtNinput, (XtArgVal) True},
	};
	Arg args[10];
	Widget zap, label, form, header;
	void uw_zapbutton ();
	struct gen_widget *gw;
/*
 * Create the popup shell, and the form to go within it.
 */
	w->fw_w = XtCreatePopupShell (w->fw_name, topLevelShellWidgetClass,
		Top, arglist, XtNumber (arglist));
	form = XtCreateManagedWidget ("form", formWidgetClass, w->fw_w,
		args, 0);
	XtSetArg (args[0], XtNdefaultDistance, 1);
	header = XtCreateManagedWidget (w->fw_name, formWidgetClass, form,
		args, 1);
/*
 * Add the label and the zap button to the form.
 */
	XtSetArg (args[0], XtNlabel, w->fw_title);
	XtSetArg (args[1], XtNjustify, XtJustifyLeft);
	XtSetArg (args[2], XtNresize, True);
	XtSetArg (args[3], XtNborderWidth, 0);
	XtSetArg (args[4], XtNfont, (XtArgVal) Labelfont);
	label = XtCreateManagedWidget ("title", labelWidgetClass, header,
		args, 5);
	XtSetArg (args[0], XtNlabel, "Zap");
	XtSetArg (args[1], XtNfromHoriz, label);
	XtSetArg (args[2], XtNfromVert, NULL);
	XtSetArg (args[3], XtNcursor, Zapcursor);
	zap = XtCreateManagedWidget ("zap", commandWidgetClass, header,args,4);
	XtAddCallback (zap, XtNcallback, uw_zapbutton, w);

# ifdef notdef
/*
 * Now add the Vpaned widget to hold the actual children.
 */
	XtSetArg (args[0], XtNfromVert, header);
	XtSetArg (args[1], XtNfromHoriz, NULL);
	XtSetArg (args[2], XtNborderWidth, 0);
	w->fw_vp = XtCreateManagedWidget ("vp", vPanedWidgetClass, form,
		args, 3);
# endif
	w->fw_form = form;
	w->fw_bottom = header;
/*
 * Finally, we need to go through and create all of the children too.
 */
	for (gw = w->fw_next; gw; gw = gw->gw_next)
		if (gw->gw_create)
			uw_cchild (w, gw);
	w->fw_flags |= WF_CREATED;
}





uw_add_child (frame, child)
struct frame_widget *frame;
struct gen_widget *child;
/*
 * Add a child widget to this frame.
 */
{
/*
 * Hook it into the list.
 */
	if (frame->fw_next == 0)
		frame->fw_next = child;
	else
	{
		struct gen_widget *wp;
		for (wp = frame->fw_next; wp->gw_next; wp = wp->gw_next)
			;
		wp->gw_next = child;
	}
	child->gw_next = 0;
	child->gw_frame = frame;
	frame->fw_nchild++;
/*
 * If this widget is already created, create the new child too.
 */
	if (frame->fw_flags & WF_CREATED)
		uw_cchild (frame, child);
}




uw_cchild (frame, child)
struct frame_widget *frame;
struct gen_widget *child;
/*
 * Create the child widget.
 */
{
	Arg formargs[3];
/*
 * Actually create the child widget.
 */
	(*child->gw_create) (child, frame->fw_form);
/*
 * Add the form resources to this widget, then get the form to actually
 * manage it.
 */
	XtSetArg (formargs[0], XtNfromHoriz, NULL);
	XtSetArg (formargs[1], XtNfromVert, frame->fw_bottom);
	XtSetArg (formargs[2], XtNvertDistance, -2);
	XtSetValues (child->gw_w, formargs, 3);
	XtManageChild (child->gw_w);
/*
 * This is the new bottommost widget.
 */
	frame->fw_bottom = child->gw_w;
}






void
uw_lcreate (w, parent)
struct list_widget *w;
Widget parent;
/*
 * Cause this widget to be realized.
 */
{
	char **entries;
	static Arg listargs[7] = {
		{ XtNforceColumns,	(XtArgVal) True		},
		{ XtNverticalList,	(XtArgVal) True		},
		{ 0 },
	};
	void uw_cmcb ();
	int uw_ldaemon ();
/*
 * Put together the list of entries.
 */
 	entries = uw_nt_to_array (w->lw_items);
	XtSetArg (listargs[2], XtNdefaultColumns,
		(w->lw_flags & LWF_HORIZONTAL) ? w->lw_nitem : 1);
	XtSetArg (listargs[3], XtNlist, (XtArgVal) entries);
	XtSetArg (listargs[4], XtNborderWidth, 0);
/*
 * Create the list subwidget.
 */
	w->lw_list = XtCreateWidget ("list", listWidgetClass, parent,
			listargs, 5);
	XtAddCallback (w->lw_list, XtNcallback,
		w->lw_type == WT_LIST ? uw_lcb : uw_cmcb, w);
/*
 * If there is a selector variable, set up a watcher for it.
 */
	if (w->lw_flags & LWF_SELECTOR)
		usy_daemon (Ui_variable_table, w->lw_select, SOP_WRITE,
			uw_ldaemon, (char *) w);
}





uw_ldaemon (sym, cw, op, ot, ov, nt, nv)
char *sym, *cw;
int op, ot, nt;
union usy_value *ov, *nv;
/*
 * The selector variable for this widget has changed.  Redo it.
 */
{
	struct list_widget *w = (struct list_widget *) cw;
	int val;
/*
 * If the widget is not currently on-screen, we do nothing.  It will be
 * taken care of the next time it's popped up.
 */
 	if ((w->lw_frame->fw_flags & WF_POPPED) == 0)
		return;
/*
 * If the value hasn't changed, we also do nothing.
 */
	if (ot == nt && ov->us_v_int == nv->us_v_int)
		return;
/*
 * Fix up the widget.
 */
 	uw_lsel_fix (w, nt, nv);
	uw_sync ();
}





uw_sync ()
/*
 * Sync up with the server.
 */
{
	if (Initialized)
		XSync (XtDisplay (Top), False);
}





struct gen_widget *
uw_cm_def ()
{
/*
 * Interactively define a command menu widget.
 */
	struct list_widget *new;
	int uw_in_cmenu (), i;
	void uw_lcreate (), ui_perform (), uw_ldestroy ();
	struct cmenu_temp cm;
	char *cp, *ip;
/*
 * Fix up a widget structure.
 */
	new = NEW (struct list_widget);
	new->lw_cbdata = 0;
	new->lw_type = WT_CMENU;
	new->lw_create = uw_lcreate;
	new->lw_destroy = uw_ldestroy;
	new->lw_cb = ui_perform;
	new->lw_flags = 0;
/*
 * Put together a cmenu struct, and get the rest of the info.
 */
	cm.c_lw = new;
	cm.c_nentry = 0;
	cm.c_select = (char *) 0;
	ERRORCATCH
		ui_subcommand ("ust$in-cmenu", "CMenu>", uw_in_cmenu, &cm);
	ON_ERROR
		for (i = 0; i < cm.c_nentry; i++)
		{
			usy_rel_string (cm.c_items[i]);
			usy_rel_string (cm.c_cmds[i]);
		}
		RESIGNAL;
	ENDCATCH
/*
 * Figure out our various lengths, and allocate some memory.
 */
	new->lw_ilen = new->lw_clen = 1;
	for (i = 0; i < cm.c_nentry; i++)
	{
		new->lw_ilen += strlen (cm.c_items[i]) + 1;
		new->lw_clen += strlen (cm.c_cmds[i]) + 1;
	}
	new->lw_ilen++; new->lw_clen++;
	ip = new->lw_items = getvm (new->lw_ilen);
	cp = new->lw_cbdata = getvm (new->lw_clen);
	new->lw_cptr = (char **) getvm (cm.c_nentry*sizeof (char *));
	new->lw_nitem = cm.c_nentry;
/*
 * Now copy the info over.
 */
	for (i = 0; i < cm.c_nentry; i++)
	{
		strcpy (ip, cm.c_items[i]);
		usy_rel_string (cm.c_items[i]);
		ip += strlen (ip) + 1;
		strcpy (cp, cm.c_cmds[i]);
		usy_rel_string (cm.c_cmds[i]);
		new->lw_cptr[i] = cp;
		cp += strlen (cp) + 1;
	}
	*ip = *cp = '\0';
/*
 * If there is a selector, copy that.
 */
	if (cm.c_select)
	{
		new->lw_select = cm.c_select;
		new->lw_flags |= LWF_SELECTOR;
		new->lw_popup = uw_lselect;
	}
	else
		new->lw_popup = 0;
/*
 * Done.
 */
 	return ((struct gen_widget *) new);
}





uw_in_cmenu (cm, cmds)
struct cmenu_temp *cm;
struct ui_command *cmds;
/*
 * Handle interactive cmenu definition.
 */
{
	struct mtemp mt;

	switch (UKEY (*cmds))
	{
	/*
	 * Finished.
	 */
	   case UIC_ENDDEF:
		if (cm->c_lw->lw_nmap > 0 && ! cm->c_select)
			ui_warning ("Map table ignored without selector");
	   	if (! cm->c_nentry)
			ui_error ("No entries given");
		else
		   	return (FALSE);
		break;
	/*
	 * A command entry.
	 */
	   case UIC_ENTRY:
	   	if (cm->c_nentry >= CM_MAX)
			ui_warning("Exceeded max entries -- this one ignored");
		else
		{
			cm->c_items[cm->c_nentry] = usy_string (UPTR(cmds[1]));
			cm->c_cmds[cm->c_nentry] = usy_string (UPTR (cmds[2]));
			cm->c_nentry++;
		}
		break;
	/*
	 * A selector variable.
	 */
	   case UIC_SELECTOR:
	   	cm->c_select = usy_string (UPTR (cmds[1]));
		break;
	/*
	 * A mapping table.
	 */
	    case UIC_MAPPING:
	    /*
	     * Start by simply allocating a big map table.
	     */
		cm->c_lw->lw_nmap = 0;
		cm->c_lw->lw_map = (struct map_table *)
			getvm (MAXMAP * sizeof (struct map_table));
	    /*
	     * Now pull in the stuff.
	     */
		mt.mtm_nmap = &cm->c_lw->lw_nmap;
		mt.mtm_t = cm->c_lw->lw_map;
	    	ui_subcommand ("ust$in-map", "  Map>", uw_in_map, &mt);
		break;
	/*
	 * Horizontal orientation.
	 */
	    case UIC_HORIZONTAL:
	    	cm->c_lw->lw_flags |= LWF_HORIZONTAL;
		break;
	/*
	 * Hmm...
	 */
	   default:
	   	ui_error ("(BUG): Unknown kw = %d\n", UKEY (*cmds));
	}
	return (TRUE);
}






uw_cmcb (xw, lw, item)
Widget xw;
struct list_widget *lw;
char *item;
/*
 * The command menu call back.
 */
{
	union usy_value v;
# ifdef X11R3
	XtListReturnStruct *ritem = (XtListReturnStruct *) item;
# else
	XawListReturnStruct *ritem = (XawListReturnStruct *) item;
# endif
/*
 * Set up the "selection" variable.
 */
	v.us_v_ptr = ritem->string;
	usy_s_symbol (Widget_vars, "selection", SYMT_STRING, &v);
/*
 * Set the selector variable, if appropriate.
 */
	if (lw->lw_flags & LWF_SELECTOR && ! lw->lw_nmap)
		uw_setselector (lw->lw_select, ritem->list_index);
/*
 * Perform the callback.
 */
	ERRORCATCH
		(*lw->lw_cb) (lw->lw_cptr[ritem->list_index]);
	ENDCATCH
/*
 * Deal with highlighting.
 */
 	if (lw->lw_flags & LWF_SELECTOR)
		uw_lselect (lw);
	else
	 	XawListUnhighlight (lw->lw_list);
}





uw_setselector (sym, value)
char *sym;
int value;
/*
 * Set this widget's selector variable.
 */
{
	union usy_value v;

	v.us_v_int = value;
	usy_s_symbol (Ui_variable_table, sym, SYMT_INT, &v);
}





void
uw_lselect (lw)
struct list_widget *lw;
/*
 * Set this widget to reflect the current selection.
 */
{
	int type, index;
	union usy_value v;
/*
 * Look up the selection variable.
 */
	if (! usy_g_symbol (Ui_variable_table, lw->lw_select, &type, &v))
	{
	 	XawListUnhighlight (lw->lw_list);
		return;
	}
/*
 * Now actually tweak the widget.
 */
	uw_lsel_fix (lw, type, &v);
}




uw_lsel_fix (lw, type, v)
struct list_widget *lw;
int type;
union usy_value *v;
/*
 * Tweak this widget according to the new selection.
 */
{
	int index;
/*
 * Do the map lookup.
 */
 	index = uw_map_lookup (type, v, lw->lw_nmap, lw->lw_map);
/*
 * Actually do something with the widget.
 */
	if (index < 0 || index >= lw->lw_nitem)
		XawListUnhighlight (lw->lw_list);
	else
		XawListHighlight (lw->lw_list, index);
}






uw_map_lookup (type, v, nmap, map)
int type, nmap;
union usy_value *v;
struct map_table *map;
/*
 * Do a map table->index lookup.
 */
{
	int index = -1, i, rt;
	union usy_value result;
/*
 * If there is a map table for this widget, let's attempt to translate
 * what we got.
 */
	if (nmap)
		for (i = 0; i < nmap; i++)
		{
			if (type != map[i].mt_type)
				continue;
			ue_do_eq (type, v, &map[i].mt_v, &result);
			if (result.us_v_int)
			{
				type = SYMT_INT;
				index = map[i].mt_target;
				break;
			}
		}
/*
 * If we still don't have a real index, try to figure out one.
 */
 	if (index < 0)
		switch (type)
		{
		   case SYMT_INT:
		   	index = v->us_v_int;
			break;
		   case SYMT_BOOL:
		   	index = v->us_v_int ? 1 : 0;
			break;
		}
/*
 * Return what we got.
 */
 	return (index);
}






uw_save (lun, all)
int lun, all;
/*
 * Save our widgets to a file.
 */
{
	char marker = LF_WIDGET;
	int uw_w_save ();

	bfput (lun, &marker, 1);
	Save_all = all;
	usy_traverse (Widget_table, uw_w_save, lun, FALSE);
	bfput (lun, &lun, 0);
}




uw_w_save (name, type, v, lun)
char *name;
int type, lun;
union usy_value *v;
/*
 * Save a single widget.
 */
{
	struct gen_widget *gw = (struct gen_widget *) v->us_v_ptr, *child;
	struct frame_widget *frame;
/*
 * If this is not a frame widget, forget it.
 */
 	if (gw->gw_type != WT_FRAME)
		return (TRUE);
/*
 * See if we should really save this one.
 */
	frame = (struct frame_widget *) gw;
	if ((frame->fw_flags & WF_INIT && ! Save_all) ||
			(frame->fw_flags & WF_INTERNAL) == 0)
		return (TRUE);
/*
 * Put out the frame-specific stuff.
 */
	ui_printf ("Saving widget '%s'\n", frame->fw_name);
	bfput (lun, &gw->gw_type, sizeof (int));
	bfput (lun, frame->fw_name, strlen (frame->fw_name) + 1);
	bfput (lun, frame->fw_title, strlen (frame->fw_title) + 1);
/*
 * Put out all of the children.
 */
	for (child = frame->fw_next; child; child = child->gw_next)
	{
		bfput (lun, &child->gw_type, sizeof (int));
		switch (child->gw_type)
		{
		   case WT_LIST:
		   	uw_s_list (lun, (struct list_widget *) child);
			break;
		   case WT_CMENU:
		   	uw_s_cmenu (lun, (struct list_widget *) child);
			break;
		   default:
		   	ui_warning("Unknown widget type %d\n", child->gw_type);
		}
	}
	return (TRUE);
}





uw_s_list (lun, lw)
int lun;
struct list_widget *lw;
/*
 * Save this list widget to the load file.
 */
{
/*
 * Start by just saving the whole damn structure, even though there's a
 * lot of stuff which will have to be superseded on the load.
 */
 	bfput (lun, lw, sizeof (struct list_widget));
/*
 * Save the command, and the selector if there is one.
 */
	bfput (lun, lw->lw_cbdata, strlen (lw->lw_cbdata) + 1);
	if (lw->lw_flags & LWF_SELECTOR)
		bfput (lun, lw->lw_select, strlen (lw->lw_select) + 1);
/*
 * Save the item list.
 */
	bfput (lun, lw->lw_items, lw->lw_ilen);
/*
 * If there is a map table, we must save it as well.
 */
	if (lw->lw_nmap)
		uw_s_map (lun, lw);
}




uw_s_map (lun, lw)
int lun;
struct list_widget *lw;
/*
 * Save the map table to the file.
 */
{
	int i;
/*
 * First, just write the entire table to the file.
 */
	bfput (lun, lw->lw_map, lw->lw_nmap*sizeof (struct map_table));
/*
 * Since strings are allocated separately, we must go through and save them
 * separately.
 */
	for (i = 0; i < lw->lw_nmap; i++)
		if (lw->lw_map[i].mt_type == SYMT_STRING)
			bfput (lun, lw->lw_map[i].mt_v.us_v_ptr,
				strlen (lw->lw_map[i].mt_v.us_v_ptr) + 1);
}




uw_s_cmenu (lun, lw)
int lun;
struct list_widget *lw;
/*
 * Save this cmenu widget to the load file.
 */
{
/*
 * Start by just saving the whole damn structure, even though there's a
 * lot of stuff which will have to be superseded on the load.
 */
 	bfput (lun, lw, sizeof (struct list_widget));
/*
 * Save the selector if there is one.
 */
	if (lw->lw_flags & LWF_SELECTOR)
		bfput (lun, lw->lw_select, strlen (lw->lw_select) + 1);
/*
 * Save the item and command lists.
 */
	bfput (lun, lw->lw_items, lw->lw_ilen);
	bfput (lun, lw->lw_cbdata, lw->lw_clen);
/*
 * If there is a map table, we must save it as well.
 */
	if (lw->lw_nmap)
		uw_s_map (lun, lw);
}





uw_load (lun, init)
int lun, init;
/*
 * Restore some widgets from the file.
 */
{
	int type, nc;
	char name[MAXTITLE], title[MAXTITLE];
	struct frame_widget *frame = 0;
	struct gen_widget *gw, *uw_l_list (), *uw_l_cmenu ();
/*
 * Go through all the widgets in the file.
 */
	while (bfget (lun, &type, sizeof (int)) > 0)
	{
	/*
	 * Deal with this particular entry.
	 */
	 	switch (type)
		{
		/*
		 * Frame widgets are used to hold the rest.
		 */
		   case WT_FRAME:
		   /*
		    * If we are already working on a frame, we're done with
		    * it and should make it official.
		    */
			if (frame)
				uw_wdef (frame);
		   /*
		    * Grab the info, and make a new frame out of it.
		    */
			bfget (lun, name, MAXTITLE);
			bfget (lun, title, MAXTITLE);
			frame = uw_make_frame (name, title);
			if (init)
				frame->fw_flags |= WF_INIT;
			break;
		/*
		 * List widgets.
		 */
		   case WT_LIST:
			gw = uw_l_list (lun);
			uw_add_child (frame, gw);
			break;
		/*
		 * Cmenu widgets.
		 */
		   case WT_CMENU:
			gw = uw_l_cmenu (lun);
			uw_add_child (frame, gw);
			break;
		/*
		 * ???
		 */
		   default:
		   	c_panic ("Unknown widget type %d in loadfile", type);
		}
	}
/*
 * Define our last frame, if called for.
 */
	if (frame)
		uw_wdef (frame);
}





struct gen_widget *
uw_l_list (lun)
int lun;
/*
 * Load a list widget from the file.
 */
{
	struct list_widget *lw;
	char ctmp[500];
	void ui_perform (), uw_ldestroy ();
/*
 * Allocate a new list widget structure, and read the old one from the file.
 */
	lw = NEW (struct list_widget);
 	bfget (lun, lw, sizeof (struct list_widget));
/*
 * Fill in the function pointers, since they may have changed.
 */
	lw->lw_create = uw_lcreate;
	lw->lw_destroy = uw_ldestroy;
	lw->lw_cb = ui_perform;
	/* lw->lw_flags &= ~(WF_CREATED | WF_POPPED); */
/*
 * Get the command, and the selector if there is one.
 */
	bfget (lun, ctmp, 500);
	lw->lw_cbdata = usy_pstring (ctmp);
	if (lw->lw_flags & LWF_SELECTOR)
	{
		bfget (lun, ctmp, 500);
		lw->lw_select = usy_pstring (ctmp);
		lw->lw_popup = uw_lselect;
	}
/*
 * Get the item list.
 */
	lw->lw_items = getvm (lw->lw_ilen);
	bfget (lun, lw->lw_items, lw->lw_ilen);
/*
 * If there is a map table, we must get it as well.
 */
	if (lw->lw_nmap)
		uw_l_map (lun, lw);
	return ((struct gen_widget *) lw);
}





uw_l_map (lun, lw)
int lun;
struct list_widget *lw;
/*
 * load the map table from the file.
 */
{
	int i;
	char ctmp[200];
/*
 * First, just read the entire table to the file.
 */
	lw->lw_map = (struct map_table *)
			getvm (lw->lw_nmap * sizeof (struct map_table));
	bfget (lun, lw->lw_map, lw->lw_nmap*sizeof (struct map_table));
/*
 * Since strings are allocated separately, we must go through and load them
 * separately.
 */
	for (i = 0; i < lw->lw_nmap; i++)
		if (lw->lw_map[i].mt_type == SYMT_STRING)
		{
			bfget (lun, ctmp, 200);
			lw->lw_map[i].mt_v.us_v_ptr = usy_pstring (ctmp);
		}
}





struct gen_widget *
uw_l_cmenu (lun)
int lun;
/*
 * Load this cmenu widget from the load file.
 */
{
	struct list_widget *lw;
	char ctmp[500], *cp;
	void ui_perform (), uw_ldestroy ();
	int i;
/*
 * Allocate and read a new structure.
 */
	lw = NEW (struct list_widget);
 	bfget (lun, lw, sizeof (struct list_widget));
/*
 * Fix up the function pointers, since they may have changed.
 */
	lw->lw_create = uw_lcreate;
	lw->lw_destroy = uw_ldestroy;
	lw->lw_cb = ui_perform;
	/* lw->lw_flags &= ~(WF_CREATED | WF_POPPED); */
/*
 * Get the selector if there is one.
 */
	if (lw->lw_flags & LWF_SELECTOR)
	{
		bfget (lun, ctmp, 500);
		lw->lw_select = usy_pstring (ctmp);
		lw->lw_popup = uw_lselect;
	}
/*
 * Get the item and command lists.
 */
	lw->lw_items = getvm (lw->lw_ilen);
	bfget (lun, lw->lw_items, lw->lw_ilen);
	lw->lw_cbdata = getvm (lw->lw_clen);
	bfget (lun, lw->lw_cbdata, lw->lw_clen);
/*
 * Now go through and recreate the cptr array.
 */
	lw->lw_cptr = (char **) getvm (lw->lw_nitem * sizeof (char *));
	cp = lw->lw_cbdata;
	for (i = 0; i < lw->lw_nitem; i++)
	{
		lw->lw_cptr[i] = cp;
		cp += strlen (cp) + 1;
	}
/*
 * If there is a map table, we must read it as well.
 */
	if (lw->lw_nmap)
		uw_l_map (lun, lw);
	return ((struct gen_widget *) lw);
}






uw_zap_widget (frame)
struct frame_widget *frame;
/*
 * Cause this widget to cease to exist.
 * Entry:
 *	FRAME	is the frame structure.
 * Exit:
 *	The widget is history.
 */
{
	struct gen_widget *zap, *next;
/*
 * First, go through and zap each child of this frame.
 */
	next = frame->fw_next;
	for (zap = next; zap; zap = next)
	{
		next = zap->gw_next;
		(*zap->gw_destroy) (zap, frame->fw_flags & WF_CREATED);
	}
/*
 * Clean up the frame widgets.
 */
	if (frame->fw_flags & WF_CREATED)
	{
	 	XtDestroyWidget (frame->fw_form);
		XtDestroyWidget (frame->fw_w);
	}
/*
 * Undefine this name and get rid of the frame.
 */
	usy_z_symbol (Widget_table, frame->fw_name);
	relvm (frame);
/*
 * Sync up, so that any deleted widgets disappear from the screen.
 */
	uw_sync ();
}






void
uw_ldestroy (gw, realized)
struct gen_widget *gw;
bool realized;
/*
 * Get rid of this list widget.
 */
{
	struct list_widget *lw = (struct list_widget *) gw;
/*
 * If this widget was realized, we must get rid of the associated X 
 * data structures.
 */
	if (realized)
		XtDestroyWidget (lw->lw_list);
/*
 * Widgets with selectors need to have the daemon disabled.
 */
	if (lw->lw_flags & LWF_SELECTOR && realized)
		usy_z_daemon (Ui_variable_table, lw->lw_select, SOP_WRITE,
			uw_ldaemon, (char *) lw);
/*
 * Go through and release memory.
 */
 	if (lw->lw_flags & LWF_SELECTOR)
		usy_rel_string (lw->lw_select);
	relvm (lw->lw_items);
	if (lw->lw_nmap)
		relvm (lw->lw_map);
	relvm (lw->lw_cbdata);
/*
 * CMENU's have some extra junk.
 */
 	if (lw->lw_type == WT_CMENU)
		relvm (lw->lw_cptr);
/*
 * Finally, get rid of the structure itself.
 */
	relvm (lw);
}




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
		return (FALSE);
/*
 * Initialize the menu structure, and add it to the list.
 */
 	menu->mbm_nentries = 0;
	menu->mbm_next = 0;
	strcpy (menu->mbm_name, UPTR (cmds[1]));
	if (mb->mw_nmenus++ == 0)
		mb->mw_menus = menu;
	else
	{
		for (list = mb->mw_menus; list->mbm_next;
				list = list->mbm_next)
			;
		list->mbm_next = menu;
	}
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
		uw_mbmcreate (mw, menu);
}





uw_mbmcreate (mw, menu)
struct menubar_widget *mw;
struct mb_menu *menu;
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
	XtSetArg (margs[0], XtNmenuName, menu->mbm_name);
	menu->mbm_button = XtCreateManagedWidget (menu->mbm_name,
		menuButtonWidgetClass, mw->mw_w, margs, ONE);
	menu->mbm_pulldown = XtCreatePopupShell (menu->mbm_name,
		simpleMenuWidgetClass, menu->mbm_button, NULL, ZERO);
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


uw_mbdestroy () {}

# endif /* XSUPPORT */

