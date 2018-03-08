/*
 * List-widget specific windowing code.
 */

# ifdef XSUPPORT
static char *rcsid = "$Id: ui_wList.c,v 1.10 2002-07-11 22:50:44 burghart Exp $";
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
# include <X11/Xaw/Command.h>
# include <X11/Xaw/List.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>

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
 * List widgets with a single callback, as well as "cmenu" widgets.
 */
struct list_widget
{
	int	lw_type;	/* = WT_LIST or WT_CMENU		*/
	struct gen_widget *lw_next;	/* Next widget in the chain	*/
	void (*lw_create)();	/* The routine to create this thing	*/
	void (*lw_popup) ();	/* Popup routine			*/
	void (*lw_destroy) ();	/* The destroy method			*/
	GenWidget *(*lw_cl) ();	/* Unused 				*/
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
 * Prototypes 
 */
int uw_ldaemon (char *sym, char *cw, int op, int ot, union usy_value *ov, 
		 int nt, union usy_value *nv);
void uw_lselect (struct list_widget *lw);
void uw_setselector (char *sym, int value);
void uw_lsel_fix (struct list_widget *lw, int type, union usy_value *v);



void
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
	void uw_lcreate (), uw_ldestroy ();
/*
 * Make a new list widget structure.
 */
	new = NEW (struct list_widget);
	new->lw_cbdata = 0;
	new->lw_type = WT_LIST;
	new->lw_create = uw_lcreate;
	new->lw_destroy = uw_ldestroy;
	new->lw_flags = 0;
	new->lw_cb = ui_perform;
	new->lw_nmap = 0;
/*
 * Now read in the info.
 */
	ERRORCATCH
		ui_subcommand ("ust$in-list", "List>", uw_in_list, (long) new);
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





int
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
	    	ui_subcommand ("ust$in-map", "  Map>", uw_in_map, (long) &mt);
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
	XawListReturnStruct *ritem = (XawListReturnStruct *) item;
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
	XtAddCallback (w->lw_list, XtNcallback, (XtCallbackProc)
		(w->lw_type == WT_LIST ? uw_lcb : uw_cmcb), w);
/*
 * If there is a selector variable, set up a watcher for it.
 */
	if (w->lw_flags & LWF_SELECTOR)
		usy_daemon (Ui_variable_table, w->lw_select, SOP_WRITE,
			uw_ldaemon, (char *) w);
}




int
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
		return (TRUE);
/*
 * If the value hasn't changed, we also do nothing.
 */
	if (ot == nt && ov->us_v_int == nv->us_v_int)
		return (TRUE);
/*
 * Fix up the widget.
 */
 	uw_lsel_fix (w, nt, nv);
	uw_sync ();
	return (TRUE);
}





struct gen_widget *
uw_cm_def ()
{
/*
 * Interactively define a command menu widget.
 */
	struct list_widget *new;
	int uw_in_cmenu (), i;
	void uw_lcreate (), uw_ldestroy ();
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
	new->lw_nmap = 0;
/*
 * Put together a cmenu struct, and get the rest of the info.
 */
	cm.c_lw = new;
	cm.c_nentry = 0;
	cm.c_select = (char *) 0;
	ERRORCATCH
		ui_subcommand ("ust$in-cmenu", "CMenu>", uw_in_cmenu,
					(long) &cm);
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




int
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
	    	ui_subcommand ("ust$in-map", "  Map>", uw_in_map, (long) &mt);
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






void
uw_cmcb (xw, lw, item)
Widget xw;
struct list_widget *lw;
char *item;
/*
 * The command menu call back.
 */
{
	union usy_value v;
	XawListReturnStruct *ritem = (XawListReturnStruct *) item;
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




void
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



void
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



void
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
		uw_s_map (lun, lw->lw_nmap, lw->lw_map);
}



void
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
		uw_s_map (lun, lw->lw_nmap, lw->lw_map);
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
	void uw_ldestroy ();
	struct map_table *uw_LoadMap ();
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
		lw->lw_map = uw_LoadMap (lun, lw->lw_nmap);
	return ((struct gen_widget *) lw);
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
	void uw_ldestroy ();
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
		lw->lw_map = uw_LoadMap (lun, lw->lw_nmap);
	return ((struct gen_widget *) lw);
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




# endif /* XSUPPORT */
