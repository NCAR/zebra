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
# include <X11/Form.h>
# include <X11/Label.h>
# include <X11/VPaned.h>

# else
# include <X11/Xaw/Box.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/VPaned.h>
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

static char *Rcsid = "$Id: ui_window.c,v 1.12 1990-05-13 11:12:27 corbet Exp $";

static bool Initialized = FALSE;
static bool Active = FALSE;	/* Is window mode active??	*/


/*
 * Fonts.
 */
# define TITLE_FONT_LEN 100
static char Title_font_name[TITLE_FONT_LEN];/* Name of the label font	*/
# define DEFAULT_TITLE_FONT \
	"-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1"


/*
 * Kludge flag used during save operations.
 */
static bool Save_all = 0;
	

/*
 * Forward routines.
 */
struct gen_widget *uw_list_def (), *uw_cm_def (), *uw_mb_def ();




uw_init ()
/*
 * Initialize.  This is stuff which can (and should) be done whether we
 * will ever be running in window mode or not.
 */
{
	union usy_value v;
/*
 * Set up our symbol tables.
 */
	Widget_table = usy_c_stbl ("ui$widget_table");
	Widget_vars = usy_c_stbl ("ui$widget_vars");
	v.us_v_ptr = (char *) Widget_vars;
	usy_s_symbol (usy_g_stbl ("usym$master_table"), "w", SYMT_SYMBOL, &v);
/*
 * Indirect variables.
 */
	usy_c_indirect (Ui_variable_table, "ui$title_font", Title_font_name,
		SYMT_STRING, TITLE_FONT_LEN);
	strcpy (Title_font_name, DEFAULT_TITLE_FONT);
	usy_c_indirect (Ui_variable_table, "ui$menu_mark_file", Mb_mark_file,
		SYMT_STRING, 200);
	strcpy (Mb_mark_file, DEFAULT_MARK_FILE);
	Mb_mark = 0;
}





uw_mode (cmds)
struct ui_command *cmds;
/*
 * perform a shift into Window mode.
 */
{
	Arg args[10];
	void uw_quit (), uw_sel (), uw_do_pop ();
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
		Top = XtAppInitialize (&Appc, Appl_name, NULL, ZERO, &Argc, 
			Argv, Resources, NULL, ZERO);
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
/*
 * Finally, if we have a widget name as an argument, go ahead and put it up.
 */
	if (cmds->uc_ctype != UTT_END)
		uw_popup (UPTR (*cmds));
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










uw_sync ()
/*
 * Sync up with the server.
 */
{
/*
 * Synchronize, if we are running.  Also call xevent, because SYNC can
 * cause things to be queued.
 */
	if (Initialized)
	{
		XSync (XtDisplay (Top), False);
		uw_xevent ();
	}
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
		   case WT_MENUBAR:
		   	uw_SaveMenubar (lun, (struct menubar_widget *) child);
			break;
		   default:
		   	ui_warning("Unknown widget type %d\n", child->gw_type);
		}
	}
	return (TRUE);
}





uw_s_map (lun, nmap, map)
int lun, nmap;
struct map_table *map;
/*
 * Save the map table to the file.
 */
{
	int i;
/*
 * First, just write the entire table to the file.
 */
	bfput (lun, map, nmap*sizeof (struct map_table));
/*
 * Since strings are allocated separately, we must go through and save them
 * separately.
 */
	for (i = 0; i < nmap; i++)
		if (map[i].mt_type == SYMT_STRING)
			bfput (lun, map[i].mt_v.us_v_ptr,
				strlen (map[i].mt_v.us_v_ptr) + 1);
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
	struct gen_widget *uw_LoadMenubar ();
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
		 * Menubars
		 */
		   case WT_MENUBAR:
		   	gw = uw_LoadMenubar (lun);
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







struct map_table *
uw_LoadMap (lun, nmap)
int lun, nmap;
/*
 * load the map table from the file.
 */
{
	int i;
	char ctmp[200];
	struct map_table *map;
/*
 * First, just read the entire table to the file.
 */
	map = (struct map_table *)
			getvm (nmap * sizeof (struct map_table));
	bfget (lun, map, nmap*sizeof (struct map_table));
/*
 * Since strings are allocated separately, we must go through and load them
 * separately.
 */
	for (i = 0; i < nmap; i++)
		if (map[i].mt_type == SYMT_STRING)
			map[i].mt_v.us_v_ptr = uw_LoadString (lun);
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








char *
uw_LoadString (lun)
int lun;
/*
 * Load a character string, which is expected to be the next record
 * in this file.
 */
{
	char ctmp[500];

	bfget (lun, ctmp, 500);
	return (usy_pstring (ctmp));
}



# endif /* XSUPPORT */
