/* 3/89 jc */
# include <unistd.h>

# ifdef XSUPPORT
/* 
 * Window system code.
 */
# include <X11/X.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/cursorfont.h>
# include <X11/Intrinsic.h>
# include <X11/Xaw/Cardinals.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Box.h>
# include <X11/Xaw/Command.h>
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

static char *Rcsid = "$Id: ui_window.c,v 1.38 2001-06-19 22:21:48 granger Exp $";

/*
 * Public variables, declared in ui_window.h
 */
Widget Top;		/* Our (unrealized) top widget		*/
XtAppContext Appc;	/* The application context		*/
XFontStruct *Labelfont;	/* The font for labels.			*/
Cursor Zapcursor;	/* The cursor for zap buttons		*/

/*
 * This pixmap holds the little mark used in pulldown menus.
 */
Pixmap Mb_mark;
char Mb_mark_file[200];

/*
 * We have two symbol tables.
 */
stbl Widget_table;	/* Where the actual widgets live	*/
stbl Widget_vars;	/* Variables used by commands and such  */


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
 * A symbol table used to hold pixmaps.
 */
static stbl PixmapTable;
static stbl FontTable;

/*
 * Prototypes
 */
struct gen_widget *uw_list_def (), *uw_cm_def (), *uw_mb_def ();
extern struct gen_widget *uw_DefIPU ();

Widget uw_MakeHeader (FrameWidget *, Widget);
static FrameWidget *uw_MakeInstance (FrameWidget *);
void uw_FPopup (Widget, XEvent *, String *, Cardinal *);
void uw_FBRet (Widget, XEvent *, String *, Cardinal *);
void uw_BringDown (FrameWidget *);
void uw_wdef (struct frame_widget *frame);
void uw_GeomPopup (char *name, char *geom);
void uw_cr_frame (struct frame_widget *w);
void uw_add_child (struct frame_widget *frame, struct gen_widget *child);
void uw_cchild (struct frame_widget *frame, struct gen_widget *child);
void uw_sync (void);
void uw_zap_widget (struct frame_widget *frame);



/*
 * The custom translations that we use.
 */
static XtActionsRec UIActions[] =
{
	{ "UIFormPopup",	uw_FPopup	},
	{ "UIBlankRet",		uw_FBRet	},
};


void
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
	PixmapTable = usy_c_stbl ("ui$pixmaps");
	FontTable = usy_c_stbl ("ui$fonts");
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
	Mb_mark_file[0] = '\0';
	Mb_mark = 0;
}




void
uw_mode (cmds)
struct ui_command *cmds;
/*
 * perform a shift into Window mode.
 */
{
	char	*popup = NULL, *geom = NULL;

	uw_ForceWindowMode (NULL, 0, 0);

	if (cmds[0].uc_ctype != UTT_END)
	{
		popup = UPTR (cmds[0]);
		if (cmds[1].uc_ctype != UTT_END)
			geom = UPTR (cmds[1]);
		uw_GeomPopup (popup, geom);
	}
}




int
uw_ForceWindowMode (popup, top, appc)
char *popup;
Widget *top;
XtAppContext *appc;
/*
 * Force the system into window mode.
 */
{
	Arg args[10];
	void uw_quit (), uw_sel (), uw_do_pop ();
	void uw_xevent ();
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
		Top = XtAppInitialize (&Appc, Appl_name, NULL, ZERO, Argc, 
			Argv, (String *) Resources, NULL, ZERO);
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
 * Add our actions.
 */
	XtAppAddActions (Appc, UIActions, TWO);
	XtRegisterGrabAction (uw_FPopup, True,
	      ButtonPressMask|ButtonReleaseMask, GrabModeAsync, GrabModeAsync);
	XawSimpleMenuAddGlobalActions (Appc);
	RdssMenuAddGlobalActions (Appc);
/*
 * If we have a widget name as an argument, go ahead and put it up.
 */
	if (popup)
		uw_popup (popup);
/*
 * Return the info.
 */
	if (top)
		*top = Top;
	if (appc)
		*appc = Appc;

	return (TRUE);
}



void
uw_XInit (top, appc)
Widget	*top;
XtAppContext	*appc;
/*
 * Initialize X stuff without actually entering window mode.  (The creates
 * no control stack problems when we just want to get the Top widget set up)
 */
{
	if (! Initialized)
	{
		Top = XtAppInitialize (&Appc, Appl_name, NULL, ZERO, Argc, 
			Argv, (String *) Resources, NULL, ZERO);
		Labelfont = XLoadQueryFont (XtDisplay (Top), Title_font_name);
		Zapcursor = XCreateFontCursor (XtDisplay (Top), XC_pirate);
		Initialized = TRUE;
	}

	*top = Top;
	*appc = Appc;
}



void
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
				  




int
uw_t_popdown (symbol, type, v, junk)
char *symbol;
int type, junk;
union usy_value *v;
/*
 * Make sure this widget is popped down.
 */
{
	struct frame_widget *fw = (struct frame_widget *) v->us_v_ptr;

	uw_BringDown (fw);
	return (TRUE);
}






void
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






int
uw_define (cmds)
struct ui_command *cmds;
{
	char *name = UPTR (cmds[0]), *title = UPTR (cmds[2]);
	int type = UINT (cmds[1]);
	struct gen_widget *gw, *uw_FormDef ();
	struct frame_widget *frame;
	bool noframe = FALSE;
/*
 * Create the frame widget first.
 */
	frame = uw_make_frame (name, title);
	frame->fw_flags = WF_INTERNAL;
/*
 * Define the real guts of the widget.
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
	   	gw = uw_mb_def (frame);
		break;
	/*
	 * Internal, frameless, popup menus.
	 */
	   case WT_INTPOPUP:
		gw = uw_DefIPU (name);
		noframe = TRUE;
		break;
	/*
	 * Forms.
	 */
	   case WT_FORM:
	   	gw = uw_FormDef (frame);
		frame->fw_flags |= WF_PROTOTYPE;
		break;

	   default:
	   	ui_error ("(BUG): unknown widget type: %d\n", type);
	}
/*
 * Tie it all together and truly define the new widget.
 */
	if (noframe)
		frame->fw_flags |= WF_NOFRAME;
	uw_add_child (frame, (struct gen_widget *) gw);
	uw_wdef (frame);
	return (TRUE);
}




void
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



int
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
		ui_subcommand ("ust$in-stack", "Stack>", uw_in_stack, 
			(long) frame);
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





int
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







int
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
	
			

void
uw_PositionWidget (Widget w)
/*
 * Get the current size and location of the popup shell widget and make
 * sure the entire widget appears on the screen (or as much as possible
 * anyway).  Since we could have already been moved based on an old size,
 * we have to start all over with positioning the menu/widget based on the
 * pointer location.
 *
 * This function originally copied from a zebra implementation in
 * src/gp/Icons.c.  18 Jun 2001, gjg
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

	/*
	 * The width will not be correct unless it is realized, but we
	 * don't want to map it yet (i.e. we don't want it to receive any
	 * events). 
	 */
	if (! XtIsRealized (w))
	{
	    XtSetMappedWhenManaged (w, False);
	    XtRealizeWidget(w);
	}

	n = 0;
	XtSetArg (args[n], XtNwidth, &width); ++n;
	XtSetArg (args[n], XtNheight, &height); ++n;
	XtSetArg (args[n], XtNborderWidth, &border); ++n;
	XtGetValues (w, args, n);

	XQueryPointer (XtDisplay(w), XtWindow(w), &root, &child,
		       &root_x, &root_y, &win_x, &win_y, &mask);
	/*
	 * Default to popping up with pointer over top left corner
	 */
	x = root_x - 5;
	y = root_y - 5;

	sw = WidthOfScreen (XtScreen(w));
	sh = HeightOfScreen (XtScreen(w));
	if (x - 5 + width + 2*border > (unsigned) sw)
		x -= width + 2*border;
	if (y - 5 + height + 2*border > (unsigned) sh)
		y = sh - height - 2*border;

	/*
	 * Set the widget to its new location.  If it hasn't changed,
	 * it will ignore this.
	 */
	n = 0;
	XtSetArg (args[n], XtNx, x); ++n;
	XtSetArg (args[n], XtNy, y); ++n;
	XtSetValues (w, args, n);
}



void
uw_popup (name)
char *name;
/*
 * Pop this particular widget up.
 */
{
	uw_GeomPopup (name, NULL);
}



void
uw_GeomPopup (name, geom)
char *name, *geom;
/*
 * Pop this particular widget up, with the specified X syntax geometry (if any)
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
 * If the widget is already popped up, pop it down so that it will pop up
 * with the new geometry. 
 */
 	if ((frame->fw_flags & WF_POPPED) && geom)
	{
	    uw_BringDown (frame);
	}
/*
 * Certain widgets are not meant to be popped up from the command line.
 */
	if (frame->fw_flags & WF_NOFRAME)
		ui_error ("Widget '%s' can not be popped up directly", name);
/*
 * If this is a prototype widget, we need to get an instance instead.
 */
	if (frame->fw_flags & WF_PROTOTYPE)
		frame = uw_MakeInstance (frame);
/*
 * Make sure it has been created.
 */
 	if (! (frame->fw_flags & WF_CREATED))
		uw_cr_frame (frame);
/*
 * Set the geometry
 */
	if (geom)
	{
		Arg	arg;

		XtSetArg (arg, XtNgeometry, geom);
		XtSetValues (frame->fw_w, &arg, 1);
	}
/*
 * Now put it up on the screen.  Position it at the pointer if no other
 * position information has been specified (even if it is already popped
 * up). 
 */
	if (! geom && frame->fw_x == NotSpecified)
	{
	    uw_PositionWidget (frame->fw_w);
	}
 	if (! (frame->fw_flags & WF_POPPED))
	{
	    XtPopup (frame->fw_w, XtGrabNone);
	}
	else
	{
	    XRaiseWindow (XtDisplay(frame->fw_w), XtWindow(frame->fw_w));
	}
	uw_sync ();
	frame->fw_flags |= WF_POPPED;
/*
 * Pass through the child widgets, doing whatever work is needed.
 */
	for (gw = frame->fw_next; gw; gw = gw->gw_next)
		if (gw->gw_popup)
			(*gw->gw_popup) (gw);
}


void
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
 * Pull it down.
 */
	uw_BringDown (frame);
}





void
uw_BringDown (frame)
FrameWidget *frame;
/*
 * Insure that this frame is not on the screen.
 */
{
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
/*
 * If it is an instance widget, we wipe it out.
 */
	if (frame->fw_flags & WF_INSTANCE)
		uw_zap_widget (frame);
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
	w->fw_ninst = w->fw_nchild = 0;
	w->fw_inst = 0;
	w->fw_x = w->fw_y = w->fw_width = w->fw_height = NotSpecified;
	strcpy (w->fw_name, name);
	strcpy (w->fw_title, title);
	return (w);
}




void
uw_cr_frame (w)
struct frame_widget *w;
/*
 * Create this frame widget.
 */
{
	Arg args[10];
	Widget zap, label, form, header;
	void uw_zapbutton ();
	struct gen_widget *gw;
	int n;
/*
 * Create the popup shell, and the form to go within it.
 */
	n = 0;
	XtSetArg (args[n], XtNinput, True);		n++;
	if (w->fw_x != NotSpecified)
	{
		XtSetArg (args[n], XtNx, w->fw_x);	n++;
		XtSetArg (args[n], XtNy, w->fw_y);	n++;
	}
	if (w->fw_width != NotSpecified)
	{
		XtSetArg (args[n], XtNwidth, w->fw_width);	n++;
		XtSetArg (args[n], XtNheight, w->fw_height);	n++;
	}
	if (w->fw_flags & WF_OVERRIDE)
	{
		XtSetArg (args[n], XtNtransient, True);	n++;
	}
	else if (w->fw_next && w->fw_next->gw_type == WT_FORM)
	{
		XtSetArg (args[n], XtNtransient, False); n++;
	}
	w->fw_w = XtCreatePopupShell (w->fw_name, topLevelShellWidgetClass,
		Top, args, n);
	form = XtCreateManagedWidget ("form", formWidgetClass, w->fw_w,
		args, 0);
/*
 * If called for, make the title and zap button header.
 */
	if (! (w->fw_flags & WF_NOHEADER))
		header = uw_MakeHeader (w, form);
	else
		header = NULL;
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




Widget
uw_MakeHeader (frame, form)
FrameWidget *frame;
Widget form;
/*
 * Create a header widget for this frame.
 */
{
	Arg args[10];
	int n;
	Widget header, label, zap;
/*
 * Create a second form to hold the title and zap button.
 */
	n = 0;
	XtSetArg (args[n], XtNdefaultDistance, 1);
	XtSetArg (args[n], XtNtop, XtChainTop);			n++;
	XtSetArg (args[n], XtNbottom, XtChainTop);		n++;
	XtSetArg (args[n], XtNleft, XtChainLeft);		n++;
	XtSetArg (args[n], XtNright, XtChainLeft);		n++;
	header = XtCreateManagedWidget ("header", formWidgetClass, form,
		args, n);
/*
 * Add the label and the zap button to the form.
 */
	n = 0;
	XtSetArg (args[n], XtNlabel, frame->fw_title);		n++;
	XtSetArg (args[n], XtNjustify, XtJustifyLeft);		n++;
	XtSetArg (args[n], XtNresize, True);			n++;
	XtSetArg (args[n], XtNborderWidth, 0);			n++;
	XtSetArg (args[n], XtNfont, (XtArgVal) Labelfont);	n++;
	label = XtCreateManagedWidget ("title", labelWidgetClass, header,
		args, n);
	n = 0;
	XtSetArg (args[n], XtNfromHoriz, label);		n++;
	XtSetArg (args[n], XtNfromVert, NULL);			n++;
	XtSetArg (args[n], XtNcursor, Zapcursor);		n++;
	zap = XtCreateManagedWidget ("zap", commandWidgetClass, header,args,n);
	XtAddCallback (zap, XtNcallback, (XtCallbackProc)uw_zapbutton, frame);
/*
 * Done.
 */
 	return (header);
}



void
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



void
uw_cchild (frame, child)
struct frame_widget *frame;
struct gen_widget *child;
/*
 * Create the child widget.
 */
{
	Arg formargs[10];
	int n;
/*
 * Actually create the child widget.
 */
	(*child->gw_create) (child, frame->fw_form);
/*
 * Add the form resources to this widget, then get the form to actually
 * manage it.
 */
	n = 0;
	XtSetArg (formargs[n], XtNfromHoriz, NULL);		n++;
	XtSetArg (formargs[n], XtNfromVert, frame->fw_bottom);	n++;
	XtSetArg (formargs[n], XtNtop, XtChainTop);		n++;
	XtSetArg (formargs[n], XtNbottom, XtChainBottom);	n++;
	XtSetArg (formargs[n], XtNleft, XtChainLeft);		n++;
	XtSetArg (formargs[n], XtNright, XtChainRight);		n++;
	XtSetValues (child->gw_w, formargs, n);
	XtManageChild (child->gw_w);
/*
 * This is the new bottommost widget.
 */
	frame->fw_bottom = child->gw_w;
}









void
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




int
uw_map_lookup (type, v, nmap, map)
int type, nmap;
union usy_value *v;
struct map_table *map;
/*
 * Do a map table->index lookup.
 */
{
	int index = -1, i;
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





void
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



int
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
 * See if we should really save this one.
 */
	frame = (struct frame_widget *) gw;
	if ((frame->fw_flags & WF_INIT && ! Save_all) ||
			(frame->fw_flags & WF_INTERNAL) == 0 ||
			frame->fw_flags & WF_INSTANCE)
		return (TRUE);
/*
 * Put out the frame-specific stuff.
 */
	if (usy_defined (Ui_variable_table, "ui$save_babble"))
		ui_printf ("Saving widget '%s'\n", frame->fw_name);
	bfput (lun, &gw->gw_type, sizeof (int));
	bfput (lun, &frame->fw_flags, sizeof (int));
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
		   case WT_INTPOPUP:
		   	uw_SavePulldown (lun, child);
			break;
		   case WT_FORM:
		   	uw_FSave (lun, child);
			break;
		   default:
		   	ui_warning("Unknown widget type %d\n", child->gw_type);
		}
	}
	return (TRUE);
}




void
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






void
uw_load (lun, init)
int lun, init;
/*
 * Restore some widgets from the file.
 */
{
	int type, flags;
	char name[MAXTITLE], title[MAXTITLE];
	struct frame_widget *frame = 0;
	struct gen_widget *gw, *uw_l_list (), *uw_l_cmenu ();
	struct gen_widget *uw_LoadMenubar (), *uw_LoadPulldown ();
	struct gen_widget *uw_FLoad ();
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
			bfget (lun, &flags, sizeof (int));
			bfget (lun, name, MAXTITLE);
			bfget (lun, title, MAXTITLE);
			frame = uw_make_frame (name, title);
			frame->fw_flags = flags & ~(WF_CREATED | WF_POPPED);
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
		 * Internal popups.
		 */
		   case WT_INTPOPUP:
		   	gw = uw_LoadPulldown (lun);
			uw_add_child (frame, gw);
			break;
		/*
		 * Forms.
		 */
		   case WT_FORM:
		   	gw = uw_FLoad (lun);
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
	return (map);
}





void
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
	if (frame->fw_flags & WF_CREATED && ! (frame->fw_flags & WF_NOFRAME))
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




void
uw_IWRealize (name, parent)
char *name;
Widget parent;
/*
 * Realize an internal widget.
 */
{
	struct gen_widget *gw = uw_g_widget (name);
	struct frame_widget *frame = (struct frame_widget *) gw;
/*
 * Real widget?
 */
	if (! gw)
		ui_error ("uw_IWRealize: undefined internal widget '%s'", 
			name);
# ifdef notdef
/*
 * Make sure this is the right type of widget.
 */
	if ((frame->fw_flags & WF_NOFRAME) == 0)
		ui_error ("(APPL BUG): uw_IWRealize on non-int widget %s", 
			name);
# endif
/*
 * Do it.
 */
	if (! (frame->fw_flags & WF_CREATED))
	{
		(*frame->fw_next->gw_create) (frame->fw_next, parent);
		frame->fw_flags |= WF_CREATED;
	}
}



Widget
uw_IWWidget (name)
char *name;
/*
 * Return the widget corresponding to an internal widget.
 */
{
	struct gen_widget *gw = uw_g_widget (name);
	struct frame_widget *frame = (struct frame_widget *) gw;
# ifdef notdef
/*
 * Make sure this is the right type of widget.
 */
	if ((frame->fw_flags & WF_NOFRAME) == 0)
		ui_error ("(APPL BUG): uw_IWRealize on non-int widget %s", 
			name);
# endif
/*
 * Do it.
 */
	return (frame->fw_next->gw_w);
}



void
uw_IWPopup (name)
char *name;
/*
 * Popup this internal widget as a spring-loaded wonder.
 */
{
	struct gen_widget *gw = uw_g_widget (name);
	struct frame_widget *frame = (struct frame_widget *) gw;
/*
 * Make sure this is the right type of widget.
 */
	if (! frame)
		ui_error ("No such widget: %s", name);
	if ((frame->fw_flags & WF_NOFRAME) == 0)
		ui_error ("(APPL BUG): uw_IWRealize on non-int widget %s", 
			name);
/*
 * If this thing has not been actually realized, do it now.
 */
	if ((frame->fw_flags & WF_CREATED) == 0)
	{
		(*frame->fw_next->gw_create) (frame->fw_next, Top);
		frame->fw_flags |= WF_CREATED;
	}
/*
 * Throw it up on the screen.
 */
	XtPopupSpringLoaded (frame->fw_next->gw_w);
}





void
uw_DoFrameParam (frame, cmds)
FrameWidget *frame;
struct ui_command *cmds;
/*
 * Deal with parameters which change the frame of a widget.
 */
{
	switch (UKEY (*cmds))
	{
	/*
	 * Maybe they don't want a header on this one.
	 */
	   case UIC_NOHEADER:
	   	frame->fw_flags |= WF_NOHEADER;
		break;
	/*
	 * They know where they want it to be.
	 */
	   case UIC_LOCATION:
	   	frame->fw_x = UINT (cmds[1]);
		frame->fw_y = UINT (cmds[2]);
		break;
	/*
	 * Or how big.
	 */
	   case UIC_SIZE:
	   	frame->fw_width = UINT (cmds[1]);
		frame->fw_height = UINT (cmds[2]);
		break;
	/*
	 * Maybe they *really* know what they want.
	 */
	   case UIC_OVERRIDE:
	   	frame->fw_flags |= WF_OVERRIDE;
		break;
	}
}



void
uw_SetGeometry (widget, x, y, width, height)
char *widget;
int x, y, width, height;
/*
 * Change the geometry of a widget from within the program.
 */
{
	FrameWidget *fw = (FrameWidget *) uw_g_widget (widget);
/*
 * Sanity checks.
 */
	if (! fw)
		ui_error ("SetGeometry call on nonexistent widget '%s'",
				widget);
	if (fw->fw_type != WT_FRAME)
		ui_error ("SetGeometry call on nonframe widget '%s'", widget);
/*
 * If the widget is realized, stuff in the values directly.
 */
	if (fw->fw_flags & WF_CREATED)
	{
		Arg args[5];
		if (x >= 0)
		{
			int n = 0;
			XtSetArg (args[n], XtNx, x);	n++;
			XtSetArg (args[n], XtNy, y);	n++;
			XtSetValues (fw->fw_w, args, n);
		}
		if (width > 0)
		{
			int n = 0;
			XtSetArg (args[n], XtNwidth, width);	n++;
			XtSetArg (args[n], XtNheight, height);	n++;
			XtSetValues (fw->fw_form, args, n);
		}
/*		XtSetValues (fw->fw_w, args, n); */
		uw_sync ();
	}
/*
 * In any case store the info in the frame.
 */
	if (x >= 0)
	{
		fw->fw_x = x;
		fw->fw_y = y;
	}
	if (width > 0)
	{
		fw->fw_width = width;
		fw->fw_height = height;
	}
}



void
uw_ForceOverride (name)
char *name;
/*
 * Force the widget by this name into override mode.
 */
{
	FrameWidget *fw = (FrameWidget *) uw_g_widget (name);
	Arg args[5];
/*
 * Sanity checks.
 */
	if (! fw)
		ui_error ("Override call on nonexistent widget '%s'", name);
	if (fw->fw_type != WT_FRAME)
		ui_error ("Override call on nonframe widget '%s'", name);
/*
 * If it's already an override, life is easy.
 */
	if (fw->fw_flags & WF_OVERRIDE)
		return;
/*
 * If the widget already exists, we need to pull it from the screen.  Then
 * set the flag.
 */
	if ((fw->fw_flags & WF_CREATED) && (fw->fw_flags & WF_POPPED))
		uw_popdown (name);
	fw->fw_flags |= WF_OVERRIDE | WF_NOHEADER;
/*
 * If the widget already exists, we must stuff it into there too.
 */
	if (fw->fw_flags & WF_CREATED)
	{
		XtSetArg (args[0], XtNoverrideRedirect, True);
		XtSetValues (fw->fw_w, args, 1);
	}
}




static FrameWidget *
uw_MakeInstance (fw)
FrameWidget *fw;
/*
 * Turn a prototype widget into an actual instance.
 */
{
	FrameWidget *inst;
	GenWidget *child;
	char NewName[60];

# ifdef notdef  /* For now don't do this, alas. */
/*
 * Search the list of already-instantiated widgets to see if there are
 * any which are not on the screen.
 */
	for (inst = fw->fw_inst; inst; inst = inst->fw_inst)
		if (! (inst->fw_flags & WF_POPPED))
			return (inst);
# endif
/*
 * Nope, we need to make one.  (KLUDGE) assuming only one child for the 
 * moment; eventually we probably need to follow the chain.
 */
	sprintf (NewName, "%s-%d", fw->fw_name, fw->fw_ninst++);
	inst = uw_make_frame (NewName, fw->fw_title);
	inst->fw_flags = fw->fw_flags;
	child = (*fw->fw_next->gw_clone) (fw->fw_next, inst);
	uw_add_child (inst, child);
	uw_wdef (inst);
/*
 * Add this instantiation to the list and return it.
 */
	inst->fw_flags &= ~WF_PROTOTYPE;
	inst->fw_flags |= WF_INSTANCE; /* Necessary? */
# ifdef notdef
	inst->fw_inst = fw->fw_inst;
	fw->fw_inst = inst;
# endif
	return (inst);
}







Pixmap
uw_GetPixmap (name)
char *name;
/*
 * Try to pull in this file as a pixmap.
 */
{
	Pixmap ret;
	unsigned int w, h;
	int xh, yh, type;
	SValue v;
	char fname[128], spath[256];
	char *delim, *path;
	static int warned = 0;
/*
 * See if we already have this one.
 */
	if (usy_g_symbol (PixmapTable, name, &type, &v))
		return ((Pixmap) v.us_v_ptr);
/*
 * Get our directory search path.  If the wanted name uses an absolute path, 
 * we just build an empty directory path string.  Otherwise check
 * for ui$bitmap_path (or the deprecated ui$bitmap_directory) as the
 * directory path to search.  Failing that, just use the current 
 * directory.
 */
	if (name[0] == '/')
	    spath[0] = '\0';
	else if (usy_g_symbol (Ui_variable_table, "ui$bitmap_path", 
			       &type, &v))
	{
	    if (type == SYMT_STRING)
		strcpy (spath, v.us_v_ptr);
	    else
		ui_warning ("ui$bitmap_path is not a string -- ignored");
	    
	    if (! warned && usy_g_symbol (Ui_variable_table, 
					  "ui$bitmap_directory", &type, &v))
	    {
		ui_warning ("ignoring ui$bitmap_directory: %s",
			    "ui$bitmap_path is defined");
		warned = 1;
	    }
	}
	else if (usy_g_symbol (Ui_variable_table, "ui$bitmap_directory", 
			       &type, &v))
	{
	    if (type == SYMT_STRING)
		strcpy (spath, v.us_v_ptr);
	    else
		ui_warning ("ui$bitmap_directory is not a string -- ignored");
	}
	else
	    spath[0] = '\0';
/*
 * Now use the path to look for the bitmap file
 */
	for (delim = path = spath; delim; path = delim + 1)
	{
	/*
	 * Build up a new program name from the next path entry.
	 */
	    if ((delim = strchr (path, ',')) != 0)
	    {
		strncpy (fname, path, delim - path);
		fname[delim - path] = '\0';
	    }
	    else
		strcpy (fname, path);

	    strcat (fname, "/");
	    strcat (fname, name);
	/*
	 * Break out if this one exists.
	 */
	    if (! access (fname, F_OK))
		break;

	    fname[0] = '\0';
	}

	if (! *fname)
	    return (None);
/*
 * Now try to pull in the file.
 */
	if (XReadBitmapFile (XtDisplay (Top), RootWindow (XtDisplay (Top), 0),
		fname, &w, &h, &ret, &xh, &yh) == BitmapSuccess)
	{
		v.us_v_ptr = (char *) ret;
		usy_s_symbol (PixmapTable, name, SYMT_POINTER, &v);
		return (ret);
	}
	return (None);
}




XFontStruct *
uw_GetFont (name)
char *name;
/*
 * Try to locate a font by this name.
 */
{
	int type;
	SValue v;
	XFontStruct *f;
/*
 * See if we've already found it.
 */
	if (usy_g_symbol (FontTable, name, &type, &v))
		return ((XFontStruct *) v.us_v_ptr);
/*
 * Look it up.
 */
	if (! (f = XLoadQueryFont (XtDisplay (Top), name)))
	{
		ui_warning ("Unable to load font %s", name);
		return (0);
	}
/*
 * Remember it and return.
 */
	v.us_v_ptr = (char *) f;
	usy_s_symbol (FontTable, name, SYMT_POINTER, &v);
	return (f);
}
		







# endif /* XSUPPORT */
