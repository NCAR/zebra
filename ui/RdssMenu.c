/* $Id: RdssMenu.c,v 1.12 2002-07-11 19:33:50 burghart Exp $ */
/*
 * Hacked up version of SimpleMenu to provide some useful stuff -- in
 * particular, better cascading menus.
 */

/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* From the original, before being hacked up:
 *------------------------------------------------------
 * SimpleMenu.c - Source code file for SimpleMenu widget.
 *
 * Date:    April 3, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium 
 *          kit@expo.lcs.mit.edu
 */

#ifdef XSUPPORT

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/XawInit.h>
#include "RdssMenuP.h"
#include "SmeMenu.h"
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Cardinals.h>

#include <X11/Xmu/Initer.h>
#include <X11/Xmu/CharSet.h>

#define streq(a, b)        ( strcmp((a), (b)) == 0 )

/*#define DEBUG*/

#ifdef DEBUG
#define ENTER(msg,w) ui_printf("RdssMenu: %s entering %s\n", 	\
			       (w)?(XtName((Widget)w)):"",msg);
#define EXIT(msg,w) ui_printf("RdssMenu: %s exiting %s\n",	\
			       (w)?(XtName((Widget)w)):"",msg);
#define IFD(cmd) cmd
#define ACK(msg,w) ui_printf("RdssMenu: %s ack %s\n",		\
			       (w)?(XtName((Widget)w)):"",msg);
#else
#define ENTER(msg,w)
#define EXIT(msg,w)
#define IFD(cmd)
#define ACK(msg,w)
#endif

#define offset(field) XtOffsetOf(RdssMenuRec, rdss_menu.field)

static XtResource resources[] = { 

/*
 * Label Resources.
 */

  {XtNlabel,  XtCLabel, XtRString, sizeof(String),
     offset(label_string), XtRString, NULL},
  {XtNlabelClass,  XtCLabelClass, XtRPointer, sizeof(WidgetClass),
     offset(label_class), XtRImmediate, (XtPointer) NULL},

/*
 * Layout Resources.
 */

  {XtNrowHeight,  XtCRowHeight, XtRDimension, sizeof(Dimension),
     offset(row_height), XtRImmediate, (XtPointer) 0},
  {XtNtopMargin,  XtCVerticalMargins, XtRDimension, sizeof(Dimension),
     offset(top_margin), XtRImmediate, (XtPointer) 0},
  {XtNbottomMargin,  XtCVerticalMargins, XtRDimension, sizeof(Dimension),
     offset(bottom_margin), XtRImmediate, (XtPointer) 0},

/*
 * Misc. Resources
 */

  { XtNallowShellResize, XtCAllowShellResize, XtRBoolean, sizeof(Boolean),
      XtOffsetOf(RdssMenuRec, shell.allow_shell_resize),
      XtRImmediate, (XtPointer) TRUE },
  {XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
      offset(cursor), XtRImmediate, (XtPointer) None},
  {XtNmenuOnScreen,  XtCMenuOnScreen, XtRBoolean, sizeof(Boolean),
      offset(menu_on_screen), XtRImmediate, (XtPointer) TRUE},
  {XtNpopupOnEntry,  XtCPopupOnEntry, XtRWidget, sizeof(Widget),
      offset(popup_entry), XtRWidget, NULL},
  {XtNbackingStore, XtCBackingStore, XtRBackingStore, sizeof (int),
      offset(backing_store), 
      XtRImmediate, (XtPointer) (Always + WhenMapped + NotUseful)},
};  
#undef offset

/*
 * We want <BtnUp> to cause more recent modal popups to popdown first, but
 * we also don't want any EnterNotify events caused by submenus popping
 * down to execute a highlight method.  The disable-highlight() action sets
 * an internal flag disabling any further highlight actions.  Highlighting
 * is automatically enabled on popup via an XtNpopupCallback.
 *
 * The other method, perhaps more direct, is to insert a call to action
 * "unmap()" before the "notify()" in the <BtnUp> translation.  The biggest
 * difference between this method and the above is the order the menu
 * windows disappear from the screen.  Unmap() will unmap the window before
 * unhighlighting its SME children, whereas using "disable-unhighlight"
 * will popdown tree post-order traversal while ignoring the resulting
 * <EnterNotify> events in the parent widgets (those further up the modal
 * cascade).
 *
 * Examples of both methods are below.  If you change the method here, you
 * may also want to change the method used by SmeMenu in its Unhighlight
 * method so that submenus are consistent.
 */
#ifdef notdef
#ifndef USE_UNMAP
static char defaultTranslations[] =
    "<EnterWindow>:     highlight()             \n\
     <LeaveWindow>:     unhighlight()           \n\
     <BtnMotion>:       highlight()             \n\
     <BtnUp>:           disable-highlight() notify() \
                        unhighlight() MenuPopdown()";
#else
static char defaultTranslations[] =
    "<EnterWindow>:     highlight()             \n\
     <LeaveWindow>:     unhighlight()           \n\
     <BtnMotion>:       highlight()             \n\
     <BtnUp>:           unmap() notify() unhighlight() MenuPopdown()";
#endif
#endif
#ifdef notdef
/*
 * Forget that stuff above and use these translations instead.  We don't
 * need to highlight on Enter events, and in fact not highlighting on enter
 * events prevents submenus from automatically popping up when menu entries
 * pop up underneath the pointer.
 */
static char defaultTranslations[] =
    "<LeaveWindow>:     unhighlight()           \n\
     <BtnMotion>:       highlight()             \n\
     <BtnUp>:           disable-highlight() notify() \
                        unhighlight() RdssMenuPopdown()";
#endif

/* 
 * These are the translations which allow button-click persistent menus.
 * If 'release' is still enabled, as it is initially on a spring-loaded
 * popup, then the release() action on a BtnUp event just disables release
 * but leaves the menu popped up.  Likewise any pointer motion disables
 * release.  Once release is disabled, the next BtnUp event performs the
 * original BtnUp sequence from the previous version of translations:
 * disable-highlight() notify() unhighlight() RdssMenuPopdown().
 */
static char defaultTranslations[] =
    "<LeaveWindow>:     disable-release() unhighlight()           \n\
     <Motion>:          disable-release() highlight()             \n\
     <BtnMotion>:       disable-release() highlight()             \n\
     <BtnUp>:           release()";


/*
 * This is the amount to shift the menu position by so that the pointer
 * appears inside the left edge of the menu entries.  Use something 
 * greater than 5 for best results.
 */
#define POINTER_X_INDENT 10

/*
 * Semi Public function definitions. 
 */
static void Redisplay(), Realize(), Resize(), ChangeManaged();
static void Initialize(), ClassInitialize(), ClassPartInitialize();
static Boolean SetValues(), SetValuesHook();
static XtGeometryResult GeometryManager();

/*
 * Action Routine Definitions
 */
static void PositionMenuAction(), PositionAndPopupMenuAction();
static void Highlight(), Unhighlight(), Notify();
static void DisableHighlight(), EnableHighlight(), Unmap(), Ungrab();
static void RdssMenuPopdownAction();
static void Release(), DisableRelease();

/* 
 * Private Function Definitions.
 */
static void MakeSetValuesRequest(), CreateLabel(), Layout();
static void AddPositionAction(), PositionMenu(), ChangeCursorOnGrab();
static Dimension GetMenuWidth(), GetMenuHeight();
static Widget FindMenu();
static SmeObject GetEventEntry();
static void MoveMenu();

static XtActionsRec actionsList[] =
{
  {"notify",            Notify},
  {"highlight",         Highlight},
  {"release",           Release},
  {"disable-release",   DisableRelease},
  {"unhighlight",       Unhighlight},
  {"disable-highlight",	DisableHighlight},
  {"enable-hightlight", EnableHighlight},
  {"ungrab",		Ungrab},
  {"unmap",		Unmap},
  {"RdssMenuPopdown",   RdssMenuPopdownAction}
};
 
static CompositeClassExtensionRec extension_rec = {
    /* next_extension */  NULL,
    /* record_type */     NULLQUARK,
    /* version */         XtCompositeExtensionVersion,
    /* record_size */     sizeof(CompositeClassExtensionRec),
    /* accepts_objects */ TRUE,
};

#define superclass (&overrideShellClassRec)
    
RdssMenuClassRec rdssMenuClassRec = {
  {
    /* superclass         */    (WidgetClass) superclass,
    /* class_name         */    "RdssMenu",
    /* size               */    sizeof(RdssMenuRec),
    /* class_initialize   */	ClassInitialize,
    /* class_part_initialize*/	ClassPartInitialize,
    /* Class init'ed      */	FALSE,
    /* initialize         */    Initialize,
    /* initialize_hook    */	NULL,
    /* realize            */    Realize,
    /* actions            */    actionsList,
    /* num_actions        */    XtNumber(actionsList),
    /* resources          */    resources,
    /* resource_count     */	XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    TRUE, 
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/ 	TRUE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    Resize,
    /* expose             */    Redisplay,
    /* set_values         */    SetValues,
    /* set_values_hook    */	SetValuesHook,
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook    */	NULL,			
    /* accept_focus       */    NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table		  */    defaultTranslations,
    /* query_geometry	  */    NULL,
    /* display_accelerator*/    NULL,
    /* extension	  */    NULL
  },{
    /* geometry_manager   */    GeometryManager,
    /* change_managed     */    ChangeManaged,
    /* insert_child	  */	XtInheritInsertChild,
    /* delete_child	  */	XtInheritDeleteChild,
    /* extension	  */    NULL
  },{
    /* Shell extension	  */    NULL
  },{
    /* Override extension */    NULL
  },{
    /* rdss Menu extension*/  NULL
  }
};

WidgetClass rdssMenuWidgetClass = (WidgetClass)&rdssMenuClassRec;

/************************************************************
 *
 * Semi-Public Functions.
 *
 ************************************************************/

/*      Function Name: ClassInitialize
 *      Description: Class Initialize routine, called only once.
 *      Arguments: none.
 *      Returns: none.
 */

static void
ClassInitialize()
{
  XawInitializeWidgetSet();
  XtAddConverter( XtRString, XtRBackingStore, XmuCvtStringToBackingStore,
		 NULL, 0 );
  XmuAddInitializer( AddPositionAction, NULL);
}

/*      Function Name: ClassInitialize
 *      Description: Class Part Initialize routine, called for every
 *                   subclass.  Makes sure that the subclasses pick up 
 *                   the extension record.
 *      Arguments: wc - the widget class of the subclass.
 *      Returns: none.
 */

static void
ClassPartInitialize(wc)
WidgetClass wc;
{
    RdssMenuWidgetClass smwc = (RdssMenuWidgetClass) wc;

/*
 * Make sure that our subclass gets the extension rec too.
 */

    extension_rec.next_extension = smwc->composite_class.extension;
    smwc->composite_class.extension = (XtPointer) &extension_rec;
}

/*      Function Name: Initialize
 *      Description: Initializes the rdss menu widget
 *      Arguments: request - the widget requested by the argument list.
 *                 new     - the new widget with both resource and non
 *                           resource values.
 *      Returns: none.
 */

/* ARGSUSED */
static void
Initialize(request, new)
Widget request, new;
{
  RdssMenuWidget smw = (RdssMenuWidget) new;

  XmuCallInitializers(XtWidgetToApplicationContext(new));

  if (smw->rdss_menu.label_class == NULL) 
      smw->rdss_menu.label_class = smeBSBObjectClass;

  smw->rdss_menu.label = NULL;
  smw->rdss_menu.entry_set = NULL;
  smw->rdss_menu.recursive_set_values = False;

  if (smw->rdss_menu.label_string != NULL)
      CreateLabel(new);

  smw->rdss_menu.menu_width = False;

  if (smw->core.width == 0) {
      smw->rdss_menu.menu_width = False;
      smw->core.width = GetMenuWidth(new, NULL);
  }

  smw->rdss_menu.menu_height = True;

  if (smw->core.height == 0) {
      smw->rdss_menu.menu_height = False;
      smw->core.height = GetMenuHeight(new);
  }

  smw->rdss_menu.highlight_enabled = True;
  smw->rdss_menu.release = False;
  smw->rdss_menu.grabbed = False;

/*
 * Add a popup_callback routine for changing the cursor.
 */
  XtAddCallback(new, XtNpopupCallback, ChangeCursorOnGrab, NULL);
/*
 * And for making sure everything is re-initialized to our start state
 */
  XtAddCallback(new, XtNpopupCallback, EnableHighlight, NULL);

}

/*      Function Name: Redisplay
 *      Description: Redisplays the contents of the widget.
 *      Arguments: w - the simple menu widget.
 *                 event - the X event that caused this redisplay.
 *                 region - the region the needs to be repainted. 
 *      Returns: none.
 */

/* ARGSUSED */
static void
Redisplay(w, event, region)
Widget w;
XEvent * event;
Region region;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    SmeObject * entry;
    SmeObjectClass class;

    if (region == NULL)
	XClearWindow(XtDisplay(w), XtWindow(w));

    /*
     * Check and Paint each of the entries - including the label.
     */

    ForAllChildren(smw, entry) {
	if (!XtIsManaged ( (Widget) *entry)) continue;

	if (region != NULL) 
	    switch(XRectInRegion(region, (int) (*entry)->rectangle.x,
				 (int) (*entry)->rectangle.y,
				 (unsigned int) (*entry)->rectangle.width,
				 (unsigned int) (*entry)->rectangle.height)) {
	    case RectangleIn:
	    case RectanglePart:
		break;
	    default:
		continue;
	    }
	class = (SmeObjectClass) (*entry)->object.widget_class;

	if (class->rect_class.expose != NULL)
	    (class->rect_class.expose)( (Widget) *entry, NULL, NULL);
    }
}

/*      Function Name: Realize
 *      Description: Realizes the widget.
 *      Arguments: w - the simple menu widget.
 *                 mask - value mask for the window to create.
 *                 attrs - attributes for the window to create.
 *      Returns: none
 */

static void
Realize(w, mask, attrs)
Widget w;
XtValueMask * mask;
XSetWindowAttributes * attrs;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;

    attrs->cursor = smw->rdss_menu.cursor;
    *mask |= CWCursor;
    if ((smw->rdss_menu.backing_store == Always) ||
	(smw->rdss_menu.backing_store == NotUseful) ||
	(smw->rdss_menu.backing_store == WhenMapped) ) {
	*mask |= CWBackingStore;
	attrs->backing_store = smw->rdss_menu.backing_store;
    }
    else
	*mask &= ~CWBackingStore;

    (*superclass->core_class.realize) (w, mask, attrs);
}

/*      Function Name: Resize
 *      Description: Handle the menu being resized bigger.
 *      Arguments: w - the simple menu widget.
 *      Returns: none.
 */

static void
Resize(w)
Widget w;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    SmeObject * entry;

    if ( !XtIsRealized(w) ) return;

    ForAllChildren(smw, entry) 	/* reset width of all entries. */
	if (XtIsManaged( (Widget) *entry))
	    (*entry)->rectangle.width = smw->core.width;
    
    Redisplay(w, (XEvent *) NULL, (Region) NULL);
}

/*      Function Name: SetValues
 *      Description: Relayout the menu when one of the resources is changed.
 *      Arguments: current - current state of the widget.
 *                 request - what was requested.
 *                 new - what the widget will become.
 *      Returns: none
 */

/* ARGSUSED */
static Boolean
SetValues(current, request, new)
Widget current, request, new;
{
    RdssMenuWidget smw_old = (RdssMenuWidget) current;
    RdssMenuWidget smw_new = (RdssMenuWidget) new;
    Boolean ret_val = FALSE, layout = FALSE;
    
    if (!XtIsRealized(current)) return(FALSE);
    
    if (!smw_new->rdss_menu.recursive_set_values) {
	if (smw_new->core.width != smw_old->core.width) {
	    smw_new->rdss_menu.menu_width = (smw_new->core.width != 0);
	    layout = TRUE;
	}
	if (smw_new->core.height != smw_old->core.height) {
	    smw_new->rdss_menu.menu_height = (smw_new->core.height != 0);
	    layout = TRUE;
	}
    }

    if (smw_old->rdss_menu.cursor != smw_new->rdss_menu.cursor)
	XDefineCursor(XtDisplay(new),
		      XtWindow(new), smw_new->rdss_menu.cursor);
    
    if (smw_old->rdss_menu.label_string !=smw_new->rdss_menu.label_string) 
	if (smw_new->rdss_menu.label_string == NULL)         /* Destroy. */
	    XtDestroyWidget((Widget) smw_old->rdss_menu.label);
	else if (smw_old->rdss_menu.label_string == NULL)    /* Create. */
	    CreateLabel(new);
	else {                                                 /* Change. */
	    Arg args[1];
	    
	    XtSetArg(args[0], XtNlabel, smw_new->rdss_menu.label_string);
	    XtSetValues((Widget) smw_new->rdss_menu.label, args, ONE);
	}
    
    if (smw_old->rdss_menu.label_class != smw_new->rdss_menu.label_class)
	XtAppWarning(XtWidgetToApplicationContext(new),
		     "No Dynamic class change of the RdssMenu Label.");
    
    if ((smw_old->rdss_menu.top_margin != smw_new->rdss_menu.top_margin) ||
	(smw_old->rdss_menu.bottom_margin != 
	 smw_new->rdss_menu.bottom_margin) /* filler.................  */ ) {
	layout = TRUE;
	ret_val = TRUE;
    }

    if (layout)
	Layout(new, NULL, NULL);

    return(ret_val);
}

/*      Function Name: SetValuesHook
 *      Description: To handle a special case, this is passed the
 *                   actual arguments.
 *      Arguments: w - the menu widget.
 *                 arglist - the argument list passed to XtSetValues.
 *                 num_args - the number of args.
 *      Returns: none
 */

/* 
 * If the user actually passed a width and height to the widget
 * then this MUST be used, rather than our newly calculated width and
 * height.
 */

static Boolean
SetValuesHook(w, arglist, num_args)
Widget w;
ArgList arglist;
Cardinal *num_args;
{
    register Cardinal i;
    Dimension width, height;
    
    width = w->core.width;
    height = w->core.height;
    
    for ( i = 0 ; i < *num_args ; i++) {
	if ( streq(arglist[i].name, XtNwidth) )
	    width = (Dimension) arglist[i].value;
	if ( streq(arglist[i].name, XtNheight) )
	    height = (Dimension) arglist[i].value;
    }

    if ((width != w->core.width) || (height != w->core.height))
	MakeSetValuesRequest(w, width, height);
    return(FALSE);
}

/************************************************************
 *
 * Geometry Management routines.
 *
 ************************************************************/

/*	Function Name: GeometryManager
 *	Description: This is the RdssMenu Widget's Geometry Manager.
 *	Arguments: w - the Menu Entry making the request.
 *                 request - requested new geometry.
 *                 reply - the allowed geometry.
 *	Returns: XtGeometry{Yes, No, Almost}.
 */

static XtGeometryResult
GeometryManager(w, request, reply)
Widget w;
XtWidgetGeometry * request, * reply;
{
    RdssMenuWidget smw = (RdssMenuWidget) XtParent(w);
    SmeObject entry = (SmeObject) w;
    XtGeometryMask mode = request->request_mode;
    XtGeometryResult answer;
    Dimension old_height, old_width;

    if ( !(mode & CWWidth) && !(mode & CWHeight) )
	return(XtGeometryNo);

    reply->width = request->width;
    reply->height = request->height;

    old_width = entry->rectangle.width;
    old_height = entry->rectangle.height;

    Layout(w, &(reply->width), &(reply->height) );

/*
 * Since we are an override shell and have no parent there is no one to
 * ask to see if this geom change is okay, so I am just going to assume
 * we can do whatever we want.  If you subclass be very careful with this
 * assumption, it could bite you.
 *
 * Chris D. Peterson - Sept. 1989.
 */

    if ( (reply->width == request->width) &&
	 (reply->height == request->height) ) {

	if ( mode & XtCWQueryOnly ) { /* Actually perform the layout. */
	    entry->rectangle.width = old_width;
	    entry->rectangle.height = old_height;	
	}
	else {
	    Layout(( Widget) smw, NULL, NULL);
	}
	answer = XtGeometryDone;
    }
    else {
	entry->rectangle.width = old_width;
	entry->rectangle.height = old_height;	

	if ( ((reply->width == request->width) && !(mode & CWHeight)) ||
	      ((reply->height == request->height) && !(mode & CWWidth)) ||
	      ((reply->width == request->width) && 
	       (reply->height == request->height)) )
	    answer = XtGeometryNo;
	else {
	    answer = XtGeometryAlmost;
	    reply->request_mode = 0;
	    if (reply->width != request->width)
		reply->request_mode |= CWWidth;
	    if (reply->height != request->height)
		reply->request_mode |= CWHeight;
	}
    }
    return(answer);
}

/*	Function Name: ChangeManaged
 *	Description: called whenever a new child is managed.
 *	Arguments: w - the simple menu widget.
 *	Returns: none.
 */

static void
ChangeManaged(w)
Widget w;
{
    Layout(w, NULL, NULL);
}

/************************************************************
 *
 * Global Action Routines.
 * 
 * These actions routines will be added to the application's
 * global action list. 
 * 
 ************************************************************/

/*ARGSUSED*/
static Widget
GetMenu(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
Cardinal *num_params;
{
	String menu_name = NULL;
	Widget menu;
	
	if (*num_params == 0)
	{
		Arg arg;

		XtSetArg(arg, XtNmenuName, &menu_name);
		XtGetValues(w, &arg, (Cardinal)1 );
		if (!menu_name)
		{
			char error_buf[BUFSIZ];
			sprintf(error_buf, "%s: %s",
			  "Xaw - RdssMenuWidget",
			  "position menu action could not retrieve menuName");
			XtAppWarning(XtWidgetToApplicationContext(w), 
				     error_buf);
			return(NULL);
		}
	}
	else if (*num_params == (Cardinal)1 )
	{
		menu_name = params[0];
	}
	else
	{
		char error_buf[BUFSIZ];
		sprintf(error_buf, "%s: %s %s",
			"Xaw - RdssMenuWidget",
			"position menu action expects zero or one",
			"parameter which is the name of the menu.");
		XtAppWarning(XtWidgetToApplicationContext(w), error_buf);
		return (NULL);
	}
	
	if ((menu = FindMenu(w, menu_name)) == NULL) {
		char error_buf[BUFSIZ];
		sprintf(error_buf, "%s: %s '%s'",
			"Xaw - RdssMenuWidget",
			"could not find menu named: ", menu_name);
		XtAppWarning(XtWidgetToApplicationContext(w), error_buf);
		return (NULL);
	}
	return (menu);
}



/*      Function Name: PositionMenuAction
 *      Description: Positions the simple menu widget.
 *      Arguments: w - a widget (no the simple menu widget.)
 *                 event - the event that caused this action.
 *                 params, num_params - parameters passed to the routine.
 *                                      we expect the name of the menu here.
 *      Returns: none
 *
 * If no parameters, and if the event widget is a MenuButton, the menu
 * name will be retrieved from the menuName resource.
 */
/* ARGSUSED */
static void
PositionMenuAction(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{ 
  Widget menu;
  XPoint loc;
  Position x,y;

  menu = GetMenu(w, event, params, num_params);
  if (!menu) return;

  switch (event->type) {
  case ButtonPress:
  case ButtonRelease:
    loc.x = event->xbutton.x_root;
    loc.y = event->xbutton.y_root;
    break;
  case EnterNotify:
  case LeaveNotify:
    loc.x = event->xcrossing.x_root;
    loc.y = event->xcrossing.y_root;
    break;
  case MotionNotify:
    loc.x = event->xmotion.x_root;
    loc.y = event->xmotion.y_root;
    break;
  default:
    PositionMenu(menu, NULL, &x, &y);
    MoveMenu(menu, x, y, 0, 0);
    return;
  }
  PositionMenu(menu, &loc, &x, &y);
  MoveMenu(menu, x, y, 0, 0);
}  


static void
DoGrab (Widget menu)
{
    RdssMenuWidget smw = (RdssMenuWidget) menu;
    smw->rdss_menu.grabbed = True;
    ACK("grabbing pointer", menu);

    XtGrabPointer (menu, 
		   /*Boolean owner_events*/ True,
		   /*event_mask*/ 
		   ButtonPressMask | ButtonReleaseMask | 
		   ButtonMotionMask | PointerMotionMask | 
		   LeaveWindowMask | EnterWindowMask,
		   /*int pointer_mode*/ GrabModeAsync,
		   /*int keyboard_mode*/ GrabModeAsync,
		   /*Window confine_to*/ None,
		   /*Cursor cursor*/ None,
		   CurrentTime);
}


/*
 * Positions, moves, pops up, and warps pointer all in a single action.
 * Must all be in one action because pointer warping requires the distance
 * the menu was moved in MoveMenu().
 *
 * If no parameters, and if the event widget is a MenuButton, the menu
 * name will be retrieved from the menuName resource.
 */
/* ARGSUSED */
static void
PositionAndPopupMenuAction(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{ 
	Widget menu;
	XPoint loc;
	XPoint *locp = &loc;
	Position x, y;
	Position dx, dy;
	Boolean spring_loaded;

	menu = GetMenu(w, event, params, num_params);
	if (!menu) return;

	switch (event->type) {
	   case ButtonPress:
	   case ButtonRelease:
		loc.x = event->xbutton.x_root;
		loc.y = event->xbutton.y_root;
		break;
	   case EnterNotify:
	   case LeaveNotify:
		loc.x = event->xcrossing.x_root;
		loc.y = event->xcrossing.y_root;
		break;
	   case MotionNotify:
		loc.x = event->xmotion.x_root;
		loc.y = event->xmotion.y_root;
		break;
	   default:
	        locp = 0;
		break;
	}
	PositionMenu(menu, locp, &x, &y);
	MoveMenu(menu, x, y, &dx, &dy);

	/*
	 * Now that we've set the position, pop it up.
	 */
	if (event->type == ButtonPress || event->type == ButtonRelease)
	    spring_loaded = True;
	else if (event->type == KeyPress || event->type == EnterNotify)
		spring_loaded = False;
	else 
	{
		XtAppWarningMsg(XtWidgetToApplicationContext(w),
				"invalidPopup","unsupportedOperation",
				"RdssMenu",
"Pop-up menu creation is only supported on ButtonPress, KeyPress or EnterNotify events.",
				(String *)NULL, (Cardinal *)NULL);
		spring_loaded = False;
	}
	
	((RdssMenuWidget)menu)->rdss_menu.release = True;
	if (spring_loaded) 
	{
	    XtPopupSpringLoaded(menu);
	    DoGrab(menu);
	}
	else 
	{
	    XtPopup(menu, XtGrabExclusive);
	}
}  



/************************************************************
 *
 * Widget Action Routines.
 * 
 ************************************************************/

/*      Function Name: Unhighlight
 *      Description: Unhighlights current entry.
 *      Arguments: w - the simple menu widget.
 *                 event - the event that caused this action.
 *                 params, num_params - ** NOT USED **
 *      Returns: none
 */

/* ARGSUSED */
static void
Unhighlight(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{ 
    RdssMenuWidget smw = (RdssMenuWidget) w;
    SmeObject entry = smw->rdss_menu.entry_set;
    SmeObjectClass class;
 
    ENTER("Unhighlight()",w)
    if ( entry == NULL)		/* meaning no Sme's to unhighlight */
    { 
	EXIT("Unhighlight(): entry_set is null",w)
	return;
    }
    
    /*
     * Our current entry (entry_set) should disappear (be set to NULL)
     * and unhighlighted if:
     *
     *	1) the entry is anything other than an smeMenuObject
     *	2) event is NULL
     *	3) it is an smeMenuObject, the event is LeaveNotify, and
     *	   the SmeMenu entry does not have a menu popped up
     *	   
     * So basically, if the entry is SmeMenu, the event LeaveNotify,
     * and the entry has a menu popped up, then we ignore the event
     * and don't call the entry's unhighlight method.
     *
     * Note that its possible that an SmeMenu object created us and is
     * calling our action artificially with a NULL event.  A NULL
     * event implies unhighlight no matter what.  Highlight() uses a
     * NULL event to indicate that we should unhighlight no matter what
     * becuase entry_set has changed.
     */

    class = (SmeObjectClass) entry->object.widget_class;

    if (((WidgetClass)class != smeMenuObjectClass) ||
	(!event) ||
	(event->type != LeaveNotify) || (! SmeMenuPoppedUp ((Widget)entry)))
    {
	(class->sme_class.unhighlight) ( (Widget) entry);
	smw->rdss_menu.entry_set = NULL;
    }
    else
    {
	IFD(ui_printf("	ignoring LeaveWindow event in SmeMenuObject %s\n",
			XtName((Widget)entry));)
    }

    IFD(ui_printf("	entry_set is now %s\n",
		(smw->rdss_menu.entry_set)?
		XtName((Widget)smw->rdss_menu.entry_set):("NULL"));)
    EXIT("Unhighlight()",w)
}


/* ARGSUSED */
static void
DisableRelease(Widget w, XEvent * event, String *params, Cardinal *num_params)
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    smw->rdss_menu.release = False;
}



/* ARGSUSED */
static void
Release(Widget w, XEvent * event, String * params, Cardinal * num_params)
{
    RdssMenuWidget smw = (RdssMenuWidget) w;

    if (! smw->rdss_menu.release)
    {
	/* "disable-highlight() notify() unhighlight() RdssMenuPopdown()" */
	DisableHighlight (w, event, 0, 0);
	Notify (w, event, 0, 0);
	Unhighlight (w, event, 0, 0);
	RdssMenuPopdown (w);
    }
    DisableRelease (w, event, 0, 0);
}


/*      Function Name: Highlight
 *      Description: Highlights current entry.
 *      Arguments: w - the simple menu widget.
 *                 event - the event that caused this action.
 *                 params, num_params - ** NOT USED **
 *      Returns: none
 */
/* ARGSUSED */
static void
Highlight(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    SmeObject entry;
    SmeObjectClass class;
    
    ENTER("Highlight()",w)
    if ( !XtIsSensitive(w) )
    {
	EXIT("Highlight(): menu not sensitive",w)
	return;
    }
    
    if ( !smw->rdss_menu.highlight_enabled )
    {
	EXIT("Highlight(): disabled",w)
	return;
    }

    entry = GetEventEntry(w, event);
    IFD(ui_printf("	event entry is %s;   entry_set is %s\n",
		(entry)?XtName((Widget)entry):"NULL",
		(smw->rdss_menu.entry_set)?
		XtName((Widget)smw->rdss_menu.entry_set):"NULL");)
    /*
     * Ignore highlight actions when the event entry is NULL, or
     * when the event entry is the current entry_set and entry
     * is not SmeMenu.  This passes all highlight actions to the
     * SmeMenu entry so that it can determine whether or not it
     * wants to pop up a menu.
     */
    if ((entry == smw->rdss_menu.entry_set) && 
	(!entry || (entry->object.widget_class != smeMenuObjectClass)))
    {
	    EXIT("Highlight(): not SmeMenu, and entry == entry_set",w)
	    return;
    }

    /*
     * If this is a new entry, unhighlight any previous set entry.  Pass
     * NULL as the event to force the unhighlight of an active SmeMenu.
     */
    if (entry != smw->rdss_menu.entry_set)
	    Unhighlight(w, NULL, params, num_params);

    if (entry == NULL) 
    {
	EXIT("Highlight(): entry is NULL",w)
	return;
    }

    if ( !XtIsSensitive( (Widget) entry)) 
    {
	smw->rdss_menu.entry_set = NULL;
	EXIT("Highlight(): entry not sensitive, entry_set set to NULL",w)
	return;
    }

    smw->rdss_menu.entry_set = entry;
    class = (SmeObjectClass) entry->object.widget_class;

    (class->sme_class.highlight) ( (Widget) entry);
    IFD(ui_printf("RdssMenu: %s exiting Highlight(): entry_set set to %s\n",
		  XtName(w),XtName((Widget)entry));)
}



/*      Function Name: Notify
 *      Description: Notify user of current entry.
 *      Arguments: w - the simple menu widget.
 *                 event - the event that caused this action.
 *                 params, num_params - ** NOT USED **
 *      Returns: none
 *
 * Theoretically speaking, if this widget is the top of a modal cascade with
 * an exclusive grab, this action may be called because of a <BtnUp>
 * event further down the cascade, in which case we don't really want to
 * trigger a notify.  If entry_set is a SmeMenu, and SmeMenu has a menu
 * popped up, then we ignore the notify and assume the event occurred further
 * down the modal tree, or completely outside the tree.  In either of those
 * cases, no notify occurs.
 *
 * Note that it is not enough to see if the event occurred in one of our own
 * entries, since a sub-menu may be popped up over our active entry.
 */
/* ARGSUSED */
static void
Notify(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    SmeObject entry = smw->rdss_menu.entry_set;
    SmeObjectClass class;
    
    if ( (entry == NULL) || !XtIsSensitive((Widget) entry) ) return;

    class = (SmeObjectClass) entry->object.widget_class;
    if (((WidgetClass)class != smeMenuObjectClass) ||
	!SmeMenuPoppedUp((Widget)entry))
    {
	(class->sme_class.notify)( (Widget) entry );
	IFD(ui_printf("RdssMenu: %s ack Notify(): notify called for %s\n",
		      XtName(w),XtName((Widget)entry));)
    }
    else
    {
	IFD(ui_printf("RdssMenu: %s ack Notify(): notify omitted for %s\n",
		      XtName(w),XtName((Widget)entry));)
    }
}


/*
 * Function: Unmap()
 *
 * Unmap the menu window.  Principly called to disable further event handling
 * while popping down an entire modal cascade.  The actual XtPopdown() and
 * XtRemoveGrab() must still be performed.
 */
/* ARGSUSED */
static void
Unmap(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
     XtUnmapWidget (w);
}


/*
 * Function: Ungrab()
 *
 * Ungrab any grabs, ie, before being popped down with MenuPopdown().
 */
/* ARGSUSED */
static void
Ungrab(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    
    if (smw->rdss_menu.grabbed)
    {
	XtUngrabPointer (w, CurrentTime);
    }
    smw->rdss_menu.grabbed = False;
}


/*
 * Function: DisableHighlight()
 *
 * Disable highlight action.
 */
/* ARGSUSED */
static void
DisableHighlight(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
     RdssMenuWidget smw = (RdssMenuWidget) w;

     smw->rdss_menu.highlight_enabled = False;
}


/*
 * Function: EnableHighlight()
 *
 * Enable highlight action.
 */
/* ARGSUSED */
static void
EnableHighlight(w, event, params, num_params)
Widget w;
XEvent * event;
String * params;
Cardinal * num_params;
{
     RdssMenuWidget smw = (RdssMenuWidget) w;

     smw->rdss_menu.highlight_enabled = True;
}



/************************************************************
 *
 * Public Functions.
 *
 ************************************************************/
 
/*	Function Name: RdssMenuAddGlobalActions
 *	Description: adds the global actions to the simple menu widget.
 *	Arguments: app_con - the appcontext.
 *	Returns: none.
 */
void
#if NeedFunctionPrototypes
RdssMenuAddGlobalActions(XtAppContext app_con)
#else
RdssMenuAddGlobalActions(app_con)
XtAppContext app_con;
#endif
{
    XtInitializeWidgetClass(rdssMenuWidgetClass);
    XmuCallInitializers( app_con );
} 

 
/*	Function Name: RdssMenuGetActiveEntry
 *	Description: Gets the currently active (set) entry.
 *	Arguments: w - the smw widget.
 *	Returns: the currently set entry or NULL if none is set.
 */
Widget
#if NeedFunctionPrototypes
RdssMenuGetActiveEntry(Widget w)
#else
RdssMenuGetActiveEntry(w)
Widget w;
#endif
{
    RdssMenuWidget smw = (RdssMenuWidget) w;

    return( (Widget) smw->rdss_menu.entry_set);
} 


/*	Function Name: RdssMenuClearActiveEntry
 *	Description: Unsets the currently active (set) entry.
 *	Arguments: w - the smw widget.
 *	Returns: none.
 */
void
#if NeedFunctionPrototypes
RdssMenuClearActiveEntry(Widget w)
#else
RdssMenuClearActiveEntry(w)
Widget w;
#endif
{
    RdssMenuWidget smw = (RdssMenuWidget) w;

    smw->rdss_menu.entry_set = NULL;
} 


void
RdssMenuPopdown (Widget w)
{
    Ungrab (w, 0, 0, 0);
    XtPopdown (w);
}



void
RdssMenuPopdownAction (Widget w)
{
    RdssMenuPopdown (w);
}



/*	Function Name: RdssSubMenuPositionAndPopup
 *	Description: Positions, moves, and popups menu, warping pointer
 *		as necessary once menu popped up.
 *	Arguments: w - the smw widget.
 *		   locn - initial suggestion for coordinates of menu
 *		   grab - kind of grab to use to popup widget
 *	Returns: *locn modified but not accurate
 */
void
#if NeedFunctionPrototypes
RdssSubMenuPositionAndPopup(
   Widget w,
   XPoint *locn,
   XtGrabKind grab)
#else
RdssSubMenuPositionAndPopup(w, locn, grab)
   Widget w;
   XPoint *locn;
   XtGrabKind grab;
#endif
{
    Position x, y;
    Position dx, dy;

    PositionMenu(w, locn, &x, &y);
    MoveMenu(w, x, y, &dx, &dy);
    DisableRelease (w, 0, 0, 0);
    XtPopup(w, grab);
} 



/************************************************************
 *
 * Private Functions.
 *
 ************************************************************/

/*	Function Name: CreateLabel
 *	Description: Creates a the menu label.
 *	Arguments: w - the smw widget.
 *	Returns: none.
 * 
 * Creates the label object and makes sure it is the first child in
 * in the list.
 */

static void
CreateLabel(w)
Widget w;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    register Widget * child, * next_child;
    register int i;
    Arg args[2];

    if ( (smw->rdss_menu.label_string == NULL) ||
	 (smw->rdss_menu.label != NULL) ) {
	char error_buf[BUFSIZ];

	sprintf(error_buf, "Xaw Simple Menu Widget: %s or %s, %s",
		"label string is NULL", "label already exists", 
		"no label is being created.");
	XtAppWarning(XtWidgetToApplicationContext(w), error_buf);
	return;
    }

    XtSetArg(args[0], XtNlabel, smw->rdss_menu.label_string);
    XtSetArg(args[1], XtNjustify, XtJustifyCenter);
    smw->rdss_menu.label = (SmeObject) 
	                      XtCreateManagedWidget("menuLabel", 
					    smw->rdss_menu.label_class, w,
					    args, TWO);

    next_child = NULL;
    for (child = smw->composite.children + smw->composite.num_children,
	 i = smw->composite.num_children ; i > 0 ; i--, child--) {
	if (next_child != NULL)
	    *next_child = *child;
	next_child = child;
    }
    *child = (Widget) smw->rdss_menu.label;
}

/*	Function Name: Layout
 *	Description: lays the menu entries out all nice and neat.
 *	Arguments: w - See below (+++)
 *                 width_ret, height_ret - The returned width and 
 *                                         height values.
 *	Returns: none.
 *
 * if width == NULL || height == NULL then it assumes the you do not care
 * about the return values, and just want a relayout.
 *
 * if this is not the case then it will set width_ret and height_ret
 * to be width and height that the child would get if it were layed out
 * at this time.
 *
 * +++ "w" can be the simple menu widget or any of its object children.
 */

static void
Layout(w, width_ret, height_ret)
Widget w;
Dimension *width_ret, *height_ret;
{
    SmeObject current_entry, *entry;
    RdssMenuWidget smw;
    Dimension width, height;
    Boolean do_layout = ((height_ret == NULL) || (width_ret == NULL));
    Boolean allow_change_size;
    height = 0;

    if ( XtIsSubclass(w, rdssMenuWidgetClass) ) {
	smw = (RdssMenuWidget) w;
	current_entry = NULL;
    }
    else {
	smw = (RdssMenuWidget) XtParent(w);
	current_entry = (SmeObject) w;
    }

    allow_change_size = (!XtIsRealized((Widget)smw) ||
			 (smw->shell.allow_shell_resize));

    if ( smw->rdss_menu.menu_height )
	height = smw->core.height;
    else
	if (do_layout) {
	    height = smw->rdss_menu.top_margin;
	    ForAllChildren(smw, entry) {
		if (!XtIsManaged( (Widget) *entry)) continue;

		if ( (smw->rdss_menu.row_height != 0) && 
		    (*entry != smw->rdss_menu.label) ) 
		    (*entry)->rectangle.height = smw->rdss_menu.row_height;
		
		(*entry)->rectangle.y = height;
		(*entry)->rectangle.x = 0;
		height += (*entry)->rectangle.height;
	    }
	    height += smw->rdss_menu.bottom_margin;
	}
	else {
	    if ((smw->rdss_menu.row_height != 0) && 
		(current_entry != smw->rdss_menu.label) )
		height = smw->rdss_menu.row_height;
	}
    
    if (smw->rdss_menu.menu_width)
	width = smw->core.width;
    else if ( allow_change_size )
	width = GetMenuWidth((Widget) smw, (Widget) current_entry);
    else
	width = smw->core.width;

    if (do_layout) {
	ForAllChildren(smw, entry)
	    if (XtIsManaged( (Widget) *entry)) 
		(*entry)->rectangle.width = width;

	if (allow_change_size)
	    MakeSetValuesRequest((Widget) smw, width, height);
    }
    else {
	*width_ret = width;
	if (height != 0)
	    *height_ret = height;
    }
}


    
/*	Function Name: AddPositionAction
 *	Description: Adds the XawPositionSimpleMenu action to the global
 *                   action list for this appcon.
 *	Arguments: app_con - the application context for this app.
 *                 data - NOT USED.
 *	Returns: none.
 */
/* ARGSUSED */
static void
AddPositionAction(app_con, data)
XtAppContext app_con;
caddr_t data;
{
    static XtActionsRec pos_action[] = {
        { "PositionRdssMenu", PositionMenuAction },
	{ "RdssPositionSimpleMenu", PositionMenuAction },
	{ "PositionAndPopupRdssMenu", PositionAndPopupMenuAction }
    };

    XtAppAddActions(app_con, pos_action, XtNumber(pos_action));
}



/*	Function Name: FindMenu
 *	Description: Find the menu give a name and reference widget.
 *	Arguments: widget - reference widget.
 *                 name   - the menu widget's name.
 *	Returns: the menu widget or NULL.
 */
static Widget 
FindMenu(widget, name)
Widget widget;
String name;
{
    register Widget w, menu;
    
    for ( w = widget ; w != NULL ; w = XtParent(w) )
	if ( (menu = XtNameToWidget(w, name)) != NULL )
	    return(menu);
    return(NULL);
}



/*	Function Name: PositionMenu
 *	Description: Places the menu
 *	Arguments: w - the simple menu widget.
 *                 location - a pointer the the position or NULL.
 *	Returns: new location in x, y
 */
static void
PositionMenu(w, location, x, y)
Widget w;
XPoint * location;
Position *x;
Position *y;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    SmeObject entry;
    XPoint t_point;
    
    *x = 0;
    *y = 0;
    if (location == NULL) {
	Window junk1, junk2;
	int root_x, root_y, junkX, junkY;
	unsigned int junkM;
	
	location = &t_point;
	if (XQueryPointer(XtDisplay(w), XtWindow(w), &junk1, &junk2, 
			  &root_x, &root_y, &junkX, &junkY, &junkM) == FALSE) {
	    char error_buf[BUFSIZ];
	    sprintf(error_buf, "%s %s", "Xaw - RdssMenuWidget:",
		    "Could not find location of mouse pointer");
	    XtAppWarning(XtWidgetToApplicationContext(w), error_buf);
	    return;
	}
	location->x = (short) root_x;
	location->y = (short) root_y;
    }
    
    /*
     * The width will not be correct unless it is realized, but we don't
     * want to map it yet (i.e. we don't want it to receive any events).
     */
    XtSetMappedWhenManaged (w, False);
    XtRealizeWidget(w);
    
    /*
     * Given x,y for optimal origin of popup menu widget, adjust so
     * that cursor will be on the default entry, the label, or the first 
     * entry.  Try to position the menu so that the pointer is always a 
     * fixed amount inside the left edge of the menu.
     */
    location->x -= (Position) POINTER_X_INDENT;
    
    if (smw->rdss_menu.popup_entry == NULL)
    {
        entry = smw->rdss_menu.label;
	if (!entry && (smw->composite.num_children > 0))
		entry = (SmeObject)smw->composite.children[0];
    }
    else
	entry = smw->rdss_menu.popup_entry;

    if (entry != NULL)
	location->y -= entry->rectangle.y + entry->rectangle.height/2;
    else /* no label, so just move it up enough to put cursor on 1st entry */
	location->y -= 5;

   /*
    * Location specified, but widget not yet actually moved.
    * Return the calculated location.
    */
    *x = location->x;
    *y = location->y;
}


/*	Function Name: MoveMenu
 *	Description: Actually moves the menu, may force it to
 *                   to be fully visible if menu_on_screen is TRUE.
 *	Arguments: w - the simple menu widget.
 *                 x, y - the current location of the widget.
 *		   dx, dy - returns amount moved if non-NULL
 *	Returns: dx, dy 
 *
 * We have two objectives here.  The first is to make sure the menu appears
 * on the screen, either at the desired location or close to it.
 * The second is to make sure the menu does not pop up with the pointer
 * on top of another entry in a position to immediately popup another
 * submenu.  If we can't show the menu to the right, move it to the other
 * side of the pointer.  NOTE that we are assuming the pointer is pretty
 * close, if not exactly at, (x,y).
 */
static void
MoveMenu(w, x, y, rdx, rdy)
Widget w;
Position x, y;
Position *rdx, *rdy;
{
    Arg arglist[2];
    Cardinal num_args = 0;
    RdssMenuWidget smw = (RdssMenuWidget) w;
    Position dx = 0, dy = 0;
    
    if (smw->rdss_menu.menu_on_screen) 
    {
	int width = w->core.width + 2 * w->core.border_width;
	int height = w->core.height + 2 * w->core.border_width;
	
	if (x >= 0)
	{
	/*
	 * Move the menu to the other side just enough past the
	 * pointer to keep the pointer from activating anything,
	 * but don't move it so far that the pointer has to leave
	 * the menu popup region of the current entry to
	 * enter the submenu.  Since the menu was originally positioned
	 * with some indent space for the pointer, add half of the
	 * indent space to the left shift.
	 */
	    int scr_width = WidthOfScreen(XtScreen(w));
	    if (x + width > scr_width)
		    dx = 0 - width + (POINTER_X_INDENT / 2);
	}
	if (x < 0) 
	    dx = 0 - x;
	
	if (y >= 0) {
	    int scr_height = HeightOfScreen(XtScreen(w));
	    if (y + height > scr_height)
		dy = (scr_height - height) - y;
	}
	if (y < 0)
	    dy = 0 - y;
    }
    
    XtSetArg(arglist[num_args], XtNx, x+dx); num_args++;
    XtSetArg(arglist[num_args], XtNy, y+dy); num_args++;
    XtSetValues(w, arglist, num_args);

    if (rdx) *rdx = dx;
    if (rdy) *rdy = dy;
}



/*	Function Name: ChangeCursorOnGrab
 *	Description: Changes the cursor on the active grab to the one
 *                   specified in out resource list.
 *	Arguments: w - the widget.
 *                 junk, garbage - ** NOT USED **.
 *	Returns: None.
 */
/* ARGSUSED */
static void
ChangeCursorOnGrab(w, junk, garbage)
Widget w;
XtPointer junk, garbage;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    
    /*
     * The event mask here is what is currently in the MIT implementation.
     * There really needs to be a way to get the value of the mask out
     * of the toolkit (CDP 5/26/89).
     */
    
    XChangeActivePointerGrab(XtDisplay(w), ButtonPressMask|ButtonReleaseMask,
			     smw->rdss_menu.cursor, 
			     XtLastTimestampProcessed(XtDisplay(w)));
}



/*      Function Name: MakeSetValuesRequest
 *      Description: Makes a (possibly recursive) call to SetValues,
 *                   I take great pains to not go into an infinite loop.
 *      Arguments: w - the simple menu widget.
 *                 width, height - the size of the ask for.
 *      Returns: none
 */
static void
MakeSetValuesRequest(w, width, height)
Widget w;
Dimension width, height;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    Arg arglist[2];
    Cardinal num_args = (Cardinal) 0;
    
    if ( !smw->rdss_menu.recursive_set_values ) {
	if ( (smw->core.width != width) || (smw->core.height != height) ) {
	    smw->rdss_menu.recursive_set_values = TRUE;
	    XtSetArg(arglist[num_args], XtNwidth, width);   num_args++;
	    XtSetArg(arglist[num_args], XtNheight, height); num_args++;
	    XtSetValues(w, arglist, num_args);
	}
	else if (XtIsRealized( (Widget) smw))
	    Redisplay((Widget) smw, (XEvent *) NULL, (Region) NULL);
    }
    smw->rdss_menu.recursive_set_values = FALSE;
}


/*      Function Name: GetMenuWidth
 *      Description: Sets the length of the widest entry in pixels.
 *      Arguments: w - the simple menu widget.
 *      Returns: width of menu.
 */
static Dimension
GetMenuWidth(w, w_ent)
Widget w, w_ent;
{
    SmeObject cur_entry = (SmeObject) w_ent;
    RdssMenuWidget smw = (RdssMenuWidget) w;
    Dimension width, widest = (Dimension) 0;
    SmeObject * entry;
    
    if ( smw->rdss_menu.menu_width ) 
	return(smw->core.width);

    ForAllChildren(smw, entry) {
	XtWidgetGeometry preferred;

	if (!XtIsManaged( (Widget) *entry)) continue;
	
	if (*entry != cur_entry) {
	    XtQueryGeometry((Widget) *entry, NULL, &preferred);
	    
	    if (preferred.request_mode & CWWidth)
		width = preferred.width;
	    else
		width = (*entry)->rectangle.width;
	}
	else
	    width = (*entry)->rectangle.width;
	
	if ( width > widest )
	    widest = width;
    }
    
    return(widest);
}


/*      Function Name: GetMenuHeight
 *      Description: Sets the length of the widest entry in pixels.
 *      Arguments: w - the simple menu widget.
 *      Returns: width of menu.
 */
static Dimension
GetMenuHeight(w)
Widget w;
{
    RdssMenuWidget smw = (RdssMenuWidget) w;
    SmeObject * entry;
    Dimension height;
    
    if (smw->rdss_menu.menu_height)
	return(smw->core.height);

    height = smw->rdss_menu.top_margin + smw->rdss_menu.bottom_margin;
    
    if (smw->rdss_menu.row_height == 0) 
	ForAllChildren(smw, entry) 
	    if (XtIsManaged ((Widget) *entry)) 
		height += (*entry)->rectangle.height;
    else 
	height += smw->rdss_menu.row_height * smw->composite.num_children;
	
    return(height);
}


/*      Function Name: GetEventEntry
 *      Description: Gets an entry given an event that has X and Y coords.
 *      Arguments: w - the simple menu widget.
 *                 event - the event.
 *      Returns: the entry that this point is in.
 */
static SmeObject
GetEventEntry(w, event)
Widget w;
XEvent * event;
{
    Position x_loc, y_loc;
    RdssMenuWidget smw = (RdssMenuWidget) w;
    SmeObject * entry;
    
    switch (event->type) {
    case MotionNotify:
	x_loc = event->xmotion.x;
	y_loc = event->xmotion.y;
	break;
    case EnterNotify:
    case LeaveNotify:
	x_loc = event->xcrossing.x;
	y_loc = event->xcrossing.y;
	break;
    case ButtonPress:
    case ButtonRelease:
	x_loc = event->xbutton.x;
	y_loc = event->xbutton.y;
	break;
    default:
	XtAppError(XtWidgetToApplicationContext(w),
		   "Unknown event type in GetEventEntry().");
	break;
    }
    
    /*
     * The inclusion of border width is necessary because we can get motion
     * events in our border, in which case we still want to know which entry
     * the pointer is in.
     */
    if ((x_loc < 0) || 
	(x_loc >= (int)smw->core.width + (2 * (int)smw->core.border_width)) || 
	(y_loc < 0) ||
	(y_loc >= (int)smw->core.height + (2 * (int)smw->core.border_width)) )
    {
	IFD(ui_printf(
	 "GetEventEntry('%s',x=%i,y=%i) outside menu window (%ix%i), returning NULL\n",
	 XtName(w), x_loc, y_loc, (int)smw->core.width, (int)smw->core.height);)
	return(NULL);
    }

    ForAllChildren(smw, entry) {
	if (!XtIsManaged ((Widget) *entry)) continue;

	if ( ((*entry)->rectangle.y < y_loc) &&
	    ((*entry)->rectangle.y + (int) (*entry)->rectangle.height > y_loc) )
	    if ( *entry == smw->rdss_menu.label )
	    {
		IFD(ui_printf(
		   "GetEventEntry('%s',x=%i,y=%i) in label, returning NULL\n",
		   XtName(w), x_loc, y_loc);)
		return(NULL);	/* cannot select the label. */
            }
	    else
	    {
		IFD(ui_printf(
		   "GetEventEntry('%s',x=%i,y=%i) in entry, returning '%s'\n",
		   XtName(w), x_loc, y_loc, XtName((Widget)*entry));)
		return(*entry);
            }
    }
    
    IFD(ui_printf(
	   "GetEventEntry('%s',x=%i,y=%i) nowhere, returning NULL\n",
	   XtName(w), x_loc, y_loc);)
    return(NULL);
}

#endif /* XSUPPORT */
