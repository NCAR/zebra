#if ( !defined(lint) && !defined(SABER) )
static char rcsid[] = "$Id: SmeMenu.c,v 1.15 2001-11-30 00:42:05 granger Exp $";
#endif 

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
 * -------------------------------------------------------
 * SmeMenu.c - Source code file for BSB Menu Entry object.
 *
 * Date:    March 15, 1990
 *
 * By:      Van Jacobson
 *          Lawrence Berkeley Laboratory
 *          van@helios.ee.lbl.gov
 *
 * This code is based on Xaw/SmeBSB.c by Chris Peterson, MIT X Consortium,
 * and most of the boilerplate (e.g., SetValues, QueryGeometry, etc.) is
 * lifted directly from SmeBSB.c.  Only things having to do with the
 * sub-menu abstraction were written by VJ.
 */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/ShellP.h>

#include <X11/Xmu/Drawing.h>

#include <X11/Xaw/XawInit.h>
#include <X11/Xaw/SimpleMenP.h>
#include "SmeMenuP.h"
#include <X11/Xaw/Cardinals.h>
#include "RdssMenu.h"

#define ONE_HUNDRED 100

/*#define DEBUG*/

#ifdef DEBUG
#define ENTER(msg,w) ui_printf("SmeMenu: %s entering %s\n", 	\
			       (w)?(XtName((Widget)w)):"",msg);
#define EXIT(msg,w) ui_printf("SmeMenu: %s exiting %s\n",	\
			       (w)?(XtName((Widget)w)):"",msg);
#define IFD(cmd) cmd
#define ACK(msg,w) ui_printf("SmeMenu: %s ack %s\n",		\
			       (w)?(XtName((Widget)w)):"",msg);
#else
#define ENTER(msg,w)
#define EXIT(msg,w)
#define IFD(cmd)
#define ACK(msg,w)
#endif

#define offset(field) XtOffset(SmeMenuObject, sme_menu.field)

static XtResource resources[] = {
  {XtNlabel,  XtCLabel, XtRString, sizeof(String),
     offset(label), XtRString, NULL},
  {XtNvertSpace,  XtCVertSpace, XtRInt, sizeof(int),
     offset(vert_space), XtRImmediate, (caddr_t) 25},
  {XtNleftBitmap, XtCLeftBitmap, XtRPixmap, sizeof(Pixmap),
     offset(left_bitmap), XtRImmediate, (caddr_t)None},
  {XtNjustify, XtCJustify, XtRJustify, sizeof(XtJustify),
     offset(justify), XtRImmediate, (caddr_t) XtJustifyLeft},
  {XtNrightBitmap, XtCRightBitmap, XtRPixmap, sizeof(Pixmap),
     offset(right_bitmap), XtRImmediate, (caddr_t)None},
  {XtNleftMargin,  XtCHorizontalMargins, XtRDimension, sizeof(Dimension),
     offset(left_margin), XtRImmediate, (caddr_t) 4},
  {XtNrightMargin,  XtCHorizontalMargins, XtRDimension, sizeof(Dimension),
     offset(right_margin), XtRImmediate, (caddr_t) 4},
  {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground), XtRString, "XtDefaultForeground"},
  {XtNfont,  XtCFont, XtRFontStruct, sizeof(XFontStruct *),
     offset(font), XtRString, "XtDefaultFont"},
  {XtNmenu,  XtCMenu, XtRString, sizeof(String),
     offset(menu), XtRString, NULL},
  {XtNpopupSelection,  XtCPopupSelection, XtRBoolean, sizeof(Boolean),
     offset(popup_last_select), XtRImmediate, (caddr_t)FALSE},
};   
#undef offset

/*
 * Semi Public function definitions. 
 */
static void Redisplay(), Destroy(), Initialize(), FlipColors(), Notify();
static void Highlight(), Unhighlight();
static void ClassInitialize();
static Boolean SetValues();
static XtGeometryResult QueryGeometry();

/* 
 * Private Function Definitions.
 */
static void GetDefaultSize(), DrawBitmaps(), GetBitmapInfo();
static void CreateGCs(), DestroyGCs();
static Widget FindPopup();

    
#define superclass (&smeClassRec)
SmeMenuClassRec smeMenuClassRec = {
  {
    /* superclass         */    (WidgetClass) superclass,
    /* class_name         */    "SmeMenu",
    /* size               */    sizeof(SmeMenuRec),
    /* class_initializer  */	ClassInitialize,
    /* class_part_initialize*/	NULL,
    /* Class init'ed      */	FALSE,
    /* initialize         */    Initialize,
    /* initialize_hook    */	NULL,
    /* realize            */    NULL,
    /* actions            */    NULL,
    /* num_actions        */    ZERO,
    /* resources          */    resources,
    /* resource_count     */	XtNumber(resources),
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE, 
    /* compress_exposure  */    FALSE,
    /* compress_enterleave*/ 	FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    Destroy,
    /* resize             */    NULL,
    /* expose             */    Redisplay,
    /* set_values         */    SetValues,
    /* set_values_hook    */	NULL,
    /* set_values_almost  */	XtInheritSetValuesAlmost,  
    /* get_values_hook    */	NULL,			
    /* accept_focus       */    NULL,
    /* intrinsics version */	XtVersion,
    /* callback offsets   */    NULL,
    /* tm_table		  */    NULL,
    /* query_geometry	  */    QueryGeometry,
    /* display_accelerator*/    NULL,
    /* extension	  */    NULL
  },{
    /* Menu Entry Fields */
      
    /* highlight */             Highlight,
    /* unhighlight */           Unhighlight,
    /* notify */		Notify,		
    /* extension	  */    NULL
  }, {
    /* BSB Menu entry Fields */  

    /* extension	  */    NULL
  }
};

WidgetClass smeMenuObjectClass = (WidgetClass) &smeMenuClassRec;

/************************************************************
 *
 * Semi-Public Functions.
 *
 ************************************************************/

/*	Function Name: ClassInitialize
 *	Description: Initializes the SmeMenuObject. 
 *	Arguments: none.
 *	Returns: none.
 */

static void 
ClassInitialize()
{
    ENTER("ClassInitialize()",0)
    XawInitializeWidgetSet();
    XtAddConverter( XtRString, XtRJustify, XmuCvtStringToJustify, NULL, 0 );
    EXIT("ClassInitialize()",0)
}

/*      Function Name: Initialize
 *      Description: Initializes the simple menu widget
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
    SmeMenuObject entry = (SmeMenuObject) new;

    ENTER("Initialize()",new)
    if (entry->sme_menu.label == NULL) 
	entry->sme_menu.label = XtName(new);
    else
	entry->sme_menu.label = XtNewString( entry->sme_menu.label );
    entry->sme_menu.up = False;
    entry->sme_menu.popup = 0;
    entry->sme_menu.needflip = False;

    GetDefaultSize(new, &(entry->rectangle.width), &(entry->rectangle.height));
    CreateGCs(new);
    EXIT("Initialize()",new)
}


/*      Function Name: Destroy
 *      Description: Called at destroy time, cleans up.
 *      Arguments: w - the simple menu widget.
 *      Returns: none.
 */
static void
Destroy(w)
Widget w;
{
    SmeMenuObject entry = (SmeMenuObject) w;

    DestroyGCs(w);
    if (entry->sme_menu.label != XtName(w))
	XtFree(entry->sme_menu.label);
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
    GC gc;
    SmeMenuObject entry = (SmeMenuObject) w;
    int	font_ascent, font_descent, y_loc;

    font_ascent = entry->sme_menu.font->max_bounds.ascent;
    font_descent = entry->sme_menu.font->max_bounds.descent;

    y_loc = entry->rectangle.y;
    
    if (XtIsSensitive(w) && XtIsSensitive( XtParent(w) ) ) {
	if ( w == XawSimpleMenuGetActiveEntry(XtParent(w)) ) {
	    XFillRectangle(XtDisplayOfObject(w), XtWindowOfObject(w), 
			   entry->sme_menu.norm_gc, 0, y_loc,
			   (unsigned int) entry->rectangle.width,
			   (unsigned int) entry->rectangle.height);
	    gc = entry->sme_menu.rev_gc;
	}
	else
	    gc = entry->sme_menu.norm_gc;
    }
    else
	gc = entry->sme_menu.norm_gray_gc;
    
    if (entry->sme_menu.label != NULL) {
	int x_loc = entry->sme_menu.left_margin;
	int len = strlen(entry->sme_menu.label);
	char * label = entry->sme_menu.label;

	switch(entry->sme_menu.justify) {
	    int width, t_width;

	case XtJustifyCenter:
	    t_width = XTextWidth(entry->sme_menu.font, label, len);
	    width = entry->rectangle.width - (entry->sme_menu.left_margin +
					      entry->sme_menu.right_margin);
	    x_loc += (width - t_width)/2;
	    break;
	case XtJustifyRight:
	    t_width = XTextWidth(entry->sme_menu.font, label, len);
	    x_loc = entry->rectangle.width - (entry->sme_menu.right_margin +
					      t_width);
	    break;
	case XtJustifyLeft:
	default:
	    break;
	}

	y_loc += (entry->rectangle.height - 
		  (font_ascent + font_descent)) / (unsigned)2 + font_ascent;
	
	XDrawString(XtDisplayOfObject(w), XtWindowOfObject(w), gc,
		    x_loc, y_loc, label, len);
    }
    DrawBitmaps(w, gc);
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
    SmeMenuObject entry = (SmeMenuObject) new;
    SmeMenuObject old_entry = (SmeMenuObject) current;
    Boolean ret_val = FALSE;

    if (old_entry->sme_menu.label != entry->sme_menu.label) {
        if (old_entry->sme_menu.label != XtName( new ) )
	    XtFree( (char *) old_entry->sme_menu.label );

	if (entry->sme_menu.label != XtName(new)) 
	    entry->sme_menu.label = XtNewString(entry->sme_menu.label);

	ret_val = TRUE;
    }

    if (entry->rectangle.sensitive != old_entry->rectangle.sensitive)
	ret_val = TRUE;

    if (entry->sme_menu.left_bitmap != old_entry->sme_menu.left_bitmap) {
	GetBitmapInfo(new, TRUE);
	ret_val = TRUE;
    }

    if (entry->sme_menu.right_bitmap != old_entry->sme_menu.right_bitmap) {
	GetBitmapInfo(new, FALSE);
	ret_val = TRUE;
    }

    if ( (old_entry->sme_menu.font != entry->sme_menu.font) ||
	 (old_entry->sme_menu.foreground != entry->sme_menu.foreground) ) {
	DestroyGCs(current);
	CreateGCs(new);
	ret_val = TRUE;
    }

    if (ret_val) {
	GetDefaultSize(new, 
		       &(entry->rectangle.width), &(entry->rectangle.height));
    }
    return(ret_val);
}

/*	Function Name: QueryGeometry.
 *	Description: Returns the preferred geometry for this widget.
 *	Arguments: w - the menu entry object.
 *                 itended, return_val - the intended and return geometry info.
 *	Returns: A Geometry Result.
 *
 * See the Intrinsics manual for details on what this function is for.
 * 
 * I just return the height and width of the label plus the margins.
 */

static XtGeometryResult
QueryGeometry(w, intended, return_val) 
Widget w;
XtWidgetGeometry *intended, *return_val;
{
    SmeMenuObject entry = (SmeMenuObject) w;
    Dimension width, height;
    XtGeometryResult ret_val = XtGeometryYes;
    XtGeometryMask mode = intended->request_mode;

    GetDefaultSize(w, &width, &height );    

    if ( ((mode & CWWidth) && (intended->width != width)) ||
	 !(mode & CWWidth) ) {
	return_val->request_mode |= CWWidth;
	return_val->width = width;
	ret_val = XtGeometryAlmost;
    }

    if ( ((mode & CWHeight) && (intended->height != height)) ||
	 !(mode & CWHeight) ) {
	return_val->request_mode |= CWHeight;
	return_val->height = height;
	ret_val = XtGeometryAlmost;
    }

    if (ret_val == XtGeometryAlmost) {
	mode = return_val->request_mode;
	
	if ( ((mode & CWWidth) && (width == entry->rectangle.width)) &&
	     ((mode & CWHeight) && (height == entry->rectangle.height)) )
	    return(XtGeometryNo);
    }

    return(ret_val);
}



static void
Highlight(w)
	Widget w;
{
	SmeMenuObject entry = (SmeMenuObject) w;
	XPoint loc;
	Window junkW;
	int x, y, junkI;
	int left, right;
	unsigned int junkU;
/*
 * Decide whether we want the menu up or not.
 * Note that the fact that our Highlight method has been called implies that
 * the pointer is in our entry region, and all we need check for popping up
 * the sub-menu is the X coordinate of the pointer relative to the X origin
 * of our entry.
 *
 * To support left-cascading menus when near the right edge, we must use
 * two x limits: one on the right for popping up, and one towards the left
 * for popping down.
 */
	ENTER("Highlight()",w)
	if (! XQueryPointer (XtDisplay (XtParent (w)), XtWindow (XtParent (w)),
			&junkW, &junkW, &junkI, &junkI, &x, &y, &junkU))
	{
		EXIT("Highlight(), query pointer failed",w);
		return;
	}
	if (!entry->sme_menu.needflip)
	{
		entry->sme_menu.needflip = TRUE;
		FlipColors(w);
	}
/*
 * Should probably get leftMargin resource and use that, or specify
 * this as a unique resource in the SmeMenu widget.  For now, use (x > 60)
 * unless width/2 is less than 60.  The left limit is the right limit less
 * 20.  The difference is fixed since we don't care so much if you have to
 * pop down a menu by highlighting another entry, whereas left-cascades are
 * impossible if the left and right limits are too close.
 */
	right = entry->rectangle.width / 2;
	right = (right > 60) ? 60 : right;
	left = right - 20;
/*
 * If we do want it up, and it's not, and there is a menu name, put it up.
 */
	if (entry->sme_menu.menu == NULL)
	{
		/* nothing to do with nothin' */;
	}
	else if (!entry->sme_menu.up && (x > right)) 
	{
		Position root_x, root_y;

		/* 
		 * Figure a location for the menu.  All we really want to do 
		 * is adjust y so that submenus always appear in the same
		 * place relative to the highlighted entry.
		 * The root-relative location is passed to 
		 * RdssPositionMenu() to position the popup.
		 */
		XtTranslateCoords (XtParent(w), w->core.x, w->core.y,
				   &root_x, &root_y);
		root_x += x;
		root_y += entry->rectangle.height/2;

		/* 
	 	 * convert the menu name to a widget if we haven't already 
		 */
		if (! entry->sme_menu.popup) 
		{
			entry->sme_menu.popup =
				FindPopup(XtParent(w), entry->sme_menu.menu);
			if (! entry->sme_menu.popup) 
			{
				entry->sme_menu.menu = NULL;
				EXIT("Highlight()",w)
				return;
			}
		}
		
		/*
		 * We now have a popup shell and a place to put it
		 */
		loc.x = root_x; loc.y = root_y;
		entry->sme_menu.up = True;
		RdssSubMenuPositionAndPopup(entry->sme_menu.popup, &loc,
					    XtGrabNonexclusive);
		IFD(ui_printf("	%s highlight popping up menu %s\n",
			      XtName(w),
			      XtName((Widget)entry->sme_menu.popup));)
	}
/*
 * If it is up, and we don't want that, bring it back down.  Our colors
 * will stay the same though, since we are at least still highlighted.
 */
 	else if (entry->sme_menu.up && (x <= left))
	{
		RdssMenuPopdown (entry->sme_menu.popup);
		entry->sme_menu.up = False;
		IFD(ui_printf("	%s highlight popping down menu %s\n",
			      XtName(w),
			      XtName((Widget)entry->sme_menu.popup));)
	}
	EXIT("Highlight(): successful",w)
}



static void
Unhighlight(w)
Widget w;
/*
 * We expect the parent menu to be smart enough to differentiate
 * false alarm LeaveWindows and only call this method when it is
 * absolutely intended that the popup should be popped down and
 * the entry unhighlighted.
 */
{
	SmeMenuObject entry = (SmeMenuObject) w;
	Widget popup = entry->sme_menu.popup;

	ENTER("Unhighlight()",w)
	if (entry->sme_menu.needflip) {
		entry->sme_menu.needflip = FALSE;
		FlipColors(w);
		ACK("	Unhighlight flipped colors",w)
	}

	if (!popup || !entry->sme_menu.up) /* No popup created or popped up */
	{
		EXIT("Unhighlight(): no popup shell created or popped up",w)
		return;
	}

	/*
	 * Unhighlight the menu shell by calling its 'unhighlight' action.
	 * The null event must be ignored by the action procedure.
	 */
	IFD(ui_printf("%s: calling unhighlight for popup '%s'\n",
		      XtName(w),XtName(popup));)
	XtCallActionProc (popup, "disable-highlight", (XEvent *)NULL,	
			  /*params*/ NULL, /*num_params*/ 0);
	XtCallActionProc (popup, "unhighlight", (XEvent *)NULL,	
			  /*params*/ NULL, /*num_params*/ 0);

	/*
	 * Then popdown the menu shell
	 */
	RdssMenuPopdown (popup);
	entry->sme_menu.up = False;
	IFD(ui_printf("%s: menu '%s' popped down\n",
		      XtName(w), XtName(popup));)
	EXIT("Unhighlight(): done",w)
}



/* ARGSUSED */
static void
Notify(w)
	Widget w;
{
	SmeMenuObject entry = (SmeMenuObject) w;

	ACK("Notify called.",w)
	if (entry->sme_menu.popup_last_select)
	   ((SimpleMenuWidget)entry->object.parent)->simple_menu.popup_entry = 
		(SmeObject)entry;
}


    
/*      Function Name: FlipColors
 *      Description: Invert the colors of the current entry.
 *      Arguments: w - the menu entry widget.
 *      Returns: none.
 */
static void 
FlipColors(w)
Widget w;
{
    SmeMenuObject entry = (SmeMenuObject) w;

    ACK("flipped colors",w)
    XFillRectangle(XtDisplayOfObject(w), XtWindowOfObject(w),
		   entry->sme_menu.invert_gc, 0, (int) entry->rectangle.y,
		   (unsigned int) entry->rectangle.width, 
		   (unsigned int) entry->rectangle.height);
}


/************************************************************
 *
 * Private Functions.
 *
 ************************************************************/

/*	Function Name: GetDefaultSize
 *	Description: Calculates the Default (preferred) size of
 *                   this menu entry.
 *	Arguments: w - the menu entry widget.
 *                 width, height - default sizes (RETURNED).
 *	Returns: none.
 */

static void
GetDefaultSize(w, width, height) 
Widget w;
Dimension * width, * height;
{
    SmeMenuObject entry = (SmeMenuObject) w;

    if (entry->sme_menu.label == NULL) 
	*width = 0;
    else
	*width = XTextWidth(entry->sme_menu.font, entry->sme_menu.label,
			    strlen(entry->sme_menu.label));

    *width += entry->sme_menu.left_margin + entry->sme_menu.right_margin;
    
    *height = (entry->sme_menu.font->max_bounds.ascent +
	       entry->sme_menu.font->max_bounds.descent);

    *height = (*height * ( ONE_HUNDRED + 
		  entry->sme_menu.vert_space )) / (unsigned)ONE_HUNDRED;
}

/*      Function Name: DrawBitmaps
 *      Description: Draws left and right bitmaps.
 *      Arguments: w - the simple menu widget.
 *                 gc - graphics context to use for drawing.
 *      Returns: none
 */

static void
DrawBitmaps(w, gc)
Widget w;
GC gc;
{
    int x_loc, y_loc;
    SmeMenuObject entry = (SmeMenuObject) w;
    
    if ( (entry->sme_menu.left_bitmap == None) && 
	 (entry->sme_menu.right_bitmap == None) ) return;

    y_loc = entry->rectangle.y + (entry->rectangle.height -
	  entry->sme_menu.left_bitmap_height) / (unsigned)2;

/*
 * Draw Left Bitmap.
 */

  if (entry->sme_menu.left_bitmap != None) {
    x_loc = (entry->sme_menu.left_margin - 
	     entry->sme_menu.left_bitmap_width) / (unsigned)2;
    XCopyPlane(XtDisplayOfObject(w), entry->sme_menu.left_bitmap,
	       XtWindowOfObject(w), gc, 0, 0, 
	       entry->sme_menu.left_bitmap_width,
	       entry->sme_menu.left_bitmap_height, x_loc, y_loc, 1);
  }

/*
 * Draw Right Bitmap.
 */

  if (entry->sme_menu.right_bitmap != None) {
    x_loc = entry->rectangle.width - (entry->sme_menu.right_margin - 
	      entry->sme_menu.right_bitmap_width) / (unsigned)2;
    XCopyPlane(XtDisplayOfObject(w), entry->sme_menu.right_bitmap,
	       XtWindowOfObject(w), gc, 0, 0, 
	       entry->sme_menu.right_bitmap_width,
	       entry->sme_menu.right_bitmap_height, x_loc, y_loc, 1);
  }
}

/*      Function Name: GetBitmapInfo
 *      Description: Gets the bitmap information from either of the bitmaps.
 *      Arguments: w - the menu entry widget.
 *                 is_left - TRUE if we are testing left bitmap,
 *                           FALSE if we are testing the right bitmap.
 *      Returns: none
 */

static void
GetBitmapInfo(w, is_left)
Widget w;
Boolean is_left;
{
    SmeMenuObject entry = (SmeMenuObject) w;    
    unsigned int depth, bw;
    Window root;
    int x, y;
    unsigned int width, height;
    char buf[BUFSIZ];
    
    if (is_left) {
	if (entry->sme_menu.left_bitmap != None) {
	    if (!XGetGeometry(XtDisplayOfObject(w), 
			      entry->sme_menu.left_bitmap, &root, 
			      &x, &y, &width, &height, &bw, &depth)) {
		sprintf(buf, "SmeMenu Object: %s %s \"%s\".", "Could not",
			"get Left Bitmap geometry information for menu entry ",
			XtName(w));
		XtAppError(XtWidgetToApplicationContext(w), buf);
	    }
	    if (depth != 1) {
		sprintf(buf, "SmeMenu Object: %s \"%s\"%s.", 
			"Left Bitmap of entry ", 
			XtName(w), " is not one bit deep.");
		XtAppError(XtWidgetToApplicationContext(w), buf);
	    }
	    entry->sme_menu.left_bitmap_width = (Dimension) width; 
	    entry->sme_menu.left_bitmap_height = (Dimension) height;
	}
    }
    else if (entry->sme_menu.right_bitmap != None) {
	if (!XGetGeometry(XtDisplayOfObject(w),
			  entry->sme_menu.right_bitmap, &root,
			  &x, &y, &width, &height, &bw, &depth)) {
	    sprintf(buf, "SmeMenu Object: %s %s \"%s\".", "Could not",
		    "get Right Bitmap geometry information for menu entry ",
		    XtName(w));
	    XtAppError(XtWidgetToApplicationContext(w), buf);
	}
	if (depth != 1) {
	    sprintf(buf, "SmeMenu Object: %s \"%s\"%s.", 
		    "Right Bitmap of entry ", XtName(w),
		    " is not one bit deep.");
	    XtAppError(XtWidgetToApplicationContext(w), buf);
	}
	entry->sme_menu.right_bitmap_width = (Dimension) width; 
	entry->sme_menu.right_bitmap_height = (Dimension) height;
    }
}      

/*      Function Name: CreateGCs
 *      Description: Creates all gc's for the simple menu widget.
 *      Arguments: w - the simple menu widget.
 *      Returns: none.
 */

static void
CreateGCs(w)
Widget w;
{
    SmeMenuObject entry = (SmeMenuObject) w;    
    XGCValues values;
    XtGCMask mask;
    
    values.foreground = XtParent(w)->core.background_pixel;
    values.background = entry->sme_menu.foreground;
    values.font = entry->sme_menu.font->fid;
    values.graphics_exposures = FALSE;
    mask        = GCForeground | GCBackground | GCFont | GCGraphicsExposures;
    entry->sme_menu.rev_gc = XtGetGC(w, mask, &values);
    
    values.foreground = entry->sme_menu.foreground;
    values.background = XtParent(w)->core.background_pixel;
    entry->sme_menu.norm_gc = XtGetGC(w, mask, &values);
    
    values.fill_style = FillTiled;
    values.tile   = XmuCreateStippledPixmap(XtScreenOfObject(w), 
					    entry->sme_menu.foreground,
					    XtParent(w)->core.background_pixel,
					    XtParent(w)->core.depth);
    values.graphics_exposures = FALSE;
    mask |= GCTile | GCFillStyle;
    entry->sme_menu.norm_gray_gc = XtGetGC(w, mask, &values);
    
    values.foreground ^= values.background;
    values.background = 0;
    values.function = GXxor;
    mask = GCForeground | GCBackground | GCGraphicsExposures | GCFunction;
    entry->sme_menu.invert_gc = XtGetGC(w, mask, &values);
}

/*      Function Name: DestroyGCs
 *      Description: Removes all gc's for the simple menu widget.
 *      Arguments: w - the simple menu widget.
 *      Returns: none.
 */

static void
DestroyGCs(w)
Widget w;
{
    SmeMenuObject entry = (SmeMenuObject) w;    

    XtReleaseGC(w, entry->sme_menu.norm_gc);
    XtReleaseGC(w, entry->sme_menu.norm_gray_gc);
    XtReleaseGC(w, entry->sme_menu.rev_gc);
    XtReleaseGC(w, entry->sme_menu.invert_gc);
}

#ifdef apollo

/*
 * The apollo compiler that we have optomizes out my code for
 * FlipColors() since it is static. and no one executes it in this
 * file.  I am setting the function pointer into the class structure so
 * that it can be called by my parent who will tell me to when to
 * highlight and unhighlight.
 */

void _XawSmeMenuApolloHack ()
{
    FlipColors();
}
#endif /* apollo */



/*
 * This is a copy of XtFindPopup, that we can reach.
 */

static Widget
FindPopup(widget, name)
    Widget widget;
    String name;
{
    register Cardinal i;
    register XrmQuark q;
    register Widget w;

    q = XrmStringToQuark(name);

    for (w=widget; w != NULL; w=w->core.parent)
	for (i=0; i<w->core.num_popups; i++)
	    if (w->core.popup_list[i]->core.xrm_name == q)
		return w->core.popup_list[i];

    return NULL;
}


Boolean
SmeMenuPoppedUp (w)
    Widget w;
/*
 * Intended to be polled by a Menu shell to be sure that this SmeMenu object
 * is expecting to be kept current in its parent's entry_set by virtue
 * of the fact that the SmeMenu object still has a menu popped up.  The entry
 * should only be unhighlighted when another Sme object in the parent is
 * highlighted, or a LeaveNotify occurs while no menu is popped up.
 */
{
	SmeMenuObject entry = (SmeMenuObject) w;
	IFD(Widget popup = entry->sme_menu.popup;)

	IFD(
	    if (popup)
	       ui_printf("SmeMenuPoppedUp(): XtIsRealized returns %s\n",
			 (XtIsRealized(popup))?("True"):("False"));
	    else
	       ui_printf("SmeMenuPoppedUp(): popup menu is NULL\n");
	    )
	IFD(ui_printf("SmeMenuPoppedUp() returning %s\n",
			(entry->sme_menu.up)?("True"):("False"));)
	return (entry->sme_menu.up);
}


