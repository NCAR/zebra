#if ( !defined(lint) && !defined(SABER) )
static char Xrcsid[] = "$XConsortium: SmeMenu.c,v 1.9 89/12/13 15:42:48 kit Exp $";
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

/*
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

#define ONE_HUNDRED 100

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
    XawInitializeWidgetSet();
    XtAddConverter( XtRString, XtRJustify, XmuCvtStringToJustify, NULL, 0 );
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

    if (entry->sme_menu.label == NULL) 
	entry->sme_menu.label = XtName(new);
    else
	entry->sme_menu.label = XtNewString( entry->sme_menu.label );
    entry->sme_menu.up = FALSE;
    entry->sme_menu.popup = 0;

    GetDefaultSize(new, &(entry->rectangle.width), &(entry->rectangle.height));
    CreateGCs(new);
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
		  (font_ascent + font_descent)) / 2 + font_ascent;
	
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

/* ARGSUSED */
static void
parent_popdown(w, closure, call_data)
	Widget w;
	XtPointer closure;
	XtPointer call_data;
{
	SmeMenuObject entry = (SmeMenuObject) closure;

	/*
	 * the top level menu has been told to pop down, if there's
	 * a menu up, tell it to pop down.
	 */
	if (entry->sme_menu.up)
		XtPopdown(entry->sme_menu.popup);
}

/* ARGSUSED */
static void
popdown_unhighlight(w, closure, call_data)
	Widget w;
	XtPointer closure;
	XtPointer call_data;
{
	SmeMenuObject entry = (SmeMenuObject) closure;

	entry->sme_menu.up = FALSE;
	if (entry->sme_menu.needflip) {
		entry->sme_menu.needflip = FALSE;
		FlipColors((Widget)entry);
	}
	if (entry->sme_menu.popup_last_select)
		((SimpleMenuWidget)entry->object.parent)->simple_menu.popup_entry = 
				(SmeObject)entry;
}

static void
Highlight(w)
	Widget w;
{
	SmeMenuObject entry = (SmeMenuObject) w;
	extern void _XawPositionMenu();
	XPoint loc;

	if (entry->sme_menu.menu && !entry->sme_menu.up) {
		Widget root;
		Position x, y;

		/* position the popup to the right of this entry */
		x = w->core.x + w->core.width - 16;
		y = w->core.y;
		for (root = w; !XtIsShell(root); ) {
			root = XtParent(root);
			x += root->core.x;
			y += root->core.y;
		}
		if (! ((ShellWidget)root)->shell.popped_up)
			return;

		/* convert the menu name to a widget if we haven't already */
		if (! entry->sme_menu.popup) {
			Widget FindPopup();

			entry->sme_menu.popup =
				FindPopup(XtParent(w), entry->sme_menu.menu);
			if (! entry->sme_menu.popup) {
				entry->sme_menu.menu = NULL;
				return;
			}
			/* XXX
			 * modal cascades don't really work and there's a
			 * good chance the submenu won't be told to popdown
			 * when the button is released.  So construct our
			 * own popdown cascade by adding callbacks to both
			 * our parent and the new popup so we get a chance
			 * to pop down everything no matter where the button
			 * is released.  Note that this code is arranged
			 * so the callbacks are only added once, not every
			 * time this menu is popped up.
			 */
			XtAddCallback(entry->sme_menu.popup, XtNpopdownCallback,
				      popdown_unhighlight, (XtPointer)entry);
			XtAddCallback(root, XtNpopdownCallback,
				      parent_popdown, (XtPointer)entry);
		}
		entry->sme_menu.up = TRUE;
		entry->sme_menu.needflip = TRUE;
		loc.x = x; loc.y = y;
		_XawPositionMenu(entry->sme_menu.popup, &loc);
		XtPopup(entry->sme_menu.popup, XtGrabNonexclusive);
		FlipColors(w);
	}
}

static void
Unhighlight(w)
	Widget w;
{
	SmeMenuObject entry = (SmeMenuObject) w;
	Widget popup = entry->sme_menu.popup;
	Window junkW;
	int x, y, junkI;
	unsigned int junkU;

	if (! popup)
		return;

	/* if the pointer is in the popup, do nothing */
	if (XQueryPointer(XtDisplay(popup), XtWindow(popup), &junkW, &junkW,
			  &junkI, &junkI, &x, &y, &junkU)) {
		if (x >= 0 && x < popup->core.width &&
		    y >= 0 && y < popup->core.height)
			return;
	}
	if (entry->sme_menu.needflip) {
		entry->sme_menu.needflip = FALSE;
		FlipColors(w);
	}
	if (entry->sme_menu.up)
		XtPopdown(popup);
}

/* ARGSUSED */
static void
Notify(w)
	Widget w;
{
	SmeMenuObject entry = (SmeMenuObject) w;

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
			  entry->sme_menu.vert_space )) / ONE_HUNDRED;
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
				  entry->sme_menu.left_bitmap_height) / 2;

/*
 * Draw Left Bitmap.
 */

  if (entry->sme_menu.left_bitmap != None) {
    x_loc = (entry->sme_menu.left_margin - 
	     entry->sme_menu.left_bitmap_width) / 2;
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
				      entry->sme_menu.right_bitmap_width) / 2;
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

Widget FindPopup(widget, name)
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

