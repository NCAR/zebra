/*
 * Hacked up pieces of the SimpleMenu widget, used by SmeMenu.
 */

#include <stdio.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/XawInit.h>
#include <X11/Xaw/SimpleMenP.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/Cardinals.h>

# ifdef notdef
#include <X11/Xmu/Initer.h>
#include <X11/Xmu/CharSet.h>
# endif

static void MoveMenu ();


/*	Function Name: _XawPositionMenu
 *	Description: Places the menu
 *	Arguments: w - the simple menu widget.
 *                 location - a pointer the the position or NULL.
 *	Returns: none.
 */

void
_XawPositionMenu(w, location)
Widget w;
XPoint * location;
{
    SimpleMenuWidget smw = (SimpleMenuWidget) w;
    SmeObject entry;
    XPoint t_point;
    
    if (location == NULL) {
	Window junk1, junk2;
	int root_x, root_y, junkX, junkY;
	unsigned int junkM;
	
	location = &t_point;
	if (XQueryPointer(XtDisplay(w), XtWindow(w), &junk1, &junk2, 
			  &root_x, &root_y, &junkX, &junkY, &junkM) == FALSE) {
	    char error_buf[BUFSIZ];
	    sprintf(error_buf, "%s %s", "Xaw - SimpleMenuWidget:",
		    "Could not find location of mouse pointer");
	    XtAppWarning(XtWidgetToApplicationContext(w), error_buf);
	    return;
	}
	location->x = (short) root_x;
	location->y = (short) root_y;
    }
    
    /*
     * The width will not be correct unless it is realized.
     */
    
    XtRealizeWidget(w);
    
    location->x -= (Position) w->core.width/2;
    
    if (smw->simple_menu.popup_entry == NULL)
	entry = smw->simple_menu.label;
    else
	entry = smw->simple_menu.popup_entry;

    if (entry != NULL)
	location->y -= entry->rectangle.y + entry->rectangle.height/2;

    MoveMenu(w, (Position) location->x, (Position) location->y);
}

/*	Function Name: MoveMenu
 *	Description: Actually moves the menu, may force it to
 *                   to be fully visable if menu_on_screen is TRUE.
 *	Arguments: w - the simple menu widget.
 *                 x, y - the current location of the widget.
 *	Returns: none 
 */

static void
MoveMenu(w, x, y)
Widget w;
Position x, y;
{
    Arg arglist[2];
    Cardinal num_args = 0;
    SimpleMenuWidget smw = (SimpleMenuWidget) w;
    
    if (smw->simple_menu.menu_on_screen) {
	int width = w->core.width + 2 * w->core.border_width;
	int height = w->core.height + 2 * w->core.border_width;
	
	if (x >= 0) {
	    int scr_width = WidthOfScreen(XtScreen(w));
	    if (x + width > scr_width)
		x = scr_width - width;
	}
	if (x < 0) 
	    x = 0;
	
	if (y >= 0) {
	    int scr_height = HeightOfScreen(XtScreen(w));
	    if (y + height > scr_height)
		y = scr_height - height;
	}
	if (y < 0)
	    y = 0;
    }
    
    XtSetArg(arglist[num_args], XtNx, x); num_args++;
    XtSetArg(arglist[num_args], XtNy, y); num_args++;
    XtSetValues(w, arglist, num_args);
}
