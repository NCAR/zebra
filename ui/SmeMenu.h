/*
 * $Id: SmeMenu.h,v 1.5 1993-04-12 18:38:14 granger Exp $
 *
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
 * SmeMenu.h - Public Header file for SmeMenu object.
 *
 * This is the public header file for the Athena Menu Sme object.
 * It is intended to be used with the simple menu widget.  This object
 * provides a hierarchical menus building block.  I.e., it is a
 * simple menu entry that invokes another simple menu.
 *
 * Date:    March 15, 1990
 *
 * By:      Van Jacobson
 *          Lawrence Berkeley Laboratory
 *          van@helios.ee.lbl.gov
 *
 * This code is based on Xaw/SmeBSB.h by Chris Peterson, MIT X Consortium.
 */

#ifndef _SmeMenu
#define _SmeMenu

#include <X11/Xmu/Converters.h>

#include <X11/Xaw/Sme.h>

/****************************************************************
 *
 * SmeMenu object
 *
 ****************************************************************/

/* Menu Menu Entry Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 callback            Callback           Callback        NULL
 destroyCallback     Callback		Pointer		NULL
 font                Font               XFontStruct *   XtDefaultFont
 foreground          Foreground         Pixel           XtDefaultForeground
 height		     Height		Dimension	0
 label               Label              String          Name of entry
 leftBitmap          LeftBitmap         Pixmap          None
 leftMargin          HorizontalMargins  Dimension       4
 rightBitmap         RightBitmap        Pixmap          None
 rightMargin         HorizontalMargins  Dimension       4
 sensitive	     Sensitive		Boolean		True
 vertSpace           VertSpace          int             25
 width		     Width		Dimension	0
 x		     Position		Position	0n
 y		     Position		Position	0

*/

typedef struct _SmeMenuClassRec    *SmeMenuObjectClass;
typedef struct _SmeMenuRec         *SmeMenuObject;

extern WidgetClass smeMenuObjectClass;

#define XtNleftBitmap "leftBitmap"
#define XtNleftMargin "leftMargin"
#define XtNrightBitmap "rightBitmap"
#define XtNrightMargin "rightMargin"
#define XtNvertSpace   "vertSpace"
#define XtNmenu	       "menu"
#define XtNpopupSelection "popupSelection"

#define XtCLeftBitmap "LeftBitmap"
#define XtCHorizontalMargins "HorizontalMargins"
#define XtCRightBitmap "RightBitmap"
#define XtCVertSpace   "VertSpace"
#define XtCMenu	       "Menu"
#define XtCPopupSelection "PopupSelection"

# if NeedFunctionPrototypes
	Boolean	SmeMenuPoppedUp (Widget);
# else
	Boolean SmeMenuPoppedUp ();
# endif

#endif /* _SmeMenu_h */
