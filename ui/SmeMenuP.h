/*
 * $Id: SmeMenuP.h,v 1.3 1993-04-12 19:50:35 granger Exp $
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
 *
 * Date:    March 15, 1990
 *
 * By:      Van Jacobson
 *          Lawrence Berkeley Laboratory
 *          van@helios.ee.lbl.gov
 *
 * This code is based on Xaw/SmeBSBP.h by Chris Peterson, MIT X Consortium,
 */

/* 
 * SmeMenuP.h - Private definitions for Sme (sub) menu object.
 * 
 */

#ifndef _XawSmeMenuP_h
#define _XawSmeMenuP_h

/***********************************************************************
 *
 * Sme Object Private Data
 *
 ***********************************************************************/

#include <X11/Xaw/SmeP.h>
#include "SmeMenu.h"

/************************************************************
 *
 * New fields for the Sme Object class record.
 *
 ************************************************************/

typedef struct _SmeMenuClassPart {
  XtPointer extension;
} SmeMenuClassPart;

/* Full class record declaration */
typedef struct _SmeMenuClassRec {
    RectObjClassPart       rect_class;
    SmeClassPart     sme_class;
    SmeMenuClassPart  sme_menu_class;
} SmeMenuClassRec;

extern SmeMenuClassRec smeMenuClassRec;

/* New fields for the Sme Object record */
typedef struct {
    /* resources */
    String label;		/* The entry label. */
    int vert_space;		/* extra vert space to leave, as a percentage
				   of the font height of the label. */
    Pixmap left_bitmap, right_bitmap; /* bitmaps to show. */
    Dimension left_margin, right_margin; /* left and right margins. */
    Pixel foreground;		/* foreground color. */
    XFontStruct * font;		/* The font to show label in. */
    XtJustify justify;		/* Justification for the label. */
    String menu;		/* the name of the menu to popup */
    Boolean popup_last_select;

/* private resources. */

    Widget popup;		/* the shell widget for the menu to popup */
    Boolean up;			/* set if popup popped up */
    Boolean needflip;		/* set if we need to unhighlight. */
    GC norm_gc;			/* normal color gc. */
    GC rev_gc;			/* reverse color gc. */
    GC norm_gray_gc;		/* Normal color (grayed out) gc. */
    GC invert_gc;		/* gc for flipping colors. */

    Dimension left_bitmap_width; /* size of each bitmap. */
    Dimension left_bitmap_height;
    Dimension right_bitmap_width;
    Dimension right_bitmap_height;

} SmeMenuPart;

/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _SmeMenuRec {
  ObjectPart         object;
  RectObjPart        rectangle;
  SmePart	     sme;
  SmeMenuPart        sme_menu;
} SmeMenuRec;

/************************************************************
 *
 * Private declarations.
 *
 ************************************************************/

#endif /* _XawSmeMenuP_h */
