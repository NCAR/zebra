/*
 * $Id: RdssMenuP.h,v 1.4 2001-11-30 00:42:05 granger Exp $
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
 */

/* Original source before being cannibalized and hacked to death
 *----------------------------------------------------------------
 * SimpleMenuP.h - Private Header file for SimpleMenu widget.
 *
 * Date:    April 3, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium
 *          kit@expo.lcs.mit.edu
 *
 * Now its RdssMenuP.h, circa April 12, 1993
 */

#ifndef _RdssMenuP_h
#define _RdssMenuP_h

#include "RdssMenu.h"
#include <X11/Xaw/SmeP.h>
#include <X11/ShellP.h>

#define ForAllChildren(smw, childP) \
  for ( (childP) = (SmeObject *) (smw)->composite.children ; \
        (childP) < (SmeObject *) ( (smw)->composite.children + \
				 (smw)->composite.num_children ) ; \
        (childP)++ )

typedef struct {
    XtPointer extension;		/* For future needs. */
} RdssMenuClassPart;

typedef struct _RdssMenuClassRec {
  CoreClassPart	          core_class;
  CompositeClassPart      composite_class;
  ShellClassPart          shell_class;
  OverrideShellClassPart  override_shell_class;
  RdssMenuClassPart	  rdssMenu_class;
} RdssMenuClassRec;

extern RdssMenuClassRec rdssMenuClassRec;

typedef struct _RdssMenuPart {

  /* resources */

  String       label_string;	/* The string for the label or NULL. */
  SmeObject   label;		/* If label_string is non-NULL then this is
				   the label widget. */
  WidgetClass  label_class;	/* Widget Class of the menu label object. */

  Dimension    top_margin;	/* Top and bottom margins. */
  Dimension    bottom_margin;
  Dimension    row_height;	/* height of each row (menu entry) */

  Cursor       cursor;		/* The menu's cursor. */
  SmeObject popup_entry;	/* The entry to position the cursor on for
				   when using XawPositionSimpleMenu. */
  Boolean      menu_on_screen;	/* Force the menus to be fully on the screen.*/
  int          backing_store;	/* What type of backing store to use. */

  /* private state */

  Boolean recursive_set_values;	/* contain a possible infinite loop. */

  Boolean menu_width;		/* If true then force width to remain 
				   core.width */
  Boolean menu_height;		/* Just like menu_width, but for height. */

  SmeObject entry_set;		/* The entry that is currently set or
				   highlighted. */
  Boolean highlight_enabled;	/* Whether to accept calls to highlight() action */

  Boolean release;              /* Whether release() should leave menu */
  Boolean grabbed;              /* True when this menu (ie top menu) has
				   grabbed the pointer and should ungrab. */

} RdssMenuPart;

typedef struct _RdssMenuRec {
  CorePart		core;
  CompositePart 	composite;
  ShellPart 	        shell;
  OverrideShellPart     override;
  RdssMenuPart	rdss_menu;
} RdssMenuRec;

#endif /* _RdssMenuP_h */
