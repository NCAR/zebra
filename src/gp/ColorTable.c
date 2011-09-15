/*
 * Color control / display manager interface code.
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */


# include <X11/Intrinsic.h>

# include <defs.h>
# include <zl_symbol.h>
# include <dm.h>
# include <dm_ctable.h>
# include <pd.h>
# include <message.h>
# include "GraphProc.h"

RCSID ("$Id: ColorTable.c,v 2.12 1999-03-01 02:04:20 burghart Exp $")

/*
 * For now, we use a simple bitmap to keep track of the colors that
 * have been allocated.
 *
 * Those days are now passed.  In the world of 32-bit video depth, we
 * can't do things that way any more.  Now we have a simple list, and
 * search it linearly.  The number of colors we actually allocate is still
 * reasonably small, so I'm hoping this won't get too expensive; otherwise
 * something a little smarter will have to be done.
 */
# define MAXCOLOR 4096		/* That oughtta be enough		*/
static Pixel AColors[MAXCOLOR];
static int NColorsAlloc = 0;


/*
 * Keep track of which colors we have allocated.
 */
static inline void
ct_MarkColor (color)
Pixel color;
{
	int nc;
/*
 * Don't mark colors more than once.
 */
	for (nc = 0; nc < NColorsAlloc; nc++)
		if (AColors[nc] == color)
			return;
	if (NColorsAlloc >= (MAXCOLOR - 1))
		msg_ELog (EF_PROBLEM, "Allocated too many colors!");
	else
		AColors[NColorsAlloc++] = color;
}



static void
ct_ClearColors ()
{
	memset ((void *) AColors, 0, NColorsAlloc*sizeof (Pixel));
	NColorsAlloc = 0;
}




/*
 * Color tables are stored by name in a special symbol table.  Each entry
 * contains the following:
 */
typedef struct ColorTable
{
	char	ct_name[80];		/* The name of this table	*/
	int	ct_ncolor;		/* The number of colors		*/
	XColor	*ct_colors;		/* The actual color values	*/
	zbool	ct_alloc;		/* The colors alloc'd from server */
} CTable;

static stbl Ctable = 0;


/*
 * Forward routine definitions.
 */
static CTable * ct_AskDMForTable FP((char *));
static void ct_DoAlloc FP((CTable *));
static int ct_MarkDealloc FP((char *, int, union usy_value *, int));


void
ct_Init ()
/*
 * Initialize the color table code.
 */
{
	Ctable = usy_c_stbl ("colors");
	ct_ClearColors ();
}




zbool
ct_LoadTable (name, colors, ncolor)
char *name;
XColor **colors;
int *ncolor;
/*
 * Obtain a named color table.
 * Entry:
 *	NAME	is the name of the color table of interest.
 * Exit:
 *	If the color table exists then:
 *	COLORS	is an array of XColor structs describing the table.
 *	NCOLOR	is the length of that array
 *		The return value is TRUE
 *	Else
 *		The return value is false.
 */
{
	union usy_value v;
	int type;
	CTable *ct;
	static XColor white;
/*
 * Try to find this table in our cache.
 */
	if (usy_g_symbol (Ctable, name, &type, &v))
		ct = (CTable *) v.us_v_ptr;
/*
 * Otherwise go and get it.
 */
	else if (! (ct = ct_AskDMForTable (name)))
	{
	/*
	 * At the very least, return a single white color.
	 */
		white.pixel = WhitePixelOfScreen (XtScreen(Graphics));
		*colors = &white;
		*ncolor = 1;
		return (FALSE);
	}
/*
 * If we need to go and do an alloc on this table, do it now.
 */
	if (! ct->ct_alloc)
		ct_DoAlloc (ct);
/*
 * Return info.  If we found a table, we can at least return the number of
 * slots asked for, even if not all the slots were allocated.
 */
	*colors = ct->ct_colors;
	*ncolor = ct->ct_ncolor;
/*
 * If we couldn't alloc the whole table, we can't claim success, even
 * though some callers will not bother to check.
 */
	if (! ct->ct_alloc)
		return (FALSE);

	return (TRUE);
}





static CTable *
ct_AskDMForTable (name)
char *name;
/*
 * Attempt to obtain this color table from the display manager.
 */
{
	struct dm_ctable *repl;
	CTable *ct;
	union usy_value v;
/*
 * Make an attempt to get the table.
 */
	if (! (repl = dm_ColorTable (name)))
		return (NULL);
/*
 * Otherwise it's time to allocate and fill in a ctable structure.
 */
	ct = ALLOC (CTable);
	strcpy (ct->ct_name, name);
	ct->ct_ncolor = repl->dmm_ncolor;
	ct->ct_colors = (XColor *) malloc (ct->ct_ncolor * sizeof (XColor));
	memcpy ((void *)ct->ct_colors, (void *)repl->dmm_cols, 
		ct->ct_ncolor*sizeof (XColor));
	ct->ct_alloc = FALSE;
	free (repl);
/*
 * Store a symbol table entry.
 */
	v.us_v_ptr = (char *) ct;
	usy_s_symbol (Ctable, name, SYMT_POINTER, &v);
/*
 * All done.
 */
	return (ct);
}




static void 
ct_DoAlloc (ct)
CTable *ct;
/*
 * Allocate all of the colors in this table from the server.
 */
{
	int color;
	Display *disp = XtDisplay (Top);
	Colormap cm = DefaultColormap (disp, 0);
/*
 * Just go through and get them all from the server.  In theory, DM has
 * already cleared all of these color values, so there should be no
 * problem with this.
 */
	ct->ct_alloc = TRUE;  /* successful unless errors prove otherwise */
	for (color = 0; color < ct->ct_ncolor; color++)
	{
		if (XAllocColor (disp, cm, ct->ct_colors + color))
		{
			ct_MarkColor (ct->ct_colors[color].pixel);
		}
		else
		{
			msg_ELog (EF_PROBLEM, "Color alloc failure");
			ct->ct_alloc = FALSE;
		/*
		 * Set a pixel value anyway for those routines which ignore
		 * our return value and the ct_alloc member.  Don't call
		 * ct_MarkColor though since it isn't actually allocated.
		 */
			ct->ct_colors[color].pixel = 
				WhitePixelOfScreen (XtScreen(Graphics));
		}
	}
}




void
ct_FreeColors ()
/*
 * Clear out the allocated colors.
 */
{
/*
 * Release them all in one swell foop.
 */
	XFreeColors (XtDisplay (Top), DefaultColormap (XtDisplay (Top), 0),
		AColors, NColorsAlloc, 0);
	ct_ClearColors ();
/*
 * Traverse through the symbol table, and mark every entry as unallocated.
 */
	usy_traverse (Ctable, ct_MarkDealloc, 0, FALSE);
}





static int
ct_MarkDealloc (ctable, type, v, junk)
char *ctable;
int type, junk;
union usy_value *v;
/*
 * Mark this color table as unallocated.
 */
{
	CTable *ct = (CTable *) v->us_v_ptr;

	ct->ct_alloc = FALSE;
	return (TRUE);
}




void
ct_DeleteTable (name)
char *name;
/*
 * Cause this color table not to exist.  This routine does *not* deallocate
 * colors used by this table.
 */
{
	CTable *ct;
	union usy_value v;
	int type;
/*
 * Look the table up.
 */
	if (! usy_g_symbol (Ctable, name, &type, &v))
		return;
/*
 * Deallocate memory.
 */
	ct = (CTable *) v.us_v_ptr;
	free (ct->ct_colors);
	free (ct);
/*
 * Get rid of the table entry.
 */
	usy_z_symbol (Ctable, name);
}





int
ct_GetColorByName (name, color)
char *name;
XColor *color;
/*
 * Grab an individual color by name.
 */
{
	XColor junk;

	if (XAllocNamedColor (XtDisplay (Top), 
		DefaultColormap (XtDisplay (Top), 0), name, color, &junk))
	{
		ct_MarkColor (color->pixel);
		return (TRUE);
	}
	return (FALSE);
}






int
ct_GetColorByRGB (color)
XColor *color;
/*
 * Grab an individual color by RGB values.
 */
{
	if (XAllocColor (XtDisplay (Top), 
			DefaultColormap (XtDisplay (Top), 0), color))
	{
		ct_MarkColor (color->pixel);
		return (TRUE);
	}
	return (FALSE);
}
