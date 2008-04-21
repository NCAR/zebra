/*
 * Handle active areas on the screen.
 */

/*		Copyright (C) 1987-1993 by UCAR
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
# include <message.h>
# include <pd.h>
# include <GraphicsW.h>

# include "GraphProc.h"
# include "ActiveArea.h"

MAKE_RCSID ("$Id: ActiveArea.c,v 2.7 1999-03-01 02:04:19 burghart Exp $")

/*
 * List creation parameters.
 */
# define AA_ICOLUMN	10	/* Initial column alloc size */
# define AA_IAREA	50	/* Initial number of areas 	*/

/*
 * The real definition of the active area list.
 */
AAList *CurrentAreas = 0;


/*
 * Forwards.
 */
static void aa_AddColumnEntry FP ((AAList *, int, int));




static AAList *
aa_CreateList ()
/*
 * Make a new active entry list.
 */
{
	AAList *list = ALLOC (AAList);
	int col;
/*
 * Just fill everything in.
 */
	list->al_narea = 0;
	list->al_nalloc = AA_IAREA;
	list->al_areas = (ActiveArea *) malloc (AA_IAREA*sizeof (ActiveArea));
	list->al_ncolumn = GWWidth (Graphics)/AA_CWIDTH + 1;
	list->al_columns = (AAColumn *)
		malloc (list->al_ncolumn*sizeof (AAColumn));
/*
 * Initialize all of the columns.
 */
	for (col = 0; col < list->al_ncolumn; col++)
		list->al_columns[col].ac_nentry =
			list->al_columns[col].ac_nalloc = 0;
	return (list);
}




void
aa_AddArea (int x, int y, int w, int h, const char *type, const char *comp, 
	    const char *plat, const char *other, void (*bdown)())
/*
 * Add an area to the active list.
 */
{
	int slot, beginc, endc;
	ActiveArea *area;
/*
 * Start by insuring that we have a list currently.
 */
	if (! CurrentAreas)
		CurrentAreas = aa_CreateList ();
/*
 * Find a spot to add this entry in the master list.
 */
	slot = CurrentAreas->al_narea++;
	if (CurrentAreas->al_narea >= CurrentAreas->al_nalloc)
	{
		(CurrentAreas->al_nalloc)++;
		CurrentAreas->al_areas = (ActiveArea *) realloc (
		        CurrentAreas->al_areas,
			CurrentAreas->al_nalloc*sizeof (ActiveArea));
	}
/*
 * Fill in the entry itself.
 */
	area = CurrentAreas->al_areas + slot;
	area->aa_x = x;
	area->aa_y = y;
	area->aa_width = w;
	area->aa_height = h;
	strcpy (area->aa_type, type);
	strcpy (area->aa_comp, comp);
	strcpy (area->aa_plat, plat);
	strcpy (area->aa_other, other ? other : "");
	area->aa_action = bdown;
/*
 * Now we add entries to each column which this thing overlaps.
 */
	beginc = x/AA_CWIDTH;
	endc = (x + w)/AA_CWIDTH;
	for (; beginc <= endc; beginc++)
		aa_AddColumnEntry (CurrentAreas, beginc, slot);
}




static void
aa_AddColumnEntry (list, col, slot)
AAList *list;
int col, slot;
/*
 * Add a column entry for this slot.
 */
{
	AAColumn *cp = list->al_columns + col;
/*
 * Sometimes we get stuff way out of bounds.
 */
	if (col < 0 || col >  GWWidth (Graphics)/AA_CWIDTH)
		return;
/*
 * Now just remember.
 */
	if (++(cp->ac_nentry) > cp->ac_nalloc)
	{
		if (cp->ac_nalloc > 0)
		{
			cp->ac_nalloc *= 2;
			cp->ac_entries = (short *) realloc (cp->ac_entries,
					cp->ac_nalloc * sizeof (short));
		}
		else
		{
			cp->ac_nalloc = AA_ICOLUMN;
			cp->ac_entries = (short *) malloc (
					cp->ac_nalloc*sizeof (short));
		}
	}
	cp->ac_entries[cp->ac_nentry - 1] = slot;
}




void
aa_ResetAreas ()
/*
 * Reset the area list.
 */
{
	CurrentAreas = 0;	/* Assume free elsewhere */
}




void
aa_ReloadAreas (list)
AAList *list;
/*
 * Reload an old active area list.
 */
{
	CurrentAreas = list;
}




void
aa_FreeList (list)
AAList *list;
/*
 * Free up this list.
 */
{
	int col;
/*
 * Make sure this isn't the current list.
 */
	if (list == CurrentAreas)
		CurrentAreas = 0;
/*
 * Go through and do the columns.
 */
	for (col = 0; col < list->al_ncolumn; col++)
		if (list->al_columns[col].ac_nentry > 0)
			free (list->al_columns[col].ac_entries);
/*
 * Then get the rest.
 */
	free (list->al_columns);
	free (list->al_areas);
	free (list);
}





ActiveArea *
aa_Which (x, y)
int x, y;
/*
 * Find out which area, if any, lies under the given coordinates.
 * Rather than take the first, take the one who's center is closest
 * to the pointer.
 */
{
#	define AA_ABS(a) ((a)<0?(-(a)):(a))
#	define AA_METRIC(ax,ay,px,py) \
	(AA_ABS((px)-(ax))+AA_ABS((py)-(ay)))
	int i;
	AAColumn *cp;
	const ActiveArea *aa;
	const ActiveArea *closest;
	int metric, dist = 0;
	int cx, cy;
		
	if (! CurrentAreas)
		return (NULL);
	cp = CurrentAreas->al_columns + x/AA_CWIDTH;
	if (! cp->ac_nentry)
		return (NULL);
	closest = NULL;
	i = 0;
	do {
		aa = CurrentAreas->al_areas + cp->ac_entries[i];
		if (aa->aa_x <= x && (aa->aa_x + aa->aa_width) >= x &&
		    aa->aa_y <= y && (aa->aa_y + aa->aa_height) >= y)
		{
			cx = aa->aa_x + (aa->aa_width / 2);
			cy = aa->aa_y + (aa->aa_height / 2);
			metric = AA_METRIC(cx,cy,x,y);
			if (! closest || (metric < dist))
			{
				closest = aa;
				dist = metric;
			}
		}
	}
	while (++i < cp->ac_nentry);

	return ((ActiveArea *)closest);		/* Cast to rm const */
}
