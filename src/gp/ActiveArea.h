/*
 * Definitions of active area data structures.
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

/* $Id: ActiveArea.h,v 2.1 1993-10-14 20:21:38 corbet Exp $ */

# define AA_PSIZE 80

typedef struct s_ActiveArea
{
	int aa_x;		/* Origin of the area	*/
	int aa_y;
	int aa_width;		/* Size of the area	*/
	int aa_height;
	char aa_comp[AA_PSIZE];	/* Relevant component		*/
	char aa_plat[AA_PSIZE];	/* The platform of interest	*/
	char aa_other[AA_PSIZE];/* Other info			*/
	void (*aa_action)();	/* Action function		*/
} ActiveArea;

/*
 * The list structure used to hold a bunch of active areas.  This thing
 * is oriented around quick lookup.  To this end, the display window is
 * split into relatively narrow columns, and a list of areas that overlap
 * each column is kept.  When somebody wants to know what area lies under
 * a given point, only the column containing that point needs to be
 * searched, which should mean that only a few actual areas need to be
 * examined.  Let's hope.
 */

typedef struct _AAColumn
{
	short ac_nentry;	/* How many entries in this column */
	short ac_nalloc;	/* How many entries allocated	*/
	short *ac_entries;	/* The actual entries		*/
} AAColumn;


# define AA_CWIDTH 20		/* Width of column, in pixels	*/

typedef struct _AAList
{
	int	al_narea;		/* How many do we have?	*/
	int	al_nalloc;		/* How many allocated	*/
	ActiveArea *al_areas;		/* The actual areas	*/
	int	al_ncolumn;		/* How many columns	*/
	AAColumn *al_columns;		/* Column info		*/
} AAList;


/*
 * The list of active areas for the current frame.
 */
extern AAList *CurrentAreas;


/*
 * Functions.
 */
extern void aa_AddArea FP ((int, int, int, int, char *, char *, char *,
			    void (*) ()));
extern void aa_ResetAreas FP ((void));
extern void aa_ReloadAreas FP ((AAList *));
extern ActiveArea *aa_Which FP ((int, int));
extern void aa_FreeList FP ((AAList *));
