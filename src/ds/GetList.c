/*
 * Figure out how to do the data access.
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

# include "defs.h"
# include "message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"

RCSID("$Id: GetList.c,v 3.9 1995-02-10 01:10:45 granger Exp $")

/*
 * Getlist lookaside list.
 */
GetList *GList = 0;


static GetList	*dgl_GetEntry FP ((void));
static void	dgl_ReturnEntry FP ((GetList *));
static int	dgl_DoList FP ((int, GetList *));
static int	dgl_Overlaps FP ((GetList *, DataFile *));
static GetList	*dgl_FixList FP ((GetList *, int, DataFile *, int *));




static GetList *
dgl_GetEntry ()
/*
 * Get a free getlist entry.
 */
{
	GetList *ret;

	if (GList)
	{
		ret = GList;
		GList = ret->gl_next;
	}
	else
		ret = ALLOC (GetList);

	ret->gl_flags = 0;
	ret->gl_npoint = 0;
	ret->gl_nsample = 0;
	ret->gl_sindex = 0;
	return (ret);
}



static void
dgl_ReturnEntry (gl)
GetList *gl;
/*
 * Return this getlist entry.
 */
{
	gl->gl_next = GList;
	GList = gl;
}



void 
dgl_ForceClosure()
/*
 * Release our free chain
 */
{
	GetList *gl, *next;

	gl = GList;
	while (gl)
	{
		next = gl->gl_next;
		free (gl);
		gl = next;
	}
	GList = NULL;
}



void
dgl_ReturnList (gl)
GetList *gl;
/* 
 * Return this entire list.
 */
{
	GetList *zap;
	while (gl)
	{
		zap = gl;
		gl = gl->gl_next;
		dgl_ReturnEntry (zap);
	}
}



GetList *
dgl_MakeGetList (pid, begin, end)
PlatformId pid;
ZebTime *begin, *end;
/*
 * Figure out how to satisfy this data object.
 */
{
	GetList *list, *l, *zap;
	ClientPlatform p;
/*
 * Make an initial, unsatisfied entry.
 */
	list = dgl_GetEntry ();
	list->gl_begin = *begin;
	list->gl_end = *end;
	list->gl_next = NULL;
	list->gl_flags = 0;
/*
 * Now try to satisfy it against the platform lists.
 */
	ds_LockPlatform (pid);
	ds_GetPlatStruct (pid, &p, TRUE);
	if (! dgl_DoList (ds_FindDF (pid, end, 0), list))
		dgl_DoList (ds_FindDF (pid, end, 1), list);
	ds_UnlockPlatform (pid);
/*
 * Remove any unsatisfied elements at the beginning of the list.
 */
	while (list && ! (list->gl_flags & GLF_SATISFIED))
	{
		zap = list;
		list = list->gl_next;
		dgl_ReturnEntry (zap);
	}
/*
 * Now wander into the middle of the list and look for unsatisfied chunks
 * therein.
 */
	for (l = zap = list; zap; zap = l->gl_next)
	{
		if (! (zap->gl_flags & GLF_SATISFIED))
		{
			l->gl_next = zap->gl_next;
			dgl_ReturnEntry (zap);
		}
		else
			l = zap;
	}
/*
 * Return what's left, if anything.
 */
	return (list);
}





static int
dgl_DoList (data, list)
int data;
GetList *list;
/*
 * Try to satisfy this getlist against the given data file list.
 */
{
	int ret = TRUE, dp;
	DataFile dfe;
	GetList *lp;
/*
 * Mark all list entries as not having been tried.
 */
	if (data < 0)
		return (FALSE);
	for (lp = list; lp; lp = lp->gl_next)
		lp->gl_flags &= ~GLF_TRIED;
/*
 * Now pass through the list and try to satisfy things.
 */
	while (list)
	{
	/*
	 * Find the first unsatisfied, untried getlist entry.  If there are no
	 * more, we're done.
	 */
		for (; list; list = list->gl_next)
			if (! (list->gl_flags & (GLF_SATISFIED | GLF_TRIED)))
				break;
		if (! list)
			return (ret);
	/*
	 * Move forward through the data list until we find an entry which
	 * overlaps with this getlist entry.
	 * MAKE THIS SMARTER!
	 * (Smarter means not passing over the files that we have already
	 *  been over N times satisfying earlier getlist entries.)
	 */
	 	for (dp = data; dp; dp = dfe.df_FLink)
		{
			ds_GetFileStruct (dp, &dfe);
			if (dgl_Overlaps (list, &dfe))
			{
				data = dp;  /* No point looking further back*/
				break;
			}
			if (TC_Less (dfe.df_end, list->gl_begin))
			{
				dp = 0;	/* Never will find it here */
				break;
			}
		}
	/*
	 * If we found nothing, this entry can not be satisfied, so we bail.
	 */
	 	if (! dp)
		{
			list->gl_flags |= GLF_TRIED;
			list = list->gl_next;
			ret = FALSE;
		}
	/*
	 * We found something.  Rework the list as appropriate.
	 */
	 	else
			list = dgl_FixList (list, dp, &dfe, &ret);
	}
	return (ret);
}





static int 
dgl_Overlaps (gp, dp)
GetList *gp;
DataFile *dp;
/*
 * See if these two entries overlap in time.
 */
{
/*
 * If the data begins before our desired time, then we have an overlap iff
 * it ends after that time.
 */
	if  (TC_Less (dp->df_begin, gp->gl_begin))
		return (TC_LessEq (gp->gl_begin, dp->df_end));
/*
 * Otherwise overlap iff the data begins before the desired end time.
 */
	else
		return (TC_LessEq (dp->df_begin, gp->gl_end));
}





static GetList *
dgl_FixList (gp, dfindex, dp, complete)
GetList *gp;
DataFile *dp;
int *complete, dfindex;
/*
 * Fix up the getlist to reflect what this datafile can do for us.  The
 * current entry is split if necessary.  COMPLETE is set FALSE iff there
 * is unsatisfied data at the front end of things.
 */
{
	GetList *new;
/*
 * If the data ends before our request, we have to split the request.
 */
	if (TC_Less (dp->df_end, gp->gl_end))
	{
		*complete = FALSE;
	/*
	 * Get a new entry and splice it into the list.
	 */
		new = dgl_GetEntry ();
		*new = *gp;
		gp->gl_next = new;
		gp->gl_flags |= GLF_TRIED;	/* No point in trying again */
	/*
	 * Now fix up the times and move past the unsatisfiable piece.
	 */
	 	gp->gl_begin = dp->df_end;	/* + 1? */
		new->gl_end = dp->df_end;
		gp = new;
	}
/*
 * At this point, we know we can satisfy the current entry to the end.
 * Mark it accordingly.  If we can go all the way back to the beginning, 
 * we're done.
 */
	gp->gl_dfindex = dfindex;
#ifdef DF_USE
	gp->gl_dfuse = dp->df_use;
#endif
	gp->gl_flags |= GLF_SATISFIED;
	if (TC_LessEq (dp->df_begin, gp->gl_begin))
		return (gp);
/*
 * Otherwise we have to split again, and leave an unsatisfied chunk for
 * the next iteration.
 */
	new = ALLOC (GetList);
	*new = *gp;
	new->gl_next = gp->gl_next;
	gp->gl_next = new;
	gp->gl_begin = dp->df_begin;
/*
 * Fix up the new entry and work from there.
 */
	new->gl_flags &= ~GLF_SATISFIED;
	new->gl_end = dp->df_begin;		/* - 1? */
	/* pmu_dsub (&new->gl_end.ds_yymmdd, &new->gl_end.ds_hhmmss, 1); */
	TC_SysToZt (TC_ZtToSys (&new->gl_end) - 1, &new->gl_end);
	return (new);
}
