/*
 * Figure out how to do the data access.
 */
static char *rcsid = "$Id: GetList.c,v 1.1 1990-11-02 08:56:19 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"


/*
 * Getlist lookaside list.
 */
GetList *GList = 0;


# ifdef __STDC__
	static GetList	*dgl_GetEntry (void);
	static void	dgl_ReturnEntry (GetList *);
	static void	dgl_ReturnList (GetList *);
	static int	dgl_DoList (int, GetList *);
	static int	dgl_Overlaps (GetList *, DataFile *);
	static GetList	*dgl_FixList (GetList *, DataFile *, int *);
# else
	static GetList	*dgl_GetEntry ();
	static void	dgl_ReturnEntry ();
	static void	dgl_ReturnList ();
	static int	dgl_DoList ();
	static int	dgl_Overlaps ();
	static GetList	*dgl_FixList ();
# endif




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




static void
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
dgl_MakeGetList (dobj)
DataObject *dobj;
/*
 * Figure out how to satisfy this data object.
 */
{
	GetList *list, *l, *zap;
	Platform *p = PTable + dobj->do_id;
/*
 * Make an initial, unsatisfied entry.
 */
	list = dgl_GetEntry ();
	list->gl_begin = dobj->do_begin;
	list->gl_end = dobj->do_end;
	list->gl_flags = 0;
	list->gl_dobj = dobj;
	list->gl_next = 0;
/*
 * Now try to satisfy it against the platform lists.
 */
	dsm_ShmLock ();
	if (! dgl_DoList (p->dp_LocalData, list))
		/* dgl_DoList (p->dp_remote, list) */ ;
	dsm_ShmUnlock ();
/*
 * Remove any unsatisfied segments, and return the rest.
 */
	while (list && ! (list->gl_flags & GLF_SATISFIED))
	{
		zap = list;
		list = list->gl_next;
		dgl_ReturnEntry (zap);
	}
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

	while (list)
	{
	/*
	 * Find the first unsatisfied getlist entry.  If there are no
	 * more, we're done.
	 */
		for (; list; list = list->gl_next)
			if (! (list->gl_flags & GLF_SATISFIED))
				break;
		if (! list)
			return (ret);
	/*
	 * Move forward through the data list until we find an entry which
	 * overlaps with this getlist entry.
	 * MAKE THIS SMARTER!
	 */
	 	for (dp = data; dp; dp = DFTable[dp].df_FLink)
			if (dgl_Overlaps (list, DFTable + dp))
				break;
	/*
	 * If we found nothing, this entry can not be satisfied, so we bail.
	 */
	 	if (! dp)
		{
			list = list->gl_next;
			ret = FALSE;
		}
	/*
	 * We found something.  Rework the list as appropriate.
	 */
	 	else
			list = dgl_FixList (list, DFTable + dp, &ret);
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
	if  (DLT (dp->df_begin, gp->gl_begin))
		return (DLT (gp->gl_begin, dp->df_end));
/*
 * Otherwise overlap iff the data begins before the desired end time.
 */
	else
		return (DLT (dp->df_begin, gp->gl_end));
}





static GetList *
dgl_FixList (gp, dp, complete)
GetList *gp;
DataFile *dp;
int *complete;
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
	if (DLT (dp->df_end, gp->gl_end))
	{
		*complete = FALSE;
	/*
	 * Get a new entry and splice it into the list.
	 */
		new = ALLOC (GetList);
		*new = *gp;
		gp->gl_next = new;
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
	gp->gl_dfindex = dp - DFTable;
	gp->gl_dfuse = dp->df_use;
	gp->gl_flags |= GLF_SATISFIED;
	if (DLE (dp->df_begin, gp->gl_begin))
		return (gp);
/*
 * Otherwise we have to split again, and leave an unsatisfied chunk for
 * the next iteration.
 */
	new = ALLOC (GetList);
	*new = *gp;
	gp->gl_next = new;
	gp->gl_begin = dp->df_begin;
/*
 * Fix up the new entry and work from there.
 */
	new->gl_flags &= ~GLF_SATISFIED;
	new->gl_end = dp->df_begin;		/* - 1? */
	return (new);
}
