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
static char *rcsid = "$Id: GetList.c,v 2.2 1991-11-22 20:50:59 kris Exp $";

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
	static int	dgl_DoList (int, GetList *);
	static int	dgl_Overlaps (GetList *, DataFile *);
	static GetList	*dgl_FixList (GetList *, DataFile *, int *);
	static bool	dgl_TimeProblem (int, DataObject *, int, int *);
	static int	dgl_RequestNewDF (Platform *, char *, time *);
	static int	dgl_GetNDFResp (struct message *,
					struct dsp_R_CreateFile *);
	static void	dgl_AbortNewDF (Platform *, int);
# else
	static GetList	*dgl_GetEntry ();
	static void	dgl_ReturnEntry ();
	static int	dgl_DoList ();
	static int	dgl_Overlaps ();
	static GetList	*dgl_FixList ();
	static bool	dgl_TimeProblem ();
	static int	dgl_RequestNewDF ();
	static int	dgl_GetNDFResp ();
	static void	dgl_AbortNewDF ();
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
	ret->gl_locs = 0;
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
	if (! dgl_DoList (LOCALDATA (*p), list))
		dgl_DoList (REMOTEDATA (*p), list);
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
		return (DLE (gp->gl_begin, dp->df_end));
/*
 * Otherwise overlap iff the data begins before the desired end time.
 */
	else
		return (DLE (dp->df_begin, gp->gl_end));
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
	new->gl_next = gp->gl_next;
	gp->gl_next = new;
	gp->gl_begin = dp->df_begin;
/*
 * Fix up the new entry and work from there.
 */
	new->gl_flags &= ~GLF_SATISFIED;
	new->gl_end = dp->df_begin;		/* - 1? */
	pmu_dsub (&new->gl_end.ds_yymmdd, &new->gl_end.ds_hhmmss, 1);
	return (new);
}






int
dgl_GetDestFile (dobj, new, begin, end)
DataObject *dobj;
bool new;
int begin, *end;
/*
 * Figure out what the next destination file should be.
 * Entry:
 *	DOBJ	is the data object describing the put request.
 *	NEW	is TRUE iff a new file should be created regardless.
 *	BEGIN	is the first sample that we are trying to store.
 * Exit:
 *	If a destination is possible then:
 *		The return value is the data file index of the destination.
 *		END is the index of the last sample which should go to
 *			this file (call again for data beyond END).
 *	else
 *		The return value is negative
 *		END is the index of the last value which can not be output.
 */
{
	Platform *plat = PTable + dobj->do_id;
	DataFile *dp;
	int newdf;
	char fname[240];
/*
 * For now, we enforce a restriction that this data must be later in time
 * than anything we have on disk now.  The reason is that doing the right
 * thing here can be somewhat complicated, involving overwriting some 
 * existing data, or moving data to put something in the middle, when there
 * is existing data after this stuff.
 */
	if (LOCALDATA (*plat) &&
			dgl_TimeProblem (LOCALDATA (*plat), dobj, begin, end))
		return (-1);
/*
 * Now we see if we can use the existing, most recent file.  Essentially, 
 * the conditions for that are: (1) the file exists, (2) we aren't being told
 * to create a new file, (3) the file is not full, and (4) there is not a 
 * huge gap in time between the end of the last file and our current time.
 * (4) is implemented for now by just checking that they refer to the same
 * day.  Something smarter is desirable.
 */
	dp = DFTable + LOCALDATA (*plat);
	if (! new && LOCALDATA (*plat) && dp->df_nsample < plat->dp_maxsamp &&
		dp->df_end.ds_yymmdd == dobj->do_times[begin].ds_yymmdd)
	{
	/*
	 * We can use it.  Calculate how many samples can go there.
	 */
		if ((*end = (begin + plat->dp_maxsamp - dp->df_nsample)) >=
				dobj->do_npoint)
			*end = dobj->do_npoint - 1;
		return (LOCALDATA (*plat));
	}
/*
 * OK, we we have to do here is to create an entirely new file.  We start that 
 * process by requesting a new data file entry from the daemon.
 */
	dfa_MakeFileName (plat, &dobj->do_times[begin], fname);
	if ((newdf = dgl_RequestNewDF (plat,fname,&dobj->do_times[begin])) < 0)
	{
		*end = dobj->do_npoint;		/* Everything fails */
		return (-1);
	}
/*
 * Have DFA get the file made for us.  They use the data object to know which
 * fields/platforms belong therein.  A bit kludgy, but it works.
 */
	if (! dfa_CreateFile (newdf, dobj))
	{
		dgl_AbortNewDF (plat, newdf);
		*end = dobj->do_npoint;
		return (-1);
	}
/*
 * Now see how much of this data we are able to put into it.
 */
	if ((*end = (begin + plat->dp_maxsamp)) >= dobj->do_npoint)
		*end = dobj->do_npoint - 1;
	return (newdf);
}







static bool
dgl_TimeProblem (dfile, dobj, begin, end)
int dfile;
DataObject *dobj;
int begin, *end;
/*
 * Perform time sanity checking.
 */
{
	DataFile *dp = DFTable + dfile;
/*
 * If our first sample is past the current end time, there is no problem.
 */
	if (DLT (dp->df_end, dobj->do_times[begin]))
		return (FALSE);
	msg_ELog (EF_DEBUG, "Time problem, dobj %d %06d, df %d %06d",
		dobj->do_times[begin].ds_yymmdd,
		dobj->do_times[begin].ds_hhmmss,
		dp->df_end.ds_yymmdd, dp->df_end.ds_hhmmss);
/*
 * It appears that there is a problem.  See now if there is any hope at all
 * of salvaging part of the data.
 */
	if (DLE (dobj->do_times[dobj->do_npoint - 1], dp->df_end))
	{
		*end = dobj->do_npoint;
		return (TRUE);
	}
/*
 * It seems that there is some overlap.  Find out where we can start
 * storing data.
 */
	for (*end = begin + 1; *end < dobj->do_npoint; *end++)
		if (DLT (dp->df_end, dobj->do_times[*end]))
			break;
	(*end)--;
	return (TRUE);
}





static int
dgl_RequestNewDF (plat, file, t)
Platform *plat;
char *file;
time *t;
/*
 * Get a new datafile entry from the DS daemon for this new file.
 * Entry:
 *	PLAT	is the platform for which this file is being created.
 *	FILE	is the name of the file.
 *	T	is the expected begin time of the data to put into the file.
 * Exit:
 *	On success, the return value is the new DF entry.  Otherwise a
 *	negative value is returned.
 */
{
	struct dsp_CreateFile dspcf;
	struct dsp_R_CreateFile dspresp;
/*
 * Put together the request for the daemon.
 */
	dspcf.dsp_type = dpt_NewFileRequest;
	dspcf.dsp_plat = plat - PTable;
	dspcf.dsp_time = *t;
	strcpy (dspcf.dsp_file, file);
/*
 * Ship it off, and pick out our response.
 */
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &dspcf, sizeof (dspcf));
	msg_Search (MT_DATASTORE, dgl_GetNDFResp, &dspresp);
	return ((dspresp.dsp_type == dpt_R_NewFileSuccess) ?
			dspresp.dsp_FileIndex : -1);
}




static int
dgl_GetNDFResp (msg, dspresp)
struct message *msg;
struct dsp_R_CreateFile *dspresp;
/*
 * Pick out our response to the new file create request.
 */
{
	struct dsp_Template *t = (struct dsp_Template *) msg->m_data;

	if (t->dsp_type == dpt_R_NewFileSuccess ||
			t->dsp_type == dpt_R_NewFileFailure)
	{
		*dspresp = * (struct dsp_R_CreateFile *) t;
		return (0);
	}
	return (1);
}




static void
dgl_AbortNewDF (plat, df)
Platform *plat;
int df;
/*
 * Abort this DF create, for some reason.
 */
{
	struct dsp_AbortNewFile abort;

	abort.dsp_type = dpt_AbortNewFile;
	abort.dsp_FileIndex = df;
	abort.dsp_pid = plat - PTable;
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &abort, sizeof (abort));
}
