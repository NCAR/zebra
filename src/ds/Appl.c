/*
 * The data store application interface.
 */
static char *rcsid = "$Id: Appl.c,v 1.2 1991-01-16 22:06:46 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"


/*
 * Local stuff.
 */
# ifdef __STDC__
	static void ds_InitPFTable (void);
	static void ds_AllocMemory (DataObject *, GetList *);
# else
	static void ds_InitPFTable ();
	static void ds_AllocMemory ();
# endif


int
ds_Initialize ()
/*
 * Hook into the data store.
 */
{
/*
 * Hook into the shared memory segment.
 */
	if (! dsm_Init ())
		return (FALSE);
/*
 * Set up the platform lookup table.
 */
	ds_InitPFTable ();

	return (TRUE);
}





static void
ds_InitPFTable ()
/*
 * Create the platform lookup table.
 */
{
	int i;
	char *slash, *strchr ();
	SValue v;
/*
 * Create the table itself.
 */
	Pf_Names = usy_c_stbl ("Platform_names");
/*
 * Just go through the platform list and make all the entries.
 */
	dsm_ShmLock ();
	for (i = 0; i < SHeader->sm_nPlatform; i++)
	{
		v.us_v_int = i;
		usy_s_symbol (Pf_Names, PTable[i].dp_name, SYMT_INT, &v);
		slash = strchr (PTable[i].dp_name, '/');
		while (slash)
		{
			usy_s_symbol (Pf_Names, slash + 1, SYMT_INT, &v);
			slash = strchr (slash + 1, '/');
		}
	}
	dsm_ShmUnlock ();
}






PlatformId
ds_LookupPlatform (name)
char *name;
/*
 * Find this platform.
 */
{
	int type;
	SValue v;

	if (! usy_g_symbol (Pf_Names, name, &type, &v))
		return (BadPlatform);
	return (v.us_v_int);
}





char *
ds_PlatformName (id)
PlatformId id;
/*
 * Get back the name for this platform.
 */
{
	return (PTable[id].dp_name);
}



int
ds_IsMobile (id)
PlatformId id;
/*
 * Return TRUE iff this is a mobile platform.
 */
{
	return (PTable[id].dp_flags & DPF_MOBILE);
}






DataObject *
ds_GetData (pid, fields, nfield, begin, end, org, sel, badflag)
PlatformId pid;
char **fields;
int nfield;
time *begin, *end;
DataOrganization org;
float sel, badflag;
/*
 * Get the requested data.
 */
{
	int i;
	DataObject *dobj = ALLOC (DataObject);
	GetList *get, *gp;
# ifdef notdef
	msg_ELog (EF_INFO, "Get data (%s) at %d %06d", fields[0],
		begin->ds_yymmdd, begin->ds_hhmmss);
# endif
/*
 * Start to fill things in.
 */
/*	dobj->do_org = PTable[pid].dp_org; */
	if (org == Org2dGrid && PTable[pid].dp_org == OrgIRGrid)
		dobj->do_org = OrgIRGrid;
	else
		dobj->do_org = org;
	dobj->do_id = pid;
	dobj->do_begin = *begin;
	dobj->do_end = *end;
	dobj->do_flags = 0;
	dobj->do_loc.l_alt = sel;	/* XXX */
	dobj->do_badval = badflag;
	dobj->do_npoint = 0;
/*
 * Move the field names over too.
 */
	dobj->do_nfield = nfield;
	for (i = 0; i < nfield; i++)
		dobj->do_fields[i] = usy_string (fields[i]);
/*
 * Make the get list.
 */
	if (! (get = dgl_MakeGetList (dobj)))
	{
		msg_ELog (EF_INFO, "GetList get failure");
		ds_FreeDataObject (dobj);	/* Complete failure	*/
		return (NULL);
	}
/*
 * Do the first pass over each list, initializing the DFA modules and getting
 * the point count.
 */
	for (gp = get; gp; gp = gp->gl_next)
		dfa_Setup (gp);
# ifdef notdef
	for (gp = get; gp; gp = gp->gl_next)
		msg_ELog (EF_DEBUG, 
			"GL dfi %d/%d, flags 0x%x, %d %06d -> %06d np %d\n",
			gp->gl_dfindex, gp->gl_dfuse, gp->gl_flags,
			gp->gl_begin.ds_yymmdd, gp->gl_begin.ds_hhmmss,
			gp->gl_end.ds_hhmmss, gp->gl_npoint);
# endif
/*
 * Allocate memory.
 */
	ds_AllocMemory (dobj, get);
/*
 * Now we pass through and actually get the data.
 */
	for (gp = get; gp; gp = gp->gl_next)
		dfa_GetData (gp);
/*
 * Free the get list.
 */
	dgl_ReturnList (get);
	return (dobj);
}





void
ds_FreeDataObject (dobj)
DataObject *dobj;
/*
 * Return this thing to the system.
 */
{
	int i;

	for (i = 0; i < dobj->do_nfield; i++)
		usy_rel_string (dobj->do_fields[i]);
	if (dobj->do_flags & DOF_FREEALLDATA)
	{
		for (i = 0; i < dobj->do_nfield; i++)
			free (dobj->do_data[i]);
	}
	else if (dobj->do_flags & DOF_FREEDATA)
		free (dobj->do_data[0]);
	free ((char *) dobj);
}




static void
ds_AllocMemory (dobj, get)
DataObject *dobj;
GetList *get;
/*
 * Allocate the memory needed to satisfy this data request.
 */
{
	int nsample, npoint, field, offset, toffset;
	GetList *gp;
	RGrid *rp;
/*
 * Pass through the list and get the total number of data points.
 */
	npoint = nsample = 0;
	for (gp = get; gp; gp = gp->gl_next)
	{
		npoint += gp->gl_npoint;
		nsample += gp->gl_nsample;
	}
/*
 * Get the memory in a big chunk.
 */
	dobj->do_data[0] = (float *)
			malloc (npoint*dobj->do_nfield*sizeof (float));
	dobj->do_times = (time *) malloc (nsample * sizeof (time));
	dobj->do_flags |= DOF_FREEDATA | DOF_FREETIME;
	dobj->do_npoint = nsample;
/*
 * Set the pointers for each field.
 */
	for (field = 1; field < dobj->do_nfield; field++)
		dobj->do_data[field] = dobj->do_data[0] + npoint*field;
/*
 * If this is a mobile platform, we need to arrange for location info.
 */
	if (ds_IsMobile (dobj->do_id))
	{
		dobj->do_aloc = (Location *)
				malloc (npoint*sizeof (Location));
		dobj->do_flags |= DOF_FREEALOC;
	}
/*
 * Fix up the getlist pointers.
 */
	offset = npoint;
	toffset = nsample;
	for (gp = get; gp; gp = gp->gl_next)
	{
		offset -= gp->gl_npoint;
		toffset -= gp->gl_nsample;
		for (field = 0; field < dobj->do_nfield; field++)
			gp->gl_data[field] = dobj->do_data[field] + offset;
		gp->gl_time = dobj->do_times + toffset;
		if (ds_IsMobile (dobj->do_id))
			gp->gl_locs = dobj->do_aloc + toffset;
	}
	if (offset != 0 || toffset != 0)
		msg_ELog (EF_PROBLEM, "BUG: Offsets nonzero: %d %d", offset,
				toffset);
/*
 * Do the organization-specific stuff.
 */
	switch (dobj->do_org)
	{
	   case OrgIRGrid:
	   	dobj->do_desc.d_irgrid.ir_npoint =
			dfa_InqNPlat (get->gl_dfindex);
		dobj->do_desc.d_irgrid.ir_loc = (Location *) malloc (
			dobj->do_desc.d_irgrid.ir_npoint*sizeof (Location));
		break;
	   case Org2dGrid:
	   case Org3dGrid:
		dfa_InqRGrid (get->gl_dfindex, &dobj->do_loc,
						&dobj->do_desc.d_rgrid);
		if (dobj->do_org == Org2dGrid)
			dobj->do_desc.d_rgrid.rg_nZ = 1;
		break;
	   case OrgScalar:
		break;
	}
}






bool
ds_GetRgridParams (pid, when, loc, rg)
PlatformId pid;
time *when;
Location *loc;
RGrid *rg;
/*
 * Get the rgrid params for this date.
 */
{
	Platform *p = PTable + pid;
	int dfindex;
/*
 * Make sure this makes sense.
 */
	if (p->dp_org != Org2dGrid && p->dp_org != Org3dGrid)
		return (FALSE);
/*
 * Now find a datafile entry we can use.
 */
	if ((dfindex = ds_FindDF (pid, when)) < 0)
		return (FALSE);
/*
 * Get the rest from the format-specific code.
 */
	loc->l_alt = 0;
	return (dfa_InqRGrid (dfindex, loc, rg));
}





int
ds_FindDF (pid, when)
PlatformId pid;
time *when;
/*
 * Find the first datafile entry before this time.
 */
{
	int ret = LOCALDATA (PTable[pid]);

	for (; ret; ret = DFTable[ret].df_FLink)
		if (DLE (DFTable[ret].df_begin, *when))
			return (ret);
	return (-1);
}




int
ds_DataTimes (platform, when, n, which, rettimes)
PlatformId platform;
time *when, *rettimes;
int n;
TimeSpec which;
/*
 * Return a list of up to "n" times related to "time" by the given
 * spec.
 */
{
	int ndone = 0, index;
/*
 * We don't do it all yet.
 */
	if (which != DsBefore)
	{
		msg_ELog (EF_PROBLEM, "Only DsBefore TimeSpec handled");
		return (0);
	}
/*
 * Scan down the datafile list until we find the first entry which
 * begins before the given time.
 */
	for (index = LOCALDATA (PTable[platform]); index; 
			index = DFTable[index].df_FLink)
		if (DLE (DFTable[index].df_begin, *when))
			break;
/*
 * Now we plow through datafile entries until we have all we want.
 */
	while (index && ndone < n)
	{
		ndone += dfa_DataTimes (index, when, which, n - ndone,
				rettimes + ndone);
		index = DFTable[index].df_FLink;
	}
	return (ndone);
}







DataObject *
ds_GetObservation (pid, fields, nfield, when, org, sel, badflag)
PlatformId pid;
char **fields;
int nfield;
time *when;
DataOrganization org;
float sel, badflag;
/*
 * Get an entire data observation.
 */
{
	int i;
	DataObject *dobj = ALLOC (DataObject);
	GetList *get;

	msg_ELog (EF_INFO, "Get obs (%s) at %d %06d", fields[0],
		when->ds_yymmdd, when->ds_hhmmss);
/*
 * Start to fill things in.
 */
	if (org == Org2dGrid && PTable[pid].dp_org == OrgIRGrid)
		dobj->do_org = OrgIRGrid;
	else
		dobj->do_org = org;
	dobj->do_id = pid;
	dobj->do_begin = *when;
	dobj->do_end = *when;
	dobj->do_flags = 0;
	dobj->do_loc.l_alt = sel;	/* XXX */
	dobj->do_badval = badflag;
	dobj->do_npoint = 0;
/*
 * Move the field names over too.
 */
	dobj->do_nfield = nfield;
	for (i = 0; i < nfield; i++)
		dobj->do_fields[i] = usy_string (fields[i]);
/*
 * Make the get list.
 */
	if (! (get = dgl_MakeGetList (dobj)))
	{
		msg_ELog (EF_INFO, "GetList get failure");
		ds_FreeDataObject (dobj);	/* Complete failure	*/
		return (NULL);
	}
/*
 * We should have a single-element get list.  Modify the times to cover the
 * entire observation of interest.
 */
	if (get->gl_next)
		msg_ELog (EF_PROBLEM, "GetObservation multiple get list!");
	get->gl_begin = DFTable[get->gl_dfindex].df_begin;
	get->gl_end = DFTable[get->gl_dfindex].df_end;
/*
 * Do the first pass over each list, initializing the DFA modules and getting
 * the point count.
 */
	dfa_Setup (get);
	ui_printf ("GL dfi %d/%d, flags 0x%x, %d %06d -> %06d np %d\n",
			get->gl_dfindex, get->gl_dfuse, get->gl_flags,
			get->gl_begin.ds_yymmdd, get->gl_begin.ds_hhmmss,
			get->gl_end.ds_hhmmss, get->gl_npoint);
/*
 * Allocate memory, then get the data.
 */
	ds_AllocMemory (dobj, get);
	dfa_GetData (get);
/*
 * Free the get list.
 */
	dgl_ReturnList (get);
	return (dobj);
}
