/*
 * The data store application interface.
 */
static char *rcsid = "$Id: Appl.c,v 1.5 1991-03-08 18:45:13 corbet Exp $";

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
	static void ds_NotifyDaemon (int, DataObject *, int, int);
	static void ds_DispatchNotify (struct dsp_Notify *);
	int ds_DSMessage (struct message *);
	static void dfa_FixBoundary (DataObject *);
# else
	static void ds_InitPFTable ();
	static void ds_AllocMemory ();
	static void ds_NotifyDaemon ();
	static void ds_DispatchNotify ();
	int ds_DSMessage ();
	static void dfa_FixBoundary ();
# endif


/*
 * The application notification table.
 */
# define MAXPLAT 128
typedef void (*VFunc) ();
VFunc ApplFuncs[MAXPLAT];



int
ds_Initialize ()
/*
 * Hook into the data store.
 */
{
	int i;
/*
 * Hook into the shared memory segment.
 */
	if (! dsm_Init ())
		return (FALSE);
/*
 * Set up the platform lookup table.
 */
	ds_InitPFTable ();
	for (i = 0; i < MAXPLAT; i++)
		ApplFuncs[i] = 0;
/*
 * Join the data store group.
 */
	msg_join ("DataStore");
	msg_AddProtoHandler (MT_DATASTORE, ds_DSMessage);

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
 * Free the get list.  If this is a boundary, we have to fix up a couple
 * of things.  Then return the data object.
 */
	dgl_ReturnList (get);
	if (dobj->do_org == OrgOutline)
		dfa_FixBoundary (dobj);
	return (dobj);
}






static void
dfa_FixBoundary (dobj)
DataObject *dobj;
/*
 * Fix up this boundary to meet the external spec.  
 */
{
# define TIMEEQ(a,b) (memcmp ((a), (b), sizeof (time)) == 0)
	int nbnd = 1, samp, bnd, npt = 0, *ip;
/*
 * Check for the simple (usual) case.
 */
	if (TIMEEQ (&dobj->do_begin, &dobj->do_end))
	{
		dobj->do_desc.d_length = ALLOC (int);
		*dobj->do_desc.d_length = dobj->do_npoint;
		dobj->do_npoint = 1;
		return;
	}
/*
 * Not so easy.  Go through and figure out how many boundaries are really
 * here.
 */
	for (samp = 1; samp < dobj->do_npoint; samp++)
		if (! TIMEEQ (dobj->do_times + samp, dobj->do_times + samp -1))
			nbnd++;
/*
 * Allocate space, then go through and assign the lengths of each boundary.
 */
	ip = dobj->do_desc.d_length = (int *) malloc (nbnd * sizeof (int));
	bnd = 0;
	ip[bnd] = 0;
	for (samp = 1; samp < dobj->do_npoint; samp++)
	{
		ip[bnd]++;
		if (! TIMEEQ (dobj->do_times + samp, dobj->do_times + samp -1))
		{
			bnd++;
			ip[bnd] = 0;
		}
	}
	ip[bnd]++;
}
				


void
ds_FreeDataObject (dobj)
DataObject *dobj;
/*
 * Return this thing to the system.
 */
{
	int i;
/*
 * Get rid of the field names.
 */
	for (i = 0; i < dobj->do_nfield; i++)
		usy_rel_string (dobj->do_fields[i]);
/*
 * Data arrays.
 */
	if (dobj->do_flags & DOF_FREEALLDATA)
	{
		for (i = 0; i < dobj->do_nfield; i++)
			free (dobj->do_data[i]);
	}
	else if (dobj->do_flags & DOF_FREEDATA && dobj->do_nfield > 0)
		free (dobj->do_data[0]);
/*
 * Locations for mobile platforms.
 */
	if (dobj->do_flags & DOF_FREEALOC)
		free (dobj->do_aloc);
/*
 * Times.
 */
	if (dobj->do_flags & DOF_FREETIME)
		free (dobj->do_times);
/*
 * Irregular grids have some additional stuff to get rid of.
 */
	if (dobj->do_org == OrgIRGrid)
	{
		free (dobj->do_desc.d_irgrid.ir_loc);
		if (dobj->do_desc.d_irgrid.ir_subplats)
			free (dobj->do_desc.d_irgrid.ir_subplats);
	}
	else if (dobj->do_org == OrgOutline)
		free (dobj->do_desc.d_length);
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
	IRGrid *irg;
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
	if (dobj->do_nfield > 0)
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
	if (ds_IsMobile (dobj->do_id) || dobj->do_org == OrgOutline)
	{
		dobj->do_aloc = (Location *)
				malloc (nsample*sizeof (Location));
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
		if (ds_IsMobile (dobj->do_id) || dobj->do_org == OrgOutline)
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
	   	irg = &dobj->do_desc.d_irgrid;
	   	irg->ir_npoint = dfa_InqNPlat (get->gl_dfindex);
		irg->ir_loc = (Location *)
			malloc (irg->ir_npoint*sizeof (Location));
		irg->ir_subplats = (PlatformId *)
			malloc (irg->ir_npoint*sizeof (PlatformId));
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

# ifdef notdef
	msg_ELog (EF_INFO, "Get obs (%s) at %d %06d", fields[0],
		when->ds_yymmdd, when->ds_hhmmss);
# endif
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
		msg_ELog (EF_DEBUG, "GetList get failure");
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
# ifdef notdef
	ui_printf ("GL dfi %d/%d, flags 0x%x, %d %06d -> %06d np %d\n",
			get->gl_dfindex, get->gl_dfuse, get->gl_flags,
			get->gl_begin.ds_yymmdd, get->gl_begin.ds_hhmmss,
			get->gl_end.ds_hhmmss, get->gl_npoint);
# endif
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






void
ds_PutData (dobj, newfile)
DataObject *dobj;
bool newfile;
/*
 * Add this data to the data store.
 * Entry:
 *	DOBJ	is the data object describing the data to be added.
 *	NEWFILE	is TRUE iff a new file is to be created to hold this data.
 * Exit:
 *	The data has been added.
 */
{
	int dfile, begin = 0, end;
/*
 * Split this data object into chunks, in case we exceed file size limits.
 */
	while (begin < dobj->do_npoint)
	{
	/*
	 * Find the destination file for this data, and put it there.  Let
	 * the daemon know that we have done it.
	 */
	 	if ((dfile = dgl_GetDestFile(dobj, newfile, begin, &end)) >= 0)
		{
			dfa_PutData (dfile, dobj, begin, end);
			ds_NotifyDaemon (dfile, dobj, begin, end);
		}
		begin = end + 1;
	}
}






static void
ds_NotifyDaemon (dfile, dobj, begin, end)
int dfile, begin, end;
DataObject *dobj;
/*
 * Tell the data store daemon about this data.
 */
{
	struct dsp_UpdateFile update;
/*
 * Fire off the message.
 */
	update.dsp_type = dpt_UpdateFile;
	update.dsp_FileIndex = dfile;
	update.dsp_EndTime = dobj->do_times[end];
	update.dsp_NSamples = end - begin + 1;
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &update, sizeof (update));
/*
 * Then let DFA know that we've signalled a revision on this file.
 */
	dfa_NoteRevision (dfile);
}






int
ds_DSMessage (msg)
struct message *msg;
/*
 * Deal with data store protocol messages.
 */
{
	struct dsp_Template *dt = (struct dsp_Template *) msg->m_data;

	switch (dt->dsp_type)
	{
	/*
	 * If they've gone and deleted data on us, we have to make sure that
	 * we close the file and forget about it.
	 */
	   case dpt_DataGone:
		dfa_ForceClose (((struct dsp_DataGone *) dt)->dsp_file);
		break;
	/*
	 * An application notification has arrived.  Dispatch it back to
	 * the appl.
	 */
	   case dpt_Notify:
		ds_DispatchNotify ((struct dsp_Notify *) dt);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown DS message type %d",
			dt->dsp_type);
		break;
	}
	return (0);
}





void
ds_DeleteData (platform, leave)
PlatformId platform;
int leave;
/*
 * Zap all data from "platform", leaving only "leave" seconds worth.
 */
{
	struct dsp_DeleteData del;

	del.dsp_type = dpt_DeleteData;
	del.dsp_plat = platform;
	del.dsp_leave = leave;
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &del, sizeof (del));
}






void
ds_RequestNotify (platform, param, func)
PlatformId platform;
int param;
void (*func) ();
/*
 * Request a notification when data is available on this platform.
 */
{
	struct dsp_NotifyRequest req;
/*
 * Fill in our request and send it off.
 */
	req.dsp_type = dpt_NotifyRequest;
	req.dsp_pid = platform;
	req.dsp_param = param;
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &req, sizeof (req));
/*
 * Stash away the function so we can call it when the notifications arrive.
 */
	ApplFuncs[platform] = func;
}




void ds_CancelNotify ()
/*
 * Cancel all data available notifications.
 */
{
	struct dsp_Template req;
	int i;

	req.dsp_type = dpt_CancelNotify;
	msg_send ("DS_Daemon", MT_DATASTORE, FALSE, &req, sizeof (req));
	for (i = 0; i < MAXPLAT; i++)
		ApplFuncs[i] = 0;
}





static void
ds_DispatchNotify (notify)
struct dsp_Notify *notify;
/*
 * An application notification has arrived -- give it back to those
 * who requested it.
 */
{
	if (ApplFuncs[notify->dsp_pid])
		(*ApplFuncs[notify->dsp_pid]) (notify->dsp_pid, 
				notify->dsp_param, &notify->dsp_when);
}
