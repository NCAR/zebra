/*
 * The data store network transfer daemon.
 */
static char *rcsid = "$Id";

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "NetXfr.h"



/*
 * Here's the array of stuff we send to people.
 */

# define MAXPLAT 256

typedef struct _DataRecip
{
	char	dr_Recip[128];		/* Who gets it		*/
	struct _DataRecip *dr_Next;	/* Next in the chain	*/
} DataRecip;

DataRecip *Recipients[MAXPLAT];
char *PlatName[MAXPLAT];

/*
 * The current sequence number.
 */
int Seq = 0;

/*
 * Are we broadcasting?
 */
int Broadcast = 0;
int Pid;			/* Our process id		*/

/*
 * Broadcast retransmit parameters.
 */
int BCastSave = 20;	/* How long we save broadcasted data so that we
			 * can service rebroadcast requests. 		*/
int BCInitialWait = 2;	/* How long we wait for missing broadcast packets
			 * before requesting the first retransmit. 	*/
int BCRetransWait = 5;	/* How long before we timeout a retransmit	*/
int BCRetransMax = 2;	/* How many times we will ask for a retransmit
			 * before we lose our patience			*/

/*
 * The queue of broadcast packets awaiting everything else.
 */
static DataBCChunk *BCQueue = 0;

/*
 * Stuff for data builds in progress.
 */
typedef struct _InProgress
{
	int		ip_Seq;		/* Sequence number	*/
	PlatformId	ip_Plat;	/* The platform		*/
	DataObject	*ip_DObj;	/* The data object	*/
	DataOffsets	ip_Offsets;	/* Offset struct, if needed	*/
	char		*ip_Arrived;	/* Indications of arrived packets*/
	struct _InProgress *ip_Next;	/* Next in chain	*/
	short		ip_NBCast;	/* # of arrived bcast pkts	*/
	short		ip_NBExpect;	/* # expected			*/
	char		ip_BCast;	/* Broadcast packets coming	*/
	char		ip_Done;	/* DONE packet arrived	*/
} InProgress;

InProgress *IPList = 0;


# ifdef __STDC__
	static int Incoming (Message *);
	static void Die (void);
	static int Dispatcher (int, struct ui_command *);
	static void NewRecipients (struct ui_command *);
	static void Run (void);
	static void DataAvailable (PlatformId, int, time *, int);
	static int NXMessage (Message *);
	static void SendChunk (PlatformId, void *, int, int, int);
	static DataObject *GetData (PlatformId, time *, int);
	static void NewData (DataHdr *);
	InProgress *FindIP (int);
	static void ContData (DataContinue *);
	static void Done (int);
	static void FinishIP (InProgress *);
	static void IncOffsets (DataOffsets *);
	static void UnknownBCast (DataBCChunk *, int);
	static void FindQueued (int);
# else
	static int Incoming ();
	static void Die ();
	static int Dispatcher ();
	static void NewRecipients ();
	static void Run ();
	static void DataAvailable ();
	static int NXMessage ();
	static void SendChunk ();
	static DataObject *GetData ();
	static void NewData ();
	InProgress *FindIP ();
	static void ContData ();
	static void Done ();
	static void FinishIP ();
	static void IncOffsets ();
	static void UnknownBCast ();
	static void FindQueued ();
# endif

/*
 * Just define the keywords here.
 */
# define NXC_SEND	1
# define NXC_RUN	2
# define NXC_AS		3
# define NXC_GET	4
# define NXC_BROADCAST	5
# define NXC_RECEIVE	6

/*
 * Return the offset of a data object field.
 */
# define DOFFSET(field) ((int) &(((DataObject *) 0)->field))


main (argc, argv)
int argc;
char **argv;
{
	char cmd[128];
	SValue v;
	stbl vtable;
	int i;
/*
 * Hook into the user interface.  Only go interactive if they didn't
 * give us a file on the command line.
 */
	if (argc > 1)
	{
		ui_init ("NetXfr.lf", FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
				SYMT_STRING, &v);
	}
	else
		ui_init ("NetXfr.lf", TRUE, FALSE);
/*
 * Set up indirect variables so that the user can do some tweaking.
 */
	vtable = usy_g_stbl ("ui$variable_table");
	usy_c_indirect (vtable, "sequence", &Seq, SYMT_INT, 0);
	usy_c_indirect (vtable, "bcastsave", &BCastSave, SYMT_INT, 0);
	usy_c_indirect (vtable, "initialwait", &BCInitialWait, SYMT_INT, 0);
	usy_c_indirect (vtable, "maxretrans", &BCRetransMax, SYMT_INT, 0);
/*
 * Hook into the message system.
 */
	msg_connect (Incoming, OURNAME);
	msg_AddProtoHandler (MT_NETXFR, NXMessage);
/*
 * Initialize the data store.
 */
	ds_Initialize ();
/*
 * Initialize the transfer list.
 */
	for (i = 0; i < MAXPLAT; i++)
		Recipients[i] = 0;
/*
 * Start reading commands.
 */
	Pid = getpid ();
	ui_get_command ("initial", "NetXfr>", Dispatcher, 0);
	Die ();
}





static void
Die ()
/*
 * Time to shut down.
 */
{
	ui_finish ();
	exit (0);
}





static int
Dispatcher (junk, cmds)
int junk;
struct ui_command *cmds;
/*
 * Deal with an incoming UI command.
 */
{
	switch (UKEY (*cmds))
	{
	/*
	 * They're telling us to send something to somebody.
	 */
	   case NXC_SEND:
	   	NewRecipients (cmds + 1);
		break;
	/*
	 * Time to start running.
	 */
	   case NXC_RUN:
	   	/* Run (); */
		msg_await ();
		break;
	/*
	 * Set up broadcasting.
	 */
	   case NXC_BROADCAST:
	   	BCastSetup (cmds + 1);
		break;
	/*
	 * Set up to receive broadcast stuff.
	 */
	   case NXC_RECEIVE:
	   	ReceiveSetup (UINT (cmds[1]));
		break;
	}
	return (TRUE);
}






static void
NewRecipients (cmds)
struct ui_command *cmds;
/*
 * Another SEND line has been encountered.
 */
{
	PlatformId plat;
	DataRecip *dp;
/*
 * Look up the platform they gave us.
 */
	if ((plat = ds_LookupPlatform (UPTR (*cmds))) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform %s", UPTR (*cmds));
		return;
	}
/*
 * If they explicitly give a different name, we'll alias for them; otherwise
 * remember this name.
 */
	if (cmds[1].uc_ctype == UTT_KW)
		cmds += 2;
	PlatName[plat] = usy_string (UPTR (*cmds));
/*
 * Pass through all of the recipients.
 */
	for (cmds++; cmds->uc_ctype != UTT_END; cmds++)
	{
		dp = ALLOC (DataRecip);
		strcpy (dp->dr_Recip, UPTR (*cmds));
		dp->dr_Next = Recipients[plat];
		Recipients[plat] = dp;
	}
/*
 * Ask for notifications on this platform.
 */
	ds_RequestNotify (plat, 0, DataAvailable);
}





static void
DataAvailable (plat, junk, t, ns)
PlatformId plat;
int junk;
time *t;
int ns;
/*
 * Data for which somebody is interested has arrived.
 */
{
	DataObject *dobj;
	DataHdr dhdr;
	DataDone done;
	int i;
	RastImg *rip;
/*
 * First thing we need to do is to get this data.
 */
	if (! (dobj = GetData (plat, t, ns)))
		return;
/*
 * Create and send out the data header.
 */
	dhdr.dh_MsgType = NMT_DataHdr;
	dhdr.dh_DataSeq = ++Seq;
	strcpy (dhdr.dh_Platform, PlatName[plat]);
	dhdr.dh_DObj = *dobj;
	dhdr.dh_BCast = Broadcast;
	SendOut (plat, &dhdr, sizeof (dhdr));
/*
 * Send out the various pieces.
 */
	SendChunk (plat, dobj->do_times, DOFFSET (do_times),
		dobj->do_npoint*sizeof (time), DOF_FREETIME);
	if (dobj->do_aloc)
		SendChunk (plat, dobj->do_aloc, DOFFSET (do_aloc),
			(dobj->do_org == OrgOutline) ?
			*dobj->do_desc.d_length*sizeof (Location) :
			dobj->do_npoint*sizeof (Location), DOF_FREEALOC);
/*
 * Send per-field stuff.
 */
	for (i = 0; i < dobj->do_nfield; i++)
	{
		SendChunk (plat, dobj->do_fields[i], DOFFSET (do_fields[i]),
			strlen (dobj->do_fields[i]) + 1, 0);
		if (! Broadcast)
			SendChunk (plat, dobj->do_data[i], DOFFSET(do_data[i]),
				dobj->do_nbyte, DOF_FREEALLDATA);
	}
/*
 * Now per-organization stuff.
 */
	switch (dobj->do_org)
	{
	/*
	 * Irgrids -- pid and location for the subplatforms.
	 */
	   case OrgIRGrid:
	   	SendChunk (plat, dobj->do_desc.d_irgrid.ir_loc,
			DOFFSET (do_desc.d_irgrid.ir_loc),
			dobj->do_desc.d_irgrid.ir_npoint*sizeof (Location), 0);
	   	SendChunk (plat, dobj->do_desc.d_irgrid.ir_subplats,
			DOFFSET (do_desc.d_irgrid.ir_subplats),
			dobj->do_desc.d_irgrid.ir_npoint*sizeof (PlatformId),
			0);
		break;
	/*
	 * Images have lots of rgrid structures, and scaling too.
	 */
	   case OrgImage:
	   	rip = &dobj->do_desc.d_img;
		SendChunk (plat, rip->ri_rg, DOFFSET (do_desc.d_img.ri_rg),
			dobj->do_npoint*sizeof (RGrid), 0);
		SendChunk (plat, rip->ri_scale,DOFFSET(do_desc.d_img.ri_scale),
			dobj->do_nfield*sizeof (ScaleInfo), 0);
		break;
	/*
	 * Length info for outlines.  UGLY KLUDGE: I don't see how anybody
	 * can enter more than one boundary at once, so we will only send
	 * info for one.  This is because there is no easy way to see how
	 * many of these there are.
	 */
	   case OrgOutline:
	   	SendChunk (plat, dobj->do_desc.d_length, 
			DOFFSET (do_desc.d_length), sizeof (int), 0);
		break;
	}
/*
 * If we're broadcasting, do that now.
 */
	if (Broadcast)
		DoBCast (plat, dobj);
/*
 * Done at last.
 */
	done.dh_MsgType = NMT_DataDone;
	done.dh_DataSeq = Seq;
	SendOut (plat, &done, sizeof (done));
}





void
SendOut (plat, stuff, len)
PlatformId plat;
void *stuff;
int len;
/*
 * Send out this stuff to all the recipients for this platform.
 */
{
	DataRecip *dp;

	for (dp = Recipients[plat]; dp; dp = dp->dr_Next)
		msg_send (dp->dr_Recip, MT_NETXFR, FALSE, stuff, len);
}




static void
SendChunk (plat, stuff, offset, len, flags)
PlatformId plat;
void *stuff;
int offset, len, flags;
/*
 * Send a chunk of stuff in a Continue packet.
 */
{
	static char *buf = 0;
	static int buflen = -1;
	static DataContinue *cont;
/*
 * Make sure our buffer is big enough.
 */
	if (len > buflen)
	{
		if (buflen > 0)
			free (buf);
		buf = malloc (len + sizeof (DataContinue) - 1);
		buflen = len;
		cont = (DataContinue *) buf;
	}
/*
 * Fill in the packet info.
 */
	cont->dh_MsgType = NMT_DataContinue;
	cont->dh_DataSeq = Seq;
	cont->dh_Offset = offset;
	cont->dh_Size = len;
	cont->dh_Flags = flags;
	memcpy (cont->dh_data, stuff, len);
/*
 * Send it.
 */
	SendOut (plat, buf, len + sizeof (DataContinue) - 1);
}




static DataObject *
GetData (plat, t, ns)
PlatformId plat;
time *t;
int ns;
/*
 * Grab the data for this info.
 */
{
	time *times, begin;
	int ntime;
	static int NFld = -1;
	static char *FList[MAXFIELD];
/*
 * Allocate a time array, then find the sample times for the new data.
 */
	if (ns > 1)
	{
		times = (time *) malloc (ns * sizeof (time));
		ntime = ds_DataTimes (plat, t, ns, DsBefore, times);
		if (ntime <= 0)
		{
			msg_ELog (EF_PROBLEM, "Data vanished for %s",
				ds_PlatformName (plat));
			return (0);
		}
		begin = times[ntime - 1];
		free (times);
	}
	else
		begin = *t;
/*
 * Get field info.
 */
	if (NFld < 0)
	{
		NFld = MAXFIELD;
		ds_GetFields (plat, t, &NFld, FList);
		{
			int i;
			ui_nf_printf ("%d Fields: ", NFld);
			for (i = 0; i < NFld; i++)
				ui_nf_printf ("%s ", FList[i]);
			ui_printf ("\n");
		}
	}
/*
 * Get the data.
 */
	return (ds_GetData (plat, FList, NFld, &begin, t,
			ds_PlatformDataOrg (plat), 0.0, 9999.9));
}





static int 
Incoming (msg)
Message *msg;
/*
 * Deal with an incoming message.
 */
{
	struct mh_template *tmpl;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
	   	tmpl = (struct mh_template *) msg->m_data;
		if (tmpl->mh_type == MH_DIE)
			Die ();
		else
			msg_ELog (EF_PROBLEM, "Weird MH msg %d",tmpl->mh_type);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown msg proto %d", msg->m_proto);
		break;
	}
	return (0);
}






static int
NXMessage (msg)
struct message *msg;
/*
 * Deal with a message in our own protocol.
 */
{
	DataTemplate *tmpl = (DataTemplate *) msg->m_data;
/*
 * See what we've got here.
 */
	switch (tmpl->dh_MsgType)
	{
	/*
	 * New data coming in.
	 */
	   case NMT_DataHdr:
	   	NewData ((DataHdr *) tmpl);
		break;

	/*
	 * Continuation of existing data.
	 */
	   case NMT_DataContinue:
	   	ContData ((DataContinue *) tmpl);
		break;

	/*
	 * Done with data.
	 */
	   case NMT_DataDone:
	   	Done (tmpl->dh_DataSeq);
		break;

	/*
	 * Offsets for bcast data.
	 */
	   case NMT_DataOffsets:
	   	IncOffsets ((DataOffsets *) tmpl);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown data proto type: %d",
				tmpl->dh_MsgType);
		break;
	}
	return (0);
}




static void
NewData (hdr)
DataHdr *hdr;
/*
 * A new data stream is beginning.
 */
{
	InProgress *ip = ALLOC (InProgress);

	msg_ELog (EF_INFO, "Begin data %s, seq %d, t %d %06d",
		hdr->dh_Platform, hdr->dh_DataSeq,
		hdr->dh_DObj.do_end.ds_yymmdd, hdr->dh_DObj.do_end.ds_hhmmss);
/*
 * Fill in our IP structure.
 */
	ip->ip_Seq = hdr->dh_DataSeq;
	if ((ip->ip_Plat = ds_LookupPlatform(hdr->dh_Platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "New data on bad platform %s",
				hdr->dh_Platform);
		free (ip);
		return;
	}
	ip->ip_DObj = ALLOC (DataObject);
	*ip->ip_DObj = hdr->dh_DObj;
	ip->ip_DObj->do_id = ip->ip_Plat;
	ip->ip_Arrived = 0;
	ip->ip_Done = FALSE;
/*
 * Add it to the list
 */
	ip->ip_Next = IPList;
	IPList = ip;
/*
 * BCast stuff.
 */
	if (ip->ip_BCast = hdr->dh_BCast)
	{
		ip->ip_NBCast = ip->ip_NBExpect = 0;
		FindQueued (ip->ip_Seq);
	}
	msg_ELog (EF_DEBUG,"BCast is %d for seq %d", ip->ip_BCast, ip->ip_Seq);
}




static void
IncOffsets (off)
DataOffsets *off;
/*
 * Deal with an incoming offsets structure.
 */
{
	InProgress *ip = FindIP (off->dh_DataSeq);

	if (ip)
		ip->ip_Offsets = *off;
}





InProgress *
FindIP (seq)
int seq;
/*
 * Find the in progress data chunk corresponding to this 
 * sequence number.
 */
{
	InProgress *ip;

	for (ip = IPList; ip; ip = ip->ip_Next)
		if (ip->ip_Seq == seq)
			break;
# ifdef notdef	/* Silence! */
	if (! ip)
		msg_ELog (EF_PROBLEM, "Unknown IP sequence %d", seq);
# endif
	return (ip);
}





static void
ContData (cont)
DataContinue *cont;
/*
 * Deal with a data continuation.
 */
{
	InProgress *ip = FindIP (cont->dh_DataSeq);
	char **hack;

	msg_ELog (EF_DEBUG, "Data seq %d, size %d off %d", cont->dh_DataSeq,
		cont->dh_Size, cont->dh_Offset);
/*
 * If we don't know about this one, bail.
 */
	if (! ip)
		return;
/*
 * Allocate the memory and fill it in.
 */
	hack = (char **) (cont->dh_Offset + (char *) ip->ip_DObj);
	*hack = malloc (cont->dh_Size);
	memcpy (*hack, cont->dh_data, cont->dh_Size);
	ip->ip_DObj->do_flags |= cont->dh_Flags;
}



int
BCastHandler (port, data, len)
int port, len;
char *data;
/*
 * Deal with a broadcast packet.
 */
{
	DataBCChunk *chunk = (DataBCChunk *) data;
	InProgress *ip;
/*
 * Make sure this is what we think it is.
 */
	if (chunk->dh_MsgType != NMT_DataBCast)
	{
		msg_ELog (EF_PROBLEM, "Funky msg type %d bcast",
			chunk->dh_MsgType);
		return (0);
	}
/*
 * Look for this IP.  If we don't find it, we need to go to some more effort
 * to decide what the hell to do with this thing.
 */
	msg_ELog (EF_INFO, "BC pkt %d, %d at %d", chunk->dh_Chunk,
		chunk->dh_Size, chunk->dh_Offset);
	if (! (ip = FindIP (chunk->dh_DataSeq)))
	{
		UnknownBCast (chunk, len);
		return (0);
	}
/*
 * If this is the first broadcast packet, do some setup.
 */
	if (! ip->ip_Arrived)
	{
	/*
	 * Set up some stuff.
	 */
		ip->ip_Arrived = malloc (chunk->dh_NChunk);
		memset (ip->ip_Arrived, 0, chunk->dh_NChunk);
		ip->ip_NBCast = 0;
		ip->ip_NBExpect = chunk->dh_NChunk;
		ip->ip_DObj->do_data[0] = (float *) malloc(chunk->dh_DataSize);
	}
/*
 * Mark this packet as arrived, and copy over the stuff.
 */
	ip->ip_Arrived[chunk->dh_Chunk] = 1;
	ip->ip_NBCast++;
	memcpy (chunk->dh_Offset + (char *) ip->ip_DObj->do_data[0], 
			chunk->dh_data, chunk->dh_Size);
/*
 * If everything is done, finish it out.
 */
	if (ip->ip_Done && ip->ip_NBCast >= ip->ip_NBExpect)
		FinishIP (ip);
}






static void
UnknownBCast (chunk, len)
DataBCChunk *chunk;
int len;
/*
 * Deal with a broadcast chunk that does not correspond to a known
 * IP.
 */
{
	DataBCChunk *new;
/*
 * Here is where we try to detect packets that we have sent out ourselves.
 * The algorithm is, basically: (1) if we are doing broadcasts, (2) the PID
 * matches, and (3) the sequence is close to ours, we assume that this
 * packet was sent by us and we drop it.
 */
	if (Broadcast && chunk->dh_ID == Pid &&
				ABS (chunk->dh_DataSeq - Seq) < 5)
	{
		msg_ELog (EF_DEBUG, "Drop own BC pkt, seq %d",
				chunk->dh_DataSeq);
		return;
	}
/*
 * Otherwise we enqueue it, waiting for the header info to arrive.
 */
	msg_ELog (EF_INFO, "Enqueue BC seq %d ch %d/%d", chunk->dh_DataSeq,
		chunk->dh_Chunk, chunk->dh_NChunk);
	new = (DataBCChunk *) malloc (len);
	memcpy (new, chunk, len);
	new->dh_Next = BCQueue;
	BCQueue = new;
}




static void
FindQueued (seq)
int seq;
/*
 * Find any enqueued bcast packets for this seq and dispatch them.
 */
{
	DataBCChunk *chunk, *last;
/*
 * See if there are any at the head of the list.
 */
	while (BCQueue && BCQueue->dh_DataSeq == seq)
	{
		chunk = BCQueue;
		BCQueue = BCQueue->dh_Next;
		BCastHandler (0, (char *) chunk, 0);
		free (chunk);
	}
/*
 * Now pass through the rest.
 */
	last = chunk = BCQueue;
	while (chunk)
	{
		if (chunk->dh_DataSeq == Seq)
		{
			last->dh_Next = chunk->dh_Next;
			BCastHandler (0, (char *) chunk, 0);
			free (chunk);
		}
		else
			last = chunk;
		chunk = last->dh_Next;
	}
}





static void 
Done (seq)
int seq;
/*
 * Finish out this chunk of data.
 */
{
	InProgress *ip = FindIP (seq);
/*
 * Make sure we know about this sequence.
 */
	if (! ip)
		return;
/*
 * If we have all of the data, we can finish this thing out now.
 */
	if (! ip->ip_BCast || ip->ip_NBCast >= ip->ip_NBExpect)
	{
		FinishIP (ip);
		return;
	}
/*
 * Otherwise just mark as "done" and hope it comes in eventually.
 * Set a timeout eventually.
 */
	msg_ELog (EF_INFO, "IP DONE, but %d segs missing",
		ip->ip_NBExpect - ip->ip_NBCast);
	ip->ip_Done = TRUE;
}




static void
FinishIP (ip)
InProgress *ip;
/*
 * This one is done.
 */
{
	InProgress *zap;
	int seq = ip->ip_Seq, i;
/*
 * If this is a broadcast-distributed chunk, apply the data offsets.
 */
	if (ip->ip_BCast)
		for (i = 1; i < ip->ip_DObj->do_nfield; i++)
			ip->ip_DObj->do_data[i] = ip->ip_DObj->do_data[0] + 
					ip->ip_Offsets.dh_Offsets[i];
/*
 * Throw this data into the data store.  FIGURE OUT WHAT TO DO ABOUT
 * NEWFILE!!!!
 */
	msg_ELog (EF_INFO, "Store sequence %d", ip->ip_Seq);
	ds_PutData (ip->ip_DObj, FALSE);
/*
 * Clear this entry out of the inprogress list.
 */
	ds_FreeDataObject (ip->ip_DObj);
	if (ip == IPList)
		IPList = ip->ip_Next;
	else
	{
		for (zap = IPList; zap->ip_Next && zap->ip_Next->ip_Seq != seq;
					zap = zap->ip_Next)
			;
		if (zap->ip_Next)
			zap->ip_Next = zap->ip_Next->ip_Next;
		else
			msg_ELog (EF_PROBLEM, "Strange...IP seq %d vanished",
					seq);
	}
	free (ip);
}
