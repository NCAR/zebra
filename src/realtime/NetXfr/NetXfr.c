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

static int Seq = 0;
static int Broadcast = 0, Port = 0;
static long Addr = 0;

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
	static void SendOut (PlatformId, void *, int);
	static void SendChunk (PlatformId, void *, int, int, int);
	static DataObject *GetData (PlatformId, time *, int);
	static void NewData (DataHdr *);
	InProgress *FindIP (int);
	static void ContData (DataContinue *);
	static void Done (int);
	static void BCastSetup (struct ui_command *);
	static int BCastHandler (int, char *, int);
	static void DoBCast (PlatformId, DataObject *);
	static void FinishIP (InProgress *);
	static void ReceiveSetup (int);
# else
	static int Incoming ();
	static void Die ();
	static int Dispatcher ();
	static void NewRecipients ();
	static void Run ();
	static void DataAvailable ();
	static int NXMessage ();
	static void SendOut ();
	static void SendChunk ();
	static DataObject *GetData ();
	static void NewData ();
	InProgress *FindIP ();
	static void ContData ();
	static void Done ();
	static void BCastSetup ();
	static int BCastHandler ();
	static void DoBCast ();
	static void FinishIP ();
	static void ReceiveSetup ();
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
BCastSetup (cmds)
struct ui_command *cmds;
/*
 * Get set up to do broadcasting.
 */
{
	int a, b, c, d;
/*
 * Figure out params.
 */
	if (sscanf (UPTR (*cmds), "%d.%d.%d.%d", &a, &b, &c, &d) != 1)
	{
		msg_ELog (EF_EMERGENCY, "Bad broadcast addr '%s'",UPTR(*cmds));
		exit (1);
	}
	Addr = d + (c << 8) + (b << 16) + (a << 24);
	Port = UINT (cmds[1]);
/*
 * Now hook into msg.
 */
	if ((Broadcast = msg_BCSetup (Addr, Port, BCastHandler)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "BCSetup failure");
		exit (1);
	}
}



static void
ReceiveSetup (port)
int port;
/*
 * Set up to receive bcast info.
 */
{
	if (msg_BCSetup (0, port, BCastHandler) < 0)
		msg_ELog (EF_PROBLEM, "Unable to setup BCast on port %d",port);
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
 * If we're broadcasting, do that now.
 */
	if (Broadcast)
		DoBCast (plat, dobj);
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
 * Done at last.
 */
	done.dh_MsgType = NMT_DataDone;
	done.dh_DataSeq = Seq;
	SendOut (plat, &done, sizeof (done));
}





static void
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




static void
DoBCast (plat, dobj)
PlatformId plat;
DataObject *dobj;
/*
 * Broadcast this data to the world.
 */
{
	DataBCChunk *chunk;
	DataOffsets offsets;
	int fld, nchunk;
	char *cdata = (char *) dobj->do_data;
/*
 * Send out the offsets first, through normal channels.  Note that this
 * assumes that the data arrays are allocated in one big chunk.
 */
	for (fld = 0; fld < dobj->do_nfield; fld++)
		offsets.dh_Offsets[fld] = dobj->do_data[fld]-dobj->do_data[0];
	offsets.dh_MsgType = NMT_DataOffsets;
	SendOut (plat, &offsets, sizeof (offsets));
/*
 * Allocate memory for our chunk, and figure out how much we can do in
 * each packet.  The point here is to create packets that won't get fragmented
 * on the ethernet on their way out.
 *
 * IP header = 20 bytes.  UDP = 8 bytes.
 */
# define CBYTES (1500 - 28)

	chunk = (DataBCChunk *) malloc (CBYTES);
	chunk->dh_Size = CBYTES - sizeof (DataBCChunk) + 1;
/*
 * Fill in other static info in the chunk.
 */
	chunk->dh_MsgType = NMT_DataBCast;
	chunk->dh_DataSeq = Seq;
	chunk->dh_Offset = 0;
	chunk->dh_DataSize = dobj->do_nbyte;
	chunk->dh_NChunk = (dobj->do_nbyte + chunk->dh_Size -1)/chunk->dh_Size;
	chunk->dh_Chunk = 0;
	msg_ELog (EF_DEBUG, "BCast in %d chunks of %d", chunk->dh_NChunk,
		chunk->dh_Size);
/*
 * Now we blast them out.
 */
	for (; chunk->dh_Chunk < chunk->dh_NChunk - 1; (chunk->dh_Chunk)++)
	{
		memcpy (chunk->dh_data, cdata, chunk->dh_Size);
		msg_BCast (Broadcast, chunk, CBYTES);
		cdata += chunk->dh_Size;
		chunk->dh_Offset += chunk->dh_Size;
	}
/*
 * Don't forget the last one.
 */
	chunk->dh_Size = dobj->do_nbyte - chunk->dh_Offset;
	memcpy (chunk->dh_data, cdata, chunk->dh_Size);
	msg_BCast (Broadcast, chunk, CBYTES);
/*
 * Free up and we're done.
 */
	free (chunk);
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

	msg_ELog (EF_DEBUG, "Begin data %s, seq %d, t %d %06d",
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
/*
 * BCast stuff.
 */
	if (ip->ip_BCast = hdr->dh_BCast)
		ip->ip_NBCast = ip->ip_NBExpect = 0;
/*
 * Add it to the list, and we're done.
 */
	ip->ip_Next = IPList;
	IPList = ip;
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
	if (! ip)
		msg_ELog (EF_PROBLEM, "Unknown IP sequence %d", seq);
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



static int
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
 * Look for this IP.  For now, we assume that the header will have arrived
 * by now.  That may not be a good assumption.
 */
	if (! (ip = FindIP (chunk->dh_DataSeq)))
	{
		msg_ELog (EF_PROBLEM, "Unknown Data Seq %d Bcast", 
			chunk->dh_DataSeq);
		return (0);
	}
/*
 * If this is the first broadcast packet, do some setup.
 */
	msg_ELog (EF_DEBUG, "BC pkt %d, %d at %d", chunk->dh_Chunk,
		chunk->dh_Size, chunk->dh_Offset);
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
	msg_ELog (EF_DEBUG, "IP DONE, but %d segs missing",
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
	msg_ELog (EF_DEBUG, "Store sequence %d", ip->ip_Seq);
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
