/*
 * Handling of broadcast stuff.
 */
static char *rcsid = "$Id: nx_BCast.c,v 1.3 1991-06-08 17:06:47 corbet Exp $";

# include <sys/time.h>
# include <sys/signal.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"
# include "DataStore.h"
# include "NetXfr.h"


/*
 * Local stuff.
 *
 * The channel on which we do our broadcasting.
 */
static int BCastChannel = 0;

/*
 * The following is used to keep track of broadcast sequences in progress,
 * and in the retransmit wait state.
 */
typedef struct _tx_BCast
{
	int	txb_Seq;	/* The sequence number of this one	*/
	int	txb_NChunk;	/* How many chunks			*/
	DataBCChunk **txb_Chunks;	/* The actual data chunks	*/
	struct _tx_BCast *txb_Next;	/* List link			*/
} tx_BCast;

tx_BCast *Tx_Current = 0;	/* Currently active sequences		*/
tx_BCast *Tx_Free = 0;		/* The struct lookaside list		*/

DataBCChunk *Ch_Free = 0;	/* Data chunk lookaside list		*/

/*
 * Bookkeeping for retransmission requests.  This stuff is
 * kept around to avoid rebroadcasting stuff multiple times.
 * Actual retransmissions are only done once a second (at most),
 * so that duplicates can be filtered out.
 */
static DataRetransRq *RetransPending = 0;
static DataRetransRq *RetransFree = 0;
static int RetransTreq = -1;		/* Timer request number	*/

static int NCAlloc = 0, NCReuse = 0;

/*
 * Routines.
 */
# ifdef __STDC__
	static tx_BCast *NewBCast (int, int);
	static DataBCChunk *GetBCastPacket (tx_BCast *, int);
	static tx_BCast *FindBCP (int);
	static void FlushRetrans (time *, void *);
	static void ZapBCast (time *, tx_BCast *);
	static void Delay (void);
	static void Alarm (void);
# else
	static tx_BCast *NewBCast ();
	static DataBCChunk *GetBCastPacket ();
	static tx_BCast *FindBCP ();
	static void FlushRetrans ();
	static void ZapBCast ();
	static void Delay ();
	static void Alarm ();
# endif



void
BCastSetup (cmds)
struct ui_command *cmds;
/*
 * Get set up to do broadcasting.
 */
{
	int a, b, c, d;
	int addr, port;
/*
 * Figure out params.
 */
	if (sscanf (UPTR (*cmds), "%d.%d.%d.%d", &a, &b, &c, &d) != 4)
	{
		msg_ELog (EF_EMERGENCY, "Bad broadcast addr '%s'",UPTR(*cmds));
		exit (1);
	}
	addr = d + (c << 8) + (b << 16) + (a << 24);
	port = UINT (cmds[1]);
/*
 * Now hook into msg.
 */
	if ((BCastChannel = msg_BCSetup (addr, port, BCastHandler)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "BCSetup failure");
		exit (1);
	}
	Broadcast = TRUE;
}



void
ReceiveSetup (port)
int port;
/*
 * Set up to receive bcast info.
 */
{
	if ((BCReceive = msg_BCSetup (0, port, BCastHandler)) < 0)
		msg_ELog (EF_PROBLEM, "Unable to setup BCast on port %d",port);
}





void
DoBCast (plat, dobj)
PlatformId plat;
DataObject *dobj;
/*
 * Broadcast this data to the world.
 */
{
	tx_BCast *bcp;
	DataBCChunk *chunk, template;
	DataOffsets offsets;
	int fld, nchunk;
	char *cdata = (char *) dobj->do_data[0];
/*
 * Set up to output this sequence.
 */
	bcp = NewBCast (Seq, dobj->do_nbyte);
/*
 * Send out the offsets first, through normal channels.  Note that this
 * assumes that the data arrays are allocated in one big chunk.
 */
	for (fld = 0; fld < dobj->do_nfield; fld++)
		offsets.dh_Offsets[fld] = dobj->do_data[fld]-dobj->do_data[0];
	offsets.dh_MsgType = NMT_DataOffsets;
	offsets.dh_DataSeq = Seq;
	SendOut (plat, &offsets, sizeof (offsets));
/*
 * Fill in the header info in our template packet.
 */
	template.dh_MsgType = NMT_DataBCast;
	template.dh_DataSeq = Seq;
	template.dh_Offset = 0;
	template.dh_DataSize = dobj->do_nbyte;
	template.dh_NChunk = bcp->txb_NChunk;
	template.dh_Size = MAXDATA;
	template.dh_Chunk = 0;
	template.dh_ID = Pid;
	msg_ELog (EF_DEBUG, "BCast in %d chunks of %d", template.dh_NChunk,
		template.dh_Size);
/*
 * Now we blast them out.
 */
	for (; template.dh_Chunk < template.dh_NChunk - 1; template.dh_Chunk++)
	{
	/*
	 * Allocate an outgoing packet and fill in header and data.
	 */
		chunk = GetBCastPacket (bcp, template.dh_Chunk);
		*chunk = template;
		memcpy (chunk->dh_data, cdata, chunk->dh_Size);
	/*
	 * Send it out and update info.
	 */
		msg_BCast (BCastChannel, chunk, CBYTES);
		cdata += chunk->dh_Size;
		template.dh_Offset += chunk->dh_Size;
	/*
	 * Maybe delay a bit to allow things to drain out.
	 */
		if ((template.dh_Chunk % BCBurst) == 1)
			Delay ();
	}
/*
 * Don't forget the last one.
 */
	chunk = GetBCastPacket (bcp, template.dh_Chunk);
	*chunk = template;
	chunk->dh_Size = dobj->do_nbyte - chunk->dh_Offset;
	memcpy (chunk->dh_data, cdata, chunk->dh_Size);
	msg_BCast (BCastChannel, chunk, CBYTES);
/*
 * Add the timeout that will eventually cause all this to go away.
 */
	tl_AddRelativeEvent (ZapBCast, bcp, BCastSave*INCFRAC, 0);
}






static tx_BCast *
NewBCast (seq, len)
int seq, len;
/*
 * Set up a new broadcast for this sequence.
 */
{
	tx_BCast *ret;
/*
 * Get a structure.
 */
	if (Tx_Free)
	{
		ret = Tx_Free;
		Tx_Free = ret->txb_Next;
	}
	else
		ret = ALLOC (tx_BCast);
/*
 * Fill it in and add it to the list.
 */
	ret->txb_Seq = seq;
	ret->txb_NChunk = (len + MAXDATA + 1)/MAXDATA;
	ret->txb_Chunks = (DataBCChunk **)
			malloc (ret->txb_NChunk * sizeof (DataBCChunk *));
	ret->txb_Next = Tx_Current;
	Tx_Current = ret;
	return (ret);
}




static DataBCChunk *
GetBCastPacket (bcp, number)
tx_BCast *bcp;
int number;
/*
 * Get a packet to send out.
 */
{
	DataBCChunk *pkt;
/*
 * Get the packet itself.
 */
	if (Ch_Free)
	{
		pkt = Ch_Free;
		Ch_Free = pkt->dh_Next;
		NCReuse++;
	}
	else
	{
		pkt = (DataBCChunk *) malloc (CBYTES);
		NCAlloc++;
	}
/*
 * Stuff it into the broadcast block, and we're done.
 */
	return (bcp->txb_Chunks[number] = pkt);
}




static void
ZapBCast (t, bcp)
time *t;
tx_BCast *bcp;
/*
 * Age out this set of broadcast data.
 */
{
	tx_BCast *last;
	int ch;
/*
 * Remove this structure from the current list.
 */
	if (bcp == Tx_Current)
		Tx_Current = bcp->txb_Next;
	else
	{
		for (last = Tx_Current; last; last = last->txb_Next)
			if (last->txb_Next == bcp)
				break;
		if (last)
			last->txb_Next = bcp->txb_Next;
		else
			msg_ELog (EF_PROBLEM,"Bcast %d vanished",bcp->txb_Seq);
	}
/*
 * Go through and free each of the packet chunks.
 */
	for (ch = 0; ch < bcp->txb_NChunk; ch++)
	{
		bcp->txb_Chunks[ch]->dh_Next = Ch_Free;
		Ch_Free = bcp->txb_Chunks[ch];
	}
	free (bcp->txb_Chunks);
/*
 * Free the structure and we're done.
 */
	bcp->txb_Next = Tx_Free;
	Tx_Free = bcp;
}





void
Retransmit (rq)
DataRetransRq *rq;
/*
 * Deal with a retransmit request.
 */
{
	DataRetransRq *ent;
	tx_BCast *bcp;
/*
 * Search the current pending list to see if somebody has already asked
 * for this one.
 */
	for (ent = RetransPending; ent; ent = ent->dh_Next)
		if (ent->dh_DataSeq == rq->dh_DataSeq &&
				ent->dh_Chunk == rq->dh_Chunk)
		{
			msg_ELog (EF_INFO, "Dup retrans %d %d",rq->dh_DataSeq,
					rq->dh_Chunk);
			return;
		}
/*
 * See also if we can deal with this sequence at all.
 */
	if (! (bcp = FindBCP (rq->dh_DataSeq)))
		return;
/*
 * OK, add this one to the list.
 */
	if (RetransFree)
	{
		ent = RetransFree;
		RetransFree = ent->dh_Next;
	}
	else
		ent = ALLOC (DataRetransRq);
	*ent = *rq;
	ent->dh_Next = RetransPending;
	RetransPending = ent;
/*
 * If there is not a timer request pending to flush these guys out,
 * put in in now.
 */
	if (RetransTreq < 0)
		RetransTreq = tl_AddRelativeEvent (FlushRetrans, 0,INCFRAC, 0);
}




static tx_BCast *
FindBCP (seq)
int seq;
/*
 * Find the broadcast block for this sequence.
 */
{
	tx_BCast *bcp;

	for (bcp = Tx_Current; bcp; bcp = bcp->txb_Next)
		if (bcp->txb_Seq == seq)
			break;
	return (bcp);
}






static void
FlushRetrans (t, junk)
time *t;
void *junk;
/*
 * Actually cause retransmits to happen.
 */
{
	tx_BCast *bcp;
	DataRetransRq *req;
	int nretrans = 0;
/*
 * Deal with each entry on the list.
 */
	while (RetransPending)
	{
	/*
	 * Remove this one.
	 */
	 	req = RetransPending;
		RetransPending = req->dh_Next;
	/*
	 * Find the broadcast structure that can satisfy this one, and
	 * send out the chunk.
	 */
	 	if (bcp = FindBCP (req->dh_DataSeq))
			msg_BCast (BCastChannel,bcp->txb_Chunks[req->dh_Chunk],
					 CBYTES);
	/*
	 * Free up this entry.
	 */
		req->dh_Next = RetransFree;
		RetransFree = req;
		nretrans++;
		if ((nretrans % BCBurst) == 0)
			Delay ();
	}
/*
 * No timer request pending.
 */
	msg_ELog (EF_INFO, "%d frames retransmitted", nretrans);
	RetransTreq = -1;
}





void
Delay ()
/*
 * Wait just a bit.
 */
{
	struct itimerval iv;

	sigblock (sigmask (SIGALRM));
	signal (SIGALRM, Alarm);
/*
 * Set up the timer.
 */
	timerclear (&iv.it_interval);
	iv.it_value.tv_sec = 0;
	iv.it_value.tv_usec = 10000;
	setitimer (ITIMER_REAL, &iv, 0);
/*
 * Now we just wait.
 */
	sigpause (0);
}



void
Alarm ()
{
	/* yawn */
}



void
PollBCast (process)
int process;
/* 
 * Poll the receive-only socket if need be.
 */
{
	if (BCReceive > 0)
	{
		Polling = ! process;
		msg_PollBCast (BCReceive);
		Polling = FALSE;
	}
}
