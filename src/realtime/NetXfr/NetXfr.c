/*
 * The data store network transfer daemon.
 */
/*    Copyright (C) 1987,88,89,90,91 by UCAR
 * University Corporation for Atmospheric Research
 *       All rights reserved
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

# include <signal.h>
# include <string.h>
# include <unistd.h>

# include <ui.h>
# include <config.h>
# include <defs.h>
# include <message.h>
# include <timer.h>
# include "DataStore.h"
# include "NetXfr.h"


RCSID("$Id: NetXfr.c,v 3.10 1997-02-14 07:36:57 granger Exp $")


/*
 * Here's the array of stuff we send to people.
 */

# define MAXPLAT CFG_MAX_PLATFORMS

typedef struct _DataRecip
{
	char	dr_Recip[128];		/* Who gets it		*/
	struct _DataRecip *dr_Next;	/* Next in the chain	*/
} DataRecip;

DataRecip *Recipients[MAXPLAT];
char *PlatName[MAXPLAT];

/*
 * Keep track of the fields for every platform.
 */
typedef struct _PFields
{
	int	pf_NField;		/* How many		*/
	FieldId	pf_Fields[MAXFIELD];	/* What they are	*/
} PField;

static PField *PFields[MAXPLAT] = { 0 };


/*
 * The current sequence number.
 */
int Seq = 0;

static int PrintUnk = 5;

int DbEL = EF_DEBUG;
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
int BCInitialWait = 1;	/* How long we wait for missing broadcast packets
			 * before requesting the first retransmit. 	*/
int BCRetransWait = 5;	/* How long before we timeout a retransmit	*/
int BCRetransMax = 2;	/* How many times we will ask for a retransmit
			 * before we lose our patience			*/
int BCBurst = 5;	/* How many packets to blast before waiting	*/
int BCReceive = 0;	/* Receive-only socket				*/
int Polling = FALSE;	/* Are we polling the broadcast socket?		*/
int IPScan = 5;		/* Scan interval in minutes			*/

/*
 * The queue of broadcast packets awaiting everything else.
 */
static DataBCChunk *BCQueue = 0, *PollQueue = 0;
static DataBCChunk *BCFree = 0;		/* Chunk free list	*/

/*
 * Kludge, of sorts.  Maintain one humungo array to accept large, broadcast
 * data chunks, to avoid getting the dynamic memory heap all fragmented,
 * huge, and ugly.
 */
# define BDASIZE (2*1024*1024)
static char BigDataArray[BDASIZE];
static int BDA_IPSeq = -1;	/* Seq which owns bda	*/

/*
 * Stuff for data builds in progress.
 */
typedef struct _InProgress
{
	int		ip_Seq;		/* Sequence number	*/
	PlatformId	ip_Plat;	/* The platform		*/
	DataChunk	*ip_Dc;		/* The data chunk	*/
	char		*ip_Arrived;	/* Indications of arrived packets*/
	char		ip_Source[MAX_NAME_LEN]; /* Who is sending these */
	struct _InProgress *ip_Next;	/* Next in chain	*/
	short		ip_NBCast;	/* # of arrived bcast pkts	*/
	short		ip_NBExpect;	/* # expected			*/
	char		ip_BCast;	/* Broadcast packets coming	*/
	char		ip_Done;	/* DONE packet arrived	*/
	char		ip_RLE;		/* Run length encoding used	*/
	char		ip_NewFile;	/* Start a new file?		*/
	char		ip_Age;		/* How long has it been waiting? */
	short		ip_TReq;	/* Timer request	*/
	short		ip_NRetrans;	/* Number of requests	*/
} InProgress;

InProgress *IPList = 0;


static int	Incoming FP ((Message *));
static void	Die FP ((void));
static int	Dispatcher FP ((int, struct ui_command *));
static void	NewRecipients FP ((struct ui_command *));
static void	DataAvailable FP ((PlatformId, int, ZebTime *, int, UpdCode));
static int	NXMessage FP ((Message *));
static void	SendAux FP ((PlatformId, AuxDataChain));
static DataChunk *GetData FP ((PlatformId, ZebTime *, int));
static void	NewData FP ((char *, DataHdr *));
InProgress	*FindIP FP ((int));
static void	Done FP ((DataDone *));
static void	FinishIP FP ((InProgress *));
static void	UnknownBCast FP ((DataBCChunk *, int));
static void	FindQueued FP ((int));
static void	ZapIP FP ((InProgress *));
static void	Timeout FP ((UItime *, int));
static void	AskRetrans FP ((InProgress *, int));
void		ProcessPolled FP ((void));
void		SendDChunk FP ((DataChunk *, int));
void		SendOut FP ((PlatformId, void *, int));
static inline	DataBCChunk *NewBCChunk FP ((void));
static inline	void FreeBCChunk FP ((DataBCChunk *));
static inline	DataPtr GetDataArray FP ((int, int));
static inline 	void FreeDataArray FP ((int seq, DataPtr array));
static void	NewADE FP ((NxAuxData *));
static void	NewArray FP ((DataArray *));
static void	ScanIP FP ((void));
static int	SendDataArray FP ((PlatformId, DataChunk *));
/*
 * Just define the keywords here.
 */
# define NXC_SEND	1
# define NXC_RUN	2
# define NXC_AS		3
# define NXC_GET	4
# define NXC_BROADCAST	5
# define NXC_RECEIVE	6
# define NXC_DIRIMAGE	7
# define NXC_FIELD	8
# define NXC_ALIAS	9


static char *CFile;

void UglyDeath ()
/*
 * Kludge to keep operations going when things die.
 */
{
	char bindir[256];
	msg_ELog (EF_EMERGENCY, "NETXFR SEG FAULT RESTART\007");
	close (msg_get_fd ());
	ShutdownSeg ();
	strcpy (bindir, GetBinDir ());
	execl (strcat (bindir, "/NetXfr"), CFile, (char *) 0);
	exit (99);
}



static inline DataPtr
GetDataArray (seq, size)
int seq, size;
/*
 * Get the data array.
 */
{
	size += 50000;	/* Memory bug kludge */

	if (size < 100000 || BDA_IPSeq >= 0 || size > BDASIZE)
		return ((DataPtr) malloc (size));
	else
	{
		BDA_IPSeq = seq;
		return ((DataPtr) BigDataArray);
	}
}



static inline void
FreeDataArray (seq, array)
int seq;
DataPtr array;
/*
 * Get the data array.
 */
{
	if (array == NULL)
		return ;
	if ((array == (DataPtr) BigDataArray) || (seq == BDA_IPSeq))
	{
		BDA_IPSeq = -1;
		if ((array != (DataPtr) BigDataArray) || (seq != BDA_IPSeq))
		{
			msg_ELog (EF_PROBLEM, 
				  "%s: mismatched sequence numbers",
				  "release of static data array");
		}
	}
	else
	{
		free (array);
	}
}




static inline DataBCChunk *
NewBCChunk ()
/*
 * Return a free broadcast chunk.
 */
{
	DataBCChunk *ret;

	if (BCFree)
	{
		ret = BCFree;
		BCFree = BCFree->dh_Next;
	}
	else
		ret = (DataBCChunk *) malloc (CBYTES + 32);
	return (ret);
}




static inline void
FreeBCChunk (chunk)
DataBCChunk *chunk;
/*
 * Free up this chunk.
 */
{
	chunk->dh_Next = BCFree;
	BCFree = chunk;
}




int
main (argc, argv)
int argc;
char **argv;
{
	char loadfile[100];
	SValue v;
	stbl vtable;
	int i;
	ZebTime t;
/*
 * Hook into the user interface.  Only go interactive if they didn't
 * give us a file on the command line.
 */
	fixdir_t ("NETXFRLOADFILE", GetLibDir(), "NetXfr.lf", loadfile, ".lf");
	if (argc > 1)
	{
		ui_init (loadfile, FALSE, TRUE);
		v.us_v_ptr = argv[1];
		usy_s_symbol (usy_g_stbl ("ui$variable_table"), "commandfile",
				SYMT_STRING, &v);
	}
	else
		ui_init (loadfile, TRUE, FALSE);
/*
 * Debug level for certain things.
 */
	if (getenv ("NXDEBUG"))
		DbEL = EF_INFO;
/*
 * Set up indirect variables so that the user can do some tweaking.
 */
	vtable = usy_g_stbl ("ui$variable_table");
	usy_c_indirect (vtable, "sequence", &Seq, SYMT_INT, 0);
	usy_c_indirect (vtable, "bcastsave", &BCastSave, SYMT_INT, 0);
	usy_c_indirect (vtable, "initialwait", &BCInitialWait, SYMT_INT, 0);
	usy_c_indirect (vtable, "maxretrans", &BCRetransMax, SYMT_INT, 0);
	usy_c_indirect (vtable, "burst", &BCBurst, SYMT_INT, 0);
	usy_c_indirect (vtable, "ipscan", &IPScan, SYMT_INT, 0);
/*
 * Hook into the message system.
 */
	msg_connect (Incoming, OURNAME);
	msg_AddProtoHandler (MT_NETXFR, NXMessage);
	SetupConfigVariables ();
/*
 * Try to do something reasonable to get a unique starting sequence number.
 */
	tl_Time (&t);
	Seq = getpid () * (t.zt_Sec % 1000);
	msg_ELog (EF_INFO, "Starting seq %d", Seq);
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
 * Kludgery.
 */
# ifdef notdef
	CFile = argv[1];
	signal (SIGSEGV, UglyDeath);
	signal (SIGBUS, UglyDeath);
# endif
/*
 * Start reading commands.
 */
	Pid = getpid ();
	ui_get_command ("initial", "NetXfr>", Dispatcher, 0);
	Die ();
	return (0);
}





static void
Die ()
/*
 * Time to shut down.
 */
{
	ui_finish ();
	ShutdownSeg ();
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
		tl_RelativeReq(ScanIP,0, IPScan*60*INCFRAC, IPScan*60*INCFRAC);
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
	   	DoReceive (UINT (cmds[1]));
		break;
	/*
	 * Direct image data out of the rasterizer.
	 */
	   case NXC_DIRIMAGE:
	   	DirImage (UPTR (cmds[1]));
		break;
	/*
	 * Field definition.
	 */
	   case NXC_FIELD:
	   	F_DeclareField (UPTR (cmds[1]), UPTR (cmds[2]), UPTR(cmds[3]));
		break;
	   case NXC_ALIAS:
	   	F_Alias (UPTR (cmds[1]), UPTR (cmds[2]));
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
		if (strchr (UPTR (*cmds), '@'))
			strcpy (dp->dr_Recip, UPTR (*cmds));
		else
			sprintf (dp->dr_Recip, "NetXfr@%s", UPTR (*cmds));
		dp->dr_Next = Recipients[plat];
		Recipients[plat] = dp;
	}
/*
 * Ask for notifications on this platform.
 */
	ds_RequestNotify (plat, 0, DataAvailable);
}





static void
DataAvailable (plat, junk, t, ns, ucode)
PlatformId plat;
int junk, ns;
ZebTime *t;
UpdCode ucode;
/*
 * Data for which somebody is interested has arrived.
 */
{
	DataChunk *dc;
	ZebTime otimes, dctime;
	int newfile;
	Location loc;
/*
 * First thing we need to do is to get this data.
 */
	if (! (dc = GetData (plat, t, ns)))
		return;
/*
 * For now, we handle the "newfile" problem by getting the first in
 * the list of obs samples, and seeing if it matches our time.
 *
 * Eventually do this with attributes?  Or perhaps with an enhanced
 * data-available notification callback...
 */
	dc_GetTime (dc, 0, &dctime);
	newfile = (ds_GetObsSamples (plat, t, &otimes, &loc, 1) > 0) &&
			TC_Eq (otimes, dctime);
	SendDChunk (dc, newfile);
	dc_DestroyDC (dc);
}





void
SendDChunk (dc, newfile)
DataChunk *dc;
int newfile;
/*
 * Ship out  a chunk of data.
 */
{
	DataHdr dhdr;
	DataDone done;
	AuxDataChain ade;
	PlatformId plat = dc->dc_Platform;
	int i, j;
/*
 * First prepare the datachunk for transmission.
 */
	dc_Serialize (dc);
/*
 * Create and send out the data header.
 */
	dhdr.dh_NewFile = newfile;
	dhdr.dh_MsgType = NMT_DataHdr;
	dhdr.dh_DataSeq = ++Seq;
	strcpy (dhdr.dh_Platform, PlatName[plat]);
	dhdr.dh_DChunk = *dc;
	dhdr.dh_BCast = Broadcast;
	dhdr.dh_BCRLE = Broadcast && dc->dc_Class == DCC_Image;
	SendOut (plat, &dhdr, sizeof (dhdr));
/*
 * Send out the auxdata entries.
 */
	for (i = 0; i < ADE_DCC_LEVELS; ++i)
		for (j = 0; j < ADE_HASH_SIZE; ++j)
		{
		   for (ade = dc->dc_AuxData[i][j]; ade; ade = ade->dca_Next)
			SendAux (plat, ade);
		}
/*
 * Send over the data itself.
 */
	done.dh_NBSent = SendDataArray (plat, dc);
/*
 * Done at last.
 */
	done.dh_MsgType = NMT_DataDone;
	done.dh_DataSeq = Seq;
	SendOut (plat, &done, sizeof (done));
}






static int
SendDataArray (plat, dc)
PlatformId plat;
DataChunk *dc;
/*
 * Send out the data array for this data chunk.
 */
{
	unsigned char *dp = (unsigned char *) dc->dc_Data;
	unsigned char *begin = dp;
	int nsent = 0;
	DataArray out;
/*
 * If we are broadcasting, we just hand it off.
 */
	if (Broadcast)
		return (DoBCast (plat, dc));
/*
 * Initialize the array.
 */
	out.dh_MsgType = NMT_DataArray;
	out.dh_DataSeq = Seq;
	out.dh_Offset = 0;
/*
 * Now plow through and send it all.
 */
	while (nsent < dc->dc_DataLen)
	{
	/*
	 * Get the array segment ready.
	 */
		if ((out.dh_Len = dc->dc_DataLen - nsent) > DTSIZE)
			out.dh_Len = DTSIZE;
		out.dh_Offset = dp - begin;
		memcpy (out.dh_Data, dp, out.dh_Len);
	/*
	 * Ship it out and move on.
	 */
		SendOut (plat, &out, sizeof (out) - (DTSIZE - out.dh_Len));
		nsent += out.dh_Len;
		dp += out.dh_Len;
	}
	return (0);
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
SendAux (plat, ade)
PlatformId plat;
AuxDataChain ade;
/*
 * Send an aux data chain entry.
 */
{
	static char *buf = 0;
	static int buflen = -1;
	static NxAuxData *nxa;
	int len = sizeof (NxAuxData) + ade->dca_Len - 1;
/*
 * Make sure our buffer is big enough.
 */
	if (len > buflen)
	{
		if (buflen > 0)
			free (buf);
		buf = malloc (len);
		buflen = len;
		nxa = (NxAuxData *) buf;
	}
/*
 * Fill in the packet info.
 */
	nxa->dh_MsgType = NMT_AuxData;
	nxa->dh_DataSeq = Seq;
	nxa->dh_Ade = *ade;
	memcpy (nxa->dh_Stuff, ade->dca_Data, ade->dca_Len);
/*
 * Send it.
 */
	SendOut (plat, buf, len);
}




static DataChunk *
GetData (plat, t, ns)
PlatformId plat;
ZebTime *t;
int ns;
/*
 * Grab the data for this info.
 */
{
	ZebTime *times, begin;
	int ntime;
	PField *pf;
	DataClass class;
/*
 * Allocate a time array, then find the sample times for the new data.
 */
	if (ns > 1)
	{
		times = (ZebTime *) malloc (ns * sizeof (ZebTime));
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
	if (! (pf = PFields[plat]))
	{
		pf = PFields[plat] = ALLOC (PField);
		pf->pf_NField = MAXFIELD;
		ds_GetFields (plat, t, &pf->pf_NField, pf->pf_Fields);
	}
/*
 * Figure out what class of DC we want.
 */
	switch (ds_PlatformDataOrg (plat))
	{
	   case OrgCmpImage:	/* Hopefully separate someday */
	   case OrgImage:	class = DCC_Image; break;
	   case Org1dGrid:
	   case Org2dGrid:
	   case Org3dGrid:	class = DCC_RGrid; break;
	   case OrgIRGrid:	class = DCC_IRGrid; break;
	   case OrgScalar:	class = DCC_Scalar; break;
	   case OrgNSpace:	class = DCC_NSpace; break;
	   case OrgOutline:	class = DCC_Boundary; break;
	   default:
		msg_ELog (EF_PROBLEM, "plat %d, org not handled", plat);
		return (NULL);
	};
/*
 * Get the data.
 */
	return (ds_Fetch (plat, class, &begin, t, pf->pf_Fields, pf->pf_NField,
			0, 0));
}










static int 
Incoming (msg)
Message *msg;
/*
 * Deal with an incoming message.
 */
{
	struct mh_template *tmpl = (struct mh_template *) msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (tmpl->mh_type == MH_DIE || tmpl->mh_type == MH_SHUTDOWN)
			Die ();
		else
			msg_ELog (EF_PROBLEM, "Weird MH msg %d",tmpl->mh_type);
		break;

	   case MT_IMAGEXFR:
		DirImageAvail (tmpl->mh_type);
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
	ProcessBCasts ();
	switch (tmpl->dh_MsgType)
	{
	/*
	 * New data coming in.
	 */
	   case NMT_DataHdr:
	   	NewData (msg->m_from, (DataHdr *) tmpl);
		break;

	/*
	 * An aux data chain entry.
	 */
	   case NMT_AuxData:
	   	NewADE ((NxAuxData *) tmpl);
		break;

	/*
	 * Data coming in straight through the msg system.
	 */
	   case NMT_DataArray:
	   	NewArray ((DataArray *) tmpl);
		break;

	/*
	 * Done with data.
	 */
	   case NMT_DataDone:
	   	Done ((DataDone *) tmpl);
		break;

	/*
	 * A retransmission request, alas.
	 */
	   case NMT_Retransmit:
	   	Retransmit ((DataRetransRq *) tmpl);
		break;

	/*
	 * Packet grabber trying to get our attention.
	 */
	   case NMT_WakeUp:
	   	break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown data proto type: %d",
				tmpl->dh_MsgType);
		break;
	}
	ProcessBCasts ();
	return (0);
}




static void
NewData (from, hdr)
char *from;
DataHdr *hdr;
/*
 * A new data stream is beginning.
 */
{
	InProgress *ip = ALLOC (InProgress);

	msg_ELog (DbEL, "Begin data %s from %s, seq %d",
		hdr->dh_Platform, from, hdr->dh_DataSeq);
/*
 * Sanity check.
 */
	if (FindIP (hdr->dh_DataSeq))
	{
		msg_ELog (EF_EMERGENCY, "DUP seq number %d from %s", 
			hdr->dh_DataSeq, from);
		return;
	}
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
	ip->ip_Arrived = 0;
	ip->ip_Done = FALSE;
	ip->ip_Age = 0;
	ip->ip_NewFile = hdr->dh_NewFile;
	strcpy (ip->ip_Source, from);
/*
 * Fill in the data chunk, and go ahead and get the data array.
 */
	ip->ip_Dc = ALLOC (DataChunk);
	*ip->ip_Dc = hdr->dh_DChunk;
	ip->ip_Dc->dc_Platform = ip->ip_Plat;
	dc_ClearADE (ip->ip_Dc);
	ip->ip_Dc->dc_Data = (DataPtr) GetDataArray (ip->ip_Seq,
					ip->ip_Dc->dc_DataLen);
/*
 * Add it to the list
 */
	ip->ip_Next = IPList;
	IPList = ip;
/*
 * BCast stuff.
 */
	if ((ip->ip_BCast = hdr->dh_BCast))
	{
		ip->ip_NRetrans = ip->ip_NBCast = 0;
		ip->ip_NBExpect = 1; /* Expect at least this many	*/
		ip->ip_RLE = hdr->dh_BCRLE;
		FindQueued (ip->ip_Seq);
	}
}





static void
NewADE (nxa)
NxAuxData *nxa;
/*
 * Here's a new aux data entry.
 */
{
	AuxDataEntry *ade = &(nxa->dh_Ade);
	DataPtr data;
	InProgress *ip;
/*
	msg_ELog (EF_DEBUG, "ADE, seq %d, len %d, class %d sub %d", 
		nxa->dh_DataSeq, nxa->dh_Ade.dca_Len, 
		nxa->dh_Ade.dca_Class, nxa->dh_Ade.dca_SubType);
*/
/*
 * Find this transfer.
 */
	if (! (ip = FindIP (nxa->dh_DataSeq)))
		return;
/*
 * Create a local copy of the data
 */
	data = (DataPtr) malloc (ade->dca_Len);
	memcpy (data, nxa->dh_Stuff, ade->dca_Len);
/*
 * Add this ade and the data by passing the fields from the ade
 */
	dc_AddADE (ip->ip_Dc, data, dc_ClassP(ade->dca_ClassId), 
		   ade->dca_SubType, ade->dca_Len, ade->dca_Free);
}





static void
NewArray (array)
DataArray *array;
/*
 * A piece of data has arrived.
 */
{
	InProgress *ip;

/*
	msg_ELog (EF_DEBUG, "Data array, seq %d, off %d len %d", 
		array->dh_DataSeq, array->dh_Offset, array->dh_Len);
*/
/*
 * Find this transfer.
 */
	if (! (ip = FindIP (array->dh_DataSeq)))
		return;
/*
 * Just copy the data over.
 */
	memcpy (((char *) ip->ip_Dc->dc_Data) + array->dh_Offset,
		array->dh_Data, array->dh_Len);
}






static void
ScanIP ()
/*
 * Search the IP list for old stuff.
 */
{
	InProgress *ip;
	DataBCChunk *chunk, *last;
	int nzapped = 0, nleft = 0;
/*
 * Go through the IP list, and increment all of the scan flags.  If a
 * particular one has been seen before, we clean it up.
 */
	for (ip = IPList; ip; ip = ip->ip_Next)
		if (ip->ip_Age++)
		{
			msg_ELog (EF_PROBLEM, "Old IP, seq %d", ip->ip_Seq);
			ZapIP (ip);
		}
/*
 * Do the same thing with queued broadcast packets.  Start with the head
 * of the list.
 */
	for (chunk = BCQueue; chunk && chunk->dh_ID; chunk = BCQueue)
	{
		BCQueue = chunk->dh_Next;
		/* 	free (chunk); */
		FreeBCChunk (chunk);
		nzapped++;
	}
/*
 * Now go inside (where the old ones really will be), increment counts, and
 * zap things.
 */
	chunk = last = BCQueue;
	while (chunk)
	{
	/*
	 * Increment the counter and see if this one is too old.
	 */
		if (chunk->dh_ID++)
		{
			nzapped++;
			last->dh_Next = chunk->dh_Next;
			/* free (chunk); */
			FreeBCChunk (chunk);
		}
		else
		{
			last = chunk;
			nleft++;
		}
	/*
	 * Move on.
	 */
		chunk = last->dh_Next;
	}
	if (nzapped)
		msg_ELog (EF_DEBUG,"%d old bc chunks zapped %d left", nzapped, 
			nleft);
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
	if (chunk->dh_MsgType != NMT_DataBCast &&
			chunk->dh_MsgType != NMT_DataBRetrans)
	{
		msg_ELog (EF_PROBLEM, "Funky msg type %d bcast",
			chunk->dh_MsgType);
		return (0);
	}
/*
 * If we are currently polling, we just set this one aside.
 * 5/92 jc So far as I know the following is not used for anything.
 */
	if (Polling)
	{
		DataBCChunk *save = (DataBCChunk *) malloc (CBYTES);
		memcpy (save, data, len);
		save->dh_Next = PollQueue;
		PollQueue = save;
		msg_ELog (EF_INFO, "Saving polled pkt");
		return (0);
	}
/*
 * Look for this IP.  If we don't find it, we need to go to some more effort
 * to decide what the hell to do with this thing.
 */
	if (! (ip = FindIP (chunk->dh_DataSeq)))
	{
		if (chunk->dh_MsgType != NMT_DataBRetrans)
			UnknownBCast (chunk, len);
		return (0);
	}
/*
 * If this is a retransmit, see if it's one we need.
 */
	if (chunk->dh_MsgType == NMT_DataBRetrans && ip->ip_Arrived &&
			ip->ip_Arrived[chunk->dh_Chunk])
		return (0);
/*
 * If this is the first broadcast packet, do some setup.
 */
	if (! ip->ip_Arrived)
	{
		ip->ip_Arrived = malloc (chunk->dh_NChunk);
		memset (ip->ip_Arrived, 0, chunk->dh_NChunk);
		ip->ip_NBCast = 0;
		ip->ip_NBExpect = chunk->dh_NChunk;
	}
/*
 * Mark this packet as arrived, and copy over the stuff.
 */
	ip->ip_Arrived[chunk->dh_Chunk] = 1;
	ip->ip_NBCast++;
	if (ip->ip_RLE)
		RL_Decode (chunk->dh_Offset +
				((unsigned char *) ip->ip_Dc->dc_Data),
				(unsigned char *) chunk->dh_data,
				chunk->dh_Size);
	else
		memcpy (chunk->dh_Offset + ((char *) (ip->ip_Dc->dc_Data)), 
			chunk->dh_data, chunk->dh_Size);
/*
 * If everything is done, finish it out.
 */
	if (ip->ip_Done && ip->ip_NBCast >= ip->ip_NBExpect)
		FinishIP (ip);
	return (0);
}








void
ProcessPolled ()
/*
 * Deal with packets we accumulated while doing other things.
 */
{
	DataBCChunk *chunk;

	while (PollQueue)
	{
	/*
	 * Remove from the queue.
	 */
		chunk = PollQueue;
		PollQueue = chunk->dh_Next;
	/*
	 * Dispatch it.
	 */
		BCastHandler (0, (char *) chunk, CBYTES);
	}
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
		return;
	if (PrintUnk-- > 0)
		msg_ELog (EF_DEBUG, "Unk queued, seq %d", chunk->dh_DataSeq);
/*
 * Otherwise we enqueue it, waiting for the header info to arrive.  Use the
 * ID Flag for scanning, now that the above check is done.
 */
	/* new = (DataBCChunk *) malloc (len); */
	new = NewBCChunk ();
	memcpy (new, chunk, len);
	new->dh_Next = BCQueue;
	new->dh_ID = 0;
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
		FreeBCChunk (chunk);
		/* free (chunk); */
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
			FreeBCChunk (chunk);
			/* free (chunk); */
		}
		else
			last = chunk;
		chunk = last->dh_Next;
	}
}





static void 
Done (done)
DataDone *done;
/*
 * Finish out this chunk of data.
 */
{
	InProgress *ip = FindIP (done->dh_DataSeq);
/*
 * Make sure we know about this sequence.
 */
	if (! ip)
		return;
/*
 * If we have all of the data, we can finish this thing out now.
 */
	ip->ip_NBExpect = done->dh_NBSent;
	if (! ip->ip_BCast || ip->ip_NBCast >= ip->ip_NBExpect)
	{
		FinishIP (ip);
		return;
	}
/*
 * Otherwise, we wait a little longer before complaining.
 */
	msg_ELog (EF_INFO, "IP %d DONE, but %d segs (of %d) missing",
		ip->ip_Seq, ip->ip_NBExpect - ip->ip_NBCast, ip->ip_NBExpect);
	if (ip->ip_NBExpect - ip->ip_NBCast > 120)
	{
		msg_ELog (EF_INFO, "I give up on %d", ip->ip_Seq);
		ZapIP (ip);
	}
	else
	{
		ip->ip_Done = TRUE;
		ip->ip_TReq = tl_RelativeReq (Timeout, (void *) ip->ip_Seq, 
			BCInitialWait*INCFRAC, 0);
	}
}




static void
Timeout (t, seq)
UItime *t;
int seq;
/*
 * This is the retransmit timeout routine.
 */
{
	InProgress *ip;
	int ch;
/*
 * If we don't find our InProgress structure, that can only mean that
 * the data arrived and it was flushed out.  So we can happily just quit.
 */
	PrintDrops ();
	PollBCast (TRUE);
	ip = FindIP (seq);
	if (! ip)
		return;
/*
 * If we have exceeded the number of timeouts we are willing to deal with,
 * we give up on this.  If any data has arrived at all, finish out the IP
 * to preserve it; otherwise just dump it.
 */
	if (++(ip->ip_NRetrans) > BCRetransMax)
	{
		msg_ELog (EF_INFO, "Too many timeouts on %d (%d missing)",
			seq, ip->ip_NBExpect - ip->ip_NBCast);
#ifdef notdef
		/* trying to store an incomplete chunk is likely to crash us */
		if (ip->ip_NBCast > 0)
			FinishIP (ip);
		else
#endif
			ZapIP (ip);
		return;
	}
/*
 * We've not yet exhausted our patience.  Go through and ask for
 * retransmits on everything we lack.  This code is suboptimal, in that it
 * can flood the world with retransmit requests if there have been a lot
 * of drops; fortunately that does not happen very often.
 */
	if (ip->ip_NBCast == 0)
		AskRetrans (ip, 0);
	else
	{
		for (ch = 0; ch < ip->ip_NBExpect; ch++)
			if (! ip->ip_Arrived[ch])
				AskRetrans (ip, ch);
	}
/*
 * Schedule a new timer request on this IP.
 */
	ip->ip_TReq = tl_RelativeReq (Timeout, (void *) seq,
			BCRetransWait*INCFRAC, 0);
}




static void
AskRetrans (ip, chunk)
InProgress *ip;
int chunk;
/*
 * Ask to have this chunk retransmitted.
 */
{
	DataRetransRq req;
# ifdef BCLOG
	msg_ELog (EF_INFO, "Beg retrans of seq %d ch %d from '%s'", ip->ip_Seq,
		chunk, ip->ip_Source);
# endif

	req.dh_MsgType = NMT_Retransmit;
	req.dh_DataSeq = ip->ip_Seq;
	req.dh_Chunk = chunk;
	msg_send (ip->ip_Source, MT_NETXFR, 0, &req, sizeof (req));
}




static void
FinishIP (ip)
InProgress *ip;
/*
 * This one is done.
 */
{
	ZebTime zt;
	char atime[30];
/*
 * Throw this data into the data store.
 */
	dc_Localize (ip->ip_Dc);
	dc_GetTime (ip->ip_Dc, 0, &zt);
	TC_EncodeTime (&zt, TC_Full, atime);
	msg_ELog (DbEL, "Store sequence %d ns %d at %s", ip->ip_Seq, 
		dc_GetNSample (ip->ip_Dc), atime);
	ds_Store (ip->ip_Dc, ip->ip_NewFile, 0, 0);
	dc_DestroyDC (ip->ip_Dc);
	ip->ip_Dc = NULL;
	ZapIP (ip);
}




static void
ZapIP (ip)
InProgress *ip;
{
	InProgress *zap;
	int seq = ip->ip_Seq;
/*
 * Free up dynamic storage.
 */
	if (seq == BDA_IPSeq)
	{
		ip->ip_Dc->dc_DataLen = 0;
		BDA_IPSeq = -1;
	}
/* 
 * Check for the remains of a partial datachunk; it must be destroyed
 * carefully since it has not been localized.
 */
	/* dc_DestroyDC (ip->ip_Dc); */
	if (ip->ip_Dc)
	{
		dc_DestroyADE (ip->ip_Dc);
		FreeDataArray (ip->ip_Seq, ip->ip_Dc->dc_Data);
		free (ip->ip_Dc);
		ip->ip_Dc = NULL;
	}
	if (ip->ip_Arrived)
		free (ip->ip_Arrived);
/*
 * Clear this entry out of the inprogress list.
 */
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


