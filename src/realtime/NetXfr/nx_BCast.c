/*
 * Handling of broadcast stuff.
 */
static char *rcsid = "$Id: nx_BCast.c,v 1.1 1991-06-06 03:48:31 corbet Exp $";


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
	if (msg_BCSetup (0, port, BCastHandler) < 0)
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
	offsets.dh_DataSeq = Seq;
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
	chunk->dh_ID = Pid;
	msg_ELog (EF_DEBUG, "BCast in %d chunks of %d", chunk->dh_NChunk,
		chunk->dh_Size);
/*
 * Now we blast them out.
 */
	for (; chunk->dh_Chunk < chunk->dh_NChunk - 1; (chunk->dh_Chunk)++)
	{
		memcpy (chunk->dh_data, cdata, chunk->dh_Size);
		msg_BCast (BCastChannel, chunk, CBYTES);
		cdata += chunk->dh_Size;
		chunk->dh_Offset += chunk->dh_Size;
	}
/*
 * Don't forget the last one.
 */
	chunk->dh_Size = dobj->do_nbyte - chunk->dh_Offset;
	memcpy (chunk->dh_data, cdata, chunk->dh_Size);
	msg_BCast (BCastChannel, chunk, CBYTES);
/*
 * Free up and we're done.
 */
	free (chunk);
}





