/*
 * Packet grabber code and interface.
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

# include <errno.h>
# include <unistd.h>
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/shm.h>
# include <sys/time.h>
# include <sys/resource.h>
# include <signal.h>

# include <ui.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include "NetXfr.h"

RCSID("$Id: nx_PktGrabber.c,v 3.9 2006-01-25 05:33:36 granger Exp $")

/*
 * The header of our shared memory segment.
 */
struct ShmHeader
{
	int	sm_NPacket;		/* How many packets we can hold	*/
	int	sm_PktSize;		/* At which size		*/
	int	sm_FirstFull;		/* Index of first full pkt	*/
	int	sm_LastFull;		/* Index of first empty pkt	*/
	int	sm_NDrop;		/* How many dropped		*/
	unsigned char sm_Data[1];	/* The data begins here		*/
};



/*
 * The actual shared memory segment.
 */
# define NPACKET	1024
# define PKTSIZE	1500
# ifdef __STDC__
	static struct ShmHeader * volatile Seg = 0;
# else
	static struct ShmHeader *Seg = 0;
# endif

static int ShmId = 0;
# define SHMKEY 0x910610

# define WAKEUP		100	/* How many pkts before child pokes parent */


static inline int
IncrIndex (index)
int index;
/*
 * Increment a SHM index.
 */
{
	return (index == (Seg->sm_NPacket - 1) ? 0 : index + 1);
}

/*
 * The name of the packet grabber process.
 */
# define CHILDNAME	"NetXfr-PG"
static int ChildPid;


static void	CreateChild FP((void));
static void	CreateShmSeg FP((void));
static void	ChildMain FP((void));


/*
 * All code here is NetXfr parent stuff.  Child code begins after the
 * comment below.
 */


void
DoReceive (port)
int port;
/*
 * Set up to receive broadcasts on this port.
 */
{
	NewPort np;
/*
 * First, make sure that everything has been set up.
 */
	if (! Seg)
	{
		CreateChild ();
		sleep (3);
	}
/*
 * Now we fire off a message telling them to connect to this port.
 */
	np.dh_MsgType = NMT_NewPort;
	np.dh_Port = port;
	msg_send (CHILDNAME, MT_NETXFR, FALSE, &np, sizeof (np));
}





static void
CreateChild ()
/*
 * Get the packet grabber process going.
 */
{
/*
 * We need a shared memory segment.
 */
	CreateShmSeg ();
/*
 * Now we fork off the child process.
 */
	if ((ChildPid = fork ()) == 0)
		ChildMain ();
}





static void
CreateShmSeg ()
/*
 * Create and initialize the shared memory segment.
 */
{
	int len = sizeof (struct ShmHeader) + NPACKET*PKTSIZE - 1;
	char *cseg;
/*
 * Make the segment exist.
 */
	if ((ShmId = shmget (SHMKEY, len, IPC_CREAT | 0777)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "Shmget err %d", errno);
		exit (1);
	}
/*
 * Make it so we can talk to it.
 */
	if ((cseg = shmat (ShmId, 0, 0)) == (char *) -1)
	{
		msg_ELog (EF_EMERGENCY, "Shmat err %d", errno);
		exit (1);
	}
	Seg = (struct ShmHeader *) cseg;
# ifdef sun
/*
 * Try to lock the segment into real memory.  If this is unavailable or
 * doesn't work, lots of packets are likely to drop on the floor during
 * page faults.
 */
	mlock (cseg, len);
# endif
/*
 * Fill it in.
 */
	Seg->sm_NPacket = NPACKET;
	Seg->sm_PktSize = PKTSIZE;
	Seg->sm_FirstFull = Seg->sm_LastFull = 0;
}





void
ProcessBCasts ()
/*
 * Go through and deal with all of the broadcast packets.
 */
{
	int slot;

	if (! Seg)
		return;
/*
 * Deal with every one.
 */
	while (Seg->sm_FirstFull != Seg->sm_LastFull)
	{
		slot = IncrIndex (Seg->sm_FirstFull);
		BCastHandler (0, (char *)Seg->sm_Data + slot*PKTSIZE, PKTSIZE);
		Seg->sm_FirstFull = slot;
	}
}


void
PrintDrops ()
{
	if (Seg && Seg->sm_NDrop)
	{
		msg_ELog (EF_INFO, "  (%d pkts dropped)", Seg->sm_NDrop);
		Seg->sm_NDrop = 0;
	}
}




void
ShutdownSeg ()
/*
 * Clean up.
 */
{
	if (! Seg)
		return;
	kill (ChildPid, SIGHUP);
	shmdt ((char *) Seg);
	shmctl (ShmId, IPC_RMID, 0);
}
	



/*******************
 *
 * Child code below here.
 *
 *******************/


/*
 * Child forwards.
 */
static int ChMsgHandler FP((Message *));
static int ChNXMessage FP((Message *));
static void HUP FP((int));


static void
ChildMain ()
/*
 * The "main program" for the child process.
 */
{
/*
 * Reconnect to the message system by closing the FD we inherited from
 * the parent, and starting over.
 */
	close (msg_get_fd ());
	msg_connect (ChMsgHandler, CHILDNAME);
	msg_ELog (EF_INFO, "I exist!");
	msg_AddProtoHandler (MT_NETXFR, ChNXMessage);
#ifndef SVR4
/*	priocntl(P_PID, PRIO_PROCESS,              */
	setpriority (PRIO_PROCESS, 0, -19);
#endif
/*
 * Wait for things to happen.
 */
	signal (SIGHUP, HUP);
	signal (SIGINT, HUP);
	msg_await ();
}




static int
ChMsgHandler (msg)
Message *msg;
/*
 * The message dispatcher.
 */
{
	struct mh_template *tmpl;

	switch (msg->m_proto)
	{
	   /* 
	    * Deal with shutdown demands.
	    */
	   case MT_MESSAGE:
	   	tmpl = (struct mh_template *) msg->m_data;
		if (tmpl->mh_type == MH_DIE || tmpl->mh_type == MH_SHUTDOWN)
			exit (0);
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
ChNXMessage (msg)
Message *msg;
/*
 * Deal with an internal protocol message.
 */
{
	DataTemplate *tm = (DataTemplate *) msg->m_data;

	switch (tm->dh_MsgType)
	{
	/*
	 * Connection requests.
	 */
	   case NMT_NewPort:
		msg_ELog(EF_INFO,"Opening port %d", ((NewPort *) tm)->dh_Port);
	   	ReceiveSetup (((NewPort *) tm)->dh_Port);
		break;
	   default:
		/* not handling any others */
		break;
	}
	return (0);
}




int
ReadBCast (port, data, len)
int port, len;
char *data;
/*
 * Actually deal with a broadcast packet.
 */
{
	int slot = IncrIndex (Seg->sm_LastFull), npacket;
/*
 * Find a free slot.
 */
	if (slot == Seg->sm_FirstFull)	/* Buffer full		*/
	{
		Seg->sm_NDrop++;
		return (0);		/* Drop		*/
	}
/*
 * Copy in the data, and increment the pointer.
 */
	memcpy (Seg->sm_Data + slot*PKTSIZE, data, len > PKTSIZE ? 
				PKTSIZE : len);
	Seg->sm_LastFull = slot;
/*
 * If we've passed the full threshold, poke the parent and get them
 * reading again.
 */
	npacket = (Seg->sm_FirstFull > Seg->sm_LastFull) ?
		Seg->sm_LastFull + Seg->sm_NPacket - Seg->sm_FirstFull :
		Seg->sm_LastFull - Seg->sm_FirstFull;
	if (npacket == WAKEUP)
	{
		DataTemplate wakeup;
		wakeup.dh_MsgType = NMT_WakeUp;
		msg_send (OURNAME, MT_NETXFR, FALSE, &wakeup, sizeof (wakeup));
	}
	return (0);
}




/*ARGSUSED*/
static void
HUP (dummy)
int dummy;
{
	msg_ELog (EF_INFO, "Hanging up");
	exit (1);
}
