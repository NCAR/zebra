/*
 * Packet grabber code and interface.
 */
static char *rcsid = "$Id: nx_PktGrabber.c,v 1.2 1991-06-14 22:17:36 corbet Exp $";

# include <errno.h>
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/shm.h>
# include <sys/time.h>
# include <sys/resource.h>
# include <signal.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include "NetXfr.h"

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
	unsigned char sm_Data[0];	/* The data begins here		*/
};



/*
 * The actual shared memory segment.
 */
# define NPACKET	1024
# define PKTSIZE	1500
static struct ShmHeader * volatile Seg = 0;
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




# ifdef __STDC__
	static void	CreateChild (void);
	static void	CreateShmSeg (void);
	static void	ChildMain (void);
# else
# endif

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
		CreateChild ();
/*
 * Now we fire off a message telling them to connect to this port.
 */
	sleep (5);
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
	char *cseg, *shmat ();
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
	mlock (cseg, len);
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
# ifdef __STDC__
	static int ChMsgHandler (Message *);
	static int ChNXMessage (Message *);
	static void HUP (void);
# else
# endif


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
	setpriority (PRIO_PROCESS, 0, -19);
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
		if (tmpl->mh_type == MH_DIE)
			exit (0);
		else
			msg_ELog (EF_PROBLEM, "Weird MH msg %d",tmpl->mh_type);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown msg proto %d", msg->m_proto);
		break;
	}
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
		msg_ELog (EF_INFO, "Opening port %d", ((NewPort *) tm)->dh_Port);
	   	ReceiveSetup (((NewPort *) tm)->dh_Port);
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




static void
HUP ()
{
	msg_ELog (EF_INFO, "Hanging up");
	exit (1);
}
