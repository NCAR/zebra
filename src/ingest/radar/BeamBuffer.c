/*
 * The shared memory beam buffer scheme.
 */
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/shm.h>
# include <sys/sem.h>
# include <errno.h>

# include <defs.h>
# include <message.h>
# include "BeamBuffer.h"

MAKE_RCSID ("$Id: BeamBuffer.c,v 2.4 1998-10-28 21:22:32 corbet Exp $")

/*
 * The beginning of our SHM segment has one of these.
 */
struct BBHeader
{
	int	bbh_Magic;		/* The magic number	*/
	int	bbh_Read;		/* Read pointer		*/
	int	bbh_Write;		/* Write pointer	*/
	int	bbh_NBeam;		/* How many beams	*/
	int	bbh_BSize;		/* How big they are	*/
	int	bbh_Sizes;		/* Offset to size array */
	int	bbh_Beams;		/* Offset to the beams array */
	int	bbh_NDrop;		/* How many dropped beams */
};

# define BB_MAGIC 0x19950502


/*
 * Pointers into the shared memory segment and other goodies.
 */
static volatile struct BBHeader *Header = 0;
static unsigned short *Sizes;
static unsigned char *Beams;

int Shm_ID = -1;
char *ShmSegment;
int Sem_ID;


/*
 * Local routines.
 */
static void BB_SetSemaphore FP ((int));
static int BB_BeamWait FP ((void));
static void BB_Detach FP ((int));




static inline int
BB_CPlus (v)
int v;
/*
 * Do a circular-buffer increment on v.
 */
{
	return ((++v >= Header->bbh_NBeam) ? 0 : v);
}







int
BB_Setup (key, bsize, nbeam)
int key, bsize, nbeam;
/*
 * Set up the shared memory beam buffer.
 * Entry:
 *	KEY	is the SHM/Semaphore key to use.
 *	BSIZE	is the size of each beam slot
 *	NBEAM	is the number of beams to buffer.
 * Exit:
 *	Return value TRUE iff the setup was successful.
 */
{
	int size;
/*
 * Detach from the old stuff if we already have a segment
 */
	if (Shm_ID >= 0)
		BB_Detach (1);
/*
 * Figure our segment size and make a segment.
 */
	size = sizeof (struct BBHeader) + nbeam*sizeof (unsigned short) +
		nbeam*bsize;
	if ((Shm_ID = shmget (key, size, IPC_CREAT + 0777)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "Error %d in shmget", errno);
		return (FALSE);
	}
/*
 * Get attached to the segment.
 */
	if ((ShmSegment = shmat (Shm_ID, 0, 0)) == (char *) -1)
	{
		msg_ELog (EF_EMERGENCY, "Error %d in shmat", errno);
		return (FALSE);
	}
/*
 * Nail this baby into memory.  We have lots of memory, right?
 */
	if (shmctl (Shm_ID, SHM_LOCK, 0) < 0)
		msg_ELog (EF_PROBLEM, "Error %d in shm LOCK", errno);
/*
 * Fill in the header.
 */
	Header = (struct BBHeader *) ShmSegment;
	Header->bbh_Read = Header->bbh_Write = 0;	/* Empty buffer */
	Header->bbh_NBeam = nbeam;
	Header->bbh_BSize = bsize;
	Header->bbh_Sizes = sizeof (struct BBHeader);
	Header->bbh_Beams = Header->bbh_Sizes + nbeam*sizeof (unsigned short);
	Header->bbh_NDrop = 0;
	Header->bbh_Magic = BB_MAGIC;
/*
 * Fix up the other pointers.
 */
	Sizes = (unsigned short *) (ShmSegment + Header->bbh_Sizes);
	Beams = (unsigned char *)ShmSegment + Header->bbh_Beams;
/*
 * Get a semaphore too.
 */
	if ((Sem_ID = semget (key, 1, IPC_CREAT | 0777)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "Error %d in semget", errno);
		return (FALSE);
	}
	BB_SetSemaphore (1);	/* No data yet */
/*
 * All done.
 */
	return (TRUE);
}






static void
BB_SetSemaphore (value)
int value;
/*
 * Set our semaphore to this value.
 */
{
	union semun su;
	su.val = value;
	if (semctl (Sem_ID, 0, SETVAL, su))
		msg_ELog (EF_PROBLEM, "Error %d setting semaphore", errno);
}




unsigned char *
BB_GetWriteBuffer ()
/*
 * Return the place where the next beam should be written.
 */
{
	return (Beams + Header->bbh_Write*Header->bbh_BSize);
}





void
BB_WriteDone (len)
int len;
/*
 * Indicate that the current write buffer is complete.
 */
{
	Sizes[Header->bbh_Write] = len;
	if (BB_CPlus (Header->bbh_Write) == Header->bbh_Read)
		Header->bbh_NDrop++;
	else
		Header->bbh_Write = BB_CPlus (Header->bbh_Write);
	BB_SetSemaphore (0);
}





int
BB_Attach (key)
int key;
/*
 * Attempt to attach to the SHM segment.
 */
{
/*
 * Get the ID.
 */
	if ((Shm_ID = shmget (key, 0, 0)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d getting shmseg", errno);
		return (FALSE);
	}
/*
 * Attach.
 */
	if ((ShmSegment = shmat (Shm_ID, 0, 0)) == (char *) -1)
	{
		msg_ELog (EF_PROBLEM, "Error %d in shmat", errno);
		return (FALSE);
	}
/*
 * Initialize pointers.
 */
	Header = (struct BBHeader *) ShmSegment;
	Sizes = (unsigned short *) (ShmSegment + Header->bbh_Sizes);
	Beams = (unsigned char *) ShmSegment + Header->bbh_Beams;
	if (Header->bbh_Magic != BB_MAGIC)
	{
		msg_ELog (EF_PROBLEM, "BB Header magic 0x%x",
				Header->bbh_Magic);
		BB_Detach (FALSE);
		return (FALSE);
	}
/*
 * Get the semaphore too.
 */
	if ((Sem_ID = semget (key, 1, 0)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d in semget", errno);
		BB_Detach (FALSE);
		return (FALSE);
	}
/*
 * Set the read pointer for an empty buffer; what's in there, if anything,
 * will be old junk anyway.
 */
	Header->bbh_Read = Header->bbh_Write;
/*
 * Looking good.
 */
	return (TRUE);
}




unsigned char *
BB_GetBeam (len)
int *len;
/*
 * Return a beam and its length.  A zero return implies fatal difficulties.
 */
{
	unsigned char	*dp;
/*
 * The looping should not be necessary, but just to be sure...
 */
	while (Header->bbh_Read == Header->bbh_Write)
		BB_BeamWait ();
/*
 * Now we have something.
 */
	dp = Beams + Header->bbh_Read*Header->bbh_BSize;
	*len = Sizes[Header->bbh_Read];
	Header->bbh_Read = BB_CPlus (Header->bbh_Read);
	return (dp);
}






static int
BB_BeamWait ()
/*
 * Wait for a new beam to come in.
 */
{
	struct sembuf op;
/*
 * Bump up the semaphore.
 */
	op.sem_num = 0;
	op.sem_op = 1;	/* Increment */
	op.sem_flg = 0;
	if (semop (Sem_ID, &op, 1) < 0)
		msg_ELog (EF_PROBLEM, "Error %d in sem incr", errno);
/*
 * Now wait for it to go back to zero.
 */
	op.sem_num = 0;
	op.sem_op = 0;
	op.sem_flg = 0;
	if (semop (Sem_ID, &op, 1) < 0)
		msg_ELog (EF_PROBLEM, "Error %d in sem zwait", errno);
}
		



static void
BB_Detach (zap)
int zap;
/*
 * Detach from our IPC resources.  The "zap" argument says whether they
 * should be destroyed, but it is ignored for now.  The machine running
 * this stuff is dedicated to the task, and not destroying the segment allows
 * either side to be restarted without greatly effecting the other.
 */
{
	shmdt (ShmSegment);
/* There seems to be no way to detach from semaphores. */
}





void
BB_Done ()
/*
 * Disconnect.
 */
{
	BB_Detach (TRUE);
}




void
BB_Dump ()
{
	printf ("Read %d, write %d, nb %d, bs %d, sizes %d beams %d nd %d\n",
			Header->bbh_Read, Header->bbh_Write, Header->bbh_NBeam,
			Header->bbh_BSize, Header->bbh_Sizes,
			Header->bbh_Beams, Header->bbh_NDrop);
}
