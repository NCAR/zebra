/*
 * Basic shared memory management routines for the data store daemon.
 */
static char *rcsid = "$Id: d_SharedMemory.c,v 1.1 1990-11-02 08:56:21 corbet Exp $";

# include <errno.h>
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/shm.h>
# include <sys/sem.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dsDaemon.h"

/*
 * The shared memory ID for our segment.
 */
static int ShmId;
static int SemId;



void
InitSharedMemory ()
/*
 * Create, attach, and initialize the shared memory segment.
 */
{
	union semun sarg;
/*
 * Create and hook into the shared memory segment.
 */
	if ((ShmId = shmget (DS_KEY, SHM_SIZE, IPC_CREAT | 0777)) < 0)
	{
		msg_ELog (EF_EMERGENCY,"Unable to create SHM: errno %d",errno);
		exit (1);
	}
	ShmSegment = (char *) shmat (ShmId, 0, 0);
	if (ShmSegment == (char *) -1)
	{
		msg_ELog (EF_EMERGENCY, "Unable to attach shm 0x%x, err %d",
			ShmId, errno);
		exit (1);
	}
/*
 * Initialize the header as far as we can.
 */
	ShmHeader = (struct ds_ShmHeader *) ShmSegment;
	ShmHeader->sm_magic = SHM_MAGIC;
	ShmHeader->sm_nDataTable = ShmHeader->sm_nPlatform = 0;
	ShmHeader->sm_PTOffset = sizeof (struct ds_ShmHeader);
	/* Data table entries must come later */
/*
 * Create and initialize the semaphores.
 */
	if ((SemId = semget (DS_KEY, 2, IPC_CREAT | 0777)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "Unable to create semaphores, %d",
				errno);
		exit (1);
	}
	sarg.val = 0;
	if (semctl (SemId, S_READ, SETVAL, &sarg) < 0)
		msg_ELog (EF_PROBLEM, "Error %d clearing read sem", errno);
	if (semctl (SemId, S_WRITE, SETVAL, &sarg) < 0)
		msg_ELog (EF_PROBLEM, "Error %d clearing write sem", errno);
}





void
ShmCleanup ()
/*
 * Get rid of everything.
 */
{
	shmdt (ShmSegment);
	shmctl (ShmId, IPC_RMID, 0);
	semctl (SemId, IPC_RMID, 0);
}





void
ShmLock ()
/*
 * Lock the shared memory segment for writing.
 */
{
	struct sembuf op;
/*
 * Set the write semaphore first.
 */
	op.sem_num = S_WRITE;
	op.sem_flg = SEM_UNDO;
	op.sem_op = 1;
	if (semop (SemId, &op, 1) < 0)
		msg_ELog (EF_PROBLEM, "Error %d setting write sem", errno);
/*
 * Now wait until the read semaphore is free.
 */
	op.sem_num = S_READ;
	op.sem_op = 0;
	op.sem_flg = 0;
	if (semop (SemId, &op, 1) < 0)
		msg_ELog (EF_PROBLEM, "Error %d waiting on read sem", errno);
}




void
ShmUnlock ()
/*
 * Unlock the shared memory segment.
 */
{
	struct sembuf op;

	op.sem_num = S_WRITE;
	op.sem_flg = SEM_UNDO | IPC_NOWAIT;
	op.sem_op = -1;
	if (semop (SemId, &op, 1) < 0)
		msg_ELog (EF_PROBLEM, "Error %d resetting write sem", errno);
}
