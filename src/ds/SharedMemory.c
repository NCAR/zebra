/*
 * Application library interface to the shared memory segment.
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
# include <sys/types.h>
# include <sys/ipc.h>
# include <sys/shm.h>
# include <sys/sem.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"

MAKE_RCSID ("$Id: SharedMemory.c,v 2.3 1992-12-23 16:51:24 corbet Exp $")


/*
 * The number of locks on the shared memory segment.  If we actually try
 * to lock through the semaphores more than once, things can deadlock, which
 * would not be good.  But we want any routine that needs things locked to
 * be able to do so without worrying about what the callers might have done.
 * Thus, this count.
 */
static int LockCount = 0;


int
dsm_Init ()
/*
 * Initialize the shared memory segment.
 */
{
	int shmid;
/*
 * Find the shared memory segment.
 */
	if ((shmid = shmget (DS_KEY, SHM_SIZE, 0)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "Unable to hook into SHM, err %d",
			errno);
		return (FALSE);
	}
/*
 * Map it in.
 */
	ShmSegment = (char *) shmat (shmid, 0, 0);
	if (ShmSegment == (char *) -1)
	{
		msg_ELog (EF_EMERGENCY, "Unable to attach shm 0x%x, err %d",
			shmid, errno);
		return (FALSE);
	}
/*
 * Now find the semaphores.
 */
	if ((Semaphore = semget (DS_KEY, 2, 0)) < 0)
	{
		msg_ELog (EF_EMERGENCY, "Unable to get semaphores: %d", errno);
		return (FALSE);
	}
/*
 * Find the various tables.
 */
	SHeader = (struct ds_ShmHeader *) ShmSegment;
	PTable = (Platform *) (ShmSegment + SHeader->sm_PTOffset);
	DFTable = (DataFile *) (ShmSegment + SHeader->sm_DTOffset);
/*
 * One last check.
 */
	if (SHeader->sm_magic != SHM_MAGIC)
		msg_ELog (EF_PROBLEM, "WARNING: SHM magic number mismatch");
	return (TRUE);
}





void
dsm_Dump ()
/*
 * Dump out the state of the shared memory stuff.
 */
{
	ui_printf ("Shared memory mapped at 0x%x; Sem ID 0x%x\n", ShmSegment,
		Semaphore);
	ui_printf ("\t%d platforms at %d (0x%x)\n", SHeader->sm_nPlatform,
		SHeader->sm_PTOffset, PTable);
	ui_printf ("\t%d DTE's at %d (0x%x), %d used\n",
		SHeader->sm_nDataTable, SHeader->sm_DTOffset, DFTable,
		SHeader->sm_nDTEUsed);
	ui_printf ("\tSemaphores: READ %d WRITE %d\n",
		semctl (Semaphore, S_READ, GETVAL, 0),
		semctl (Semaphore, S_WRITE, GETVAL, 0));
}






void
dsm_ShmLock ()
/*
 * Lock the segment for read access.
 */
{
	struct sembuf op;
/*
 * If we're already locked, don't worry.
 */
	if (LockCount++)
		return;
/*
 * Before anything, increase the read semaphore.
 */
again:
	op.sem_num = S_READ;
	op.sem_flg = SEM_UNDO;
	op.sem_op = 1;
	if (semop (Semaphore, &op, 1) < 0)
		msg_ELog (EF_PROBLEM, "Error %d incr read semaphore", errno);
/*
 * If a write lock has been established, release our read lock and wait
 * for the write to go away.
 */
	if (semctl (Semaphore, S_WRITE, GETVAL, 0) > 0)
	{
	/*
	 * Reset the read semaphore.
	 */
		op.sem_op = -1;
		if (semop (Semaphore, &op, 1) < 0)
			msg_ELog (EF_PROBLEM, "Error %d reset read sem",errno);
	/*
	 * Wait on write.
	 */
		op.sem_num = S_WRITE;
		op.sem_op = 0;
		op.sem_flg = 0;
		if (semop (Semaphore, &op, 1) < 0)
			msg_ELog (EF_PROBLEM, "Error %d wait write sem",errno);
	/*
	 * Now start over.
	 */
	 	goto again;
	}
}




void
dsm_ShmUnlock ()
/*
 * Unlock a previously locked read semaphore.
 */
{
	struct sembuf op;
/*
 * If multiple locks remain, don't do anything now.
 */
	if (--LockCount)
		return;
/*
 * Actually unlock.
 */
	op.sem_num = S_READ;
	op.sem_flg = SEM_UNDO | IPC_NOWAIT;
	op.sem_op = -1;
	if (semop (Semaphore, &op, 1) < 0)
		msg_ELog (EF_PROBLEM, "Error %d decr read sem", errno);
}
