/*
 * Basic shared memory management routines for the data store daemon.
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
static char *rcsid = "$Id: d_SharedMemory.c,v 2.1 1991-09-26 22:59:37 gracio Exp $";

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

/*
 * The lock count -- so that ShmLock calls can be nested and work properly.
 */
static int NLock = 0;


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
 * If we're already locked, only increment the count now.
 */
	if (NLock++ > 0)
		return;
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
/*
 * If this is a nested lock, we don't really free things yet.
 */
	if (--NLock > 0)
		return;
/*
 * Turn it loose.
 */
	op.sem_num = S_WRITE;
	op.sem_flg = SEM_UNDO | IPC_NOWAIT;
	op.sem_op = -1;
	if (semop (SemId, &op, 1) < 0)
		msg_ELog (EF_PROBLEM, "Error %d resetting write sem", errno);
}
