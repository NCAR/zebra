/*
 * IS variables.
 * 
 * $Id: is_vars.h,v 1.4 1991-12-11 20:40:05 martin Exp $
 */
/*
 * Copyright (C) 1987,88,89,90,91 by UCAR University Corporation for
 * Atmospheric Research All rights reserved
 * 
 * No part of this work covered by the copyrights herein may be reproduced or
 * used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular
 * purpose. UCAR does not indemnify any infringement of copyright, patent, or
 * trademark through use or modification of this software.  UCAR does not
 * provide maintenance or updates for its software.
 */

#include "defs.h"
#include "message.h"
#include "timer.h"

#define MAX_PROC_ARGS 30
#define MAX_STTY_ARGS 20
#define MAX_FILE_NAME 256
#define MAX_RESTARTS  5
#define LS_BEGIN "ls -1d "
#define LS_END " 2> /dev/null"

enum CONFIG_TYPE {
	IS_FTYPE, IS_CTYPE, IS_PTYPE
};

/*
 * the following defines entries in a linked list of process identifiers. A
 * given configuration may (in the future) have more than one process running
 * at a given time. It doesn't make sense to allow this yet since the data
 * store cannot deal with data older than the youngest objects
 */
struct proc_entry {
	int             pid;
	struct proc_entry *next;
};

/* the following defines an ingest scheduler configuration */
struct is_config {
	enum CONFIG_TYPE type;	/* either file type or serial type */
	char           *name;	/* configuration name */
	char           *platform;	/* platform name */
	char           *filename;	/* filename to match against
					 * (file type only) */
	char           *movedir;/* directory to move finished files to, null
				 * for no move (file type only) */
	char            delete;	/* true if file delete after ingest (file
				 * type only) */
	char           *process;/* ingest process name, should be absolute */
	int             n_proc_args;	/* number of arguments */
	char           *proc_args[MAX_PROC_ARGS];	/* the arguments */
	int             interval;	/* reschedule interval (file type
					 * only) */
	char            timer;	/* true if timer is running (means that file
				 * type is active) */
	int             timer_slot;	/* timer slot */
	int             active;	/* number of active processes associated with
				 * this configuration */
	char            ingest_file[MAX_FILE_NAME];	/* complet path name of
							 * file to ingest */
	struct proc_entry *proc;/* linked list of the active processes */
	char            basename[MAX_FILE_NAME];	/* just the basename of
							 * the file to ingest */
	char            rollover;	/* true if an ingester was rerun
					 * since another file was ready,
					 * after the previous ingestor had
					 * exited. Inhibits timed scheduling */
	char            restart;/* true if serial ingestors are to be
				 * restarted after exit */
	int             n_restarts;	/* it is incremented for each restart
					 * up to MAX_RESTARTS limit, then it
					 * is turned off */
};
