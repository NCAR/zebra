/*
 * Data store daemon-specific definitions.
 */
/* $Id: dsDaemon.h,v 3.28 2002-10-22 08:12:19 granger Exp $ */
/*
 * The platform and data tables, via pointer.
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


#ifndef _zeb_dsDaemon_h_
#define _zeb_dsDaemon_h_

#include <time.h>

#include <defs.h>
#include <config.h>

#include "dsPrivate.h"
#include "d_Source.h"

/*
 * This is the "derived" structure which the daemon actually uses in
 * its tables.  It adds the run-time information the daemon maintains for 
 * each platform instance.  The idea is that the info exchanged between
 * daemon and client can be extracted directly from the 'pi' member, and
 * a pointer to a PlatformInstance from the table can be cast to a 
 * DaemonPlatform to get at the daemon run-time info.
 */
typedef struct ds_DaemonPlatform
{
	PlatformInstance pi;		/* Shared instantiated platform	*/

	/* ----- The rest is run-time info needed by the daemon ----- */

	unsigned short dp_NewSamps;	/* New samps (not yet notified) */
	unsigned short dp_OwSamps;	/* Overwritten samps (n.y.n.)	*/
} 
DaemonPlatform;


/*
 * Allow the revision numbering to be chosen from the config file.
 */
extern zbool StatRevisions;

/*
 * A global debug flag and some statistics variables
 */
extern zbool Debug;
extern zbool ParseOnly;

/*
 * This variable is TRUE only during the initial file scan.
 */
extern zbool InitialScan;	/* True implies first data scan	*/

/*
 * This variable reflects the number of platforms scanned so far
 * in the initial scan.  It should be ignored once InitialScan is false.
 */
extern int PlatformsScanned;

/*
 * Some useful timing information 
 */
extern time_t LastScan;		/* Time of latest full scan	*/
extern time_t LastCache;	/* Time to which cache files are up-to-date */
extern time_t Genesis;		/* Time when daemon started	*/

/*
 * Daemon public routines
 */
void DataFileGone (DataFile *df);
const char *DataFilePath (Source *src, const Platform *p, 
			  const DataFileCore *dfc);
void BuildDataFile (DataFile *df, const DataFileCore *dfc, Source *src, 
		    const Platform *p);

/*
 * Datascan
 */
void	DataScan (Source *src);
void	Rescan (Source *src, const Platform *plat, zbool all);
long	StatRevision (const char* name, ino_t *inode, int *isfile);

# ifdef UI_H_SYMBOLS
/*
 * UI command parsing external prototypes; only called in Daemon.c
 */
int	dc_Handler (int junk, struct ui_command *cmds);
# endif /* UI_H_SYMBOLS */

/*
 * Debuggin' routines
 */
void	dbg_DumpClass FP ((const PlatformClass *pc));
void	dbg_DumpInstance FP ((const Platform *pi));
void	dbg_DumpStatus FP ((void));
void	dbg_DumpTables FP((void));
int	dbg_AnswerQuery FP((char *who));
int	dbg_Append FP((char *buf, char *str, int len));
int	dbg_DirtyCount FP((void));
int	dbg_CompositeCount FP((void));
int	dbg_SubplatCount FP((void));
void	dbg_EncodeElapsed FP((char *prefix, time_t *start, time_t *end,
			      char *dest));

#endif /* ! _zeb_dsDaemon_h_ */
