/*
 * Data store daemon-specific definitions.
 */
/* $Id: dsDaemon.h,v 2.1 1991-09-26 23:01:53 gracio Exp $ */
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
Platform *PTable;
DataFile *DFTable;

/*
 * The default data directory.
 */
char DefDataDir[80];


/*
 * This is a kludge to make it easier to keep a uniform init file.  If this
 * flag is set, no remote directories will be accessed.
 */
extern int DisableRemote;


# ifdef __STDC__
	void InitSharedMemory (void);
	Platform *dt_NewPlatform (char *);
	Platform *dt_FindPlatform (char *, int);
	DataFile *dt_NewFile (void);
	void dt_FreeDFE (DataFile *);
	void dt_AddToPlatform (Platform *, DataFile *, int);
	void dc_DefPlatform (char *);
# else
	void InitSharedMemory ();
	Platform *dt_NewPlatform ();
	Platform *dt_FindPlatform ();
	DataFile *dt_NewFile ();
	void dt_FreeDFE ();
	void dt_AddToPlatform ();
	void dc_DefPlatform ();
# endif
