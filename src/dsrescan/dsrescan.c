/*
 * Force a rescan of a platform.
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
static char *rcsid = "$Id: dsrescan.c,v 1.1.1.1 1993-04-02 00:34:26 granger Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include <copyright.h>
# include "DataStore.h"


# ifdef __STDC__
extern char *getenv (char *);
# else
extern char *gentenv();
# endif

main (argc, argv)
int argc;
char **argv;
{
	PlatformId plat;
	int all = 0;
	char pname[50];
/*
 * Check args.
 */
	if (argc != 2)
	{
		printf ("Usage: %s platform | ALL\n", argv[0]);
		exit (1);
	}
/*
 * Get initialized.
 */
	usy_init ();
	sprintf (pname, "Rescan-%d", getpid ());
	msg_connect (0, pname);
	ds_Initialize ();
/*
 * Figure out the params, then do the dirty work.
 */
	if (! strcmp (argv[1], "all") || ! strcmp (argv[1], "ALL"))
		all = TRUE;
	else if ((plat = ds_LookupPlatform (argv[1])) == BadPlatform)
	{
		printf ("Unknown platform: '%s'\n", argv[1]);
		exit (1);
	}
	ds_ForceRescan (plat, all);
	exit (0);
}
