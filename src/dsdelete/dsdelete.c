/*
 * Data store file deletion utility.  Be careful out there.
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
static char *rcsid = "$Id: dsdelete.c,v 2.1 1991-09-26 23:03:09 gracio Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include <copyright.h>
# include "DataStore.h"


extern char *getenv (char *);


main (argc, argv)
int argc;
char **argv;
{
	PlatformId plat;
	int leave;
/*
 * Check args.
 */
	if (argc != 3)
	{
		printf ("Usage: %s platform leave-seconds\n", argv[0]);
		exit (1);
	}
/*
 * Get initialized.
 */
	usy_init ();
	msg_connect (0, getenv ("USER"));
	ds_Initialize ();
/*
 * Figure out the params, then do the dirty work.
 */
	if ((plat = ds_LookupPlatform (argv[1])) == BadPlatform)
	{
		printf ("Unknown platform: '%s'\n", argv[1]);
		exit (1);
	}
	leave = atoi (argv[2]);
	ds_DeleteData (plat, leave);
	exit (0);
}
