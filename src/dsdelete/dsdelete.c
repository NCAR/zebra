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
static char *rcsid = "$Id: dsdelete.c,v 2.5 1993-08-05 18:14:27 corbet Exp $";

# include "defs.h"
# include "message.h"
# include "ui_expr.h"
# include <copyright.h>
# include "DataStore.h"

#ifndef SVR4
extern char *getenv FP((char *));
#endif

main (argc, argv)
int argc;
char **argv;
{
	PlatformId plat;
	int type;
	SValue v;
	ZebTime zaptime;
	struct parse_tree *pt;
/*
 * Check args.
 */
	if (argc != 3)
	{
		printf ("Usage: %s platform zap-time\n", argv[0]);
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
/*
 * See which format of zap time they give.
 */
	if (! (pt = ue_parse (argv[2], 0, FALSE)))
	{
		printf ("Funky date: '%s'\n", argv[2]);
		exit (1);
	}
	ue_eval (pt, &v, &type);
	if (type == SYMT_INT)
	{
		tl_Time (&zaptime);
		zaptime.zt_Sec -= v.us_v_int;
	}
	else if (type == SYMT_DATE)
		TC_UIToZt (&v.us_v_date, &zaptime);
	else
	{
		printf ("Bad date: %s\n", argv[2]);
		exit (1);
	}
/*
 * Now do it.
 */
	ds_DeleteData (plat, &zaptime);
	exit (0);
}
