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
static char *rcsid = "$Id: dsdelete.c,v 2.7 1993-10-19 21:20:47 granger Exp $";

# include "defs.h"
# include "message.h"
# include "ui_expr.h"
# include <copyright.h>
# include "DataStore.h"

#ifndef SVR4
extern char *getenv FP((char *));
#endif

#define USAGE \
"Deletes files from the named platform.  Use with caution!\n\
All files are deleted whose times fall before a calculated cutoff time.\n\
If 'zap-time' is a number, or a UI expression which evaluates to a number,\n\
the cutoff time is that number of seconds prior to the current ZEB time.\n\
Otherwise the cutoff time is 'zap-time' interpreted as an absolute time\n\
in UI format, 'dd-mmm-yy[,hh:mm:ss]'.\n\
   -o\tdeletes the single observation which contains the cutoff time\n\
   -z\tdeletes the single most recent observation\n\
   -h\tprints this usage message\n\
Examples: \n\
   dsdelete -z radar                  Delete the most recent radar file\n\
   dsdelete prof915h '(3600*24)'      Delete files 24 hrs earlier and before\n\
   dsdelete -o gms 29-feb-93,4:15:42  Delete file containing the given time\n\
   dsdelete test 0                    Delete all files in platform 'test'\n"


void
usage(prog)
char *prog;
{
	printf ("Usage: %s [-h] [-o] [-z] platform zap-time\n", prog);
	printf ("%s", USAGE);
}


main (argc, argv)
int argc;
char **argv;
{
	PlatformId plat;
	int type;
	SValue v;
	char cbuf[100];
	ZebTime zaptime, now;
	bool obs = FALSE, last_obs = FALSE;
 	struct parse_tree *pt;
	extern char *optarg;
	extern int optind;
	int c;

	while ((c = getopt (argc, argv, "hoz")) != -1)
	{
		switch (c)
		{
		   case 'o':
			obs = TRUE;
			break;
		   case 'z':
			last_obs = TRUE;
			break;
		   case 'h':
			usage(argv[0]);
			exit(0);
		   case '?':
			usage(argv[0]);
			exit(1);
		}
	}
/*
 * Check args.  Still need at least a platform name.
 */
	if (! (optind < argc))
	{
		usage(argv[0]);
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
	if ((plat = ds_LookupPlatform (argv[optind])) == BadPlatform)
	{
		printf ("Unknown platform: '%s'\n", argv[optind]);
		exit (1);
	}
/*
 * At this point, if they want the most recent observation, then we don't
 * need a time from the command line.
 */
	if (last_obs)
	{
	/*
	 * Calculate the cutoff time by inquiring about the most recent
	 * time available.
	 */
		tl_Time (&now);
		if (! ds_DataTimes (plat, &now, 1, DsBefore, &zaptime))
		{
			printf ("No observations available to delete.\n");
			exit (1);
		}
	}
	else
	{
		/*
		 * See which format of zap time they give.
		 */
		++optind;
		if (! (optind < argc))
		{
			printf ("Time argument missing.\n");
			usage(argv[0]);
			exit (1);
		}
		if (! (pt = ue_parse (argv[optind], 0, FALSE)))
		{
			printf ("Funky date: '%s'\n", argv[optind]);
			exit (1);
		}
		ue_eval (pt, &v, &type);
		if (type == SYMT_INT)
		{
			tl_Time (&zaptime);
			zaptime.zt_Sec -= v.us_v_int;
		}
		else if (type == SYMT_DATE)
		{
			TC_UIToZt (&v.us_v_date, &zaptime);
		}
		else
		{
			printf ("Bad date: %s\n", argv[optind]);
			exit (1);
		}
	}
	/*
	 * Now do it.
	 */
	strcpy (cbuf, "Deleting %s ");
	TC_EncodeTime (&zaptime, TC_Full, cbuf+strlen(cbuf));
	printf (cbuf, (obs || last_obs) ? "at" : "before");
	printf ("\n");
	if (obs || last_obs)
		ds_DeleteObs (plat, &zaptime);
	else
		ds_DeleteData (plat, &zaptime);
	exit (0);
}
