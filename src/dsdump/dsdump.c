/*
 * Data Store dumpout.
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

# include "defs.h"
# include "message.h"
# include <copyright.h>
# include "DataStore.h"
/* # include "dsPrivate.h" */
/* # include "dslib.h" */

MAKE_RCSID ("$Id: dsdump.c,v 3.7 1993-04-26 16:00:50 corbet Exp $")


msg_handler ()
{ }




main (argc, argv)
int argc;
char **argv;
{
	int i, pid, np, nplat;
	static char *field = "velocity";
	ZebTime begin, end, ts[5];
	time kludge;
	Location locs[5];
	char atime[40];
	PlatformInfo pi;

	msg_connect (msg_handler, "DSDump");
	usy_init ();
	if (! ds_Initialize ())
		exit (1);
/*
 * How many platforms?
 */
	nplat = ds_GetNPlat ();
	printf ("We have %d platforms.\n", nplat);
	for (i = 0; i < nplat; i++)
	{
		ds_GetPlatInfo (i, &pi);
		if (argc < 2 || ! strcmp (argv[1], pi.pl_Name))
		{
			ds_LockPlatform (i);
			dump_platform (i, &pi);
			ds_UnlockPlatform (i);
		}
	}
	exit (0);




# ifdef notdef
/*
 * Try a data get.
 */
	if ((pid = ds_LookupPlatform ("cp4")) == BadPlatform)
	{
		ui_printf ("Lookup failure on 'cp4\n");
		exit (1);
	}
	ui_printf ("\ncp4 PID is %d\n", pid);
/*
 * Get some sample info.
 */
	kludge.ds_yymmdd = 910802;
	kludge.ds_hhmmss = 220100;
	TC_UIToZt (&kludge, &begin);
	np = ds_GetObsSamples (pid, &begin, ts, locs, 5);
	ui_printf ("ds_GetObsSamples: Got %d samples\n", np);
	for (i = 0; i < np; i++)
	{
		TC_EncodeTime (ts + i, TC_Full, atime);
		ui_printf ("\t%d: %s, alt %.2f\n", i, atime, locs[i].l_alt);
	}
/*
 * Obs times.
 */
	np = ds_GetObsTimes (pid, &begin, ts, 5, 0);
	ui_printf ("ds_GetObsTimes (no attr): Got %d times\n", np);
	for (i = 0; i < np; i++)
	{
		TC_EncodeTime (ts + i, TC_Full, atime);
		ui_printf ("\t%d: %s\n", i, atime);
	}
	np = ds_GetObsTimes (pid, &begin, ts, 5, "sur");
	ui_printf ("ds_GetObsTimes (attr sur): Got %d times\n", np);
	for (i = 0; i < np; i++)
	{
		TC_EncodeTime (ts + i, TC_Full, atime);
		ui_printf ("\t%d: %s\n", i, atime);
	}
	exit (0);
# endif
}



# ifdef notdef
static struct fname
{
	int	flag;
	char	*name;
} Flags[] =
{
	{	DPF_MOBILE,	"mobile"	},
	{	DPF_COMPOSITE,	"composite"	},
	{	DPF_DISCRETE,	"discrete"	},
	{	DPF_REGULAR,	"regular"	},
	{	DPF_SUBPLATFORM, "subplatform"	},
	{	DPF_REMOTE,	"remote-dir"	},
};

# define NFLAG (sizeof (Flags)/sizeof (struct fname))

# endif


dump_platform (pid, pi)
PlatformId pid;
PlatformInfo *pi;
{
	int i, index;
	DataSrcInfo dsi;
	DataFileInfo dfi;
/*
 * Don't dump subplats.
 */
	if (pi->pl_SubPlatform)
		return;

	ui_printf ("\nPlatform %s, %d data sources", pi->pl_Name,
		pi->pl_NDataSrc);
	if (pi->pl_Mobile)
		ui_printf (" (MOBILE)");
	ui_printf ("\n");
/*
 * Now we need to dump out each source.
 */
	for (i = 0; i < pi->pl_NDataSrc; i++)
	{
		ds_GetDataSource (pid, i, &dsi);
		ui_printf (" Data source '%s', in %s, type %d\n", 
			dsi.dsrc_Name, dsi.dsrc_Where, dsi.dsrc_Type);
		for (index = dsi.dsrc_FFile; index > 0; index = dfi.dfi_Next)
		{
			ds_GetFileInfo (index, &dfi);
			PrintInfo (index, &dfi);
		}
	}
}



PrintInfo (index, dfi)
int index;
DataFileInfo *dfi;
/*
 * Dump out file info.
 */
{
	char abegin[40], aend[20];

/*
 * Pull out the date information and encode it.
 */
	TC_EncodeTime (&dfi->dfi_Begin, TC_Full, abegin);
	TC_EncodeTime (&dfi->dfi_End, TC_TimeOnly, aend);
/*
 * Now print.
 */
	ui_printf ("  %c %4d  %s  %s > %s [%hu]\n",
		dfi->dfi_Archived ? 'A' : 'N',
		index, dfi->dfi_Name, abegin, aend, dfi->dfi_NSample);
}		




# ifdef notdef

dumpchain (which, start, dlen)
char *which;
int start, dlen;
/*
 * Dump out a datafile chain.
 */
{
	DataFile *dp;
	char abegin[40], aend[20];

	while (start)
	{
		dp = DFTable + start;
	/*
	 * Pull out the date information and encode it.
	 */
		TC_EncodeTime (&dp->df_begin, TC_Full, abegin);
		TC_EncodeTime (&dp->df_end, TC_TimeOnly, aend);
	/*
	 * Do the print.
	 */
		ui_printf ("  %s%c %2d/%d-%d %s %s > %s [%hu]\n", which,
			(dp->df_flags & DFF_Archived) ? 'A' : 'N',
			start, dp->df_use, dp->df_rev, dp->df_name,
			abegin, aend, dp->df_nsample);
		start = dp->df_FLink;
	}
}

# endif
