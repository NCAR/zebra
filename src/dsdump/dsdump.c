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

# include "../include/defs.h"
# include "../include/message.h"
# include <copyright.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
MAKE_RCSID ("$Id: dsdump.c,v 3.5 1992-11-18 18:33:30 granger Exp $")


msg_handler ()
{ }




main (argc, argv)
int argc;
char **argv;
{
	int i, pid, np;
	static char *field = "velocity";
	ZebTime begin, end, ts[5];
	time kludge;
	Location locs[5];
	char atime[40];

	msg_connect (msg_handler, "DSDump");
	usy_init ();
	if (! ds_Initialize ())
		exit (1);
	dsm_ShmLock ();
	dsm_Dump ();
	for (i = 0; i < SHeader->sm_nPlatform; i++)
		if (argc < 2 || ! strcmp (argv[1], PTable[i].dp_name))
			dump_platform (PTable + i);
	dsm_ShmUnlock ();
	/* exit (0); */




/*
 * Try a data get.
 */
	if ((pid = ds_LookupPlatform ("cp4")) == BadPlatform)
	{
		ui_printf ("Lookup failure on 'cp4\n");
		exit (1);
	}
	ui_printf ("\ncp4 PID is %d\n", pid);
#ifdef notdef
	begin.ds_yymmdd = end.ds_yymmdd = 910312;
	begin.ds_hhmmss = 73210;
	end.ds_hhmmss = 73210;
	data = ds_GetData (pid, &field, 1, &begin, &end, OrgImage,
		0.0, 99.9);
# endif
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
/*
 * Print it out.
 */
# ifdef notdef
	ui_printf ("%d grid points\n", np = data->do_desc.d_irgrid.ir_npoint);
	ui_printf ("%d plain points\n", data->do_npoint);
	for (i = 0; i < 10; i++)
		ui_printf ("pres[%02d] = %7.2f at %d %d\n", i,
			data->do_data[0][i], data->do_times[i].ds_yymmdd,
			data->do_times[i].ds_hhmmss);
/*
 * Try times. 
 */
 	pid = ds_LookupPlatform ("mesonet");
 	begin.ds_hhmmss = 221000;
	printf ("%d times: ", ds_DataTimes (pid, &begin, 5, DsBefore, ts));
	for (i = 0; i < 5; i++)
		printf ("%06d ", ts[i].ds_hhmmss);
	printf ("\n");
	field = "tdry";
	data = ds_GetData (pid, &field, 1, ts, ts, Org2dGrid, 3.0, 99.9);

	if ((pid = ds_LookupPlatform ("kingair")) == BadPlatform)
		printf ("Kingair platform unknown\n");
	else
	{
		field = "temperature";
		begin.ds_hhmmss = 215000;
		end.ds_hhmmss = 220000;
		data = ds_GetData (pid, &field, 1, &begin, &end, OrgScalar,
			0.0, 9999.9);
	}
# endif
}



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



dump_platform (p)
Platform *p;
{
	int i;

	if (p->dp_flags & DPF_SUBPLATFORM)
		return;

	ui_printf ("Platform '%s', dir '%s'\n\tFlags 0x%x ( ", p->dp_name,
		p->dp_dir, p->dp_flags);
	for (i = 0; i < NFLAG; i++)
		if (p->dp_flags & Flags[i].flag)
			ui_printf ("%s ", Flags[i].name);
	ui_printf (")\n");
	if (! (p->dp_flags & DPF_SUBPLATFORM))
	{
		dumpchain ("L", p->dp_LocalData, strlen (p->dp_dir) + 1);
		if (p->dp_flags & DPF_REMOTE)
			dumpchain ("R", p->dp_RemoteData, strlen(p->dp_dir)+1);
	}
}




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
