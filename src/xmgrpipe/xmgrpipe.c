/*
 * Pipe a time series of data to ACE/gr
 *
 * Usage: xmgrpipe begin-time end-time platform field
 *
 * To use platform - field pairs, provide -p option for naming a platform
 * which stays in effect for all following field lists up to the next -p.
 * Build a list of platforms and a list of field lists from which to 
 * fetch data.
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

# include <stdio.h>
# include <string.h>
# include "defs.h"
# include "message.h"
# include <copyright.h>
# include "DataStore.h"

MAKE_RCSID ("$Id: xmgrpipe.c,v 1.2 1995-04-20 13:43:33 granger Exp $")

static void Spew FP((DataChunk *dc, FieldId field, ZebTime *end));
static void PipeData FP((PlatformId pid, ZebTime *begin, ZebTime *end,
			 FieldId *fids, int nfields));
static int ParseTime FP((char *stime, ZebTime *zt));


static void
usage (prog)
char *prog;
{
	printf("Usage: %s -h\n", prog);
	printf("       %s [options] begin end platform fields...\n", prog);
	printf("\t-t <title>     Title to use for plot\n");
	printf("\t-s <subtitle>  Subtitle to use for plot\n");
	printf("\t-p <plist>     Platform list for following field lists\n");
	printf("\t-h             Print this usage information\n");
}


static int
msg_handler ()
{ }



main (argc, argv)
int argc;
char **argv;
{
	int i, opt;
	ZebTime begin, end;
	char name[256];
	char *platform;
	char *title = NULL;
	char *subtitle = NULL;
	FieldId fids[256];
	int nfields;
	PlatformId pid;
/*
 * First check for the help option
 */
	if ((argc > 1) && (!strcmp(argv[1], "-h")))
	{
		usage (argv[0]);
		exit (0);
	}

	sprintf (name, "xmgrpipe-%d", getpid());
	msg_connect (msg_handler, name);
	usy_init ();
	if (! ds_Initialize ())
	{
		printf("%s: could not connect to DataStore daemon\n",argv[0]);
		exit (1);
	}
/*
 * Traverse the options, setting options as encountered
 */
	opt = 1;
	do {
		if ((opt < argc) && (argv[opt][0] == '-'))
		{
			switch (argv[opt][1])
			{
			   case 't':
				title = argv[opt+1];
				IngestRemoveOptions (&argc, argv, opt, 2);
				break;
			   case 's':
				subtitle = argv[opt+1];
				IngestRemoveOptions (&argc, argv, opt, 2);
				break;
			   default:
				printf ("%s: illegal option '%s'\n",
					argv[0], argv[opt]);
				usage (argv[0]);
				exit (1);
				break;
			}
		}
		++opt;
	}
	while (opt < argc);
/*
 * Now we should have our required parameters remaining
 */
	if (argc < 5)
	{
		printf ("too few parameters\n");
		usage (argv[0]);
		exit (3);
	}
	if (! ParseTime (argv[1], &begin) ||
	    ! ParseTime (argv[2], &end))
	{
		exit (9);
	}
	if (TC_Less (end, begin))
	{
		printf ("begin time must precede end time\n");
		exit (4);
	}

	pid = ds_LookupPlatform (argv[3]);
	if (pid == BadPlatform)
	{
		printf ("bad platform: %s\n", argv[3]);
		exit (5);
	}
/*
 * Fetch fields from the file first so that we get units from there
 */
	nfields = 256;
	ds_GetFields (pid, &begin, &nfields, fids);
	nfields = 0;
	for (i = 4; i < argc; ++i)
	{
		char *fields[32];
		int nparse, j;

		nparse = CommaParse (argv[i], fields);
		for (j = 0; j < nparse; ++j)
		{
			fids[nfields++] = F_Lookup (fields[j]);
#ifdef DEBUG
			printf ("%s ", fields[j]);
#endif
		}
	}
/*
 * Print our preamble, containing things like title and subtitle
 */

/*
 * Then pipe the actual data
 */	
	PipeData (pid, &begin, &end, fids, nfields);
/*
 * Done.
 */
	exit(0);
}



static int
ParseTime (stime, zt)
char *stime;
ZebTime *zt;
/*
 * Return non-zero if successful
 */
{
	int type;
	SValue v;
	char cbuf[128];
	ZebTime zaptime, now;
 	struct parse_tree *pt;
 	struct parse_tree *ue_parse();

	if (! (pt = ue_parse (stime, 0, FALSE)))
	{
		printf ("Funky date: '%s'\n", stime);
		return (0);
	}
	ue_eval (pt, &v, &type);
	if (type != SYMT_DATE)
	{
		printf ("Bad date: %s\n", stime);
		return (0);
	}
	TC_UIToZt (&v.us_v_date, zt);
	return (1);
}

/*
 * I had to store these in arrays because the sun cc compiler did not
 * like @ and newlines in string literals.  Arghh.
 */
static char *Preamble[] = {
	"@page 5",
	"@page inout 5",
	"@link page off",
	"@with g0",
	"@g0 on",
	"@g0 label off",
	"@g0 hidden false",
	"@g0 type xy",
	"@g0 autoscale type AUTO",
	"@g0 fixedpoint off",
	"@g0 fixedpoint type 0",
	"@g0 fixedpoint xy 0.000000, 0.000000",
	"@g0 fixedpoint format general general",
	"@g0 fixedpoint prec 6, 6",
	"@    title color 4",
	"@    subtitle color 4",
	NULL
};

static char *Xaxis[] = {
	"@    xaxis  tick on",
	"@    xaxis  label \"Time\"",
	"@    xaxis  ticklabel type spec",
	"@    xaxis  ticklabel prec 1",
	"@    xaxis  ticklabel format decimal",
	"@    xaxis  ticklabel layout horizontal",
	"@    xaxis  ticklabel skip 0",
	"@    xaxis  ticklabel stagger 0",
	"@    xaxis  ticklabel op bottom",
	"@    xaxis  ticklabel sign normal",
	"@    xaxis  ticklabel start type auto",
	"@    xaxis  ticklabel start 0.000000",
	"@    xaxis  ticklabel stop type auto",
	"@    xaxis  ticklabel stop 0.000000",
	"@    xaxis  ticklabel char size 1.000000",
	"@    xaxis  ticklabel font 4",
	"@    xaxis  ticklabel color 1",
	"@    xaxis  ticklabel linewidth 1",
	"@    xaxis  tick major grid off",
	"@    xaxis  tick minor grid off",
	"@    xaxis  tick op both",
	"@    xaxis  tick type spec",
	NULL
};


static void
PipeData (pid, begin, end, fids, nfields)
PlatformId pid;
ZebTime *begin;
ZebTime *end;
FieldId *fids;
int nfields;
/*
 * Fetch data for the given fields during the given time and send it
 * standard out in xmgr parameter and dataset format
 */
{
	DataChunk *dc;
	int i;
	int isecs;
	char **line;
	ZebTime lt;
	TimePrintFormat tc;
	char buf[256];
#define NTIMETICKS 4
/*
 * First set some ACE/gr parameters, especially the time ticks.
 */
	line = Preamble;
	while (*line)
		printf ("%s\n", *line++);
	TC_EncodeTime (begin, TC_Full, buf);
	strcat (buf, " - ");
	TC_EncodeTime (end, TC_Full, buf+strlen(buf));
	printf ("@    subtitle \"%s\"\n", buf);
	printf ("@    title \"%s: %s (%s)\"\n", ds_PlatformName(pid),
		F_GetName (fids[0]), F_GetUnits (fids[0]));
	line = Xaxis;
	while (*line)
		printf ("%s\n", *line++);
	isecs = (end->zt_Sec - begin->zt_Sec) / (NTIMETICKS + 1);
	tc = TC_TimeOnly;
	lt = *begin;
	if (isecs > 24*3600)
	{
		isecs -= isecs % 24*3600;
		tc = TC_DateOnly;
		lt.zt_Sec -= begin->zt_Sec % (24*3600);
		lt.zt_Sec += 24*3600;
	}
	else if (isecs > 3600)
	{
		isecs -= isecs % 3600;
		lt.zt_Sec -= begin->zt_Sec % (3600);
		lt.zt_Sec += 3600;
	}
	else if (isecs > 60)
	{
		isecs -= isecs % 60;
		lt.zt_Sec -= begin->zt_Sec % (60);
		lt.zt_Sec += 60;
	}
	printf ("@    xaxis tick spec %d\n",
		(end->zt_Sec - lt.zt_Sec) / isecs + 1);
	i = 0;
	while (TC_LessEq (lt, *end))
	{
		printf ("@    xaxis tick %d, %f\n", i, 
			(lt.zt_Sec - end->zt_Sec) / 60.0);
		TC_EncodeTime (&lt, tc, buf);
		printf ("@    xaxis ticklabel %d, \"%s\"\n", i, buf);
		lt.zt_Sec += isecs;
		++i;
	}
/*
 * Snarf.
 */
	dc = ds_Fetch (pid, DCC_Scalar, begin, end, fids, nfields, 0, 0);
	if (! dc)
		return;
/*
 * Print.
 */
	for (i = 0; i < nfields; ++i)
	{
		printf ("&\n");		/* data set separator */
		Spew (dc, fids[i], end);
	}
	dc_DestroyDC (dc);
}



static void
Spew (dc, field, end)
DataChunk *dc;
FieldId field;
ZebTime *end;
/*
 * Dump this data into a file.
 */
{
	int ns, samp;
	float dv;
	ZebTime zt;

	ns = dc_GetNSample (dc);
	for (samp = 0; samp < ns; samp++)
	{
		dv = dc_GetScalar (dc, samp, field);
		dc_GetTime (dc, samp, &zt);
		fprintf(stdout, "%.1f %.3f\n", (zt.zt_Sec - end->zt_Sec)/60.0,
			dv);
	}
}


