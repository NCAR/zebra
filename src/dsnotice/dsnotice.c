/*
 * Print data available notifications (DANs) on platforms to the terminal
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

# include <string.h>
# include <unistd.h>

# include <defs.h>
# include <message.h>
# include <copyright.h>
# include <DataStore.h>
# include <Platforms.h>

RCSID ("$Id: dsnotice.c,v 1.9 1999-03-01 02:03:56 burghart Exp $")

static void ReceiveNotify FP ((PlatformId pid, int param, ZebTime *when,
			      int nsample, UpdCode ucode));

static int ShowPlatDir = 0;

void
usage (prog)
char *prog;
{
   fprintf(stderr,"Usage: %s -help\n", prog);
   fprintf(stderr,"   Print this usage message\n");
   fprintf(stderr,"Usage: %s [-all] [-dir] [regexp ...]\n", prog);
   fprintf(stderr,"   Print notifications for all platforms or only those\n");
   fprintf(stderr,"   matching the given regular expressions.\n");
   fprintf(stderr,"   -all   \tReceive notifies all platforms.\n");
   fprintf(stderr,"   -dir   \tShow platform directory on each notice.\n");
   fprintf(stderr,"   -log   \tSend notices to event logger.\n");
   fprintf(stderr,"   Options can be abbreviated to any number of letters.\n");
}


static int
handler (msg)
Message *msg;
{
	struct mh_template *tmpl;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
	   	tmpl = (struct mh_template *) msg->m_data;
		if ((tmpl->mh_type == MH_DIE) || 
		    (tmpl->mh_type == MH_SHUTDOWN))
		{
			exit (0);
		}
		else
			msg_ELog (EF_PROBLEM, "Weird MH msg %d",tmpl->mh_type);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Unknown msg proto %d", msg->m_proto);
		break;
	}
	return (0);
}



int
main (argc, argv)
int argc;
char **argv;
{
	PlatformId *plist;
	int all = FALSE;
	char pname[50];
	char **platname = NULL;
	int i, j, p, nplat, total;
	int len;
	int send_mask;
	int print_mask;
/*
 * Check args.
 */
	if ((argc < 2) || 
	    (((len = strlen(argv[1])) > 1) && 
	     (!strncmp (argv[1], "-help", len))))
	{
		usage (argv[0]);
		exit (0);
	}
/*
 * Get initialized.
 */
	sprintf (pname, "Notice-%d", getpid ());
	if (! msg_connect (handler, pname))
	{
		msg_ELog (EF_PROBLEM, "could not connect to message manager");
		exit (1);
	}
	if (! ds_Initialize ())
	{
		msg_ELog (EF_PROBLEM, "could not connect to datastore");
		exit (1);
	}
/*
 * Default is not to send to logger but to print to console.
 */
	send_mask = msg_ELSendMask (0);
	print_mask = msg_ELPrintMask (EF_ALL);
/*
 * Figure out the params, then do the dirty work.
 */
	p = 0;
	if (argc > 1)
		platname = (char **)malloc((argc - 1)*sizeof(char *));
	for (i = 1; i < argc; ++i)
	{
		len = strlen(argv[i]);
		if ((argv[i][0] != '-') || (len < 2))
			platname[p++] = argv[i];
		else if (!strncmp(argv[i], "-all", len))
			all = TRUE;
		else if (!strncmp(argv[i], "-dir", len))
			ShowPlatDir = TRUE;
		else if (!strncmp(argv[i], "-log", len))
		{
			/* Toggle the notice locations */
			send_mask = msg_ELSendMask (send_mask);
			print_mask = msg_ELPrintMask (print_mask);
		}
		else
		{
			fprintf (stderr, "%s: invalid option '%s'\n",
				argv[0], argv[i]);
			usage (argv[0]);
			exit (99);
		}
	}

	if (all)
	{
		if (p > 0)
		{
			fprintf (stderr, 
				 "%s: -all takes no platform name arguments\n",
				 argv[0]);
			exit (99);
		}
		plist = ds_SearchPlatforms (NULL, &total, FALSE, FALSE);
		for (i = 0; i < total; ++i)
		{
			ds_RequestNotify (plist[i], 0, ReceiveNotify);
		}
		if (plist) free (plist);
	}
	else
	{
		total = 0;
		for (i = 0; i < p; ++i)
		{
			plist = ds_SearchPlatforms (platname[i], &nplat, 
						    FALSE, FALSE);
			if (plist == NULL)
				fprintf (stderr, "No matches for '%s'\n",
					 platname[i]);
			else
			{
				for (j = 0; j < nplat; ++j)
				{
					ds_RequestNotify (plist[j], 0,
						  	  ReceiveNotify);
				}
				free (plist);
				total += nplat;
			}
		}
	}
	msg_ELog (EF_INFO, "Receiving notices for %d platforms.", total);
	if (platname)
		free (platname);

	/*
	 * Wait for those notices to come in
	 */
	msg_await ();
	exit (0);
}





static void
ReceiveNotify (pid, param, when, nsample, ucode)
PlatformId pid;
int param;
ZebTime *when;
int nsample;
UpdCode ucode;
{
	char tbuf[64];
	const DataFile *df;
	const Platform *p = dt_FindPlatform (pid);
	/*
	 * Print lines of the form:
	 *
	 * <platform> <filename> <datadir> <time> <nsamples> <ucode>
	 */
	TC_EncodeTime (when, TC_Full, tbuf);
	df = ds_FindBefore (pid, when);
	msg_ELog (EF_INFO, "%s %s %s %i %s", 
		  p ? pi_Name (p) : "NULL", 
		  ShowPlatDir ? df->df_fullname : df->df_core.dfc_name,
		  tbuf, df->df_core.dfc_nsample, 
		  (ucode == UpdOverwrite) ? "owr" :
		  ((ucode == UpdInsert) ? "ins" : "app"));
	/* fflush (stdout); */
}
