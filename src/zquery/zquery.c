/*
 * The zeb query module.
 */
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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
# include <unistd.h>
# include <defs.h>
# include <message.h>

RCSID ("$Id: zquery.c,v 1.8 2004-07-15 17:24:10 granger Exp $")

#define DEFAULT_DELAY 	0

static int IncMsg FP((struct message *msg));
static int QResp FP((char *info));

static int QUERY_DONE = 0x02020202;

static void
usage (argv0)
char *argv0;
{
	printf ("Usage: %s [-h] [-t timeout] process\n", argv0);
	printf ("   <timeout> of zero implies indefinite wait (default)\n");
	exit (1);
}


int
main (argc, argv)
int argc;
char **argv;
{
	int ret;
	int arg;
	int delay = DEFAULT_DELAY;
	char name[MSG_MAXNAMELEN];
	char *query;
/*
 * Check for a couple options
 */
	arg = 1;
	while (arg < argc)
	{
		if (! strcmp (argv[arg], "-t"))
		{
			if (arg+1 < argc)
				delay = atoi (argv[++arg]);
			else
				usage (argv[0]);
		}
		else if (! strcmp (argv[arg], "-h"))
			usage (argv[0]);
		else
			break;
		++arg;
	}
/*
 * Make sure they left us a recipient.
 */
	if (arg + 1 != argc)
		usage (argv[0]);
/*
 * Hook in and send the query..
 */
	sprintf (name, "zquery-%i", getpid());
	if (! msg_connect (IncMsg, name))
	{
	  printf ("Failed to connect to message manager.\n");
	  return 1;
	}
	query = argv[arg];
	msg_SendQuery (query, QResp);
/*
 * Now we just wait, but perhaps we're not to be infinitely patient.
 */
	if (! delay)
	{
	  ret = msg_await ();
	}
	else
	{
	  do {
	    ret = msg_poll (delay);
	  }
	  while (ret == 0);

	  if (ret == MSG_TIMEOUT)
	  {
	    printf ("no response from '%s'\n", query);
	  }
	}
	return ((ret != QUERY_DONE) ? 1 : 0);
}




static int
IncMsg (msg)
struct message *msg;
{
	if (msg->m_proto != MT_MESSAGE)
		return (0);
	if (msg->m_len == 0)
		return (QUERY_DONE);
	printf ("%s\n", msg->m_data);
	return (0);
}





static int
QResp (info)
char *info;
/*
 * Something came back.
 */
{
	if (! info)
		exit (0);
	printf ("%s\n", info);
	return (0);
}
