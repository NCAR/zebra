/*
 * Message server status grabber.
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

# include <defs.h>
# include <message.h>
# include <copyright.h>

RCSID("$Id: mstatus.c,v 2.7 2004-07-15 17:24:34 granger Exp $")

static char Username[128] = { '\0' };
static int ShowUser = 0;

static int STATUS_OK = 0x01010101;

static void
usage ()
{
	printf ("usage: mstatus [-help] [-user] [host ...]\n");
	printf ("   -h   Help\n   -u   Username of message handler\n");
}


int
main (argc, argv)
int argc;
char *argv[];
{
	int handler ();
	struct mh_template tm;
	char **hosts = NULL;
	char manager[ 128 ];
	int i, nhost = 0;
	int status = 0;

	if (! msg_connect (handler, "Status reporter"))
		exit (1);

	for (i = 1; i < argc; ++i)
	{
		int optlen = strlen(argv[i]);
		if (optlen < 2 || argv[i][0] != '-')
		{
			if (! hosts)
				hosts = (char **) 
					malloc (argc * sizeof(char *));
			hosts[nhost++] = argv[i];
		}
		else if (! strncmp (argv[i], "-help", optlen))
		{	
			usage ();
			exit (0);
		}
		else if (! strncmp (argv[i], "-user", optlen))
			ShowUser = 1;
		else
		{
			printf ("unknown option %s\n", argv[i]);
			usage ();
			exit (1);
		}
	}

	tm.mh_type = MH_STATS;
	if (hosts == NULL)
	{
		msg_send (MSG_MGR_NAME, MT_MESSAGE, 0, &tm, sizeof (tm));
		status = msg_await ();
	}
	else
	{
	        status = STATUS_OK;
		for (i = 0; i < nhost; ++i)
		{
			if (ShowUser)
				Username[0] = '\0';
			else
				printf ("----- %s\n", hosts[i]);
			sprintf (manager, "%s@%s", MSG_MGR_NAME, hosts[i]);
			msg_send (manager, MT_MESSAGE, 0, &tm, sizeof (tm));
			status &= msg_await ();
		}
		free (hosts);
	}

	return ((status == STATUS_OK) ? 0 : 1);
}



int
handler (msg)
struct message *msg;
{
	struct mh_stats *mhs = (struct mh_stats *) msg->m_data;

	if (msg->m_proto != MT_MESSAGE)
		return (0);
	if (mhs->mh_type != MH_STATS)
	{
		fprintf (stderr, "unexpected message type %d\n", mhs->mh_type);
		return (1);
	}
	if (mhs->mh_text[0] == '\0')
		return (STATUS_OK);
	if (! ShowUser)
		printf ("%s\n", mhs->mh_text);
	else if (! Username[0])
	{
		int i = 0;
		char *paren = strchr (mhs->mh_text, '(');
		while (++paren && *paren != ')')
			Username[i++] = *paren;
		printf ("%s\n", Username);
	}
	return (0);
}
