/*
 * Exercise the "ping" feature.
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
# include <signal.h>
# include <unistd.h>
# include <string.h>

# include <defs.h>
# include <message.h>

MAKE_RCSID("$Id: msg_ping.c,v 2.5 1995-06-29 22:37:19 granger Exp $")

#define DEFAULT_DELAY 	20

#define EXIT_OK		0
#define EXIT_TIMEOUT 	1
#define NO_CONNECT 	2
#define EXIT_SHUTDOWN	3
#define ERR_UNKNOWN	9
#define BAD_OPTION	99

static int Handler FP((Message *));
static void Ping FP((char *host, int proto));
static void Timeout FP((int sig));

int NSent = 0, NGot = 0;

static void
usage (prog)
char *prog;
{
	printf ("usage: %s [-t timeout] [-h] [-c] [host|client] ...\n", prog);
	printf ("   timeout is seconds before timing out (default is %d),\n",
		DEFAULT_DELAY);
	printf ("   -h selects host ping, -c selects client ping.\n");
	printf ("   Options can appear any number of times and affect\n");
	printf ("   the host or client names which follow it up to the\n");
	printf ("   next option.  A name containing '@' automatically uses\n");
	printf ("   client ping.  Host pinging is the default.  If no\n");
	printf ("   host or client name arguments, the message manager is\n");
	printf ("   is pinged.\n");
	printf ("example: %s -h zappa -c timer timer@zappa -h zorro\n", prog);
}


void
main (argc, argv)
int argc;
char **argv;
{
	char name[64];
	int proto = MT_PING;
	int delay = DEFAULT_DELAY;
	int pinged = 0;
	int i;
/*
 * Hook in.
 */
	sprintf (name, "Mad Pinger %i", getpid());
	if (! msg_connect (Handler, name))
		exit (NO_CONNECT);
/*
 * Go through and do it.
 */
	i = 1;
	while (i < argc)
	{
		if (argv[i][0] != '-')
		{
			Ping (argv[i], proto);
			pinged = 1;
		}
		else if (! strcmp(argv[i], "-h"))
			proto = MT_PING;
		else if (! strcmp(argv[i], "-c"))
			proto = MT_CPING;
		else if (! strcmp(argv[i], "-t"))
		{
			++i;
			if (i < argc)
				delay = atoi(argv[i]);
			else
			{
				printf ("%s: -t needs argument\n", argv[0]);
				exit (BAD_OPTION);
			}
			delay = delay > 0 ? delay : DEFAULT_DELAY;
		}
		else
		{
			printf ("%s: unknown option '%s'\n", 
				argv[0], argv[i]);
			usage (argv[0]);
			exit (BAD_OPTION);
		}
		++i;
	}
	if (! pinged)
		Ping (NULL, proto);

	signal (SIGALRM, Timeout);
	alarm (delay);
	msg_await ();
	exit (ERR_UNKNOWN);
}




static void
Ping (host, proto)
char *host;
int proto;	/* MT_PING or MT_CPING */
/*
 * PING this host.
 */
{
	char to[60];
/*
 * Figure out who this is going to.
 */
	if (host)
	{
		if (strchr (host, '@'))
		{
			strcpy (to, host);
			proto = MT_CPING;
		}
		else if (proto == MT_PING)
			sprintf (to, "%s@%s", MSG_MGR_NAME, host);
		else /* proto == MT_CPING */
			strcpy (to, host);
	}
	else
		strcpy (to, MSG_MGR_NAME);
/*
 * Send it.
 */
	msg_send (to, proto, FALSE, NULL, 0);
	NSent++;
}




static int
Handler (msg)
Message *msg;
/*
 * Deal with responses.
 */
{
	if ((msg->m_proto == MT_MESSAGE) && (msg->m_len > 0))
	{
		struct mh_template *tm;
		tm = (struct mh_template *) msg->m_data;
		if (tm->mh_type == MH_SHUTDOWN)
		{
			printf ("msg_ping: shutdown received\n");
			exit (EXIT_SHUTDOWN);
		}
	}
	else if (msg->m_proto != MT_PING)
	{
		printf ("Strange response protocol %d.\n", msg->m_proto);
		exit (ERR_UNKNOWN);
	}
	printf ("Response from %s\n", msg->m_from);
	if (++NGot >= NSent)
		exit (EXIT_OK);
	return (0);
}


/*ARGSUSED*/
static void
Timeout (sig)
int sig;
{
	printf ("msg_ping: timeout: got %d boings from %d pings\n",
		NGot, NSent);
	exit (EXIT_TIMEOUT);
}

