/*
 * Exercise the "ping" feature.
 */
static char *rcsid = "$Id: msg_ping.c,v 2.1 1991-09-12 02:02:09 corbet Exp $";
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
# include "message.h"


# ifdef __STDC__
	int	Handler (Message *);
# else
	int	Handler ();
# endif

int NSent = 0, NGot = 0;

main (argc, argv)
int argc;
char **argv;
{
	int i;
/*
 * Hook in.
 */
	if (! msg_connect (Handler, "Mad Pinger"))
		exit (1);
/*
 * Go through and do it.
 */
	if (argc < 2)
		Ping (0);
	else
		for (i = 1; i < argc; i++)
			Ping (argv[i]);
	msg_await ();
}





Ping (host)
char *host;
/*
 * Ping this host.
 */
{
	char to[60];
	int proto = MT_PING;
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
		else
			sprintf (to, "%s@%s", MSG_MGR_NAME, host);
	}
	else
		strcpy (to, MSG_MGR_NAME);
/*
 * Send it.
 */
	msg_send (to, proto, FALSE, to, 0);
	NSent++;
}




int
Handler (msg)
Message *msg;
/*
 * Deal with responses.
 */
{
	if (msg->m_proto != MT_PING)
	{
		printf ("Strange response protocol %d.\n", msg->m_proto);
		exit (1);
	}
	printf ("Response from %s\n", msg->m_from);
	if (++NGot >= NSent)
		exit (0);
	return (0);
}
