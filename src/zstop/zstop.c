/*
 * Message server zapper.
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

RCSID("$Id: zstop.c,v 1.5 1995-06-29 22:39:04 granger Exp $")

static char *argv0;

void
main (argc, argv)
int argc;
char **argv;
{
	int handler ();
	struct mh_template tm;

	if ((argv0 = strrchr(argv[0], '/')) != NULL)
		++argv0;
	else
		argv0 = argv[0];
	if (! msg_connect (handler, "Grim reaper"))
		exit (1);
	tm.mh_type = MH_DIE;
	msg_send (MSG_MGR_NAME, MT_MESSAGE, 0, &tm, sizeof (tm));
	/* 
	 * Verify receipt of the death notice instead of rudely exiting
	 * right away and causing write errors in the handler
	 */
	while (msg_poll(5) != MSG_TIMEOUT)
	{
		/* wait for a shutdown message or a timeout */
	}
	printf ("%s: shutdown message never received.\n", argv0);
	exit (1);
}



int
handler (msg)
Message *msg;
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;

	if (tm->mh_type == MH_SHUTDOWN)
	{
		printf ("%s: message manager shutting down\n", argv0);
		exit (0);
	}
	else
		return (0);
}
