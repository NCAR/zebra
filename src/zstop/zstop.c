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

RCSID("$Id: zstop.c,v 1.10 2004-07-05 18:04:20 granger Exp $")

static char *argv0;


/*
 * Send the shutdown message to a particular process.
 */
static void
SendShutdown (char *name, int broadcast)
{
	struct mh_template tm;

	tm.mh_type = MH_SHUTDOWN;
	msg_send (name, MT_MESSAGE, broadcast, &tm, sizeof (tm));
}



static void
SendDie ()
{
	struct mh_template tm;

	tm.mh_type = MH_DIE;
	msg_send (MSG_MGR_NAME, MT_MESSAGE, 0, &tm, sizeof (tm));
}


static void
Usage (char *prog)
{
    printf ("Usage: %s [-help]\n", prog);
    printf ("       %s [-help] [-b(roadcast) group] [client] [...]\n", prog);
    printf ("If no arguments, send the DIE message to the\n");
    printf ("message manager.\n");
    exit (1);
}



int
main (argc, argv)
int argc;
char **argv;
{
	int handler ();
	int i;
	int len;

	if ((argv0 = strrchr(argv[0], '/')) != NULL)
		++argv0;
	else
		argv0 = argv[0];


	/* Check for help request */
	if (argc > 1 && (len = strlen (argv[1])) > 1 && 
	    strncmp("-help",argv[1],len) == 0)
	{
	    Usage (argv0);
	}

	if (! msg_connect (handler, "Grim reaper"))
		exit (1);
	/*
	 * Process the command line arguments.
	 */
	i = 1;
	while (i < argc)
	{
	    char *name;
	    int broadcast;

	    len = strlen (argv[i]);

	    if (len > 1 && strncmp("-broadcast",argv[i],len) == 0)
	    {
		broadcast = 1;
		++i;
		if (! argv[i]) Usage (argv0);
		name = argv[i];
		printf ("%s: broadcasting shutdown to group %s\n", 
			argv0, name);
	    }
	    else
	    {
		broadcast = 0;
		name = argv[i];
		printf ("%s: sending shutdown to client %s\n", argv0, name);
	    }
	    ++i;
	    SendShutdown (name, broadcast);
	}

	if (argc == 1)
	{
	    /*
	     * Default is still to just kill the message manager. 
	     */
	    SendDie ();

	    /* 
	     * Check that we receive notice of the shutdown we just tried
	     * to start.  
	     */
	    while (msg_poll(5) == 0)
	    {
		/* wait for a shutdown message or a timeout */
	    }
	    if (argc == 1)
	    {
		printf ("%s: shutdown message never received.\n", argv0);
		exit (1);
	    }
	}

	/* 
	 * Try to avoid some rude errors about lost connections by
	 * clearing messages (such as automatic event logger messages)
	 * before disconnecting.
	 */
	msg_poll(1);
	msg_disconnect ();
	exit (0);
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

	return (0);
}
