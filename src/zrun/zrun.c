/*
 * Run a command in a zeb process.
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

# include <unistd.h>

# include <defs.h>
# include "message.h"

MAKE_RCSID ("$Id: zrun.c,v 1.3 1995-04-20 08:02:29 granger Exp $")


int IncMsg ();


int
main (argc, argv)
int argc;
char **argv;
{
	char name[20];
/*
 * Make sure they gave us the info
 */
	if (argc != 3)
	{
		printf ("Usage: %s process command\n", argv[0]);
		exit (1);
	}
/*
 * Hook in and send the command
 */
	sprintf (name, "zrun-%i", getpid() );
	msg_connect (IncMsg, name);
	cp_Exec (argv[1], argv[2]);
/*
 * Clear any messages, and give the command time to be sent.
 */
	while (msg_poll(1) != MSG_TIMEOUT)
		/* keep polling */;
	exit (0);
}




int
IncMsg (msg)
struct message *msg;
{
	if (msg->m_proto != MT_MESSAGE)
		return (0);
	return (0);
}


