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

# include <defs.h>
# include "message.h"

MAKE_RCSID ("$Id: zquery.c,v 1.2 1994-10-11 16:27:08 corbet Exp $")


int IncMsg (), QResp ();


main (argc, argv)
int argc;
char **argv;
{
/*
 * Make sure they gave us a recipient.
 */
	if (argc != 2)
	{
		printf ("Usage: %s process\n", argv[0]);
		exit (1);
	}
/*
 * Hook in and send the query..
 */
	msg_connect (IncMsg, "zquery");
	msg_SendQuery (argv[1], QResp);
/*
 * Now we just wait.
 */
	msg_await ();
}




int
IncMsg (msg)
struct message *msg;
{
	if (msg->m_proto != MT_MESSAGE)
		return;
	if (msg->m_len == 0)
		return (1);
	printf ("%s\n", msg->m_data);
	return (0);
}





int
QResp (info)
char *info;
/*
 * Something came back.
 */
{
	if (! info)
		exit (0);
	printf ("%s\n", info);
}
