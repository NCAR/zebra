/*
 * Message server status grabber.
 */
static char *rcsid = "$Id: mstatus.c,v 2.1 1991-09-12 02:02:09 corbet Exp $";

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
# include "message.h"
# include <copyright.h>

main ()
{
	int handler ();
	struct mh_template tm;

	if (! msg_connect (handler, "Status reporter"))
		exit (1);
	tm.mh_type = MH_STATS;
	msg_send (MSG_MGR_NAME, MT_MESSAGE, 0, &tm, sizeof (tm));
	msg_await ();
}



handler (msg)
struct message *msg;
{
	if (msg->m_proto != MT_MESSAGE)
		return;
	if (msg->m_len == 0)
		return (1);
	printf ("%s\n", msg->m_data);
	return (0);
}
