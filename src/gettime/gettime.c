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
static char *rcsid = "$Id: gettime.c,v 2.4 1995-06-29 22:34:14 granger Exp $";
/*
 * Timer module test code.
 */
# include <signal.h>
# include "defs.h"
# include "message.h"
# include "copyright.h"
# include "timer.h"


int msg_handler (), intr (), Slot;
void alrm ();

int
main (argc, argv)
int argc;
char **argv;
{
	struct tm_req tr;
	UItime t;

	msg_connect (msg_handler, "GetTime");
/*
 * Just get and print the time.
 */
	tl_GetTime (&t);
	printf ("Time is %d %d\n", t.ds_yymmdd, t.ds_hhmmss);
	return (0);
}



msg_handler (msg)
struct message *msg;
{
	return (0);
}



int
intr ()
{
	struct tm_req tr;

	printf ("OUCH! (%d)\n", Slot);
	tl_Cancel (Slot);
	return (0);
}



void
alrm (now, param)
UItime *now;
char *param;
{
	printf ("Alarm at %d %d, param '%s'\n", now->ds_yymmdd,
		now->ds_hhmmss, param);
}
