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
/*
 * Change the current time.
 */
# include <signal.h>

# include <defs.h>
# include <message.h>
# include <copyright.h>
# include <timer.h>

RCSID ("$Id: settime.c,v 2.6 1995-07-06 04:49:33 granger Exp $")


int msg_handler (), intr (), Slot;
void alrm ();

int
main (argc, argv)
int argc;
char **argv;
{
	struct tm_prt prt;
	date t;

	if (argc != 3)
	{
		printf ("Usage: settime YYMMDD HHMMSS\n");
		exit (1);
	}
	msg_connect (msg_handler, getenv ("USER"));
/*
 * Put together and send off the request.
 */
	prt.tr_type = TR_PRT;
	t.ds_yymmdd = atoi (argv[1]);
	t.ds_hhmmss = atoi (argv[2]);
	TC_UIToZt (&t, &prt.tr_time);
	prt.tr_scale = 1;
	msg_send ("Timer", MT_TIMER, FALSE, &prt, sizeof (prt));
	return (0);
}



int
msg_handler (msg)
struct message *msg;
{
	return (0);
}




int
intr ()
{
	printf ("OUCH! (%d)\n", Slot);
	tl_Cancel (Slot);
	return (0);
}



void
alrm (now, param)
UItime *now;
char *param;
{
	printf ("Alarm at %li %li, param '%s'\n", now->ds_yymmdd,
		now->ds_hhmmss, param);
}
