/*
 * The pdmonitor client, meant to run under epoch/emacs.
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
# include <stdio.h>
# include <unistd.h>
# include <defs.h>
# include <message.h>
# include "pd.h"
# include "pdmon.h"

MAKE_RCSID ("$Id: pdmon.c,v 1.6 1999-03-23 16:41:49 burghart Exp $")

char *Process;


/*
 * Forwards.
 */
int Handler FP ((Message *));
int Input FP ((int));
void MonitorMsg FP ((pdmTemplate *));
static void UnHook FP ((void));



int
main (argc, argv)
int argc;
char **argv;
/*
 * pdmon process
 */
{
	char pname[MSG_MAXNAMELEN];
	pdmTemplate pt;
/*
 * See that they know what they are doing.
 */
	if (argc != 2)
	{
		printf ("Usage: pdmon process\n");
		exit (1);
	}
	Process = argv[1];
/*
 * Hook into the message system.
 */
	sprintf (pname, "pdmon-%s", Process);
	msg_connect (Handler, pname);
	msg_add_fd (0, Input);
/*
 * Connect to the process.
 */
	pt.pt_Type = pdm_HookIn;
	msg_send (Process, MT_PDMON, FALSE, &pt, sizeof (pt));
/*
 * Wait.
 */
	msg_await ();
	return (0);
}




int
Handler (msg)
Message *msg;
/*
 * Here is a message.
 */
{
	switch (msg->m_proto)
	{
	   case MT_PDMON:
	   	MonitorMsg ((pdmTemplate *) msg->m_data);
		break;
	}
	return (0);
}




void
MonitorMsg (pt)
pdmTemplate *pt;
/*
 * Deal with a monitor message.
 */
{
	pdmPD *ppd;

	switch (pt->pt_Type)
	{
	   case pdm_Exit:
	   	printf ("$Exit: Graphics process exit.\n");
		exit (0);

	   case pdm_MyPD:
	   	ppd = (pdmPD *) pt;
		printf ("%d\n", ppd->pt_Len);
		fflush (stdout);
		write (1, ppd->pt_Pd, ppd->pt_Len);
		break;
	default:
	  break;
	}
}





int
Input (fd)
int fd;
/*
 * Some sort of input from the editor.
 */
{
	char line[100];
	int len, nread, nfail = 0;
	char *bp;
	pdmPD *ppd;
/*
 * Pull in the length line.
 */
	if (!fgets (line, sizeof (line), stdin))
	{
		UnHook ();
		exit (0);
	}
	if (! sscanf (line, "%d", &len))
	{
		printf ("Bad length line\n");
		exit (1);
	}
/*
 * Get some space and read in the PD.
 */
	ppd = (pdmPD *) malloc (sizeof (pdmPD) + len);
	ppd->pt_Type = pdm_NewPD;
	ppd->pt_Len = len;
	bp = ppd->pt_Pd;
	for (nread = 0; nread < len; )
	{
		int thisread = read (0, bp, len - nread);
		if (thisread <= 0)
		{
                        if (++nfail > 5)
                        {
                                UnHook ();
                                printf ("Read error\n");
                                exit (1);
                        }
		}
                nfail = 0;
		nread += thisread;
		bp += thisread;
	}
/*
 * Ship it off to the process.
 */
	msg_send (Process, MT_PDMON, FALSE, ppd, sizeof (pdmPD) + len);
	free (ppd);
	return (0);
}




static void
UnHook ()
/*
 * Disconnect from the monitored process.
 */
{
	pdmTemplate pt;

	pt.pt_Type = pdm_UnHook;
	msg_send (Process, MT_PDMON, FALSE, &pt, sizeof (pt));
}
