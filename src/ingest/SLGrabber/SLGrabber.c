/*
 * Generic serial line grabber process.
 *
 *	SLGrabber name port speed number recipient
 *
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

# include <copyright.h>
# include <stdio.h>
# include <errno.h>
# include <fcntl.h>
# include <sgtty.h>

# include <defs.h>
# include <message.h>
# include <timer.h>
# include "SLGrabber.h"

MAKE_RCSID("$Id: SLGrabber.c,v 2.5 1994-02-25 10:32:44 granger Exp $")

int Fd;
FILE *FpFd;


# ifdef __STDC__
	int MHandler (Message *);
	void Connect (char *, char *, char *);
	void GetPort (char *, char *);
	void Dial (char *);
	int SLData ();
	void Die (void);
	void Send (char *);
	void CheckDCD (UItime *, int);
# else
	int MHandler();
	void Connect();
	void GetPort();
	void Dial ();
	int SLData ();
	void Die ();
	void Send ();
	void CheckDCD();
# endif

# define CHECKINT 900		/* DCD Check interval	*/
int TSlot;

char LBuf[500];
int NLBuf = 0;
char *Port, *Speed, *Number, *Recip;


main (argc, argv)
int argc; 
char **argv;
{
/*
 * The usual sanity checking.
 */
	if (argc != 6)
	{
		printf ("Usage: SLGrabber name port speed number recipient\n");
		exit (1);
	}
/* 
 * Hook in.
 */
	if (! msg_connect (MHandler, argv[1]))
	{
		printf ("Message connect failure\n");
		exit (1);
	}
/*
 * Connect to the port.
 */
	Connect (Port = argv[2], Speed = argv[3], Number = argv[4]);
	Recip = argv[5];
	if (! strcmp (Recip, "print"))
		Recip = 0;
/*
 * Now we just wait for things.
 */
	msg_add_fd (Fd, SLData);
	msg_await ();
}






MHandler (msg)
Message *msg;
/*
 * Somebody wants something.
 */
{
/*
 * Everything is assumed to be a message handler event.
 */	
	if (msg->m_proto == MT_MESSAGE)
	{
		struct mh_template *tmpl = (struct mh_template *) msg->m_data;
		if (tmpl->mh_type == MH_SHUTDOWN)
			Die ();
	}
	return (0);
}



void
Die ()
{
	close (Fd);
	exit (1);
}



void
Connect (port, speed, number)
char *port, *speed, *number;
/*
 * Make a connection to this port.
 */
{
/*
 * Get the port.
 */
	GetPort (port, speed);
/*
 * Now dial it.
 */
	Dial (number);
}




void
GetPort (port, speed)
char *port, *speed;
/*
 * Open and tweak the serial port.
 */
{
	static struct speed_table
	{
		char *c_speed;
		int n_speed;
	} Speeds[] =
	{
		{ "1200",	B1200 	},
		{ "2400",	B2400 	},
		{ "9600",	B9600 	},
		{ "19200",	B19200 	},
		{ 0,		0	},
	};
	int i;
	struct sgttyb tbuf;
/*
 * Locate our speed.
 */
 	for (i = 0; Speeds[i].c_speed; i++)
		if (! strcmp (speed, Speeds[i].c_speed))
			break;
	if (! Speeds[i].c_speed)
	{
		msg_ELog (EF_PROBLEM, "Bad speed %s", speed);
		exit (1);
	}
/*
 * Open the port.
 */
	if ((Fd = open (port, O_RDWR, 0)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, port);
		exit (1);
	}
	FpFd = fdopen (Fd, "r+");
# ifdef notdef
/*
 * Get the current TTY parameters.
 */ 
	if (ioctl (Fd, TIOCGETP, &tbuf) != 0)
	{
		msg_ELog (EF_EMERGENCY, "ioctl error (%d)", errno);
		exit (1);
	}
# endif
/*
 * Tweak it.
 */
	tbuf.sg_flags = RAW;
	tbuf.sg_ispeed = tbuf.sg_ospeed = Speeds[i].n_speed;	
/*
 * Store the new parameters.
 */
	if (ioctl (Fd, TIOCSETP, &tbuf) != 0)
	{
		msg_ELog (EF_EMERGENCY, "Ioctl error %d", errno);
		exit (1);
	}
}




void WritePort (stuff)
char *stuff;
/*
 * Write this stuff.
 */
{
	write (Fd, stuff, strlen (stuff));
}



int
ReadPort (dest, len)
char *dest;
int len;
/*
 * Read something back.
 */
{
	return (read (Fd, dest, len));
}





void
Dial (number)
char *number;
/*
 * Work until something comes in.
 */
{
	while (! DoDial (number))
		sleep (20);		/* Should check msg */
}



int
DoDial (number)
char *number;
/*
 * Try to make a connection to this number.
 */
{
	char buf[80];
	int nc;
/*
 * Put the modem in the state we want.
 */
	WritePort ("AT V0 E0\r");
	sleep (5);
	(void) ReadPort (buf, 80);	/* Clean out junk */
/*
 * Tell it to dial.
 */
	WritePort ("ATDT");
	WritePort (number);
	WritePort ("\r");
/*
 * Get something back and see what it thinks.
 */
again:
	nc = ReadPort (buf, 1);
	switch (buf[0])
	{
		case '\r':
		case '\n':
			msg_ELog (EF_DEBUG, "Swallow newline");
			goto again;

		case '0':
		case '1':
		case '5':
			msg_ELog (EF_INFO, "Connected.");
# ifdef notdef
			sleep (10);
			WritePort ("\r");
# endif
			TSlot = tl_AddRelativeEvent (CheckDCD, 0,
				CHECKINT*INCFRAC, CHECKINT*INCFRAC);
			return (TRUE);

		case '3':
			msg_ELog (EF_PROBLEM, "No carrier detected.");
			break;
		case '4':
			msg_ELog (EF_PROBLEM, "Command error.");
			break;
		case '6':
			msg_ELog (EF_PROBLEM, "No dialtone.");
			break;
		case '7':
			msg_ELog (EF_PROBLEM, "Black Box is busy.");
			break;
		case '8':
			msg_ELog (EF_PROBLEM, "No answer.");
			break;
		default:
			msg_ELog (EF_PROBLEM, "Didn't expect to get (%c).",
				buf[0]);
			break;
	}
	return (FALSE);
}






int
SLData ()
/*
 * Here's some serial line data.
 */
{
	int nread = ReadPort (LBuf + NLBuf, 500 - NLBuf), nleft, i;
	char *cp, *newline, *strchr ();
/*
 * If things are dead, start over.
 */
	if (nread <= 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d reading port", errno);
		close (Fd);	/* Easy way to drop DTR */
		Connect (Port, Speed, Number);
	}
/*
 * Zap out the top bit for now.
 */
	for (cp = LBuf + NLBuf, i = 0; i < nread; i++)
		*cp++ &= 0x7f;
/*
 * Skip past returns, then parse out any lines that we find.
 */
	NLBuf += nread;
	for (cp = LBuf; cp - LBuf < NLBuf && *cp < '\040'; cp++)
		;
/*
 * Now look for newlines indicating that whole lines have been read.
 */
	LBuf[NLBuf] = '\0';
	while ((cp - LBuf) < NLBuf && (newline = strchr (cp, '\n')))
	{
		*newline = '\0';
		Send (cp);
		cp = newline + 1;
	}
/*
 * Copy over what's left and we're done.
 */
	nleft = NLBuf - (cp - LBuf);
	if (cp - LBuf < NLBuf)
		memcpy (LBuf, cp, nleft);
	NLBuf = nleft;
	return (0);
}





void
Send (data)
char *data;
/*
 * Send this data.
 */
{
	static char dbuf[1024];
	sldata *sld = (sldata *) dbuf;
/*
 * Prepare the packet.
 */
	sld->sl_type = SL_DATA;
	sld->sl_len = strlen (data) + 1;
	strcpy (sld->sl_data, data);
/*
 * Out it goes.
 */
	if (Recip)
		msg_send (Recip, MT_SLDATA, FALSE, sld,
			sld->sl_len + sizeof (sldata));
	else
		printf ("%s\n", data);
}








void
CheckDCD (t, junk)
UItime *t;
int junk;
/*
 * Check to see that we are still connected here.
 */
{
	int mbits;
/*
 * Do the check.
 */
	if (ioctl (Fd, TIOCMGET, &mbits) < 0)
		msg_ELog (EF_PROBLEM, "DCD Check ioctl failure %d", errno);
	else if (! mbits & TIOCM_CAR)
		msg_ELog (EF_PROBLEM, "Carrier lost on port");
	else
		return;
/*
 * Cancel the timer request.
 */
	tl_Cancel (TSlot);
/*
 * Now close the port and start over.
 */
	close (Fd);
	Connect (Port, Speed, Number);
}
