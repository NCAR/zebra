/*
 * UAV serial line grabber process.
 *
 *	uav_grabber name port speed consumer
 *
 * SLGrabber could not be used because it makes all kinds of assumptions
 * about reading textual data, stripping high-order bits, skipping newlines
 * and the like.  This is essentially SLGrabber with those assumptions
 * removed and the ability to detect GCS packet structures.

TODO:

Allow Ingest options?
Make sure everything is prototype up top.
More status info
Testing mode which reads bytes from file and sends to consumer

 */
/*		Copyright (C) 1987-2000 by UCAR
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
# include <string.h>

# include <defs.h>
# include <message.h>
# include <timer.h>
# include "SLGrabber.h"
# include "Serial.h"

RCSID("$Id: SLGrabber.c,v 2.14 2000-07-19 23:08:46 granger Exp $")

#define DEFAULT_NAME	"Serial"	/* Default message name */

static char *Consumer = 0;

static int  MHandler FP ((Message *));
static void Usage FP((const char *));
static void Send (SerialConnection *conn, const ubyte *, int len);



static void
Usage (prog)
const char *prog;
{
   printf("Usage: %s [options] [consumer]\n", prog);
   printf("Read binary or line text data from a serial port and\n");
   printf("send it to a consumer process via the message handler.\n");
   printf("With no consumer specified, send the data to standard output.\n");
   SerialUsage ();
   printf("SLGrabber Options:\n"); 
   printf("   -h          Print this usage message\n");	
   printf("   -n <name>   Our message handler name [%s]\n", DEFAULT_NAME);
}



static void
ParseCommandLine (int *argc, char *argv[], char **name)
{
	extern int optind;
	extern char *optarg;
	int c;

	Consumer = 0;
	while ((c = getopt(*argc, argv, "hn:")) >= 0)
	{
		switch (c)
		{
		   case 'h':
			Usage (argv[0]);
			exit (0);
		   case 'n':
			*name = optarg;
			break;
		   case '?':
		   default:
			Usage (argv[0]);
			exit (10);
			break;
		}
	}
	if (*argc - optind > 1)
	{
		printf ("%s: only one consumer name allowed", argv[0]);
		exit (1);
	}
	else if (*argc - optind == 1)
	{
		Consumer = argv[optind];
	}
 	if (Consumer && ! strcmp (Consumer, "print"))
		Consumer = NULL;
}



int
main (int argc, char **argv)
{
	char *name;
	char buf[64];
	SerialConnection *conn;
/*
 * First thing we do is initialize a connection to the defaults, and
 * then customize it according to the command line options.
 */
	conn = SerialInitialize ();
	SerialParseOptions (conn, &argc, argv);

	name = DEFAULT_NAME;
	ParseCommandLine (&argc, argv, &name);
/* 
 * Hook in.  (Use dash, colon is not a valid character in message names.)
 */
	sprintf (buf, "%s", name);
	if (! msg_connect (MHandler, buf))
		SerialDie ("message connect failure", 2);
/*
 * Connect to the port and pass our serial data handler.
 */
	SerialConnect (conn, &Send);
/*
 * Now we just wait for things.
 */
	msg_await ();
/*
 * We shouldn't really reach this point, so return non-zero.
 */
	return 1;
}





static int
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
			SerialDie ("message handler shutdown", 0);
	}
	return (0);
}


static void
Send (SerialConnection *conn,
      const ubyte *data,	/* our array of data bytes 		   */
      int len)			/* number of bytes to send from data array */
/*
 * A serial data handler which either sends the data off in a message to
 * the Consumer, or else writes the data to output.  Nothing fancy here.
 */
{
	static char *dbuf = 0;
	static int dlen = 0;
	SerialMode mode = SerialGetMode (conn);

	if (len <= 0)		/* just in case */
		return;

	if (Consumer)
	{
	        SLdata *sld = (SLdata *) dbuf;

		if (!dbuf)
		{
		    dlen = sizeof(SLdata) + len + 32;
		    dbuf = (char *) malloc (dlen);
		}
		else if (dlen < len)
		{
		    dlen = len + 32;
		    dbuf = (char *) realloc (dbuf, dlen);
		}
	/*
	 * Prepare the packet.
	 */
		sld->sl_type = SL_DATA;
		sld->sl_len = len;
		memcpy (sld->sl_data, data, len);
	/*
	 * Out it goes.
	 */
		msg_send (Consumer, MT_SLDATA, FALSE, sld,
			  sld->sl_len + sizeof (SLdata) - 1);
	}
	else if (mode == TextMode)
	{
	    /* Print the text with the line separations and flush it out. */
	    fprintf (stdout, "%s\n", data);
	    fflush (stdout);
	}
	else
	{
	    /* Write the bytes to stdout. */
	    fwrite (data, len, 1, stdout);
	}
}


