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
# include <fcntl.h>
# include <termios.h>
# include <sys/ioctl.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include <timer.h>
# include "SLGrabber.h"

MAKE_RCSID("$Id: SLGrabber.c,v 2.13 2000-06-15 23:54:45 granger Exp $")

typedef enum { UnspecifiedMode, TextMode, ByteMode } Mode;

typedef unsigned char ubyte;		/* unsigned byte data		*/

#define DEFAULT_NAME	"Serial"	/* Default message name */
#define DEFAULT_TERM	"/dev/ttya"	/* Default port device	*/
#define DEFAULT_SPEED	"9600"		/* Default baud rate	*/
#define DEFAULT_PHONE	NULL		/* No default phone no. */
#define DEFAULT_MODE	TextMode	/* historical		*/
#define DEFAULT_BLOCK	512		/* default block size	*/

#define CHECKINT 	900		/* DCD Check interval	*/
#define BUFSIZE       	500		/* serial buffer length	*/

typedef struct _Connection
{
	Mode	c_Mode;
	int 	c_Fd;
	int 	c_TSlot;
	char 	*c_Port;
	char 	*c_Speed;
	char 	*c_Number;
	char 	*c_Consumer;
	int	c_blocksize;
        int     c_striphibit;
	int 	c_nbuf;
	ubyte	c_buf[BUFSIZE];
} Connection;

/*
 * This is necessary until msg_add_fd allows a parameter to be passed
 * to the handler routine.
 */
Connection *Global;

static int  MHandler FP ((Message *));
static void MWait FP ((int timeout));
static void Connect FP ((Connection *));
static void Reconnect FP ((Connection *));
static void GetPort FP ((Connection *));
static void Dial FP ((Connection *));
static int  SLDataHandler FP((void));
static void Die FP((const char *, int exit_code));
static void Usage FP((const char *));
static int  SendLines FP((Connection *conn, int start));
static int  SendBlocks FP((Connection *conn, int start));
static void Send FP ((Connection *conn, const ubyte *, int len));
static void CheckDCD FP ((UItime *t, int parm));
static int  DoDial FP((Connection *conn));


static void
Usage (prog)
const char *prog;
{
   printf("Usage: %s [options] [consumer]\n", prog);
   printf("Read binary or line text data from a serial port and\n");
   printf("send it to a consumer process via the message handler.\n");
   printf("With no consumer specified, send the data to standard output.\n");
   printf("Options:\n"); 
   printf("   -h          Print this usage message\n");	
   printf("   -n <name>   Our message handler name [%s]\n", 
	  DEFAULT_NAME);
   printf("   -p <port>   The serial port to connect to [%s]\n",
	  DEFAULT_TERM);
   printf("   -s <baud>   The baud rate to use for the port [%s]\n",
	  DEFAULT_SPEED);
   printf("   -d <ph#>    Phone number to dial [default is none]\n");
   printf("   -l          Character lines and text mode.\n");
   printf("               Passes character data to consumer\n");
   printf("               line by line, removing line feeds and returns.\n");
   printf("   -7          Clear high-order bit in text mode. [default off]\n");
   printf("   -u          Binary mode.  Bytes are shipped to the consumer\n");
   printf("               exactly as they are read.\n");
   printf("   -f <size>   Blocking factor in bytes.  Data will be sent\n");
   printf("               to the consumer in blocks no larger than this\n");
   printf("               size, but possibly smaller. [%d]\n",
	  DEFAULT_BLOCK);
   printf("The default mode is %s\n", 
	  (DEFAULT_MODE == TextMode) ? "LINE" : "BYTE");
}


static void
Die (msg, code)
const char *msg;
int code;
{
	printf ("%s\n", msg);
	if (Global->c_Fd >= 0)
		close (Global->c_Fd);
	exit (code);
}


static void
InitializeConnection (conn)
Connection *conn;
{
	conn->c_Mode = DEFAULT_MODE;
	conn->c_Fd = -1;
	conn->c_TSlot = -1;
	conn->c_Port = DEFAULT_TERM;
	conn->c_Speed = DEFAULT_SPEED;
	conn->c_Number = DEFAULT_PHONE;
 	conn->c_Consumer = NULL;
	conn->c_blocksize = DEFAULT_BLOCK;
	conn->c_striphibit = 0;
	conn->c_nbuf = 0;
	conn->c_buf[0] = 0;
}



static void
ParseCommandLine (argc, argv, conn, name)
int *argc;
char *argv[];
Connection *conn;	/* Connection parameters */
char **name;		/* Our process name	 */
{
	extern int optind;
	extern char *optarg;
	int c;

	while ((c = getopt(*argc, argv, "hn:p:s:d:luf:")) >= 0)
	{
		switch (c)
		{
		   case 'h':
			Usage (argv[0]);
			exit (0);
		   case 'l':
			conn->c_Mode = TextMode;
			break;
		   case 'u':
			conn->c_Mode = ByteMode;
			break;
		   case 'f':
			conn->c_blocksize = atoi(optarg);
			break;
		   case 's':
			conn->c_Speed = optarg;
			break;
		   case '7':
			conn->c_striphibit = 1;
			break;
		   case 'n':
			*name = optarg;
			break;
		   case 'p':
			conn->c_Port = optarg;
			break;
		   case 'd':
			conn->c_Number = optarg;
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
		conn->c_Consumer = argv[optind];
	}
}



main (argc, argv)
int argc; 
char **argv;
{
	Connection conn;
	char *name;
	char buf[64];
/*
 * First thing we do is initialize a connection to the defaults, and
 * then customize it according to the command line options.
 */
	InitializeConnection (&conn);
	Global = &conn;
	name = DEFAULT_NAME;
	ParseCommandLine (&argc, argv, &conn, &name);

	if (conn.c_Consumer && ! strcmp (conn.c_Consumer, "print"))
		conn.c_Consumer = NULL;
/* 
 * Hook in.  (Use dash, colon is not a valid character in message names.)
 */
	sprintf (buf, "%s-%s", name, conn.c_Port);
	if (! msg_connect (MHandler, buf))
		Die ("message connect failure", 2);
/*
 * Connect to the port.
 */
	Connect (&conn);
/*
 * Now we just wait for things.
 */
	msg_await ();
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
			Die ("message handler shutdown", 0);
	}
	return (0);
}


static void
MWait (timeout)
int timeout;
/*
 * Keep polling for messages until <timeout> seconds have passed
 */
{
	ZebTime last;
	ZebTime now;

	msg_ELog (EF_DEBUG, "waiting %d seconds", timeout);
	tl_Time(&last);
	while (timeout > 0)
	{
		if (msg_poll (timeout) == MSG_TIMEOUT)
			break;
		tl_Time (&now);
		timeout -= now.zt_Sec - last.zt_Sec;
		last = now;
	}
}



static void
Connect (conn)
Connection *conn;
/*
 * Make a connection to this port.  Connect() should only be called once,
 * and only after the first initialization of the connection.  Reconnect()
 * should be called all other times.
 */
{
/*
 * Get the port.
 */
	GetPort (conn);
/*
 * Now dial it, if so desired.
 */
	if (conn->c_Number)
		Dial (conn);
/*
 * Start watching for input.
 */
	msg_add_fd (conn->c_Fd, SLDataHandler);
}



static void
Reconnect (conn)
Connection *conn;
/*
 * Close our fd, create a new connection, and change our fd with the
 * message handler.  We enter this routine with the assumption that
 * the connection members which may have been changing, such as the
 * buffer, fd, and timer slot, need to be reset to their initialized
 * state before trying a Connect() again.  
 */
{
	if (conn->c_Fd >= 0)
	{
		msg_delete_fd (conn->c_Fd);
		close (conn->c_Fd);	/* Easy way to drop DTR */
		conn->c_Fd = -1;
	}
/*
 * Cancel any timer requests.
 */
	if (conn->c_TSlot >= 0)
	{
	    tl_Cancel (conn->c_TSlot);
	    conn->c_TSlot = -1;
	}
/*
 * Clear the buffer.
 */
	conn->c_nbuf = 0;
	conn->c_buf[0] = 0;
/*
 * Finally we can attempt to connect.
 */
	Connect (conn);
}



static void
GetPort (conn)
Connection *conn;
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
	struct termios tbuf;
/*
 * Locate our speed.
 */
 	for (i = 0; Speeds[i].c_speed; i++)
		if (! strcmp (conn->c_Speed, Speeds[i].c_speed))
			break;
	if (! Speeds[i].c_speed)
	{
		msg_ELog (EF_PROBLEM, "Bad speed %s", conn->c_Speed);
		Die ("unrecognized baud rate", 3);
	}
/*
 * Open the port.
 */
	if ((conn->c_Fd = open (conn->c_Port, O_RDWR, 0)) < 0)
	{
		perror ("serial line grabber port");
		msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, 
			  conn->c_Port);
		Die ("couldn't open port", 4);
	}
/*
 * Get the current TTY parameters.
 */ 
	if (tcgetattr (conn->c_Fd, &tbuf) != 0)
	{
		perror ("getting tty parms");
		msg_ELog (EF_EMERGENCY, "tcgetattr error (%d)", errno);
		Die ("couldn't get tty parms", 5);
	}
/*
 * Set to 8 bit, no parity, 1 stop bit, hang up on last close.
 */
	tbuf.c_cflag = CS8 | CREAD | HUPCL;
	tbuf.c_cflag |= Speeds[i].n_speed;
/*
 * Tweak it to be as raw as possible.
 */
	tbuf.c_iflag = 0;
	tbuf.c_oflag = 0;
	tbuf.c_lflag = 0;
/*
 * Store the new parameters.
 */
	if (tcsetattr (conn->c_Fd, TCSANOW, &tbuf) != 0)
	{
		perror ("tcsetattr");
		msg_ELog (EF_EMERGENCY, "Error %d setting tty parameters", 
			  errno);
		Die ("could not set tty parameters", 6);
	}
}




static void
WritePort (conn, text)
Connection *conn;
char *text;
/*
 * Write this stuff.
 */
{
	write (conn->c_Fd, text, strlen(text)+1);
}




static void
WriteBytes (conn, bytes, len)
Connection *conn;
const ubyte *bytes;
int len;
/*
 * Write this stuff.
 */
{
	write (conn->c_Fd, bytes, len);
}



static int
ReadPort (conn, dest, len)
Connection *conn;
ubyte *dest;
int len;
/*
 * Read something back.
 */
{
	return (read (conn->c_Fd, dest, len));
}





static void
Dial (conn)
Connection *conn;
/*
 * Work until something comes in.
 */
{
	while (! DoDial (conn))
		MWait (20);		/* Check messages while waiting */
}



static int
DoDial (conn)
Connection *conn;
/*
 * Try to make a connection to this number.
 */
{
	char buf[80];
	int nc;
/*
 * Put the modem in the state we want.
 */
	WritePort (conn, "AT V0 E0\r");
	sleep (5);
	(void) ReadPort (conn, buf, 80);	/* Clean out junk */
/*
 * Tell it to dial.
 */
	WritePort (conn, "ATDT");
	WritePort (conn, conn->c_Number);
	WritePort (conn, "\r");
/*
 * Get something back and see what it thinks.
 */
again:
	nc = ReadPort (conn, buf, 1);
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
			conn->c_TSlot = tl_AddRelativeEvent (CheckDCD, 
			     conn, CHECKINT*INCFRAC, CHECKINT*INCFRAC);
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






static int
SLDataHandler ()
/*
 * Here's some serial line data.
 */
{
	Connection *conn;
	int nread, nused;
	char *cp, *newline;
/*
 * Get our global connection.  If we ever need to handle more than one
 * connection, we'll have to look it up by file descriptor.
 */
	conn = Global;
	nread = ReadPort (conn, conn->c_buf + conn->c_nbuf, 
			  BUFSIZE - conn->c_nbuf - 1);
			  /* the (... - 1) allows space for \0 if necessary */
/*
 * If things are dead, start over.
 */
	if (nread < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d reading port", errno);
		Reconnect (conn);
		return (0);
	}
	else if (nread == 0)
	{
		msg_ELog (EF_PROBLEM, "0 bytes read from port, %s", 
			  "trying to reconnect.");
		msg_ELog (EF_DEBUG, "buffer size: %d, used: %d",
			  BUFSIZE, conn->c_nbuf);
		Reconnect (conn);
		return (0);
	}
/*
 * Otherwise update our buffer status and pass it on according to mode
 */
	conn->c_nbuf += nread;
/*
 * Go ahead and terminate our buffer, just in case someone expects it.
 * Since we made room for the null termination above, why waste it...?
 */
	conn->c_buf[conn->c_nbuf] = '\0';
	if (conn->c_Mode == TextMode)
	{
		nused = SendLines (conn, conn->c_nbuf - nread);
	}
	else
	{
		nused = SendBlocks (conn, conn->c_nbuf - nread);
	}
/*
 * Copy over what's left and we're done.  Use memmove since this is likely
 * an overlapping copy.
 */
	if (nused > 0)
	{
	    int nleft = conn->c_nbuf - nused;
	    if (nleft > 0)
		memmove (conn->c_buf, conn->c_buf + nused, nleft);
	    conn->c_nbuf -= nused;
	}
	if (conn->c_nbuf + 1 == BUFSIZE)
	{
	    /* 
	     * We've just tried to process data, and yet our buffer
	     * is full: buffer overrun!  Throw out the current data.
	     */
	    msg_ELog (EF_PROBLEM, "buffer full, %s",
		      "dropping data to clear buffer");
	    msg_ELog (EF_DEBUG, "buffer size: %d, used: %d, processed: %d",
		      BUFSIZE, conn->c_nbuf, nused);
	    conn->c_nbuf = 0;
	    conn->c_buf[0] = '\0';
	}
	return (0);
}



static int
SendLines (conn, start)
Connection *conn;
int start;		/* where in the buffer the new data begins */
/*
 * Send all the lines of text we can, and return how far in the buffer
 * we got.
 */
{
	ubyte *cp;
	char *newline;
/*
 * Zap out the top bit for now, but only those characters we haven't
 * processed before.
 */
	if (conn->c_striphibit)
	{
		for (cp = conn->c_buf + start; 
		     cp < conn->c_buf + conn->c_nbuf; cp++)
			*cp &= 0x7f;
	}
/*
 * Now look for newlines indicating that whole lines have been read,
 * from the beginning of the buffer.
 */
	cp = conn->c_buf;
	conn->c_buf[conn->c_nbuf] = '\0';
	while ((cp - conn->c_buf < conn->c_nbuf) &&
	       ((newline = strchr (cp, '\n')) != 0))
	{
		*newline = '\0';
		Send (conn, cp, strlen(cp)+1);
		(char *) cp = newline + 1;
	}
/*
 * It's up to our caller to move what's left in the buffer to the front of
 * the buffer.
 */
	return (cp - conn->c_buf);
}



static int
SendBlocks (conn, start)
Connection *conn;
int start;		/* Index of newset data -- not used */
/*
 * Send all of the data in the buffer, but in blocks no larger than
 * the block size specified in the connection.  No bytes will remain
 * in the buffer when this function returns.
 */
{
	ubyte *ub;
	int i;

	ub = conn->c_buf;
/*
 * Send whatever whole blocks we have
 */
	while (ub + conn->c_blocksize < conn->c_buf + conn->c_nbuf)
	{
		Send (conn, ub, conn->c_blocksize);
		ub += conn->c_blocksize;
	}
/*
 * Send what's left
 */
	if (ub < conn->c_buf + conn->c_nbuf)
		Send (conn, ub, (conn->c_buf + conn->c_nbuf) - ub);
	return (conn->c_nbuf);
}



static void
Send (conn, data, len)
Connection *conn;
const ubyte *data;	/* our array of data bytes 		   */
int len;	/* number of bytes to send from data array */
/*
 * Send this data.
 */
{
	static char *dbuf = 0;

	if (len <= 0)		/* just in case */
		return;

	if (conn->c_Consumer)
	{
	        SLdata *sld = (SLdata *) dbuf;

		if (!dbuf)
		{
		        dbuf = (char *) malloc (sizeof(SLdata) + BUFSIZE);
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
		msg_send (conn->c_Consumer, MT_SLDATA, FALSE, sld,
			  sld->sl_len + sizeof (SLdata) - 1);
	}
	else if (conn->c_Mode == TextMode)
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



static void
CheckDCD (t, parm)
UItime *t;
int parm;
/*
 * Check to see that we are still connected here.
 */
{
	Connection *conn = (Connection *)parm;
	int mbits, status;
# ifdef linux
	struct termios tinfo;
	status = tcgetattr (conn->c_Fd, &tinfo);
	mbits = tinfo.c_line;	/* Non-standard! */
# else
	status = ioctl (conn->c_Fd, TIOCMGET, &mbits);
# endif
/*
 * Do the check.
 */
	if (status < 0)
		msg_ELog (EF_PROBLEM, "DCD Check ioctl failure %d", errno);
	else if (! mbits & TIOCM_CAR)
		msg_ELog (EF_PROBLEM, "Carrier lost on port");
	else
		return;
/*
 * Attempt another connection.
 */
	Reconnect (conn);
}
