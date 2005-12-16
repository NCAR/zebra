
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
# include "Serial.h"
# include "Options.h"

MAKE_RCSID("$Id: Serial.c,v 2.5 2005-12-16 07:16:18 granger Exp $")

#define DEFAULT_TERM	"/dev/ttya"	/* Default port device	*/
#define DEFAULT_SPEED	"9600"		/* Default baud rate	*/
#define DEFAULT_PHONE	NULL		/* No default phone no. */
#define DEFAULT_MODE	TextMode	/* historical		*/
#define DEFAULT_BLOCK	512		/* default block size	*/

#define CHECKINT 	900		/* DCD Check interval	*/
#define BUFSIZE       	1024		/* serial buffer length	*/



typedef struct _Connection
{
    SerialMode	c_Mode;
    int 	c_Fd;
    int 	c_TSlot;
    char 	*c_Port;
    char 	*c_Speed;
    char 	*c_Number;
    char 	*c_read;
    int		c_blocksize;
    int    	c_striphibit;
    int 	c_nbuf;
    ubyte	c_buf[BUFSIZE];
    int         c_flags;
    int		c_echo;
    SerialDataHandlerP c_handler;
} Connection;


/*
 * This is necessary until msg_add_fd allows a parameter to be passed
 * to the handler routine.
 */
static Connection *Global = 0;


static void MWait FP ((int timeout));
static void Connect FP ((Connection *));
static void Reconnect FP ((Connection *));
static void GetPort FP ((Connection *));
static void SimulatePort (Connection *conn);
static void Dial FP ((Connection *));
static int  SLDataHandler FP((void));
static int  SendLines FP((Connection *conn, int start));
static int  SendBlocks FP((Connection *conn, int start));
static void CheckDCD FP ((UItime *t, int parm));
static int  DoDial FP((Connection *conn));


void
SerialUsage ()
{
   printf("Serial Options:\n");
   printf("  -port <port>  The serial port to connect to [%s]\n",
	  DEFAULT_TERM);
   printf("  -speed <baud> The baud rate to use for the port [%s]\n",
	  DEFAULT_SPEED);
   printf("  -dial <ph#>   Phone number to dial [default is none]\n");
   printf("  -7            Clear high-order bit in text mode. [default off]\n");
   printf("  -factor <size>\n"
	  "                Blocking factor in bytes.  Data will be sent\n");
   printf("                to the consumer in blocks no larger than this\n");
   printf("                size, but possibly smaller. [%d]\n",
	  DEFAULT_BLOCK);
   printf("  -hangup       Hang-up (drop DTR) when the port is closed.\n");
   printf("  -nohangup     Don't hang-up (drop DTR) when port is closed.\n");
   printf("  -read <file>  Simulate serial input by reading from <file>.\n");
   printf("  -echo         Echo serial input to the standard output.\n");
   printf("  -lines        Character lines and text mode.\n");
   printf("                Passes character data to consumer\n");
   printf("                line by line, removing line feeds and returns.\n");
   printf("  -binary       Binary mode.  Bytes are shipped to the consumer\n");
   printf("                exactly as they are read.\n");
   printf("The default mode is %s\n", 
	  (DEFAULT_MODE == TextMode) ? "LINE" : "BYTE");
}





void
SerialParseOptions (SerialConnection *sc, int *argc, char *argv[])
/*
 * Parse and remove our serial options.
 */
{
    int c;
    Connection *conn = (Connection *) sc;
    static const char *options[] = 
    { "-port", OptionArgument,
      "-lines",
      "-binary",
      "-factor",
      "-speed", OptionArgument,
      "-7",
      "-dial", OptionArgument,
      "-hangup",
      "-nohangup",
      "-read", OptionArgument,
      "-echo",
      0
    };

    OptionSetupAbbrevs (argc, argv, options, 0 /* no abbreviations*/);
    while ((c = OptionNext()) >= 0)
    {
	switch (c)
	{
	case 'l':
	    conn->c_Mode = TextMode;
	    break;
	case 'b':
	    conn->c_Mode = ByteMode;
	    break;
	case 'e':
	    conn->c_echo = 1;
	    break;
	case 'f':
	    conn->c_blocksize = atoi(OptionArg());
	    break;
	case 's':
	    conn->c_Speed = OptionArg();
	    break;
	case '7':
	    conn->c_striphibit = 1;
	    break;
	case 'p':
	    conn->c_Port = OptionArg();
	    break;
	case 'd':
	    conn->c_Number = OptionArg();
	    break;
	case 'h':
	    conn->c_flags |= HUPCL;
	    break;
	case 'n':
	    conn->c_flags &= ~HUPCL;
	    break;
	case 'r':
	    conn->c_read = OptionArg();
	default:
	    /* Leave other options for application. */
	    break;
	}
    }
}



void
SerialDie (const char *msg, int code)
{
    printf ("%s\n", msg);
    SerialDisconnect ((SerialConnection *)Global);
    exit (code);
}



void
SerialDisconnect (SerialConnection *sc)
{
    Connection *conn = (Connection *) sc;
    if (conn && conn->c_Fd >= 0)
    {
	msg_delete_fd (conn->c_Fd);
	close (conn->c_Fd);
    }
}



static void
InitializeConnection (Connection *conn)
{
	conn->c_Mode = DEFAULT_MODE;
	conn->c_Fd = -1;
	conn->c_TSlot = -1;
	conn->c_Port = DEFAULT_TERM;
	conn->c_Speed = DEFAULT_SPEED;
	conn->c_Number = DEFAULT_PHONE;
	conn->c_blocksize = DEFAULT_BLOCK;
	conn->c_striphibit = 0;
	conn->c_nbuf = 0;
	conn->c_buf[0] = 0;
	conn->c_flags = CS8 | CREAD | HUPCL;
	conn->c_read = 0;
	conn->c_echo = 0;
}



SerialConnection *
SerialInitialize ()
{
    Connection *conn;

    conn = (Connection *) malloc (sizeof (Connection));
    InitializeConnection (conn);
    return (SerialConnection *) conn;
}


void
SerialSetHangupOnClose (SerialConnection *sc, int enable)
{
    Connection *conn = (Connection *) sc;
    conn->c_flags &= ~HUPCL;
    if (enable)
	conn->c_flags |= HUPCL;
}



void
SerialSetMode (SerialConnection *sc, SerialMode mode)
{
    Connection *conn = (Connection *) sc;
    conn->c_Mode = mode;
}
    

SerialMode
SerialGetMode (SerialConnection *sc)
{
    Connection *conn = (Connection *) sc;
    return (conn->c_Mode);
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


/*
 * This is the weak link in our layout.  The last connection passed to
 * SerialConnect() becomes the new singleton connection on which we listen
 * for data.  Any current connection gets disconnected.  This is just to
 * avoid setting up a fd map here to get the actual Connection.
 */
void
SerialConnect (SerialConnection *sc, SerialDataHandlerP handler)
{
    Connection *conn = (Connection *) sc;
    conn->c_handler = handler;
    SerialDisconnect ((SerialConnection *)Global);
    Global = conn;
    Connect (conn);
}



static void
Connect (Connection *conn)
/*
 * Make a connection to this port.  Connect() should only be called once,
 * and only after the first initialization of the connection.  Reconnect()
 * should be called all other times.
 */
{
    /*
 * Get the port.
 */
    if (! conn->c_read)
    {
	GetPort (conn);
	/*
	 * Now dial it, if so desired.
	 */
	if (conn->c_Number)
	    Dial (conn);
    }
    else
    {
	SimulatePort (conn);
    }
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
		close (conn->c_Fd);	/* Easy way to drop DTR (if HUPCL) */
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
SimulatePort (Connection *conn)
{
    /*
     * Open the simulation file as the port.
     */
    if ((conn->c_Fd = open (conn->c_read, O_RDONLY, 0)) < 0)
    {
	perror ("serial line simulation file");
	msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, 
		  conn->c_read);
	SerialDie ("couldn't open serial input", 4);
    }
}



/*
 * Open the given connection and return its fd, else -1.
 * Do not activate any listening or handlers for the connection,
 * just open the port.  Disconnect (close) any file already open
 * for the connection.
 */
int
SerialOpen (SerialConnection *sc)
{
    Connection *conn = (Connection *) sc;

    SerialDisconnect (sc);
    GetPort (conn);
    return (conn->c_Fd);
}



static void
GetPort (Connection *conn)
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
		{ "300",	B300 	},
		{ "1200",	B1200 	},
		{ "2400",	B2400 	},
		{ "4800",	B4800 	},
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
		SerialDie ("unrecognized baud rate", 3);
	}
/*
 * Open the port.
 */
	if ((conn->c_Fd = open (conn->c_Port, O_RDWR, 0)) < 0)
	{
		perror ("serial line port");
		msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, 
			  conn->c_Port);
		SerialDie ("couldn't open port", 4);
	}
/*
 * Get the current TTY parameters.
 */ 
	if (tcgetattr (conn->c_Fd, &tbuf) != 0)
	{
		perror ("getting tty parms");
		msg_ELog (EF_EMERGENCY, "tcgetattr error (%d)", errno);
		SerialDie ("couldn't get tty parms", 5);
	}
/*
 * Set to 8 bit, no parity, 1 stop bit, hang up on last close.
 */
	tbuf.c_cflag = conn->c_flags;
	tbuf.c_cflag |= Speeds[i].n_speed;
/*
 * Tweak it to be as raw as possible.
 */
	tbuf.c_iflag = 0;
	tbuf.c_oflag = 0;
	tbuf.c_lflag = 0;

	tbuf.c_cc[VTIME] = 0;
	tbuf.c_cc[VMIN] = 1;
/*
 * Store the new parameters.
 */
	if (tcsetattr (conn->c_Fd, TCSANOW, &tbuf) != 0)
	{
		perror ("tcsetattr");
		msg_ELog (EF_EMERGENCY, "Error %d setting tty parameters", 
			  errno);
		SerialDie ("could not set tty parameters", 6);
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
	    if (conn->c_read)
	    {
		SerialDie ("End of simulation input.", 0);
	    }
	    else
	    {
		msg_ELog (EF_PROBLEM, "0 bytes read from port, %s", 
			  "trying to reconnect.");
		msg_ELog (EF_DEBUG, "buffer size: %d, used: %d",
			  BUFSIZE, conn->c_nbuf);
		Reconnect (conn);
		return (0);
	    }
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



static void
Send (Connection *conn, ubyte *cp, int len)
{
    if (conn->c_echo)
    {
	if (conn->c_Mode == TextMode)
	{
	    /* Print the text with the line separations and flush it out. */
	    fprintf (stdout, "%s\n", (char *)cp);
	    fflush (stdout);
	}
	else
	{
	    /* Write the bytes to stdout. */
	    fwrite (cp, len, 1, stdout);
	}
    }
    if (conn->c_handler)
    {
	(*conn->c_handler)((SerialConnection *)conn, cp, len);
    }
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
        char *cp;
	char *newline;
/*
 * Zap out the top bit for now, but only those characters we haven't
 * processed before.
 */
	if (conn->c_striphibit)
	{
	  for (cp = (char*)conn->c_buf + start; 
	       cp < (char*)conn->c_buf + conn->c_nbuf; cp++)
			*cp &= 0x7f;
	}
/*
 * Now look for newlines indicating that whole lines have been read,
 * from the beginning of the buffer.
 */
	cp = (char*)conn->c_buf;
	conn->c_buf[conn->c_nbuf] = '\0';
	while ((cp - (char*)conn->c_buf < conn->c_nbuf) &&
	       ((newline = strchr (cp, '\n')) != 0))
	{
		*newline = '\0';
		Send (conn, (ubyte*)cp, strlen(cp)+1);
		cp = newline + 1;
	}
/*
 * It's up to our caller to move what's left in the buffer to the front of
 * the buffer.
 */
	return (cp - (char*)conn->c_buf);
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
