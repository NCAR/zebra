/*
 * The message handler.
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

# include <unistd.h>
# include <sys/uio.h>
# include <pwd.h>	/* to get username from uid */
# include <stdio.h>
# include <stdarg.h>
# include <errno.h>
# include <netdb.h>
# include <fcntl.h>
# include <sys/types.h>
# include <sys/time.h>
# include <sys/socket.h>
# include <sys/un.h>
/*
 * sys/socket.h includes linux/socket.h, which includes linux/uio.h, which
 * gives its own definition of iovec, hence skip this include on linux
 */
# ifndef linux
# include <sys/uio.h>
# endif
# include <netinet/in.h>
# include <signal.h>
# include <string.h>
#if defined(SVR4) || defined (SYSV)
# include <sys/file.h>
#endif

# include <defs.h>
# include <copyright.h>
# include <zl_symbol.h>

# define MESSAGE_MANAGER	/* define prototypes for netread functions */
# include <message.h>

RCSID ("$Id: message.c,v 2.63 2002-05-11 16:40:08 vanandel Exp $")

/*
 * Symbol tables.
 */
static stbl Proc_table;		/* Lookup table for processes	*/
static stbl Group_table;	/* Table for groups.		*/
static stbl Inet_table;		/* Internet connections		*/
static stbl InetGripeTable;	/* Inet complaint limiter	*/
static stbl InetAvoidTable;	/* Machines which time out and slow us down */
/*
 * The master sockets for incoming connections.
 */
static int M_un_socket = -1;	/* Unix domain socket		*/
static int M_in_socket = -1;	/* Internet domain socket	*/
static char UnSocketName[120];	/* Name of Un socket		*/
/*
 * An FD set which knows about all file descriptors that we are interested
 * in.
 */
static fd_set Allfds;
static fd_set WriteFds;
static int NWriteFd = 0, MaxWriteFd = 0;
static int Port = 0;
static int EMask = 0x00;	/* Wait until we know an ELogger exists */
static int Dying = FALSE;
static int Debug = FALSE;	/* Debuggin' mode */

/*
 * The delayed write queue.
 */
typedef struct _DWrite
{
	int	dw_nbyte;		/* Length of the write		*/
	int	dw_nsent;		/* How much already sent	*/
	char	*dw_data;		/* The actual data		*/
	struct _DWrite *dw_next;	/* Next link in queue		*/
} DWrite;

/*
 * Delayed write parameters -- where we gripe, and where we start dropping
 * messages.  These are pretty generous.
 */
# define DWGRIPE	 50000
# define DWDROP		250000

# define UNKNOWN_NAME	"(Unknown)"	/* 'name' of yet un-named connection */
# define INETCTIME	2		/* timeout for inet connections */
# define MINFD		4		/* min fd to search for connections */

/*
 * This structure describes a connection.
 */
typedef struct connection 
{
	char	c_name[MSG_MAXNAMELEN];	/* The name of the connection	*/
	int	c_fd;			/* The file descriptor.		*/
	int	c_nsend;		/* Messages sent		*/
	int	c_bnsend;		/* Bytes sent			*/
	int	c_nrec;			/* Messages received		*/
	int	c_bnrec;		/* Bytes received		*/
	int	c_pid;			/* Process ID			*/
	Message c_msg;			/* Incoming message		*/
	unsigned short c_nread;		/* How much has been read	*/
	char	c_inprog;		/* Read in progress		*/
	char	c_inet;			/* Internet connection		*/
	DWrite	*c_dwrite;		/* The delayed write chain	*/
	DWrite	*c_dwtail;		/* The end of same		*/
	int	c_ndwrite;		/* How much dwrite data		*/
	int	c_griped;		/* How much have we griped?	*/
} Connection;
Connection *MH_conn;		/* Fake connection for local stuff */

/*
 * Process groups are maintained through a structure like this.
 */
struct group
{
	char	g_name[MSG_MAXNAMELEN];	/* The name of this group	*/
	short	g_nprocs;		/* The number of procs in this grp */
	struct connection **g_procs;	/* The array of connections.	*/
};


/*
 * This array maps file descriptors onto their associated connections.
 */
static struct connection *Fd_map[ FD_MAP_SIZE ] = { 0 };
static int Nfd;

/*
 * Who are we?
 */
char Hostname[MSG_MAXNAMELEN];

/*
 * Our very own host table for connecting to hosts (possibly ourself) using
 * different ports.  An asterisk matches any host.  Someday maybe all '*'
 * entries will be attempted successively until a connection is made if no
 * exact match is found.  For now, the first '*' entry is tried if an exact
 * match is not found.  If the services entry is found, it is entered as
 * the global match in the hostports table.  That way if a host name is not
 * found in the table, the service port will be attempted; otherwise the
 * default port wil be attempted.  In fact, at the moment EnterHost only
 * allows one ANYHOST entry, as each entry overrides an existing one of the
 * same name.
 * 
 * An optional address allows a single host to be known by different names
 * (but the same address) and to connect using different ports.  The ultimate
 * goal is to allow the table to be dynamic, and then add a message type to
 * the protocol which announces new internet connections to other hosts so
 * that the message handlers on the other hosts can add an entry to their
 * host table.
 */
struct hostportent {
	char *hp_name;
	char *hp_addr;
	int hp_port;
};
#define MAX_HOSTS 30

static const char *ANYHOST = "*";

struct hostportent HostPorts[ MAX_HOSTS ];

static int NHosts = 0;

/*
 * Global statistics.
 */
static int S_nmessage = 0, S_bnmessage = 0, S_nbcast = 0, S_bnbcast = 0;
static int S_npipe = 0, S_ndisc = 0, S_NDRead = 0, S_NDWrite = 0;
static time_t S_genesis = 0;      /* in the beginning, there was time... */

/*
 * How long we wait between announcing a shutdown and really going down.
 */
# define SHUTDOWN_DELAY 15

/*
 * Message tap stuff.
 */
struct MTap
{
	int	mt_who;		/* Tapper		*/
	struct msg_mtap mt_tapee;
	struct MTap *mt_next;	/* Next in chain	*/
} *Taps = 0;


/*
 * Forwards.
 */
Connection *FindRecipient FP ((char *));
Connection *MakeConnection FP ((char *));
Connection *InitConnection FP ((int fd, int inet));
static void FreeConnection FP ((Connection *cp));

static void AddHost FP((const char *host));
static int FindHost FP((const char *host, int *port, int exact));
static void ReadHosts FP((const char *file));
static void EnterHost FP((const char *host, const char *addr, int port));
static void FreeHosts FP((void));

void	Greeting FP ((int, struct message *));
void	add_to_group FP ((struct connection *, char *, struct message *));
int	in_group (char *name, struct connection *conp);
int	clear_group FP ((char *name, int type, union usy_value *v,
			 struct connection *conp));
int	free_group FP ((char *name, int type, union usy_value *v, int unused));
static int stat_group FP ((char *name, int type, union usy_value *v,
			   char *text));
void	FixAddress FP ((char *));
void	MsgProtocol FP ((int, Message *));
void	AnswerPing FP ((int, Message *));
static	Message *ReadMessage FP ((int));

static void DelayWrite FP ((Connection *, struct iovec *, int, int));
static void DoDelayedWrite FP ((fd_set *));
static int TryWrite FP ((Connection *));

static void ClientQuery FP ((int, Message *));

static void Tap FP ((int, Message *));
void	DoTaps FP ((Message *));
int	TapperWants FP ((struct MTap *, Message *));
void	SendTap FP ((struct MTap *, Message *));
static void FreeTap FP ((int fd));

void	SetNonBlock FP ((int));
void	ReallyDie FP ((void));
void	GetInetPort FP ((void));
void	EnterServicePort ();
void	NewInConnection FP ((void));
void	new_un_connection FP ((void));
static void MakeUnixSocket FP ((void));
static void MakeInetSocket FP ((void));
static void UpdateLogMask FP ((struct message *msg));
static void BroadcastMask (int mask);
static void send_log (int flags, char* fmt, ...);
static void zmlog (int flags, char* fmt, ...);
static void inc_message FP ((int nsel, fd_set *fds));
static void send_msg FP ((struct connection *conp, struct message *msgp));
static void deadconn FP ((int fd));
static void die FP ((void));
static void dispatch FP ((int fd, struct message *msg));
static void ce_connect FP ((struct connection *conp));
static void ce_join FP ((struct connection *conp, char *group));
static void ce_disconnect FP ((struct connection *conp));
static void broadcast FP ((struct message *msg, struct connection *conp));
static void route FP ((int fd, struct message *msg));
static void reject FP ((int fd, struct message *msg, void *data, int len));
static void identify FP ((int fd, struct message *msg));
static void join FP ((int fd, struct message *msg));
static void listgroup FP ((int fd, struct message *msg));
static void create_group FP ((struct connection *conp, char *name));
static void Stats FP ((struct connection *conp, char *to, int query));
static void ack FP ((struct connection *conp, struct message *msg));
static int psig FP ((void));




static void
usage ()
{
	fprintf (stderr, "Usage: message [-internet] [-debug] [-nofork]\n");
	fprintf (stderr, "         [-file <file>] [-port <port>]\n");
	fprintf (stderr, "         [-session <name>]\n");
	fprintf (stderr, "         [<name>[@<host>][:<port>]] ... \n");
	fprintf (stderr, "where the options can be abbreviated ");
	fprintf (stderr, "to single characters:\n");
	fprintf (stderr, " -help      %s", "This usage message\n");
	fprintf (stderr, " -version   %s", "Print version information\n");
	fprintf (stderr, " -copyright %s", "Print copyright information\n");
	fprintf (stderr, " -internet  %s",
		 "Create an inbound Internet socket, disabled by default\n");
	fprintf (stderr, " -debug     %s",
		 "Debug mode: log all messages to stdout\n");
	fprintf (stderr, " -port      %s",
		 "Listen on <port> for inbound Internet connections\n");
	fprintf (stderr, " -nofork    %s",
		 "Do not fork into the background; useful under debuggers\n");
	fprintf (stderr, " -file      %s",
		 "Add the session entries in <file> to the address table,\n");
	fprintf (stderr, "            %s",
	 "where entry lines contain: <session name> <host address> <port>\n");
	fprintf (stderr, " -session   Use <name> as our session identity\n");
	fprintf (stderr, " name@host:port\n");
	fprintf (stderr, "            Use address <host>:<port> for ");
	fprintf (stderr, "connections to <name>;\n");
	fprintf (stderr, "            add the entry to the address table\n");
}


static void
InitFDMap ()
/*
 * Initialize our Fd_map
 */
{
	int i;

	Nfd = 0;
	for (i = 0; i < FD_MAP_SIZE; ++i)
		Fd_map[i] = NULL;
}


int
main (argc, argv)
int argc;
char **argv;
{
	int inet = FALSE;
	int nofork = FALSE;
	char *host = NULL;
	fd_set fds, wfds;
	int i;

/* 
 * Initialize host table with our default service port, so
 * that the default can be overridden by command-line or
 * session file host entries.
 */
	EnterServicePort ();
/*
 * Check args.
 */
	i = 1;
	while (i < argc)
	{
		int optlen = strlen (argv[i]);

		if (argv[i][0] != '-')
		{
			AddHost (argv[i]);
		}
		else if (argv[i][1] == '\0')
		{
			usage ();
			exit (1);
		}
		else if (! strncmp (argv[i], "-help", optlen))
		{
			usage ();
			exit (0);
		}
		else if (! strncmp (argv[i], "-version", optlen))
		{
			printf ("%s%s", Z_version(), Z_cppsymbols());
			printf ("%s", Z_rcsid());
			printf ("Message protocol version: %s\n",
				MSG_PROTO_VERSION);
			exit (0);
		}
		else if (! strncmp (argv[i], "-copyright", optlen))
		{
			printf ("%s", Z_copyright());
			exit (0);
		}
		else if (! strncmp (argv[i], "-internet", optlen))
			inet = TRUE;
		else if (! strncmp (argv[i], "-debug", optlen))
			Debug = TRUE;
		else if (! strncmp (argv[i], "-port", optlen) && i+1 < argc)
		{
			Port = atoi (argv[++i]);
		}
		else if (! strncmp (argv[i], "-file", optlen) && i+1 < argc)
		{
			ReadHosts (argv[++i]);
		}
		else if (! strncmp (argv[i], "-session", optlen) && i+1 < argc)
		{
			host = argv[++i];
		}
		else if (! strncmp (argv[i], "-nofork", optlen))
			nofork = TRUE;
		else
		{
			fprintf (stderr, "unrecognized option: %s\n", argv[i]);
			usage ();
			exit (1);
		}
		++i;
	}
/*
 * Initialize our begin time.
 */
	S_genesis = time (NULL);
/*
 * Create our symbol tables.
 */
	usy_init ();
	Proc_table = usy_c_stbl ("Process_table");
	Group_table = usy_c_stbl ("Group_table");
	Inet_table = usy_c_stbl ("Inet_table");
	InetGripeTable = usy_c_stbl ("InetGripeTable");
	InetAvoidTable = usy_c_stbl ("InetAvoidTable");
/*
 * Signals 
 */
	signal (SIGPIPE, (void(*)(/*int*/)) psig);
	signal (SIGHUP, SIG_IGN);
/*
 * We need our host name to look up our port in the host table.  Use
 * the name given on the command line if we got one.
 */
	if (! host && ! (host = getenv ("HOST")))
	{
		printf ("Who the hell are we?\n");
		exit (1);
	}
	if (strlen(host) >= sizeof(Hostname))
	{
		fprintf (stderr, "HOST name '%s' is too long?!?!\n", host);
		exit (1);
	}
	strcpy (Hostname, host);
/*
 * Get our inet port.  Do so even if we are not opening a listening
 * socket, since we may want to connect outbound.
 */
	GetInetPort ();
/*
 * Initialize the file descriptor map before we start making connections
 */
	InitFDMap ();
/*
 * Create Unix and Internet sockets.
 */
	MakeUnixSocket ();
	if (inet)
		MakeInetSocket ();
/*
 * OK initial setup is complete.  Back off into daemon mode...
 */
	if (! nofork && fork ())
		exit (0);	/* Parent goes away */
/*
 * Set up our select stuff.
 */
	FD_ZERO (&Allfds);
	FD_SET (M_un_socket, &Allfds);
	if (M_in_socket >= 0)
		FD_SET (M_in_socket, &Allfds);
/*
 * Now it's loop time.
 */
	for (;;)
	{
		int nsel;
	/*
	 * Do a select, waiting for something to happen.
	 */
		fds = Allfds;
		if (NWriteFd)
		{
			wfds = WriteFds;
			nsel = select (Nfd, (SelectSet *)&fds, 
				       (SelectSet *)&wfds, (SelectSet *)0,
				       (struct timeval *) NULL);
		}
		else
			nsel = select (Nfd, (SelectSet *)&fds, (SelectSet *)0,
			       (SelectSet *)0, (struct timeval *) NULL);
	/*
	 * See what happened.
	 */
		if (nsel < 0 && errno != EINTR)
		{
			perror ("Select");
			break;		/* XXX */
		}
	/*
	 * If it's from our master socket, accept a new connection.
	 */
		if (M_un_socket >= 0)
		{
			listen (M_un_socket, 5);	/* XXXX?? */
			if (FD_ISSET (M_un_socket, &fds))
			{
				new_un_connection ();
				nsel--;
			}
		}
	/*
	 * Look for Internet connections too.
	 */
	 	if (M_in_socket >= 0 && FD_ISSET (M_in_socket, &fds))
		{
			NewInConnection ();
			nsel--;
		}
	/*
	 * If we are writing, try to drain our output now.
	 */
	 	if (NWriteFd)
			DoDelayedWrite (&wfds);
	/*
	 * See if we have data coming in from somewhere.
	 */
		if (nsel)
			inc_message (nsel, &fds);
	}	
	return (1);
}



static void
AddHost (host)
const char *host;
/*
 * Extract a host name and a port from this string and insert it into
 * the host table.
 */
{
	char *colon;
	char *addr;
	char *at;
	int port;

	at = strchr (host, '@');
	colon = strrchr (host, ':');
	if (at)
		*at++ = '\0';
	if (colon)
		*colon++ = '\0';
	if (! host[0] || (at && ! at[0]) || (colon && ! colon[0]))
	{
		fprintf (stderr, "bad host entry '%s'\n", host);
		exit (1);
	}
	if (at)
		addr = at;
	else
		addr = NULL;
	if (colon)
		port = atoi (colon);
	else
		port = DEFAULT_PORT;

	EnterHost (host, addr, port);
}


static void
EnterHost (host, addr, port)
const char *host;
const char *addr;
int port;
{
	int i;
	int oldport;
	struct hostportent *hp;

	if (NHosts >= MAX_HOSTS)
	{
		fprintf (stderr, "too many host entries; max is %d\n",
			 MAX_HOSTS);
		exit (1);
	}
	/*
	 * See if it already exists, in which case this overrides it
	 */
	i = FindHost (host, &oldport, TRUE);
	if (i < 0)
	{
		i = NHosts++;
		hp = HostPorts + i;
	}
	else
	{
		hp = HostPorts + i;
		free (hp->hp_name);
		if (hp->hp_addr)
			free (hp->hp_addr);
	}
	hp->hp_name = (char *) malloc (strlen(host) + 1);
	if (! hp->hp_name)
	{
		fprintf (stderr, "malloc failed: host name for table\n");
		exit (1);
	}
	strcpy (hp->hp_name, host);
	if (addr)
	{
		hp->hp_addr = (char *) malloc (strlen(addr) + 1);
		if (! hp->hp_addr)
		{
			fprintf (stderr, "malloc failed: host addr\n");
			exit (1);
		}
		strcpy (hp->hp_addr, addr);
	}
	else
		hp->hp_addr = NULL;

	hp->hp_port = port;

	if (Debug)
		printf ("host entry %d, %s@%s:%d\n", i, hp->hp_name, 
			(hp->hp_addr) ? hp->hp_addr : "*", hp->hp_port);
}



static void
ReadHosts (file)
const char *file;
/*
 * Read the host-ports file and add entries to the host table.
 * Lines are of the form:
 *	host	address	     port
 * Comments begin with a '#' and end at the end of the line.
 * Line lengths are limited to 255 characters.
 */
{
	char line[256];
	char host[256];
	char addr[256];
	int port;
	FILE *infile;
	int i, scan, n;

	infile = fopen (file, "r");
	if (! infile)
	{
		perror (file);
		exit (1);
	}
	i = 0;
	line[255] = '\0';
	while (fgets (line, 256, infile) != NULL)
	{
		++i;
		if ((unsigned int)strlen(line) >= (unsigned int)255)
		{
			fprintf (stderr, "file %s: line %i too long\n", 
				 file, i);
			continue;
		}
		n = 0;
		scan = sscanf (line, "%s%n", host, &n);
		/*
		 * ignore blank lines (sscanf returns 0) and comments
		 */
		if (scan == 0 || host[0] == '#' || host[0] == '\0' || n == 0)
			continue;
		if ((scan = sscanf (line + n, " %s %i ", addr, &port)) != 2)
		{
			fprintf (stderr, "file %s: error on line %d: %s", 
				 file, i, line + n);
			exit (1);
		}
		EnterHost (host, addr, port);
	}
	fclose (infile);
}



static int
FindHost (host, port, exact)
const char *host;
int *port;
int exact;
/*
 * Find a match for this host and return its port number.  If exact is
 * non-zero, the name must match exactly.  Returns the entry's index into
 * the host table if successful, less than zero otherwise.
 */
{
	int i;

	/*
	 * Search for an exact match first.
	 */
	for (i = 0; i < NHosts; ++i)
	{
		if (! strcmp (host, HostPorts[i].hp_name))
		{
			*port = HostPorts[i].hp_port;
			return (i);
		}
	}
	if (exact)
		return (-1);
	for (i = 0; i < NHosts; ++i)
	{
		if (! strcmp (ANYHOST, HostPorts[i].hp_name))
		{
			*port = HostPorts[i].hp_port;
			return (i);
		}
	}
	return (-1);
}	



static void 
FreeHosts ()
/*
 * Release all of the host entries and allocated strings
 */
{
	int i;
	struct hostportent *hpe;

	for (hpe = HostPorts, i = 0; i < NHosts; ++i, ++hpe)
	{
		if (hpe->hp_addr)
			free (hpe->hp_addr);
		if (hpe->hp_name)
			free (hpe->hp_name);
	}
	NHosts = 0;
}



static void
MakeUnixSocket ()
/*
 * Create the Unix domain socket.
 */
{
	struct sockaddr_un saddr;
	char *sn;

	if ((M_un_socket = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
	{
		perror ("Unable to create UNIX socket");
		exit (1);
	}
	Nfd = M_un_socket + 1;
/*
 * Bind it to it's name.
 */
 	saddr.sun_family = AF_UNIX;
	if (! (sn = getenv (MSG_SOCKET_VARIABLE)))
		sn = getenv ("ZEB_SOCKET");
	if (! sn)
		sn = UN_SOCKET_NAME;
	if ((strlen (sn) >= sizeof(saddr.sun_path)) ||
	    (strlen (sn) >= sizeof(UnSocketName)))
	{
		fprintf (stderr, "socket path too long: '%s'\n", sn);
		exit (1);
	}
	strcpy (saddr.sun_path, sn);
	if (bind (M_un_socket, (struct sockaddr *) &saddr,
			sizeof (struct sockaddr_un)) < 0)
	{
		perror ("Unable to bind UNIX socket");
		exit (1);
	}
	strcpy (UnSocketName, sn);
/*
 * Tell the system that we want connections.
 */
 	listen (M_un_socket, 5);
}



void
EnterServicePort ()
{
	struct servent *service;
/*
 * Try to get the service port as a fallback for outbound connections,
 * but any other default (*@*) entries will override this one, either
 * from the command line or in a session file.
 */
	if ((service = getservbyname (SERVICE_NAME, "tcp")) != NULL)
	{
#ifdef notdef
		if (! Port)
		{
			Port = service->s_port;
			if (Debug) printf ("%s on port %d, tcp service %s\n",
					   "Listening", Port, SERVICE_NAME);
		}
#endif
		EnterHost (ANYHOST, NULL, service->s_port);
	}
}



void
GetInetPort ()
/*
 * Find out which port to use for inbound inet connections, and enter a
 * default port in the table for outbound connections.
 */
{
	int cmd_port = Port;	   /* did we get a command-line default? */
/*
 * Try to look up our port number.  The command-line option takes precedence,
 * followed by an exact match for our host name in the host table, followed 
 * by a pattern match in the host table.  With no command-line or session
 * file host-port map entries, the default pattern match will be the services
 * entry.  Finally, with no services entry, the last resort is to use the 
 * hardcoded default.
 */
	if (Port != 0)
	{
		if (Debug) printf ("Listening on cmd-line port %d\n", Port);
	}
	else if (FindHost (Hostname, &Port, FALSE) >= 0)
	{
		if (Debug) printf ("%s on port %d from host table\n", 
				   "Listening", Port);
	}
	else
	{
		if (! Port)
		{
			Port = DEFAULT_PORT;
			if (Debug) 
			   printf ("%s %s: listening on default port %d\n",
				   SERVICE_NAME, "service not found", Port);
		}
		if (cmd_port)
			EnterHost (ANYHOST, NULL, cmd_port);
		else
			EnterHost (ANYHOST, NULL, DEFAULT_PORT);
	}
}



static void
MakeInetSocket ()
/*
 * Create our internet domain socket.
 */
{
	struct sockaddr_in saddr;
	int ntry = 0;
	int one = 1;
/*
 * Get our socket.  Failures here are nonfatal, since we can, in some way,
 * get by without internet connectivity.
 */
	if ((M_in_socket = socket (AF_INET, SOCK_STREAM, 0)) < 0)
	{
		perror ("Internet socket get");
		return;
	}

/*
 * If another copy died earlier, we want to reuse our socket address,
 * rather than having to retry until the system cleans up the
 * old address
 * 
 */
	if (setsockopt(M_in_socket,SOL_SOCKET,SO_REUSEADDR,(char *)&one,
	     sizeof one) == -1) 
	{
	     perror ("setsockopt");
	     (void) close(M_in_socket);
	     return;
	}

/*
 * Fill in the address info, and bind to this port.
 */
again:
	saddr.sin_family = AF_INET;
	saddr.sin_addr.s_addr = INADDR_ANY;
	saddr.sin_port = Port;
	if (bind (M_in_socket, (struct sockaddr *) &saddr, sizeof (saddr)) < 0)
	{
	/*
	 * If we get an address in use message, wait and try again.  If 
	 * we've recently shut down and restarted, these obnoxious things
	 * can hang around for a while.
	 */
	 	if (errno == EADDRINUSE)
		{
			if (ntry++ == 0)
				printf ("Waiting for INET socket to clear");
			printf (".");
			fflush (stdout);
			sleep (5);
			goto again;
		}
		perror ("INET Socket bind");
		zmlog (EF_PROBLEM, "bind error %d, %s", errno,
		     "continuing with no INET socket");
		M_in_socket = -1;
	}
	if (ntry)
		printf ("\n");
/*
 * Tell them we're ready.
 */
	if (M_in_socket >= 0)
	{
		listen (M_in_socket, 5);
		if (M_in_socket >= Nfd)
			Nfd = M_in_socket + 1;
	}
}



Connection *
InitConnection (fd, inet)
int fd;
int inet;
/*
 * Put together a connection structure, and add it to the queue.  If we
 * can't get memory for a new connection, we'll have to refuse it.
 */
{
	Connection *conp;
/*
 * Two things can make this fail: we cannot malloc the space for another
 * connection, or we have a file descriptor larger than the number we
 * allocated in our map.
 */
	if (fd >= FD_MAP_SIZE)
	{
		send_log (EF_EMERGENCY, "fd %d higher than max allowed %d",
			  fd, FD_MAP_SIZE - 1);
		return (NULL);
	}
 	conp = (struct connection *) malloc (sizeof (struct connection));
	if (conp == NULL)
	{
		send_log (EF_EMERGENCY, "%s: aborting accepted %s connection",
			  "malloc failed", (inet) ? "IN" : "UNIX");
		return (NULL);
	}
	conp->c_fd = fd;
	conp->c_nsend = conp->c_bnsend = 0;
	conp->c_nrec = conp->c_bnrec = 0;
	conp->c_inet = inet;
	conp->c_griped = 0;
	conp->c_ndwrite = 0;
	conp->c_inprog = 0;
	conp->c_pid = 0;
	conp->c_msg.m_data = NULL;
	conp->c_dwrite = conp->c_dwtail = NULL;
	strcpy (conp->c_name, UNKNOWN_NAME);
	Fd_map[fd] = conp;
/*
 * Add this one to our list of FD's.
 */
	FD_SET (fd, &Allfds);
	if (fd >= Nfd)
		Nfd = fd + 1;
/*
 * Mark this thing for nonblocking I/O and we're done.
 */
	SetNonBlock (fd);
	return (conp);
}



static void
Hail (conp)
Connection *conp;
/*
 * Put together a greeting and send it out.
 */
{
	struct message msg;
	struct mh_greeting greet;

	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_to[0] = '\0';	/* No name yet */
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = 0;
	msg.m_len = sizeof (struct mh_greeting);
	msg.m_data = (char *) &greet;
	msg.m_seq = EMask;
	greet.mh_type = MH_GREETING;
	strcpy (greet.mh_version, MSG_PROTO_VERSION);
	strcpy (greet.mh_session, Hostname);
	send_msg (conp, &msg);
}



void
new_un_connection ()
/*
 * Deal with an incoming unix-domain connection.
 */
{
	struct connection *conp;
	int conn;
/*
 * Accept the new connection.
 */
	conn = accept (M_un_socket, (struct sockaddr *) 0,
		       (int *) 0);
	if (conn < 0)
	{
		perror ("Accept error on UNIX socket");
		send_log (EF_PROBLEM, "accept error %d on UNIX socket", errno);
		return;
	}
	send_log (EF_DEBUG, "Accepted unix connection %d", conn);
/*
 * Initialize the connection and hail our new client
 */
	if (! (conp = InitConnection (conn, FALSE)))
	{
		close (conn);
		return;					/* oh well */
	}
	Hail (conp);
}



void
NewInConnection ()
/*
 * Deal with an incoming internet-domain connection.
 */
{
	struct connection *conp;
	struct sockaddr_in saddr;
	int saddrlen;
	unsigned long inaddr;
	int conn;
	int one = 1;
/*
 * Accept the new connection.
 */
	saddrlen = sizeof(saddr);
	conn = accept (M_in_socket, (struct sockaddr *) &saddr, &saddrlen);
	if (conn < 0)
	{
		perror ("Accept error on UNIX internet-domain socket");
		send_log (EF_PROBLEM, "error %d accepting INET domain socket",
			  errno);
		return;
	}
	inaddr = ntohl(saddr.sin_addr.s_addr);
	send_log (EF_DEBUG, "%s %d, %lu.%lu.%lu.%lu:%d", 
		  "accepted INET connection", conn,
		  (inaddr & 0xff000000) >> 24,
		  (inaddr & 0x00ff0000) >> 16,
		  (inaddr & 0x0000ff00) >> 8,
		  (inaddr & 0x000000ff) >> 0,
		  saddr.sin_port);
	setsockopt (conn, SOL_SOCKET, SO_REUSEADDR, 
		    (char *)&one, sizeof (one));
/*
 * Time to initialize a connection on this fd and hail our new client
 */
	if (! (conp = InitConnection (conn, TRUE)))
	{
		close (conn);
		return;
	}
	Hail (conp);
}




static void
send_msg (conp, msgp)
struct connection *conp;
struct message *msgp;
/*
 * Send the given message to this connection.  Be careful about sending log
 * messages, lest we enter an infinite recursion of calls to send_msg from
 * within send_msg and DelayWrite.  Use log() instead.  To see messages at
 * this level, turn on debugging.
 *
 * If we add our hostname to msgp->m_from before sending, restore m_from
 * before returning.
 */
{
	struct iovec iov[2];
	int nwrote, nwant;
	char *at = NULL;
/*
 * If this message is going out over the net, we need to append
 * our host name to the from field.  But don't overrun our memory to do it.
 */
	if (conp->c_inet)
	{
		unsigned int space;

		space = sizeof(msgp->m_from) - strlen(msgp->m_from) - 1;
		at = msgp->m_from + strlen(msgp->m_from);
		if (space < strlen(Hostname) + 1)
		{
			zmlog (EF_PROBLEM, "from field too long: '%s@%s'",
			     msgp->m_from, Hostname);
			return;
		}
		else
		{
			at[0] = '@';
			strcpy (at+1, Hostname);
		}
	}
/*
 * Put together our I/O vector describing the data.
 */
	iov[0].iov_base = (caddr_t) msgp;
	iov[0].iov_len = sizeof (struct message);
	iov[1].iov_base = (caddr_t) msgp->m_data;
	iov[1].iov_len = msgp->m_len;
/*
 * Keep stats.
 */
	conp->c_nrec++;
	conp->c_bnrec += msgp->m_len;
	S_nmessage++;
	S_bnmessage += msgp->m_len;
	nwant = iov[0].iov_len + iov[1].iov_len;
/*
 * Now write it.  Careful: if there is already a delayed write queue, we
 * just add to it.
 */
	if (conp->c_dwrite)
		DelayWrite (conp, iov, 2, 0);
	else if ((nwrote = writev (conp->c_fd, iov, 2)) == nwant)
		/* do nothing and return below */ ;
/*
 * Linux will fail a write with errno=0.  Hmph.
 */
	else if (errno == 0)
		DelayWrite (conp, iov, 2, nwrote > 0 ? nwrote : 0);
	else if (errno == EWOULDBLOCK || errno == 0)
		DelayWrite (conp, iov, 2, nwrote > 0 ? nwrote : 0);
	else if (errno == ECONNREFUSED)	/* weird */
	{
		zmlog (EF_PROBLEM, "ConRefused status on %s", conp->c_name);
		DelayWrite (conp, iov, 2, nwrote > 0 ? nwrote : 0);
	}
	else
	{
	    /* No need for alarm if a write fails during a shutdown... */
	    if (M_un_socket >= 0)
	    {
		zmlog (EF_PROBLEM, "Write failed for %s, errno %d",
		       conp->c_name, errno);
	    }
	    deadconn (conp->c_fd);
	}
	if (at)
		*at = '\0';
	return;
}





static void
DelayWrite (conp, iov, niov, nwrote)
Connection *conp;
struct iovec *iov;
int niov, nwrote;
/*
 * Save up some data which couldn't make it.
 */
{
	DWrite *dwp;
/*
 * Check how much stuff is building up here.
 */
	if (conp->c_ndwrite > ((conp->c_griped + 1) * DWGRIPE))
	{
		++conp->c_griped;
		if (conp->c_ndwrite > DWDROP)
			send_log (EF_EMERGENCY, 
				  "DROPPING MESSAGES to %s; %s %d bytes",
				  conp->c_name, "delayed write queue exceeds",
				  DWDROP);
		else
			send_log (EF_PROBLEM, 
				  "WARNING %d: Proc %s not reading messages",
				  conp->c_griped, conp->c_name);
	}
	if (conp->c_ndwrite > DWDROP)
	{
		return;	/* alas */
	}
/*
 * Skip past iovecs which were completely written.
 */
	zmlog (EF_DEBUG, "DWRITE %d, iov %d wrote %d", conp->c_fd, 
	     niov, nwrote);
	while (nwrote >= iov->iov_len)
	{
		/* zmlog (EF_DEBUG, "Skip %d", iov->iov_len); */
		if (nwrote == iov->iov_len)
			zmlog (EF_DEBUG, "DWrite IOV equal case");
		nwrote -= iov->iov_len;
		iov++;
		niov--;
	}
/*
 * Now save each one which remains.
 */
	while (niov > 0)
	{
	/*
	 * Get the space.
	 */
		int nbyte;
		char *data;

		if (iov->iov_len <= 0)
		{	
			/* No sense saving an empty iovec */
			zmlog (EF_DEBUG, "Skipping iovec with len <= 0");
			iov++;
			niov--;
			nwrote = 0;
			continue;
		}

		nbyte = iov->iov_len - nwrote;
		data = (char *) malloc (nbyte);
		dwp = ALLOC (DWrite);
		if (! dwp || ! data)
		{
			zmlog (EF_EMERGENCY, 
			  "malloc failed for %d data bytes and dw struct, %s",
			  nbyte, "messages lost");
			if (dwp) free (dwp);
			if (data) free (data);
			break;
		}
		dwp->dw_nbyte = nbyte;
		dwp->dw_data = data;
		dwp->dw_nsent = 0;
		dwp->dw_next = 0;
	/*
	 * Move over the data and put it into the chain.
	 */
		memcpy (dwp->dw_data, (char*)(iov->iov_base) + nwrote, 
			dwp->dw_nbyte);
		if (conp->c_dwrite)
			conp->c_dwtail->dw_next = dwp;
		else
			conp->c_dwrite = dwp;
		conp->c_dwtail = dwp;
		conp->c_ndwrite += dwp->dw_nbyte;
		zmlog (EF_DEBUG, "Save %d -> %d", iov->iov_len - nwrote, 
		     conp->c_ndwrite);
	/*
	 * Onward.
	 */
	 	iov++;
		niov--;
		nwrote = 0;
	}
/*
 * Make sure that we will write here when the opportunity arises.
 */
	if (! FD_ISSET (conp->c_fd, &WriteFds))
	{
		FD_SET (conp->c_fd, &WriteFds);
		NWriteFd++;
		if (conp->c_fd > MaxWriteFd)
			MaxWriteFd = conp->c_fd;
	}
	S_NDWrite++;
}






static void
DoDelayedWrite (fds)
fd_set *fds;
/*
 * Try to execute delayed writes.
 */
{
	int fd;
/*
 * Pass through the list of fds.
 */
	for (fd = MINFD; fd <= MaxWriteFd; fd++)
	{
		Connection *cp;
	/*
	 * See if we can write here.
	 */
		if (! Fd_map[fd] || ! FD_ISSET (fd, fds))
			continue;
		cp = Fd_map[fd];
	/*
	 * Give it a go.
	 */
		while (cp->c_dwrite)
			if (! TryWrite (cp))
				break;
	/*
	 * If there is no more writing to be done here, clear out the
	 * info.  Use log instead of send_log since the delayed client
	 * will likely be the event logger.
	 */
		zmlog (EF_DEBUG, "%d left on %d", cp->c_ndwrite, cp->c_fd);
	 	if (! cp->c_dwrite)
		{
			FD_CLR (cp->c_fd, &WriteFds);
			NWriteFd--;
			cp->c_griped = 0;
		}
	}
}






static int
TryWrite (cp)
Connection *cp;
/*
 * Try to do a write on this connection.
 */
{
	int nwrote, nwant;
	DWrite *dw = cp->c_dwrite;
/*
 * Just try.
 */
	nwant = dw->dw_nbyte - dw->dw_nsent;
	zmlog (EF_DEBUG, "TRY %d at %d on %d", nwant, dw->dw_nsent, cp->c_fd);
	if ((nwrote = write (cp->c_fd, dw->dw_data + dw->dw_nsent, nwant))
				== nwant)
	{
		cp->c_dwrite = dw->dw_next;
		cp->c_ndwrite -= nwant;
		free (dw->dw_data);
		free (dw);
		return (TRUE);
	}
/*
 * No go.  If this is another WOULDBLOCK error, we make a note of what we
 * were able to get rid of and wait again.
 */
	if (errno == EWOULDBLOCK || errno == 0)
	{
		if (nwrote > 0)
		{
			cp->c_ndwrite -= nwrote;
			dw->dw_nsent += nwrote;
		}
	}
/*
 * If it's dead, we assume that it will be taken care of later.
 */
	else
	{
		FD_CLR (cp->c_fd, &WriteFds);
		NWriteFd--;
	}
	return (FALSE);
}



static void
inc_message (nsel, fds)
int nsel;
fd_set *fds;
/*
 * Deal with an incoming message.
 */
{
	Message *msg;
	int fd;
/*
 * Pass through the list of file descriptors.
 */
	for (fd = MINFD; fd < Nfd && nsel; fd++)
	{
	/*
	 * See if there is something on this FD.
	 */
		if (! Fd_map[fd] || ! FD_ISSET (fd, fds))
			continue;
		nsel--;
	/*
	 * Bring in the message, and dispatch it if it's all here.
	 */
	 	if ((msg = ReadMessage (fd)))
		{
			FixAddress (msg->m_to);
			if (Debug && Fd_map[fd]->c_inet && 
			    strchr (msg->m_to, '@'))
			{
#ifdef DESPERATE
				FILE *fd = fopen ("/tmp/msgbug", "w+");
				fprintf (fd, "@ left in net msg.  %s->%s\n",
					 msg->m_from, msg->m_to);
				fprintf (fd, "Hostname is %s.\n", Hostname);
#endif
				send_log (EF_PROBLEM, 
					  "host %s: @ left in net msg: %s->%s",
					  Hostname, msg->m_from, msg->m_to);
			}
			dispatch (fd, msg);
		/*
		 * This message will no longer exist if the connection died,
		 * and it may not have had any data to begin with.  Reset
		 * data pointer to NULL, since msg likely points to the
		 * connection's permanent message header and we need to know
		 * that the data has been freed.
		 */
			if (Fd_map[fd] && msg->m_data)
			{
				free (msg->m_data);
				msg->m_data = NULL;
			}
		}
	}
}




static Message *
ReadMessage (fd)
int fd;
/*
 * Pull in a piece of a message from this source.
 */
{
	Connection *cp = Fd_map [fd];
	struct message *msg = &cp->c_msg;
	int nb, nread;
/*
 * If there is not currently a read in progress, we will read in a
 * message header.  The header is short enough that we assume we can
 * pull it in with one read.
 */
	if (! cp->c_inprog)
	{
		nb = msg_XX_netread (fd, (char *)msg, sizeof (struct message));
	/*
	 * Check for a negative return.  Solaris 2.4 seems to drop out of
	 * a select for network connections before it is actually willing
	 * to hand over the data....
	 *
	 * As an extra weirdness, neither gcc-2.6.3 nor Sun CC correctly
	 * test a negative value against sizeof(...).  Weird.
	 */
		if (nb < 0)
			return (0);
	/*
	 * If we get nothing, the connection has been broken.
	 */
		if (nb < sizeof (struct message))
		{
			if (nb > 0)
				send_log (EF_PROBLEM, 
					  "Short message (%d) from %s", nb,
					  cp->c_name);
			deadconn (fd);
			return (0);
		}
		if (msg->m_len > MSG_MAX_DATALEN)
		{
			send_log (EF_PROBLEM, "%s, len %d from %s", 
				  "CORRUPT or HUGE msg",
				  msg->m_len, cp->c_name);
			/* jump ship or we'll corrupt ourselves too */
			deadconn (fd);
			return (0);
		}
	/*
	 * Set up for the rest.
	 */
		if (msg->m_len > 0)
			msg->m_data = malloc (msg->m_len);
		else
			msg->m_data = NULL;
		cp->c_inprog = TRUE;
		cp->c_nread = 0;
	/*
	 * Clients do not fill in the 'from' field; fix it
	 * now since we make assumptions that it's valid later on.
	 * Inet hosts must have sent a correct 'from' field to us.
	 */
		if (! cp->c_inet)
			strcpy (msg->m_from, cp->c_name);
	}
/*
 * Pull in the message text.
 */
	if (msg->m_len > 0)
	{
		if ((nread = msg_netread (fd, msg->m_data + cp->c_nread,
		       msg->m_len - cp->c_nread)) <= 0 && errno != EWOULDBLOCK)
		{
			deadconn (fd);
			return (0);
		}
		cp->c_nread += nread;
	}
/*
 * If we're done, mark that no read is in progress and return the message.
 */
	if (cp->c_nread >= msg->m_len)
	{
		cp->c_inprog = FALSE;
		cp->c_nsend++;
		cp->c_bnsend += msg->m_len;
		return (msg);
	}
	S_NDRead++;
	return (0);
}





void
FixAddress (addr)
char *addr;
/*
 * Remove the "@host" from the destination, if we're that host.
 */
{
	char *at;

	at = strrchr (addr, '@');
	if (at && ! strcmp (at + 1, Hostname))
		*at = '\0';
}



static void
FreeConnection (cp)
Connection *cp;
/*
 * Free any existing message data, delayed write data, and the 
 * connection structure itself.
 */
{
	DWrite *dwp;

	if (cp->c_msg.m_data)
		free (cp->c_msg.m_data);
	cp->c_msg.m_data = NULL;
	dwp = cp->c_dwrite;
	while (dwp)
	{
		DWrite *next = dwp->dw_next;
		free (dwp->dw_data);
		free (dwp);
		dwp = next;
	}
	free ((char *) cp);
}



static void
deadconn (fd)
int fd;
/*
 * Mark this connection as being dead.  Called from within send_msg,
 * so use zmlog() instead of send_log().
 */
{
	Connection *cp = Fd_map[fd];
	int setmask = 0;
/*
 * Just in case we try to disconnect more than once.
 */
	if (! cp)
	    return;

	S_ndisc++;
/*
 * Clean up our data structures.
 */
	if (cp->c_inet)
		usy_z_symbol (Inet_table, cp->c_name);
	else
		usy_z_symbol (Proc_table, cp->c_name);
/*
 * If this process was a member of the event logger group, meaning it
 * was interested in log messages and may have set the mask, reset the
 * mask to zero so that other loggers will OR their own.
 */
	if (in_group (EVENT_LOGGER_GROUP, cp))
	{
		setmask = 1;
	}
	usy_traverse (Group_table, clear_group, (long)cp, FALSE);
/*
 * Send out the notification.
 */
 	ce_disconnect (cp);
/*
 * Clear out the mtap list too.
 */
	FreeTap (fd);
/*
 * Release the connection
 */
	FreeConnection (cp);
/*
 * Clean up FDs
 */
	FD_CLR (fd, &Allfds);
	if (FD_ISSET (fd, &WriteFds))
	{
		FD_CLR (fd, &WriteFds);
		NWriteFd--;
	}
	Fd_map[fd] = NULL;
	shutdown (fd, 2);
	close (fd);
/*
 * Reset the event mask to zero, in case the dead event logger didn't.
 */
	if (setmask)
	{
		send_log (EF_DEBUG, "event logger died: resetting mask");
		BroadcastMask (EF_SETMASK);
		EMask = 0;
	}
}



int
in_group (char *name, struct connection *conp)
/*
 * Return non-zero if the given connection belongs to the named group.
 */
{
	union usy_value v;
	int type;
	int found = 0;

	if (usy_g_symbol (Group_table, name, &type, &v))
	{
		int i;
		struct group *grp = (struct group *) v.us_v_ptr;

		for (i = 0; i < grp->g_nprocs; i++)
		{
			if (grp->g_procs[i] == conp)
			{
				found = 1;
				break;
			}
		}
	}
	return (found);
}
		



int
clear_group (name, type, v, conp)
char *name;
int type;
union usy_value *v;
struct connection *conp;
/*
 * Make sure that this connection is no longer part of this group.
 */
{
	int i, j;
	struct group *grp = (struct group *) v->us_v_ptr;

	for (i = 0; i < grp->g_nprocs; i++)
	{
		if (grp->g_procs[i] == conp)
		{
			for (j = i; j < (grp->g_nprocs - 1); j++)
				grp->g_procs[j] = grp->g_procs[j+1];
			(grp->g_nprocs)--;
			break;
		}
	}
	/*
	 * Remove this group if now empty
	 */
	if (grp->g_nprocs <= 0)
	{
		send_log (EF_DEBUG, "removing empty group: %s", grp->g_name);
		free_group (name, type, v, 0);
	}
	return (TRUE);
}





int
free_group (name, type, v, unused)
char *name;
int type;
union usy_value *v;
int unused;
/*
 * Make sure this group is empty and then free its memory.
 */
{
	int i;
	struct group *grp = (struct group *) v->us_v_ptr;
	char buf[256];

	if (grp->g_nprocs > 0)
	{
		sprintf (buf, "group %s still has %hu members: ",
			 grp->g_name, grp->g_nprocs);
		for (i = 0; i < grp->g_nprocs; i++)
		{
			strcat (buf, grp->g_procs[i]->c_name);
			strcat (buf, " ");
		}
		send_log (EF_PROBLEM, "%s", buf);
	}
	if (grp->g_procs)
		free (grp->g_procs);
	free (grp);
	usy_z_symbol (Group_table, name);
	return (TRUE);
}






static int
CloseInet (host, type, v, junk)
char *host;
int type, junk;
SValue *v;
/*
 * Tell this connection to close.
 */
{
	struct mh_template t;
	struct connection *conn = (struct connection *) v->us_v_ptr;
	Message msg;

	if (strlen(MSG_MGR_NAME) + strlen(host) + 1 >= sizeof(msg.m_to))
	{
		send_log (EF_PROBLEM, "recipient name too long: %s@%s",
			  MSG_MGR_NAME, host);
	}
	sprintf (msg.m_to, "%s@%s", MSG_MGR_NAME, host);
	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = 0;
	msg.m_len = sizeof (t);
	msg.m_data = (char *) &t;
	t.mh_type = MH_NETCLOSE;
	send_msg (conn, &msg);
	return (TRUE);
}
	



static void
BroadcastMask (int mask)
/*
 * This routine is functionally similar to the SendEverybody() function
 * in the EventLogger.  Just broadcast a new mask to everybody, and if
 * setting the mask let the event logger group know, whose members may
 * be over an internet connection.
 */
{
	struct message msg;

	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_proto = MT_ELOG;
	msg.m_flags = MF_BROADCAST;
	msg.m_seq = 0;
	msg.m_len = sizeof (mask);
	msg.m_data = (char *) &mask;

	strcpy (msg.m_to, MSG_EVERYBODY);
	broadcast (&msg, 0);

	if (mask & EF_SETMASK)
	{
		strcpy (msg.m_to, EVENT_LOGGER_GROUP);
		broadcast (&msg, 0);
	}
}



static void
SendShutdown ()
{
	struct message msg;
	struct mh_template tmpl;
/*
 * Send out a message saying that it's all over.  Note that we do not
 * explicitly broadcast to inet connections (they are not in the Everybody
 * group), else we could take down the entire net.
 */
	strcpy (msg.m_to, MSG_EVERYBODY);
	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = MF_BROADCAST;
	msg.m_seq = 0;
	msg.m_len = sizeof (tmpl);
	msg.m_data = (char *) &tmpl;
	tmpl.mh_type = MH_SHUTDOWN;
	broadcast (&msg, 0);
}



static void
die ()
/*
 * Give up the ghost.
 */
{
	send_log (EF_DEBUG, "%s; going down in %d seconds",
		  "broadcasting shutdown", SHUTDOWN_DELAY);
	SendShutdown ();
/*
 * Get rid of the socket now.  That means nobody can connect during the
 * remaining up time, but it also means that we get out of the way.
 */
	unlink (UnSocketName);
	if (M_un_socket >= 0)
	{
		shutdown (M_un_socket, 2);
		close (M_un_socket);
		FD_CLR (M_un_socket, &Allfds);
		M_un_socket = -1;
	}
/*
 * Likewise stop listening to our inbound internet socket.
 */
	if (M_in_socket >= 0)
	{
		send_log (EF_DEBUG, "closing inbound inet socket");
		close (M_in_socket);
		FD_CLR (M_in_socket, &Allfds);
		M_in_socket = -1;
	}
/*
 * Now set an alarm and continue to operate for a little while longer so that
 * processes can communicate while they shut down.
 */
	signal (SIGALRM, (void(*)(/*int*/)) ReallyDie);
	alarm (SHUTDOWN_DELAY);
}




void
ReallyDie ()
/*
 * Shut us down for real.
 */
{
	int i;
	int deadbeats = 0;

	Dying = TRUE;
/*
 * Forcibly close any remaining client connections before closing
 * network connections, in case any network hosts are interested in
 * the demise of any of our clients.
 */
	send_log (EF_DEBUG, "closing remaining client connections...");
	for (i = 0; i < FD_MAP_SIZE; i++)
	{
		if (Fd_map[i] && (Fd_map[i]->c_inet == 0))
		{
			++deadbeats;
			deadconn (i);
		}
	}
	send_log (EF_DEBUG, "%d client connections forced close", deadbeats);
/*
 * Close out network connections.
 */
	send_log (EF_DEBUG, "closing inet connections...");
	usy_traverse (Inet_table, CloseInet, 0, FALSE);
	for (i = 0; i < FD_MAP_SIZE; i++)
	{
		if (Fd_map[i])
		{
			deadconn (i);
		}
	}
	FreeHosts ();
/*
 * Free group nodes and make sure our accounting of group members was correct
 */
	usy_traverse (Group_table, free_group, 0, FALSE);
/*
 * Annihilate Symbol tables
 */
	usy_z_stbl (Proc_table);
	usy_z_stbl (Group_table);
	usy_z_stbl (Inet_table);
	usy_z_stbl (InetGripeTable);
	usy_z_stbl (InetAvoidTable);
/* 
 * And finally exit
 */
	zmlog (EF_DEBUG, "exiting");
	exit (0);
}





static void
dispatch (fd, msg)
int fd;
struct message *msg;
/*
 * Do something about this message.
 */
{
/*
 * Branch out based on the protocol.  Most we just pass on through, but 
 * a couple are special.
 */
	switch (msg->m_proto)
	{
	/*
	 * MT_MESSAGE stuff is intended for us.
	 */
	   case MT_MESSAGE:
	   	MsgProtocol (fd, msg);
		break;
	/*
	 * Message tapping.
	 */
	   case MT_MTAP:
	   	Tap (fd, msg);
		break;
	/*
	 * We answer pings, but only if directed at us.  Otherwise
	 * we route the ping to the intended recipient.
	 */
	   case MT_PING:
	   case MT_CPING:
	   	if (! strcmp (msg->m_to, MSG_MGR_NAME))
			AnswerPing (fd, msg);
		else
			route (fd, msg);
		break;
	/*
	 * Ditto for queries.
	 */
	   case MT_QUERY:
	   	if (! strcmp (msg->m_to, MSG_MGR_NAME))
			Stats (Fd_map[fd], msg->m_from, 1);
		else
			route (fd, msg);
		break;
	/*
	 * Check for elog messages sent to us or broadcast to Everybody.
	 */
	   case MT_ELOG:
		if ((msg->m_flags & MF_BROADCAST) &&
		    (! strcmp (msg->m_to, MSG_EVERYBODY)))
		{
			UpdateLogMask (msg);
			route (fd, msg);
		}
		else if (! strcmp (msg->m_to, MSG_MGR_NAME))
		{
			UpdateLogMask (msg);
		}
		else
		{
			route (fd, msg);
		}
		break;
	/*
	 * Most stuff just gets sent through to the destination.
	 */
	   default:
		route (fd, msg);
		break;
	}
}




void
AnswerPing (fd, msg)
int fd;
Message *msg;
/*
 * Answer a ping message.
 */
{
	strcpy (msg->m_to, msg->m_from);
	strcpy (msg->m_from, MSG_MGR_NAME);
	msg->m_proto = MT_PING;
	send_msg (Fd_map[fd], msg);
}





void
MsgProtocol (fd, msg)
int fd;
Message *msg;
/*
 * Deal with an MT_MESSAGE message.
 */
{
	struct mh_template *tm = (struct mh_template *) msg->m_data;

	switch (tm->mh_type)
	{
	/*
	 * Some process identifying itself.
	 */
	   case MH_IDENTIFY:
		identify (fd, msg);
		break;
	/*
	 * The greeting from an internet connection we made before.
	 */
	   case MH_GREETING:
		Greeting (fd, msg);
		break;
	/*
	 * An unexpected ack from across the ether...?
	 */
	   case MH_ACK:
		send_log (EF_PROBLEM, "unexpected ack from %s", msg->m_from);
		break;
	/*
	 * Join a process group.
	 */
	   case MH_JOIN:
		join (fd, msg);
		break;
	/*
	 * Somebody wants statistics.  But not necessarily from us.
	 */
	   case MH_STATS:
		if (! strcmp (msg->m_to, MSG_MGR_NAME))
			Stats (Fd_map[fd], msg->m_from, 0);
		else
			route (fd, msg);
		break;
	/*
	 * Close out an internet connection.
	 */
	   case MH_NETCLOSE:
	   	send_log (EF_INFO, "NETCLOSE from %s", msg->m_from);
		deadconn (fd);
		break;
	/*
	 * Some process reporting it's PID.
	 */
	   case MH_PID:
	   	Fd_map[fd]->c_pid = ((struct mh_pidmsg *) tm)->mh_pid;
		break;
 	/*
	 * Client query.
	 */
	   case MH_CQUERY:
	   	ClientQuery (fd, msg);
		break;
	/*
	 * For client query responses, we just route them on through.
	 */
	   case MH_CQREPLY:
	   	route (fd, msg);
		break;
	/*
	 * Client messages must have come off an inet connection.  Just
	 * forward them on.
	 */
	    case MH_CLIENT:
		broadcast (msg, Fd_map[fd]);
		break;
	/*
	 * Someone wants a list of clients in a group.
	 */
	   case MH_LISTGROUP:
		listgroup (fd, msg);
		break;
	/*
	 * Remote message manager did not find a recipient; route the
	 * rejection back to the sender
	 */
	   case MH_NOTFOUND:
		if (! strcmp (msg->m_to, MSG_MGR_NAME))
		    send_log (EF_DEBUG, "msg to %s rejected by %s: %s",
			      ((struct mh_ident*) tm)->mh_name,  msg->m_from,
			      "recipient not found");
		else
		    route (fd, msg);
		break;
	/*
	 * If they want us to die, we'll go along with it...
	 */
	   case MH_DIE:
		die ();
		break;
	/*
	 * Someone (i.e., zstop) is sending a shutdown to someone in
	 * particular.  If us, then die, else send it on.
	 */
	   case MH_SHUTDOWN:
		if (! strcmp (msg->m_to, MSG_MGR_NAME))
		    die ();
		else
		    route (fd, msg);
		break;

	   default:
		send_log (EF_PROBLEM, "Funky MESSAGE type: %d", tm->mh_type);
		break;
	}
}





static void
ClientQuery (fd, qmsg)
int fd;
Message *qmsg;
/*
 * Deal with a client query.
 */
{
	Connection *conp = Fd_map[fd];
	struct mh_ident *mhi = (struct mh_ident *) qmsg->m_data;
	struct mh_BoolRepl mb;
	Message msg;
	char *at;
/*
 * Format up a reply.
 */
	mb.mh_type = MH_CQREPLY;
	strcpy (msg.m_to, qmsg->m_from);
	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = 0;
	msg.m_data = (char *) &mb;
	msg.m_len = sizeof (mb);
/*
 * If this is a local process, just try to look it up, and send back a
 * reply.
 */
	FixAddress (mhi->mh_name);
	if (! (at = strchr (mhi->mh_name, '@')))
	{
		mb.mh_reply = usy_defined (Proc_table, mhi->mh_name);
		send_msg (conp, &msg);
	}
/*
 * If they are after a non-local process, we need to at least insure that
 * we can reach the far end.  If so, we defer the problem to them; otherwise
 * we say no.
 */
	else if (! FindRecipient (mhi->mh_name))
	{
		mb.mh_reply = FALSE;
		send_msg (conp, &msg);
	}
/*
 * Otherwise we need to route this one on through to a remote msg.
 */
	else
	{
		strcat (qmsg->m_to, at);
		route (fd, qmsg);
	}
}





static void
identify (fd, msg)
int fd;
struct message *msg;
/*
 * Deal with an identify request.
 */
{
	struct connection *conp = Fd_map[fd];
	struct mh_ident *ident = (struct mh_ident *) msg->m_data;
	int type;
	SValue v;
/*
 * Make sure this person isn't changing his/her (:-) name.
 */
 	if (strcmp (conp->c_name, UNKNOWN_NAME))
	{
		send_log (EF_PROBLEM, "Attempted name change %s to %s", 
			  conp->c_name, ident->mh_name);
		/* nak (fd, msg); */
		return;
	}
/*
 * See if this name is already in use... and try to do something rational
 * about it.  For now, kill the existing one and let the new one take over.
 */
	if (usy_g_symbol (conp->c_inet ? Inet_table : Proc_table,
			  ident->mh_name, &type, &v))
	{
		Connection *curr = (Connection *) v.us_v_ptr;
		send_log (EF_PROBLEM, 
			  "%s: existing %s being replaced by new connection",
			  "name space collision", ident->mh_name);
		deadconn (curr->c_fd);
	}
/*
 * Fix things up.
 */
 	strcpy (conp->c_name, ident->mh_name);
	v.us_v_ptr = (char *) conp;
	usy_s_symbol (conp->c_inet ? Inet_table : Proc_table, conp->c_name,
				SYMT_POINTER, &v);
/* 
 * Add this guy to the "everybody" group, now that he has a name, but only
 * if it's a local connection.
 */
	if (! conp->c_inet)
	{
	 	add_to_group (conp, "Everybody", 0);
		ack (conp, msg);
	}
	else if (usy_defined (InetAvoidTable, conp->c_name))
	{
		send_log (EF_INFO, "%s woke up", conp->c_name);
		usy_z_symbol (InetAvoidTable, conp->c_name);
	}
/*
 * Finally, send out the client event for those who are interested.
 */
 	ce_connect (conp);
}





void
Greeting (fd, msg)
int fd;
struct message *msg;
/*
 * Deal with an internet greeting.
 */
{
	char *at;
	struct connection *conp = Fd_map[fd];
/*
 * See to it that we agree with the remote machine as to their name.
 */
	at = strrchr (msg->m_from, '@');
	if (! at || strcmp (conp->c_name, at + 1))
		send_log (EF_PROBLEM, "INET machine %s thinks it's '%s'!", 
			  conp->c_name, at ? at + 1 : msg->m_from);
}



static void
route (fd, msg)
int fd;
struct message *msg;
/*
 * Route this message on through to its destination.
 */
{
	struct connection *conp;
/*
 * Hand off copies to any eavesdroppers.
 */
	if (Taps)
		DoTaps (msg);
/*
 * If this is a broadcast, deal with it separately.
 */
 	if (msg->m_flags & MF_BROADCAST)
	{
		broadcast (msg, Fd_map[fd]);
		return;
	}
/*
 * Look up the recipient.
 */
	if ((conp = FindRecipient (msg->m_to)))
	{
		send_msg (conp, msg);
	}
/*
 * It's about time we handled the destination unknown case.
 */
	else
	{
		struct mh_ident mh;
		mh.mh_type = MH_NOTFOUND;
		strcpy (mh.mh_name, msg->m_to);
		reject (fd, msg, &mh, sizeof (mh));
		send_log (EF_DEBUG, "rejecting msg %s -> %s: %s",
			  msg->m_from, msg->m_to, "destination unknown");
	}
}



static void
reject (fd, msg, data, len)
int fd;
struct message *msg;
void *data;
int len;
/*
 * Rebound a message back to the sender.
 */
{
	struct message rej;

	strcpy (rej.m_to, msg->m_from);
	strcpy (rej.m_from, MSG_MGR_NAME);
	rej.m_proto = MT_MESSAGE;
	rej.m_flags = 0;
	rej.m_seq = msg->m_seq;
	rej.m_data = (char *) data;
	rej.m_len = len;
	send_msg (Fd_map[fd], &rej);
}



struct connection *
FindRecipient (recip)
char *recip;
/*
 * Try to find the connection to send this message through.
 */
{
	int type;
	SValue v;
	char *at;
/*
 * See if this is a local connection; if so, we just look for somebody
 * we know directly.
 */
	if (! (at = strrchr (recip, '@')))
	{
		if (! usy_g_symbol (Proc_table, recip, &type, &v))
			return (0);
		return ((struct connection *) v.us_v_ptr);
	}
/*
 * If this is an already-established Internet connection, we ship things
 * out that way.
 */
	if (usy_g_symbol (Inet_table, at + 1, &type, &v))
		return ((struct connection *) v.us_v_ptr);
/*
 * Otherwise we gotta make the connection ourselves.
 */
	return (MakeConnection (at + 1));
}



static void
Alarm ()
{
	send_log (EF_DEBUG, "alarm: timeout (%d seconds) for %s",
		  INETCTIME, "internet connection");
}


struct connection *
MakeConnection (host)
char *host;
/*
 * Make a connection to this host.
 */
{
	int sock, one = 1;
	struct sockaddr_in addr;
	struct hostent *hp;
	struct connection *conp;
	struct mh_ident ident;
	struct message msg;
	unsigned long inaddr;
	char *hostaddr;
	int port;
	int idx;
	SValue v;
/*
 * See if we don't want to deal with this host at all.
 */
	if (usy_defined (InetAvoidTable, host))
		return (0);
/*
 * See if there's a translation for this host name to an Internet address
 * and port.  Otherwise we look up the host name itself.
 */
	if ((idx = FindHost (host, &port, FALSE)) >= 0)
	{
		hostaddr = HostPorts[idx].hp_addr;
		if (! hostaddr)
			hostaddr = host;
		send_log (EF_DEBUG, "%s for %s in host table: %s:%d",
			  "Found addr", host, hostaddr, port);
	}
	else
	{
		port = Port;
		hostaddr = host;
		send_log (EF_DEBUG, "%s for %s in host table: %s %d",
			  "Match not found", host, "using port", port);
	}
/*
 * Look up the name of the host to connect to.
 */
	if (! (hp = gethostbyname (hostaddr)))
	{
		send_log (EF_PROBLEM, "Attempt to connect to unknown host %s",
			  host);
		return (NULL);
	}
/*
 * We need a socket to do this with.
 */
	if ((sock = socket (AF_INET, SOCK_STREAM, 0)) < 0)
	{
		send_log (EF_PROBLEM, "Error %d getting new inet socket", 
			  errno);
		return (NULL);
	}
/*
 * Set up a timeout for this connection.
 */
	signal (SIGALRM, (void (*)(/*int*/)) Alarm);
	alarm (INETCTIME);
/*
 * Fill in the sockaddr structure, and try to make the connection.
 */
	inaddr = ntohl(*(unsigned long *)hp->h_addr);
	memcpy (&addr.sin_addr, hp->h_addr, hp->h_length);
	addr.sin_family = AF_INET;
	addr.sin_port = port;
	send_log (EF_DEBUG, "%s %s, aka %s, address %lu.%lu.%lu.%lu:%d", 
		  "connecting to ", host, hostaddr, 
		  (inaddr & 0xff000000) >> 24,
		  (inaddr & 0x00ff0000) >> 16,
		  (inaddr & 0x0000ff00) >> 8,
		  (inaddr & 0x000000ff) >> 0,
		  addr.sin_port);
	if (connect (sock, (struct sockaddr *) &addr, sizeof (addr)) < 0)
	{
		SValue v;
		if (errno == EINTR)	/* timeout */
		{
			send_log(EF_PROBLEM, "connection timeout, avoiding %s",
				 host);
			usy_s_symbol (InetAvoidTable, host, SYMT_INT, &v);
		}
		if (! usy_defined (InetGripeTable, host))
		{
			send_log(EF_PROBLEM, "Error %d connecting to host %s",
				 errno, host);
			usy_s_symbol (InetGripeTable, host, SYMT_INT, &v);
		}
		close (sock);
		alarm (0);
		return (NULL);
	}
	setsockopt (sock, SOL_SOCKET, SO_REUSEADDR, 
		    (char *)&one, sizeof (one));
	alarm (0);

	if (! (conp = InitConnection (sock, TRUE)))
	{
		close (sock);
		send_log (EF_PROBLEM, "connection to host %s failed", host);
		return (NULL);
	}
	if (strlen(host) >= sizeof(conp->c_name))
	{
		send_log (EF_PROBLEM, "connect to '%s': hostname too long", 
			  host);
		strncpy (conp->c_name, host, sizeof(conp->c_name));
		conp->c_name[sizeof(conp->c_name) - 1] = '\0';
	}
	else
		strcpy (conp->c_name, host);
/*
 * Update host name symbol tables
 */
	v.us_v_ptr = (char *) conp;
	usy_s_symbol (Inet_table, host, SYMT_POINTER, &v);
	usy_z_symbol (InetGripeTable, host);
/*
 * Shove an identify down the pipe.  Don't append our host name here since
 * send_msg will do it for us.
 */
	ident.mh_type = MH_IDENTIFY;
	strcpy (ident.mh_name, Hostname);
	sprintf (msg.m_from, "%s", MSG_MGR_NAME);
	sprintf (msg.m_to, "%s@%s", MSG_MGR_NAME, host);
	msg.m_proto = MT_MESSAGE;
	msg.m_flags = 0;
	msg.m_len = sizeof (ident);
	msg.m_data = (char *) &ident;
	send_msg (conp, &msg);
/*
 * Rather than wait for the greeting, just return the connection now.
 */
	return (conp);
}



static void
join (fd, msg)
int fd;
struct message *msg;
/*
 * This process or internet host wants to join a new group.  Let's let them.
 */
{
	struct mh_ident *ident = (struct mh_ident *) msg->m_data;
	struct connection *conp = Fd_map[fd];
/*
 * Do the actual addition.
 */
 	add_to_group (conp, ident->mh_name, msg);
/*
 * Send back an ack to local clients.  Remote hosts send back the
 * ack on their end and aren't expecting another one from us.
 */
	if (! conp->c_inet)
		ack (conp, msg);
/*
 * Send out the event.
 */
 	ce_join (conp, ident->mh_name);
}



static void
listgroup (fd, msg)
int fd;
struct message *msg;
/*
 * This process wants of list of group members.
 */
{
	struct message out;
	struct mh_members *mg = (struct mh_members *) msg->m_data;
	struct mh_members *reply = NULL;
	struct group *grp = NULL;
	char *name;
	int i, len;
	int type;
	SValue v;

	len = sizeof (struct mh_members);
	name = (mg->mh_group[0] ? mg->mh_group : MSG_EVERYBODY);
 	if (usy_g_symbol (Group_table, name, &type, &v))
	{
	 	grp = (struct group *) v.us_v_ptr;
		len += ((grp->g_nprocs - 1) * sizeof (mg->mh_client[0]));
	}
	reply = (struct mh_members *) malloc (len);
	if (! reply)
	{
		send_log (EF_EMERGENCY, 
			  "malloc failed for reply to listgroup %s", name);
		return;
	}
	reply->mh_type = MH_GROUP;
	strcpy (reply->mh_group, name);
	reply->mh_nclient = (grp ? grp->g_nprocs : 0);
	for (i = 0; grp && (i < grp->g_nprocs); i++)
	{
		strcpy (reply->mh_client[i], grp->g_procs[i]->c_name);
	}

	strcpy (out.m_to, msg->m_from);
	strcpy (out.m_from, MSG_MGR_NAME);
	out.m_proto = MT_MESSAGE;
	out.m_len = len;
	out.m_flags = 0;
	out.m_data = (char *) reply;
	send_msg (Fd_map[fd], &out);
	if (reply)
		free (reply);
}




static void
ack (conp, msg)
struct connection *conp;
struct message *msg;
/*
 * Acknowledge this message back to the sender.
 */
{
	struct mh_ack ack;
	struct message out;
/*
 * Just fill it in.
 */
	strcpy (out.m_to, msg->m_from);
	strcpy (out.m_from, MSG_MGR_NAME);
	out.m_proto = MT_MESSAGE;
	out.m_len = sizeof (ack);
	out.m_flags = 0;
	out.m_data = (char *) &ack;
	ack.mh_type = MH_ACK;
	ack.mh_number = msg->m_seq;
/*
 * ...and send it.
 */
 	send_msg (conp, &out);
}



void
add_to_group (conp, name, msg)
struct connection *conp;
char *name;
struct message *msg;
/*
 * Add this connection to the group specified by the given name.
 */
{
	union usy_value v;
	int type, i;
	struct group *grp;
	char *at;
	struct connection *remote;
/*
 * See if we're dealing with a remote group here.
 */
	if ((at = strrchr (name, '@')))
		*at++ = '\0';
/*
 * Look up this group in our symbol table.
 */
 	if (! usy_g_symbol (Group_table, name, &type, &v))
	/*
	 * No such group -- we'll have to create it.
	 */
	 	create_group (conp, name);
/*
 * Add this entry.
 */
	else
	{
	/*
	 * Make sure this connection is not already a member.  That can
	 * happen easily with inet connections.
	 */
	 	grp = (struct group *) v.us_v_ptr;
		for (i = 0; i < grp->g_nprocs; i++)
			if (grp->g_procs[i] == conp)
				break;
	/*
	 * Add this one if necessary.
	 */
		if (i >= grp->g_nprocs)	/* Not there yet */
		{
			grp->g_nprocs++;
			grp->g_procs = (struct connection **)
				realloc (grp->g_procs,
				grp->g_nprocs*sizeof (struct connection *));
			if (! grp->g_procs)
				send_log (EF_EMERGENCY, 
					  "%s for addition of %s to group %s",
					  "realloc failed", conp->c_name,
					  grp->g_name);
		 	grp->g_procs[grp->g_nprocs - 1] = conp;
		}
	}
/*
 * If this is a remote group join request, forward it on to the remote machine.
 */
	if (at)
	{
		sprintf (msg->m_to, "%s@%s", MSG_MGR_NAME, at);
		if ((remote = FindRecipient (msg->m_to)))
			send_msg (remote, msg);
	}
}




static void
create_group (conp, name)
struct connection *conp;
char *name;
/*
 * Create a new group, with this connection as a member.
 */
{
	struct group *grp;
	union usy_value v;
/*
 * Get the new structure and fill it in.
 */
 	grp = (struct group *) malloc (sizeof (struct group));
	if (! grp)
	{
		send_log (EF_EMERGENCY, "malloc failed for group %s", name);
		return;
	}
	grp->g_procs = (struct connection **)
			malloc (sizeof (struct connection *));
	if (! grp->g_procs)
	{
		send_log (EF_EMERGENCY, "%s creating group %s for %s", 
			  "malloc failed", name, conp->c_name);
		free (grp);
		return;
	}
	grp->g_procs[0] = conp;
	grp->g_nprocs = 1;
	strcpy (grp->g_name, name);
/*
 * Add it to our symbol table.
 */
 	v.us_v_ptr = (char *) grp;
	usy_s_symbol (Group_table, name, SYMT_POINTER, &v);
}





static void
broadcast (msg, conp)
struct message *msg;
struct connection *conp;
/*
 * Broadcast a message to a whole group of processes.
 */
{
	struct group *grp;
	int i, type;
	union usy_value v;
	char *at;
	Connection *netcon;
/*
 * If this thing has an @ in it, what we really want to do is to ship it
 * across the net.
 */
	if ((at = strrchr (msg->m_to, '@')))
	{
		if ((netcon = FindRecipient (msg->m_to)))
			send_msg (netcon, msg);
	}
/*
 * Otherwise we distribute locally.
 */
	else
	{
	/*
	 * Lookup the group.
	 */
	 	if (! usy_g_symbol (Group_table, msg->m_to, &type, &v))
			return;
		grp = (struct group *) v.us_v_ptr;
	/*
	 * Stats.
	 */
		S_nbcast++;
		S_bnbcast += msg->m_len;
	/*
	 * Now step through the connections, sending to each, but being careful
	 * not to send to the originator of the message.
	 */
	 	for (i = 0; i < grp->g_nprocs; i++)
		{
			if (grp->g_procs[i] != conp)
				send_msg (grp->g_procs[i], msg);
		}
	}
}




static void
ce_send (conp, event, group)
struct connection *conp;
int event;
char *group;
/*
 * Give notice to all of this client event.
 */
{
	struct message msg;
	struct mh_clientevent cl;
	int i;
/*
 * Fill in our message.
 */
	msg.m_proto = MT_MESSAGE;
	strcpy (msg.m_from, MSG_MGR_NAME);
	strcpy (msg.m_to, MSG_CLIENT_EVENTS);
	msg.m_seq = conp->c_fd;
	msg.m_flags = MF_BROADCAST;
	msg.m_len = sizeof (cl);
	msg.m_data = (char *) &cl;
	strcpy (cl.mh_client, conp->c_name);
	cl.mh_group[0] = '\0';
	if (group)
		strcpy (cl.mh_group, group);
	cl.mh_type = MH_CLIENT;
	cl.mh_evtype = event;
	cl.mh_inet = conp->c_inet;
/*
 * First broadcast locally, if we're not in the process of shutting down.
 */
	if (! Dying)
	{
		strcpy (cl.mh_client, conp->c_name);
		broadcast (&msg, conp);
	}
/*
 * Then send it out over any network connections we may have.  We rely on 
 * send_msg to restore our m_from field after each call.
 */
	strcpy (cl.mh_client, conp->c_name);
	strcat (cl.mh_client, "@");
	strcat (cl.mh_client, Hostname);
	for (i = MINFD; i < Nfd; i++)
	{
		if (Fd_map[i] && Fd_map[i]->c_inet)
			send_msg (Fd_map[i], &msg);
	}
}



static void
ce_connect (conp)
struct connection *conp;
/*
 * Send out a client event noting that this client has connected.
 */
{
	ce_send (conp, MH_CE_CONNECT, NULL);
}



static void
ce_disconnect (conp)
struct connection *conp;
/*
 * Send out a client disconnect message.
 */
{
/*
 * Don't announce the demise of internet connections, though there is some
 * info here that might be useful...
 */
	if (conp->c_inet)
		return;
	ce_send (conp, MH_CE_DISCONNECT, NULL);
}



static void
ce_join (conp, group)
struct connection *conp;
char *group;
/*
 * Send out a client group join message.
 */
{
	ce_send (conp, MH_CE_JOIN, group);
}



static int
psig ()
/*
 * Cope with pipe signals.  We don't have to actually *do* anything, since
 * the dead process will be noted later.
 */
{
	signal (SIGPIPE, (void (*)(/*int*/)) psig);
	S_npipe++;
	return (0);
}




static void
Stats (conp, to, query)
struct connection *conp;
char *to;
int query;	/* nonzero if this a MT_QUERY rather the MH_STATS */
/*
 * Send statistics back to this guy.
 */
{
	char buffer[2048];
	char *text;
	char *username;
	struct message msg;
	struct mh_stats *mhs = (struct mh_stats *) buffer;
	struct mh_template *mh = (struct mh_template *) buffer;
	int len; /* sizeof msg_data, including null chr, excl the string */
	int i;
/*
 * Fill in a message structure to be used in sending back the data.
 */
	strcpy (msg.m_to, to);
	strcpy (msg.m_from, MSG_MGR_NAME);
	if (query)
	{
		msg.m_proto = MT_QUERY;
		msg.m_flags = 0;
		msg.m_data = (char *) mh;
		mh->mh_type = MHQ_QTEXT;
		text = buffer + sizeof (struct mh_template);
		len = sizeof (struct mh_template) + 1;
	}
	else
	{
		msg.m_proto = MT_MESSAGE;
		msg.m_flags = 0;
		msg.m_data = (char *) mhs;
		mhs->mh_type = MH_STATS;
		text = mhs->mh_text;
		len = sizeof (struct mh_stats);
	}
/*
 * Process name, user name, user id:
 */
	if (!(username = getenv ("USER")) && !(username = getenv ("LOGNAME")))
	{
		struct passwd *pw = getpwuid (getuid());
		username = pw->pw_name;
	}
	sprintf (text, "'%s'@%s: pid %i, uid %i (%s)", 
		 MSG_MGR_NAME, Hostname, (int) getpid(), 
		 (int) getuid(), (username) ? username : "unknown");
/*
 * The 1 character in mh_text included in sizeof holds space for the '\0'
 */
	msg.m_len = len + strlen (text);
	send_msg (conp, &msg);
/*
 * UNIX socket path and Internet port:
 */
	sprintf (text, "unix socket: %s; ", UnSocketName);
	if (M_in_socket >= 0)
		sprintf(text+strlen(text), "internet socket: port %d", Port);
	else
		strcat (text, "internet socket: not enabled");
	msg.m_len = len + strlen (text);
	send_msg (conp, &msg);
/*
 * Session genesis time
 */
	sprintf (text, "session began: %s", (char *) ctime(&S_genesis));
	sprintf (text+strlen(text)-1, "; proto version '%s'", 
		 MSG_PROTO_VERSION);
	msg.m_len = len + strlen (text);
	send_msg (conp, &msg);
/*
 * Throughput stats
 */
	sprintf (text, "%d messages sent, %d bytes (%d/%d broadcast)",
		S_nmessage, S_bnmessage, S_nbcast, S_bnbcast);
	msg.m_len = len + strlen (text);
	send_msg (conp, &msg);
	sprintf (text,
		"\t%d disconnects, with %d pipe signals, %d del rd %d wt",
		S_ndisc, S_npipe, S_NDRead, S_NDWrite);
	msg.m_len = len + strlen (text);
	send_msg (conp, &msg);
/*
 * Now go through and report on each connection.
 */
	for (i = 0; i < Nfd; i++)
	{
		struct connection *c;
		if (! (c = Fd_map[i]))
			continue;
		sprintf (text,
			" %s '%s' on %d (p %d), send %d/%d, rec %d/%d, nd %d",
			c->c_inet ? "Internet" : "Process ",
			c->c_name, i, c->c_pid, c->c_nsend, c->c_bnsend,
			c->c_nrec, c->c_bnrec, c->c_ndwrite);
		msg.m_len = len + strlen (text);
		send_msg (conp, &msg);
	}
/*
 * Current event mask.
 */
	sprintf (text, "Event mask: %0#x", EMask);
	msg.m_len = len + strlen (text);
	send_msg (conp, &msg);
/*
 * Finally list our groups and the members of each.
 */
	strcpy (text, "Groups:\n");
	usy_traverse (Group_table, stat_group, (long)text, FALSE);
	msg.m_len = len + strlen (text);
	send_msg (conp, &msg);
/*
 * Send the EOF and quit.
 */
	if (query)
	{
		mh->mh_type = MHQ_QDONE;
		msg.m_len = sizeof (struct mh_template);
	}
	else
	{
		mhs->mh_text[0] = '\0';
		msg.m_len = len;
	}
	send_msg (conp, &msg);
}



/*ARGSUSED*/
static int
stat_group (name, type, v, text)
char *name;
int type;
union usy_value *v;
char *text;
{
	int i;
	struct group *grp = (struct group *) v->us_v_ptr;

	sprintf (text+strlen(text), " %s (%d): ", 
		 grp->g_name, grp->g_nprocs);
	for (i = 0; i < grp->g_nprocs; i++)
	{
		sprintf (text+strlen(text), "%s%s", (i) ? ", " : "", 
			 grp->g_procs[i]->c_name);
	}
	strcat (text, "\n");
	return (TRUE);
}



static void
Tap (conn, msg)
int conn;
Message *msg;
/*
 * Deal with an incoming tap request.
 */
{
	struct MTap *mt = ALLOC (struct MTap);
/*
 * Fill in the tap info and add it to the list.
 */
	if (! mt)
	{
		send_log (EF_PROBLEM, "malloc failed for tap node");
		return;
	}
	mt->mt_who = conn;
	mt->mt_tapee = * (struct msg_mtap *) msg->m_data;
	mt->mt_next = Taps;
	Taps = mt;
}



void
DoTaps (msg)
Message *msg;
/*
 * Send copies of this message to anybody who wants it.
 */
{
	struct MTap *eavesdropper;

	for (eavesdropper = Taps; eavesdropper; 
			eavesdropper = eavesdropper->mt_next)
		if (TapperWants (eavesdropper, msg))
			SendTap (eavesdropper, msg);
}




static void
FreeTap (fd)
int fd;
/*
 * Remove any tap nodes for this connection.
 */
{
	if (Taps)
	{
		struct MTap *mt;

		if (Taps->mt_who == fd)
		{
			mt = Taps;
			Taps = mt->mt_next;
		}
		else
		{
			struct MTap *last = Taps;

			for (mt = Taps->mt_next; mt; mt = mt->mt_next)
			{
				if (mt->mt_who == fd)
				{
					last->mt_next = mt->mt_next;
					break;
				}
				last = mt;
			}
		}
		if (mt)
			free (mt);
	}
}




int
TapperWants (who, msg)
struct MTap *who;
Message *msg;
/*
 * See if this person wants this message.
 */
{
	int i;
	struct msg_mtap *mt = &who->mt_tapee;

	if (mt->mt_nclient == 0 && mt->mt_nproto == 0)
		return (TRUE);
	for (i = 0; i < mt->mt_nproto; i++)
		if (msg->m_proto == mt->mt_protos[i])
			return (TRUE);
	for (i = 0; i < mt->mt_nclient; i++)
		if (! strcmp (msg->m_to, mt->mt_clients[i]) ||
		    ! strcmp (msg->m_from, mt->mt_clients[i]))
		    	return (TRUE);
	return (FALSE);
}




void
SendTap (who, msg)
struct MTap *who;
Message *msg;
/*
 * Send this message to the indicated eavesdropper.
 */
{
	Message outmsg;
	struct connection *c = Fd_map[who->mt_who];
/*
 * Assemble the outgoing message.
 */
	strcpy (outmsg.m_to, c->c_name);
	strcpy (outmsg.m_from, MSG_MGR_NAME);
	outmsg.m_proto = MT_MTAP;
	outmsg.m_flags = 0;
	outmsg.m_len = sizeof (Message) + msg->m_len;
	outmsg.m_data = malloc (outmsg.m_len);
	if (! outmsg.m_data)
	{
		send_log (EF_PROBLEM, "malloc failed for tap message data");
		return;
	}
	memcpy (outmsg.m_data, msg, sizeof (Message));
	memcpy (outmsg.m_data + sizeof (Message), msg->m_data, msg->m_len);
/*
 * Ship it out and clean up.
 */
	send_msg (c, &outmsg);
	free (outmsg.m_data);
}



void
SetNonBlock (fd)
int fd;
/*
 * Make this thing do non-blocking I/O.
 */
{
# ifdef hpux
/*
 * The HP has, believe it or not, THREE different types of non-blocking I/O,
 * all of which are subtly different.  FNDELAY creates weirdness, but
 * FIOSNBIO seems to work the way we expect....
 */
	int arg = 1;
	if (ioctl (fd, FIOSNBIO, &arg) < 0)
# else
	if (fcntl (fd, F_SETFL, FNDELAY) < 0)
# endif
		send_log (EF_PROBLEM, "Error %d doing FNDELAY", errno);
}




static void
UpdateLogMask (msg)
struct message *msg;
{
	struct msg_elog *el = (struct msg_elog *) msg->m_data;

	if (el->el_flag & EF_SETMASK)
	{
		EMask = el->el_flag & ~EF_SETMASK;
	}
	else if (el->el_flag & EF_ORMASK)
	{
		EMask |= (el->el_flag & ~EF_ORMASK);
	}
}




static void
zmlog (int flags, char* fmt, ...)
/*
 * Log a message short of sending it.  Meant for within send_msg, since using
 * send_log can lead to infinite or indefinite recursion.
 */
{
	char mbuf[512];
	va_list args;
/*
 * No sense in compiling a message if we won't be printing it
 */
	if (! (flags & (EF_PROBLEM | EF_EMERGENCY)) && (! Debug))
		return;
/*
 * Format the message.
 */
 	va_start (args, fmt);
	vsprintf (mbuf, fmt, args);
	va_end (args);
	fprintf (stderr, "%s (%s): %s\n", MSG_MGR_NAME, Hostname, mbuf);
}



static void
send_log (int flags, char* fmt, ...)
/*
 * Send a message to the event logger.
 */
{
	char mbuf[512];
	va_list args;
	struct message msg;
	struct msg_elog *el;
	int len;
/*
 * Format the message.
 */
 	va_start (args, fmt);
	el = (struct msg_elog *) mbuf;
	vsprintf (el->el_text, fmt, args);
	va_end (args);
	if (Debug || (flags & (EF_PROBLEM | EF_EMERGENCY)))
		fprintf (stderr, "%s (%s): %s\n", MSG_MGR_NAME, Hostname,
			 el->el_text);
	if ((flags & EMask) == 0)
		return;
/*
 * Now broadcast this message to the event logger group.
 */
	el->el_flag = flags;
	len = sizeof (*el) + strlen (el->el_text);
	msg.m_proto = MT_ELOG;
	strcpy (msg.m_to, EVENT_LOGGER_GROUP);
	strcpy (msg.m_from, MSG_MGR_NAME);
	msg.m_flags = MF_BROADCAST;
	msg.m_len = len;
	msg.m_data = (char *) el;
	broadcast (&msg, 0);
}


