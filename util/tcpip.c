/*----------------------------------------------------------------------*\
 * tcp/ip generic server and client access routines			*
 * tcp_rdr.c								*
 * UCAR/NCAR/ATD/RDP/G. Forrest Cook	June 3, 1991			*
\*----------------------------------------------------------------------*/

# include <sys/types.h>
# include <sys/socket.h>
# include <netinet/in.h>
# include <arpa/inet.h>
# include <netdb.h>
# include <signal.h>
# include <stdio.h>
# include <string.h>
# include <fcntl.h>
# include <unistd.h>
# include <stdlib.h>

# define BACKLOG 5		/* max length of pending connections que */
struct servent *getservbyname ();
struct hostent *gethostbyname ();
void (*oldsig)();

/*----------------------------------------------------------------------*\
 * Wait for a tcp/ip socket request, then start up the connection.	*
 * Returns -1 on error, or a non-negative socket number on success.	*
 * This function is not needed for programs fired off by inetd.		*
\*----------------------------------------------------------------------*/

int tcp_srv_start (service)
char *service;
{
	char ostr[80];
	int skt, realskt;
	struct sockaddr_in addr;
	struct servent *srv;

/*
 * Find the pam_data service number
 */
	if ((srv = getservbyname (service, "tcp")) != NULL)
	{
		sprintf (ostr, "Service <%s> found, port:%d",
				service, srv->s_port);
		log_gmt (ostr);
	}
	else
	{
		sprintf (ostr, "Unable to get service <%s>", service);
		log_gmt (ostr);
		gmt_perr (": getservbyname");
		return (-1);
	}

/*
 * Create a socket.
 */
	if ((skt = socket (AF_INET, SOCK_STREAM, 0)) < 0)
	{
		gmt_perr (": socket");
		return (-1);
	}

/*
 * Fill in the name structure.
 */
	addr.sin_family = AF_INET;
	addr.sin_addr.s_addr = INADDR_ANY;
	addr.sin_port = srv->s_port;

/*
 * Bind to the socket.
 */
	if (bind (skt, (struct sockaddr *) &addr, sizeof (addr)) < 0)
	{
		gmt_perr (": bind");
		return (-1);
	}

	log_gmt ("waiting for a TCP/IP server request");

/*
 * Listen for connections to the socket
 */
	if (listen (skt, BACKLOG))
	{
		gmt_perr (": listen");
		return (-1);
	}

/*
 * Accept a connection on the socket.
 */
	if ((realskt = accept (skt, NULL, (int *) 0)) < 0)
	{
		gmt_perr (": accept");
		return (-1);
	}

	log_gmt ("connected");

	return (realskt);
}

/*----------------------------------------------------------------------*\
 *	Set up a tcp/ip client connection				*
 *	Based on Jon Corbet's routine					*
 * Returns -1 on error, or a non-negative socket number on success	*
\*----------------------------------------------------------------------*/

int tcp_cli_start (hostname, service)
char *hostname, *service;
{
	char ostr[80];
	int skt;
	int opv = 1;				/* non zero to enable option */
	int opl = sizeof (int);			/* length of opv */
	struct sockaddr_in addr;
	struct servent *srv;
	struct hostent *host;

/*
 * Find the pam_data service number
 */
	if ((srv = getservbyname (service, "tcp")) != NULL)
	{
		sprintf (ostr, "Service <%s> found, port:%d",
			service, srv->s_port);
		log_gmt (ostr);
	}
	else
	{
		sprintf (ostr, "Unable to get service <%s>", service);
		log_gmt (ostr);
		gmt_perr (": getservbyname");
		return (-1);
	}

/*
 * Find our host.
 */
 	if ((host = gethostbyname (hostname)) == NULL)
	{
		sprintf (ostr, "Host %s was not found", hostname);
		log_gmt (ostr);
		return (-1);
	}
/*
 * Something we do here causes occasional alarm signals -- I don't have the
 * faintest idea why.  We'll just ignore them. (jc)
 */
 	oldsig = signal (SIGALRM, SIG_IGN);
/*
 * Fixup the addr struct.
 */
 	memset ((char *) &addr, sizeof (struct sockaddr_in), 0);
	memcpy ((char *) &addr.sin_addr, host->h_addr, host->h_length);
	addr.sin_family = host->h_addrtype;
	addr.sin_port = srv->s_port;

	if ((skt = socket (host->h_addrtype, SOCK_STREAM, 0)) < 0)
	{
		gmt_perr (": socket");
		signal (SIGALRM, oldsig);
		return (-1);
	}
/*
 * Get connected to the remote machine.
 */
	log_gmt ("Opening the TCP/IP client connection");

 	if (connect (skt, (struct sockaddr *) &addr, sizeof (addr)) < 0)
	{
		gmt_perr (": connect");

		if (close (skt) < 0)
			gmt_perr (": close");

		signal (SIGALRM, oldsig);
		return (-1);
	}

/*
 * install the keepalive timer, SIGPIPE int is set to ignore
 */
	if (setsockopt(skt, SOL_SOCKET, SO_KEEPALIVE, (char *)&opv, opl) < 0)
	{
		gmt_perr (": setsockopt");
		signal (SIGALRM, oldsig);
		return (-1);
	}

	log_gmt ("Connected");

/*
 * All done!
 */
	signal (SIGALRM, oldsig);

	return (skt);
}

/*----------------------------------------------------------------------*\
 *	Close the tcp/ip server connection.				*
 *	Returns 0 on error, 1 on success				*
\*----------------------------------------------------------------------*/

int tcp_srv_close (socket, verb)
int socket, verb;
{
	if (verb) log_gmt ("Closing the TCP/IP server connection");

	if (socket < 0) return (0);

	if (close (socket) < 0)
	{
		gmt_perr (": close");
		return (0);
	}

	return (1);
}

/*----------------------------------------------------------------------*\
 *	Close the tcp/ip client connection.				*
 *	Returns 0 on error, 1 on success				*
\*----------------------------------------------------------------------*/

int tcp_cli_close (socket, verb)
int socket, verb;
{
	if (verb) log_gmt ("Closing the TCP/IP client connection");

	if (socket < 0) return (0);

	if (close (socket) < 0)
	{
		gmt_perr (": close");
		return (0);
	}

	return (1);
}

/*----------------------------------------------------------------------*\
 * Split off the tcp/ip output stream and the stdio stream for inetd	*
 * processes.  Point the stdio to a logfile for logging messages.	*
\*----------------------------------------------------------------------*/

int inetd_stdio_set (logfile)
char *logfile;
{
	int skt;

	if ((skt = dup (1)) == -1) exit (-1);

	if (close (1) == -1) exit (-1);

/*
 * If the file exists, append to it, otherwise create a new one.
 */
	if (open (logfile, O_APPEND|O_WRONLY, 0664) == -1)
	{
		if (open (logfile, O_CREAT|O_WRONLY, 0664) == -1)
			exit (-1);
	}

	return (skt);
}

/*----------------------------------------------------------------------*\
 * Return the remote machine's internet number.				*
 * Warning: this doesn't seem to work on inetd processes due to some	*
 * kind of weirdness with the stdio being tweaked.			*
\*----------------------------------------------------------------------*/

char *tcp_peername (socket, ostr)
int socket;
char *ostr;
{
	struct sockaddr_in name;
	int namelen = sizeof name;

	if (getpeername (socket, (struct sockaddr *)&name, &namelen) < 0)
	{
		sprintf (ostr, "getpeername error");
	}
	else sprintf (ostr, "connected to: %s", inet_ntoa (name.sin_addr));

	return (ostr);
}

/*----------------------------------------------------------------------*/
