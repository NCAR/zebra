# include <sys/types.h>
# include <sys/socket.h>
# include <netinet/in.h>
# include <netdb.h>
# include <stdio.h>
# include <signal.h>
# include "netdisk.h"

# ifdef titan
# include "../h/t_fname.h"
# endif


# define TRUE 1
# define FALSE 0

# define MAXBUFSIZE	65536	/* Maximum size for network comm. buffer */
# define MAXIO		4096	/* Max bytes sent at one time */

static FILE *Sin, *Sout;
static int Connected = FALSE;

static int Len = 0;		/* Hold length of last move for cli_bio_wait */
/*
 * Forward declarations
 */
char	*rpl_getstr (), *rpl_getbyt ();
/*
 * Inline min (x, y)
 */
# define	MIN(x,y)	((x) > (y) ? (y) : (x))





netdisk_setup (file)
char *file;
/*
 * Get connected to the netdisk server.
 */
{
	char hostname[40], servname[40];
	int skt, status;
	struct sockaddr_in addr;
	struct servent *getservbyname (), *srv;
	struct hostent *gethostbyname (), *host;
	void (*oldsig)();
/*
 * If we are already connected, no work need be done.
 */
	if (Connected)
		return (TRUE);
/*
 * Do a lookup on our service.
 */
 	if ((srv = getservbyname ("rdss_bio", "tcp")) == NULL)
		ui_error ("Unable to get netfile service -- sorry");
/*
 * Find our host.
 */
	strncpy (servname, file, strcspn (file, ":"));
	servname[strcspn (file, ":")] = '\0';
 	if ((host = gethostbyname (servname)) == NULL)
		ui_error ("%s would appear not to exist!", servname);
/*
 * Something we do here causes occasional alarm signals -- I don't have the
 * faintest idea why.  We'll just ignore them.
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
		signal (SIGALRM, oldsig);
		ui_error ("I can't get a socket for net communications!");
	}
/*
 * Get connected to the remote machine.
 */
	ui_printf ("Connecting to netfile server..."); fflush (stdout);
 	status = connect (skt, (struct sockaddr *) &addr, sizeof (addr));
	if (status < 0)
	{
		signal (SIGALRM, oldsig);
		ui_error ("I am unable to connect to the calibration server");
	}
	ui_printf ("done\n");
/*
 * Create a couple of FILE descriptors for this socket.
 */
	Sin = fdopen (skt, "r");
	skt = dup (skt);
	Sout = fdopen (skt, "w");
/*
 * Get the banner line.
 */
	rpl_receive ();
	ui_printf ("'%s'\n", rpl_getstr ());
/*
 * Tell it who we are.
 */
	gethostname (hostname, 40);
	msg_addstr (hostname);
	msg_addstr (getenv ("USER"));
	msg_send ();
	rpl_receive ();
	ui_printf ("'%s'\n", rpl_getstr ());
/*
 * All done!
 */
	Connected = TRUE;
	signal (SIGALRM, oldsig);
	return (TRUE);
}



cli_bio_open (file)
char *file;
{
	int		*fileid;

	netdisk_setup (file);
	msg_addopc (OP_BIO_OPEN);
	msg_addstr (&file[strcspn (file, ":") + 1]);
	msg_send ();
	rpl_receive ();
	fileid = (int *) rpl_getbyt (4);
	return (*fileid);
}




cli_bio_view (file)
char *file;
{
	int	*fileid;
		
	
	netdisk_setup (file);
	msg_addopc (OP_BIO_VIEW);
	msg_addstr (&file[strcspn (file, ":") + 1]);
	msg_send ();
	rpl_receive ();
	fileid = (int *) rpl_getbyt (4);
	return (*fileid);
}





cli_bio_create (file, alloc, extend)
char *file;
int *alloc, *extend;
{
	int	*fileid;

	netdisk_setup (file);
	msg_addopc (OP_BIO_CREATE);
	msg_addstr (&file[strcspn (file, ":") + 1]);
	msg_addbyt ((char *) alloc, 4);
	msg_addbyt ((char *) extend, 4);
	msg_send ();
	rpl_receive ();
	fileid = (int *) rpl_getbyt (4);
	return (*fileid);
}




cli_bio_temp (file, alloc, extend)
char *file;
int *alloc, *extend;
{
	int	*fileid;

	netdisk_setup (file);
	msg_addopc (OP_BIO_TEMP);	
	msg_addstr (&file[strcspn (file, ":") + 1]);
	msg_addbyt ((char *) alloc, 4);
	msg_addbyt ((char *) extend, 4);
	msg_send ();
	rpl_receive ();
	fileid = (int *) rpl_getbyt (4);
	return (*fileid);
}	





cli_bio_close (lun)
int lun;
{
	if (!Connected) return;
	msg_addopc (OP_BIO_CLOSE);
	msg_addbyt ((char *) &lun, 4);
	msg_send ();
}





cli_bio_read (lun, block, buffer, nbytes)
int lun, *block, *nbytes;
char *buffer;
{
	int		*nread;

	if (!Connected) return (-1);
	msg_addopc (OP_BIO_READ);
	msg_addbyt ((char *) &lun, 4);
	msg_addbyt ((char *) block, 4);
	msg_addbyt ((char *) nbytes, 4);
	msg_send ();
	rpl_receive ();
	nread = (int *) rpl_getbyt (4);
	if (*nread > 0)
	{
		char *pos = rpl_getbyt (*nread);	

		memcpy (buffer, pos, *nread);
	}
	Len = *nread;
	return (*nread);	
}





cli_bio_wait (lun)
int lun;
{
	return (Len);
}





cli_bio_write (lun, block, buffer, nbytes)
int lun, *block, *nbytes;
char *buffer;
{
	if (!Connected) return (-1);
	msg_addopc (OP_BIO_WRITE);
	msg_addbyt ((char *) &lun, 4);
	msg_addbyt ((char *) block, 4);
	msg_addbyt ((char *) nbytes, 4);
	msg_addbyt (buffer, *nbytes);
	msg_send ();
	rpl_receive ();
	Len = *(int *) rpl_getbyt (4);
	return (Len);		
}





msg_addopc (opc)
int opc;
{
	char	temp = opc;

	msg_addbyt (&temp, 1);
}





msg_addstr (str)
char *str;
{
	msg_addbyt (str, strlen (str) + 1);
}





static char	Xmit_buf[MAXBUFSIZE];
static int	Xmit_pos;

msg_addbyt (ptr, num)
char *ptr;
int num;
{
	memcpy (&Xmit_buf[Xmit_pos], ptr, num);
	Xmit_pos += num;
}





msg_send ()
{
	int	num, ntimes, nsend;
	double	ceil ();

	num = Xmit_pos;
	Xmit_pos = 0;
	ntimes = ceil ((double) num / (double) MAXIO);
	tcp_write (&ntimes, 4);
	while (ntimes--)
	{
		nsend = MIN (num, MAXIO);
		tcp_write (&nsend, 4);
		tcp_write (&Xmit_buf[Xmit_pos], nsend);
		Xmit_pos += nsend;
		num -= nsend;
	} 
	Xmit_pos = 0;
}





static char	Rply_buf[MAXBUFSIZE];  
static int	Rply_pos = 0;

char *
rpl_getstr ()
{
	char	*ptr = &Rply_buf[Rply_pos];

	Rply_pos += (strlen (ptr) + 1);
	return (ptr);
}





char *
rpl_getbyt (num)
int num;
{
	char	*ptr = &Rply_buf[Rply_pos];

	Rply_pos += num;
	return (ptr);
}





rpl_receive ()
{
	char	temp[4];
	int	ntimes, ngot;

	tcp_read (&ntimes, 4);
	Rply_pos = 0;
	while (ntimes--)
	{
		tcp_read (&ngot, 4);
		tcp_read (&Rply_buf[Rply_pos], ngot);
		Rply_pos += ngot;
	}
	Rply_pos = 0;
}	





tcp_read (buf, len)
char *buf;
int len;
{
	if (! fread (buf, 1, len, Sin))
	{
		fclose (Sin);
		fclose (Sout);
		Connected = FALSE;
		ui_error ("rpl_receive...Connection failed");
	}
}





tcp_write (buf, len)
char *buf;
int len;
{
	void (*oldsig) ();
/*
 * Major kludge: Ignore pipe signals for the duration of this write, so that
 * we don't die if the connection goes away.  The problem should be caught
 * in the "get" call that will come after this send.
 *
 * Sunview doesn't like us to call this directly, but I'm going to try.
 */
 	oldsig = signal (SIGPIPE, SIG_IGN);
	fwrite (buf, 1, len, Sout);
	fflush (Sout);
	(void) signal (SIGPIPE, oldsig);
}	
