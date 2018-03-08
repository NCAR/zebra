# ifdef NETACCESS

# include <stdlib.h>
# include <string.h>
# include <sys/types.h>
# include <sys/socket.h>
# include <netinet/in.h>
# include <netdb.h>
# include <stdio.h>
# include <signal.h>
# include "netdisk.h"

# define TRUE 1
# define FALSE 0

# define MAXBUFSIZE	65536	/* Maximum size for network comm. buffer */
# define MAXIO		4096	/* Max bytes sent at one time */

static FILE *Sin, *Sout;
static int Connected = FALSE;

static int Len = 0;		/* Hold length of last move for cli_bio_wait */

/*
 * Transmit and reply buffers and our current position in each
 */
static char	*Rply_buf = NULL;
static int	Rply_pos = 0;
static char	*Xmit_buf = NULL;
static int	Xmit_pos = 0;



/*
 * Prototypes
 */
char	*rpl_getstr (), *rpl_getbyt ();
void msg_addopc (int opc);
void msg_addstr (char *str);
void msg_addbyt (char *ptr, int num);
void ntd_msg_send (void);
void rpl_receive (void);
void tcp_read (char *buf, int len);
void tcp_write (char *buf, int len);



/*
 * Inline min (x, y)
 */
# define	MIN(x,y)	((x) > (y) ? (y) : (x))




int
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
		ui_error ("Unable to get netdisk service -- sorry");
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
	ui_printf ("\nConnecting to netdisk server..."); fflush (stdout);
 	status = connect (skt, (struct sockaddr *) &addr, sizeof (addr));
	if (status < 0)
	{
		signal (SIGALRM, oldsig);
		ui_error ("I am unable to connect to the netdisk server");
	}
	ui_printf ("done\n");
/*
 * Create a couple of FILE descriptors for this socket.
 */
	Sin = fdopen (skt, "r");
	skt = dup (skt);
	Sout = fdopen (skt, "w");
/*
 * Create the transmit and reply buffers
 */
	Xmit_buf = (char *) malloc (MAXBUFSIZE * sizeof (char));
	Rply_buf = (char *) malloc (MAXBUFSIZE * sizeof (char));
/*
 * Get the banner line.
 */
	rpl_receive ();
# ifdef notdef
	ui_printf ("'%s'\n", rpl_getstr ());
# endif
/*
 * Tell it who we are.
 */
	gethostname (hostname, 40);
	msg_addstr (hostname);
	msg_addstr (getenv ("USER"));
	ntd_msg_send ();
	rpl_receive ();
# ifdef notdef
	ui_printf ("'%s'\n", rpl_getstr ());
# endif
/*
 * All done!
 */
	Connected = TRUE;
	signal (SIGALRM, oldsig);
	return (TRUE);
}


int
cli_bio_open (file)
char *file;
{
	int		*fileid;

	netdisk_setup (file);
	msg_addopc (OP_BIO_OPEN);
	msg_addstr (&file[strcspn (file, ":") + 1]);
	ntd_msg_send ();
	rpl_receive ();
	fileid = (int *) rpl_getbyt (4);
	return (*fileid);
}



int
cli_bio_view (file)
char *file;
{
	int	*fileid;
		
	
	netdisk_setup (file);
	msg_addopc (OP_BIO_VIEW);
	msg_addstr (&file[strcspn (file, ":") + 1]);
	ntd_msg_send ();
	rpl_receive ();
	fileid = (int *) rpl_getbyt (4);
	return (*fileid);
}




int
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
	ntd_msg_send ();
	rpl_receive ();
	fileid = (int *) rpl_getbyt (4);
	return (*fileid);
}



int
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
	ntd_msg_send ();
	rpl_receive ();
	fileid = (int *) rpl_getbyt (4);
	return (*fileid);
}	




int
cli_bio_close (lun)
int lun;
{
	if (!Connected) return (FALSE);
	msg_addopc (OP_BIO_CLOSE);
	msg_addbyt ((char *) &lun, 4);
	ntd_msg_send ();
	return (TRUE);
}




int
cli_bio_read (lun, block, buffer, nbytes)
int lun, *block, *nbytes;
char *buffer;
{
	int		nread;

	if (!Connected) return (FALSE);
	msg_addopc (OP_BIO_READ);
	msg_addbyt ((char *) &lun, 4);
	msg_addbyt ((char *) block, 4);
	msg_addbyt ((char *) nbytes, 4);
	ntd_msg_send ();
	rpl_receive ();
	nread = *(int *) rpl_getbyt (4);
	if (nread > 0)
		memcpy (buffer, rpl_getbyt (nread), nread);
	Len = nread;
	return (nread);	
}




int
cli_bio_wait (lun)
int lun;
{
	return (Len);
}




int
cli_bio_write (lun, block, buffer, nbytes)
int lun, *block, *nbytes;
char *buffer;
{
	if (!Connected) return (FALSE);
	msg_addopc (OP_BIO_WRITE);
	msg_addbyt ((char *) &lun, 4);
	msg_addbyt ((char *) block, 4);
	msg_addbyt ((char *) nbytes, 4);
	msg_addbyt (buffer, *nbytes);
	ntd_msg_send ();
	rpl_receive ();
	Len = *(int *) rpl_getbyt (4);
	return (Len);		
}




int
cli_dcreate (file)
char *file;
{
	netdisk_setup (file);
	msg_addopc (OP_DCREATE);
	msg_addstr (&file[strcspn (file, ":") + 1]);
	ntd_msg_send ();
	rpl_receive ();
	return (*(int *) rpl_getbyt (4));
}




int
cli_dopen (file)
char *file;
{
	netdisk_setup (file);
	msg_addopc (OP_DOPEN);
	msg_addstr (&file[strcspn (file, ":") + 1]);
	ntd_msg_send ();
	rpl_receive ();
	return (*(int *) rpl_getbyt (4));
}




int
cli_dview (file)
char *file;
{
	netdisk_setup (file);
	msg_addopc (OP_DVIEW);
	msg_addstr (&file[strcspn (file, ":") + 1]);
	ntd_msg_send ();
	rpl_receive ();
	return (*(int *) rpl_getbyt (4));
}




int
cli_dappend (file)
char *file;
{
	netdisk_setup (file);
	msg_addopc (OP_DAPPEND);
	msg_addstr (&file[strcspn (file, ":") + 1]);
	ntd_msg_send ();
	rpl_receive ();
	return (*(int *) rpl_getbyt (4));
}



int
cli_dput (fnum, buf, len)
int fnum, len;
char *buf;
{
	if (!Connected) return (FALSE);
	msg_addopc (OP_DPUT);
	msg_addbyt ((char*)&fnum, 4);
	msg_addbyt ((char*)&len, 4);
	msg_addbyt (buf, len);
	ntd_msg_send ();
	rpl_receive ();
	return (*(int *) rpl_getbyt (4));
}




int
cli_dget (fnum, buf, max)
int fnum, max;
char *buf;
{
	int	nread;

	if (!Connected) return (FALSE);
	msg_addopc (OP_DGET);
	msg_addbyt ((char*)&fnum, 4);
	msg_addbyt ((char*)&max, 4);
	ntd_msg_send ();
	rpl_receive ();
	nread = *(int *) rpl_getbyt (4);
	if (nread > 0)
		memcpy (buf, rpl_getbyt (nread), nread);
	return (nread);
}




int
cli_drfa (fnum, rfa)
int fnum;
short *rfa;
{
	if (!Connected) return (FALSE);
	msg_addopc (OP_DRFA);
	msg_addbyt ((char*)&fnum, 4);
	ntd_msg_send ();
	rpl_receive ();
	memcpy (rfa, rpl_getbyt (6), 6);
	return (TRUE);
}




int
cli_dagain (fnum)
int fnum;
{
	if (!Connected) return (FALSE);
	msg_addopc (OP_DAGAIN);
	msg_addbyt ((char*)&fnum, 4);
	ntd_msg_send ();
	return (TRUE);
}




int
cli_dfind (fnum, rfa)
int fnum;
short rfa[3];
{
	if (!Connected) return (FALSE);
	msg_addopc (OP_DFIND);
	msg_addbyt ((char*)&fnum, 4);
	msg_addbyt ((char*)rfa, 6);
	ntd_msg_send ();
	return (TRUE);
}




int
cli_dclose (fnum)
int fnum;
{
	if (!Connected) return (FALSE);
	msg_addopc (OP_DCLOSE);
	msg_addbyt ((char*)&fnum, 4);
	ntd_msg_send ();
	return (TRUE);
}




int
cli_drewind (fnum)
int fnum;
{
	if (!Connected) return (FALSE);
	msg_addopc (OP_DREWIND);
	msg_addbyt ((char*)&fnum, 4);
	ntd_msg_send ();
	return (TRUE);
}




void
msg_addopc (opc)
int opc;
{
	char	temp = opc;

	msg_addbyt (&temp, 1);
}




void
msg_addstr (str)
char *str;
{
	msg_addbyt (str, strlen (str) + 1);
}




void
msg_addbyt (ptr, num)
char *ptr;
int num;
{
	memcpy (&Xmit_buf[Xmit_pos], ptr, num);
	Xmit_pos += num;
}




void
ntd_msg_send ()
{
	int	num, ntimes, nsend;
	double	ceil ();

	num = Xmit_pos;
	Xmit_pos = 0;
	ntimes = ceil ((double) num / (double) MAXIO);
	tcp_write ((char*)&ntimes, 4);
	while (ntimes--)
	{
		nsend = MIN (num, MAXIO);
		tcp_write ((char*)&nsend, 4);
		tcp_write (&Xmit_buf[Xmit_pos], nsend);
		Xmit_pos += nsend;
		num -= nsend;
	} 
	Xmit_pos = 0;
}





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




void
rpl_receive ()
{
	char	temp[4];
	int	ntimes, ngot;

	tcp_read ((char*)&ntimes, 4);
	Rply_pos = 0;
	while (ntimes--)
	{
		tcp_read ((char*)&ngot, 4);
		tcp_read (&Rply_buf[Rply_pos], ngot);
		Rply_pos += ngot;
	}
	Rply_pos = 0;
}	




void
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




void
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





/*
 * Logical unit stuff
 */

static long Lun_table[256][2];
static int Initialized = 0;

long
lun_type (long lun)
{
	if (lun == (long) stdin || lun == (long) stdout || lun == (long) stderr)
		return (LUN_LOCAL);
	return (Lun_table[lun - 3][0]);
}




long
lun_lookup (long lun)
{
	if (lun == (long) stdin || lun == (long) stdout || 
	    lun == (long) stderr)
		return (lun);
	return (Lun_table[lun - 3][1]);
}




int
lun_assign (long lun, long type)
{
	int	i;

	if (!Initialized)
	{
		memset ((char *) Lun_table, (char) LUN_FREE, 512);
		Initialized = 1;
	}
	for (i=0; i<255; i++)
		if (Lun_table[i][0] == LUN_FREE)
		{
			Lun_table[i][0] = type;
			Lun_table[i][1] = lun;
			return (i + 3);
		}
	perror ("Out of space in Lun_table!");
	return (0);
}




void
lun_deassign (long lun)
{
	if (lun == (long) stdin || lun == (long) stdout || lun == (long) stderr)
		return;
	Lun_table[lun - 3][0] = LUN_FREE;
}

# endif /* NETACCESS */
