/*
 * Exercise the "ping" feature.
 */
static char *rcsid = "$Id: msg_ping.c,v 2.0 1991-07-18 23:15:57 corbet Exp $";

# include "../include/defs.h"
# include "message.h"


# ifdef __STDC__
	int	Handler (Message *);
# else
	int	Handler ();
# endif

int NSent = 0, NGot = 0;

main (argc, argv)
int argc;
char **argv;
{
	int i;
/*
 * Hook in.
 */
	if (! msg_connect (Handler, "Mad Pinger"))
		exit (1);
/*
 * Go through and do it.
 */
	if (argc < 2)
		Ping (0);
	else
		for (i = 1; i < argc; i++)
			Ping (argv[i]);
	msg_await ();
}





Ping (host)
char *host;
/*
 * Ping this host.
 */
{
	char to[60];
	int proto = MT_PING;
/*
 * Figure out who this is going to.
 */
	if (host)
	{
		if (strchr (host, '@'))
		{
			strcpy (to, host);
			proto = MT_CPING;
		}
		else
			sprintf (to, "%s@%s", MSG_MGR_NAME, host);
	}
	else
		strcpy (to, MSG_MGR_NAME);
/*
 * Send it.
 */
	msg_send (to, proto, FALSE, to, 0);
	NSent++;
}




int
Handler (msg)
Message *msg;
/*
 * Deal with responses.
 */
{
	if (msg->m_proto != MT_PING)
	{
		printf ("Strange response protocol %d.\n", msg->m_proto);
		exit (1);
	}
	printf ("Response from %s\n", msg->m_from);
	if (++NGot >= NSent)
		exit (0);
	return (0);
}
