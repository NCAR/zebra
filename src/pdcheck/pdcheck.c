/*
 * Simple interface to pd the library for checking syntax of pd files
 */

#include <stdio.h>

#include <config.h>
#include <defs.h>
#include <message.h>
#include <pd.h>

RCSID ("$Id: pdcheck.c,v 1.1 1995-04-18 22:30:25 granger Exp $")

#ifdef notdef
void uw_GetFText () {};
#endif

int
main (argc, argv)
int argc;
char *argv[];
{
	int i;
	int debug = 0;
	int err = 0;

	if (argc < 2)
	{
		printf ("usage: %s [-debug] pdfile [pdfile ...]\n", argv[0]);
		exit (1);
	}

	usy_init ();
	msg_connect (0, "pdcheck");
	msg_ELPrintMask (EF_ALL);

	/*
	 * For each file name on the command line, try to read it into
	 * a plot description.  All messages are sent to the terminal.
	 */
	for (i = 1; i < argc; ++i)
	{
		plot_description pd;

		if (! strcmp(argv[i], "-debug"))
		{
			debug = 1;
			continue;
		}
		msg_ELog (EF_INFO, "%s", argv[i]);
		pd = pd_Read (argv[i]);
		if (! pd)
		{
			msg_ELog (EF_PROBLEM, "read of '%s' failed", argv[i]);
			++err;
			continue;
		}
		if (debug)
		{
			raw_plot_description *rpd;
			char *data;
			/*
			 * Bite the bullet and try to parse the pd without
			 * a null terminator
			 */
			rpd = pd_Unload (pd);
			pd_Release (pd);
			msg_ELog (EF_DEBUG, "realloc'ing and reloading");
			data = (char *) malloc (rpd->rp_len);
			memcpy (data, rpd->rp_data, rpd->rp_len);
			free (rpd->rp_data);
			rpd->rp_data = data;
			pd = pd_Load (rpd);
			pd_RPDRelease (rpd);
		}
		pd_Release (pd);
	}
	return (err);
}


