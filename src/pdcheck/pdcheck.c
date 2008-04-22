/*
 * Simple interface to pd the library for checking syntax of pd files
 */

#include <stdio.h>
#include <string.h>

#include <config.h>
#include <defs.h>
#include <message.h>
#include <pd.h>

void usy_init();

RCSID ("$Id: pdcheck.c,v 1.3 1995-06-29 22:37:41 granger Exp $")

int
main (argc, argv)
int argc;
char *argv[];
{
	int i;
	int debug = 0;
	int err = 0;
	int test = 0;

	if (argc < 2)
	{
		printf ("usage: %s [-debug] [-test] pdfile [pdfile ...]\n", 
			argv[0]);
		exit (1);
	}

	usy_init ();
	msg_connect (0, "pdcheck");
	msg_ELPrintMask (EF_ALL & ~EF_DEBUG);

	/*
	 * For each file name on the command line, try to read it into
	 * a plot description.  All messages are sent to the terminal.
	 */
	for (i = 1; i < argc; ++i)
	{
		plot_description pd;

		if (! strcmp(argv[i], "-debug"))
		{
			msg_ELPrintMask (EF_ALL);
			debug = 1;
			continue;
		}
		else if (! strcmp(argv[i], "-test"))
		{
			msg_ELPrintMask (EF_PROBLEM | EF_EMERGENCY);
			test = 1;
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
		if (debug || test)
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


