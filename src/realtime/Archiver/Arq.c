/*
 * For lack of a better name while in need of some quick verification.
 */

#include <stdio.h>
#include <time.h>

#include <defs.h>
#include <message.h>
#include <DataStore.h>

#include "Database.h"


static void
usage (char *arg0)
{
	printf ("usage: %s [-h] <database>\n", arg0);
	printf ("   where -h gives help, and\n");
	printf ("   <database> is the database file without the extension\n");
	printf ("examples:\n");
	printf ("   %s DumpDataBase\n", arg0);
	printf ("   %s DumpedFiles | grep spol | sort\n", arg0);
}


int
main (int argc, char *argv[])
{
	char *database = NULL;
	const char *key;
	DataFileCore dfc;
	int nf, i;

	for (i = 1; i < argc; ++i)
	{
		if (! strcmp (argv[i], "-h"))
		{
			usage (argv[0]);
			exit (0);
		}
		else if (! database)
		{
			database = argv[i];
		}
		else
		{
			usage (argv[0]);
			exit (1);
		}
	}

	msg_connect (NULL, argv[0]);

	database = argv[1];
	if (db_Read (database) != 0)
	{
		msg_ELog (EF_PROBLEM, "could not open database '%s'",
			  database);
		exit (1);
	}

	nf = 0;
	for (key = db_First (&dfc); key; key = db_Next (&dfc))
	{
	    char revdate[32];
	    
	    ++nf;
	    strftime (revdate, sizeof (revdate), "%d-%b-%Y,%T", 
		      gmtime (&dfc.dfc_rev));
	    printf ("%-55s %-20s\n", key, revdate);
	}
	fprintf (stderr, "---\n%d records.\n", nf);
	db_Close ();
	exit (0);
}
