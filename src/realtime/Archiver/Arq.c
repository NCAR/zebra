/*
 * For lack of a better name while in need of some quick verification.
 */

#include <stdio.h>

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
	DataFileInfo dfi;
	ZebraTime zt;
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

	key = db_First (&dfi, &zt);
	nf = 0;
	while (key)
	{
		++nf;
		printf ("%-55s %-20s\n", key, TC_AscTime (&zt, TC_Full));
		key = db_Next (&dfi, &zt);
	}
	fprintf (stderr, "---\n%d records.\n", nf);
	db_Close ();
	exit (0);
}


