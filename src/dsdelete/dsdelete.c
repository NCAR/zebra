/*
 * Data store file deletion utility.  Be careful out there.
 */
static char *rcsid = "$Id: dsdelete.c,v 2.0 1991-07-18 22:53:23 corbet Exp $";

# include "../include/defs.h"
# include "../include/message.h"
# include "DataStore.h"


extern char *getenv (char *);


main (argc, argv)
int argc;
char **argv;
{
	PlatformId plat;
	int leave;
/*
 * Check args.
 */
	if (argc != 3)
	{
		printf ("Usage: %s platform leave-seconds\n", argv[0]);
		exit (1);
	}
/*
 * Get initialized.
 */
	usy_init ();
	msg_connect (0, getenv ("USER"));
	ds_Initialize ();
/*
 * Figure out the params, then do the dirty work.
 */
	if ((plat = ds_LookupPlatform (argv[1])) == BadPlatform)
	{
		printf ("Unknown platform: '%s'\n", argv[1]);
		exit (1);
	}
	leave = atoi (argv[2]);
	ds_DeleteData (plat, leave);
	exit (0);
}
