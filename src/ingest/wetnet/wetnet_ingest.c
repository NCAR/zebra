/* 
 * Inget data from a DAP format file.
 */
# include <errno.h>
# include <stdio.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <config.h>

static int	Die FP ((void));


main( argc, argv )
int argc;
char **argv;
{
	char	ourname[20], line[80];
	int	month, day, year, hour, minute, second, numfields, nlines = 0;
	float	lat, lon, amplitude, multiplicity;
	ZebTime	zt, prevtime;
	FILE	*infile;
	PlatformId	pid;
	FieldId		fids[10];
	Location	loc;
	DataChunk	*dc;
/*
 * Sanity check.
 */
	if (argc != 3)
	{
		fprintf (stderr, "Usage: %s <platform> <file>\n", argv[0]);
		exit (1);
	}
/*
 * Hook in.
 */
	sprintf (ourname, "wetnet_%x", getpid ());
	usy_init ();
	msg_connect (Die, ourname);
	ds_Initialize ();
	msg_ELog (EF_INFO, "Ingesting file '%s'", argv[2]);
/*
 * Check the platform.
 */
	if ((pid = ds_LookupPlatform (argv[1])) == BadPlatform)
	{
		msg_ELog (EF_EMERGENCY, "Bad platform %s", argv[1]);
		exit (1);
	}
/*
 * Set up fields.
 */
	fids[0] = F_Lookup ("amplitude");
	fids[1] = F_Lookup ("multiplicity");
	numfields = 2;
/*
 * Open the data file.
 */
	if (! (infile = fopen (argv[2], "r")))
	{
		msg_ELog (EF_EMERGENCY, "Error %d opening '%s'", errno, 
			  argv[2]);
		exit (1);
	}
/*
 * Plow.
 */
	while (fgets (line, sizeof (line), infile)) 
	{
		if ((++nlines % 1000) == 0)
			msg_ELog (EF_DEBUG, "Line %d", nlines);
		

		sscanf (line, "%d/%d/%d %d:%d:%d %f %f %f %f", &month, &day,
			&year, &hour, &minute, &second, &lat, &lon, &amplitude,
			&multiplicity);
	/*
	 * Create a data chunk.
	 */
		dc = dc_CreateDC (DCC_Scalar);
		dc->dc_Platform = pid;
		dc_SetScalarFields (dc, numfields, fids);
	/*
	 * Extract a time.
	 */
		TC_ZtAssemble (&zt, year, month, day, hour, minute, second, 0);
		if (zt.zt_Sec == prevtime.zt_Sec)
			zt.zt_MicroSec = prevtime.zt_MicroSec + 1;
		prevtime = zt;
	/*
	 * Extract a location.
	 */ 
		loc.l_lat = lat;
		loc.l_lon = lon;
		loc.l_alt = 0;
	/*
	 * Stuff the fields and location into the data chunk.
	 */
		dc_AddScalar (dc, &zt, 0, fids[0], &amplitude);
		dc_AddScalar (dc, &zt, 0, fids[1], &multiplicity);
		dc_SetLoc (dc, 0, &loc);
	/*
	 * Store the data.
	 */
		ds_StoreBlocks (dc, FALSE, NULL, 0);
		dc_DestroyDC (dc);
	}
/*
 * Finish up.
 */
	fclose (infile);
	Die ();
}

static int
Die ()
{
	msg_ELog (EF_INFO, "Exiting.");
	exit (0);
}

