/*
 * Read in Peter Coppin's position data for the Franklin during TOGA/COARE
 */
# include <stdio.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>



int MHandler FP ((Message *));
DataChunk *MakeDC FP ((PlatformId, FieldId));
/*
 * togast file yymmdd platform
 */

main (argc, argv)
int argc;
char **argv;
{
	char	line[80];
	FILE	*fp;
	date	uidate;
	ZebTime	zt;
	int	fracsec, nsamp;
	FieldId	fid;
	DataChunk	*dc;
	PlatformId	pid;
	Location	loc;
/*
 * Check.
 */
 	if (argc != 3)
	{
		fprintf (stderr, "Usage: %s <file> <platform>\n", argv[0]);
		exit (1);
	}
/*
 * Open the file.
 */
	if ((fp = fopen (argv[1], "r")) == NULL)
	{
		perror (argv[1]);
		exit (1);
	}
/*
 * Hook in.
 */
	usy_init ();
	msg_connect (MHandler, "franklin");
	ds_Initialize ();
	if ((pid = ds_LookupPlatform (argv[2])) == BadPlatform)
	{
		fprintf (stderr, "Unknown platform: %s\n", argv[2]);
		exit (1);
	}
	fid = F_Lookup ("bogus");
/*
 * Initial DC.
 */
	dc = MakeDC (pid, fid);
	nsamp = 0;
/*
 * Now we plow.
 */
	loc.l_alt = 0.0;

	while (fgets (line, 80, fp) != NULL)
	{
	/*
	 * Read the next line
	 */
		if (! strncmp (line, "0000", 4))
			break;

		if (sscanf (line, "%06d%06d.%4d\t%f\t%f", &uidate.ds_yymmdd, 
			    &uidate.ds_hhmmss, &fracsec, &loc.l_lat,
			    &loc.l_lon) != 5)
		{
			msg_ELog (EF_INFO, "Exiting on weird line: %s", line);
			break;
		}
	/*
	 * Convert to Zeb time and build the sample
	 */
		TC_UIToZt (&uidate, &zt);
		dc_AddScalar (dc, &zt, nsamp, fid, &loc.l_alt);
		dc_SetLoc (dc, nsamp, &loc);
	/*
	 * Perhaps store.
	 */
		if (++nsamp > 100)
		{
			ds_Store (dc, FALSE, 0, 0);
			dc_DestroyDC (dc);
			dc = MakeDC (pid, fid);
			nsamp = 0;
		}
	}

	if (nsamp)
		ds_Store (dc, FALSE, 0, 0);
	
	dc_DestroyDC (dc);
}






DataChunk *
MakeDC (pid, fid)
PlatformId pid;
FieldId fid;
/*
 * Make the DC.
 */
{
	DataChunk *dc = dc_CreateDC (DCC_Scalar);
	dc->dc_Platform = pid;
	dc_SetScalarFields (dc, 1, &fid);
	return (dc);
}



int 
MHandler (msg)
Message *msg;
{
}
