/*
 * Read in position data for the Wecoma during TOGA/COARE
 */
# include <stdio.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>


# define WSAMP	100	/* Do a write every WSAMP samples */

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
	float	fjday;
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
	msg_connect (MHandler, "wecoma");
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
 * Now we plow, reading lines of floating point julian date, lat, and lon
 */
	loc.l_alt = 0.0;

	while (fscanf (fp, "%f%f%f", &fjday, &loc.l_lat, &loc.l_lon) == 3)
	{
	/*
	 * We aren't given the year, so we have to infer it.  TOGA/COARE
	 * IOPs ran from 11/92 to 2/93.  Start with a time of 00:00 on 1 Jan
	 * of the appropriate year.
	 */
		if (fjday > 182.0)
			TC_ZtAssemble (&zt, 1992, 1, 0, 0, 0, 0, 0);
		else
			TC_ZtAssemble (&zt, 1993, 1, 0, 0, 0, 0, 0);
	/*
	 * Add in the julian days (86400 seconds/day)
	 */
		zt.zt_Sec += (int)(86400 * fjday);
	/*
	 * Convert to Zeb time and build the sample
	 */
		dc_AddScalar (dc, &zt, nsamp % WSAMP, fid, &loc.l_alt);
		dc_SetLoc (dc, nsamp % WSAMP, &loc);
	/*
	 * Perhaps store.
	 */
		if ((++nsamp % WSAMP) == 0)
		{
			ds_Store (dc, (nsamp == 1), 0, 0);
			dc_DestroyDC (dc);
			dc = MakeDC (pid, fid);
		}
	}

	if ((nsamp % WSAMP))
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
