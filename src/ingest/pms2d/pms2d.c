/*
 * Zeb ingest for NCAR Electra 2-d probe data as processed by Robert
 * Black at HOAA/AOML/HRD.   See pms2d.doc for a description of the data
 * files.
 *
 * pms2d <in_file> <platform>
 */
# include <stdio.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>

# define WSAMP	20	/* Do a write every WSAMP samples */

/*
 * The header record.  See the documentation or the creation of the field
 * list in the code below for info on what's in probedata.
 */
typedef struct _Header
{
	float	date;		/* float YYMMDD */
	float	time;		/* SS.MSEC	*/
	float	attenuation;	/* 5.5 cm radar attenuation (dBZ/km)	*/
	float	avg_time;	/* averaging time (sec)	*/
	float	tas;		/* true airspeed	*/
	float	rec_count;	/* # recs this sample (includes header)	*/
	float	probedata[26];	/* 2d precip and cloud probe data	*/
} Header;

/*
 * Prototypes
 */
int MHandler FP ((Message *));
DataChunk *MakeDC FP ((PlatformId, FieldId *, int));




main (argc, argv)
int argc;
char **argv;
{
	FILE	*fp;
	Header	hdr;
	float	rec[32];
	char	ourname[40];
	int	yymmdd, year, month, day, sec, usec, nsamp, fld, i;
	ZebTime	zt;
	FieldId fids[29];
	FieldId	f_atten, f_avg_time, f_tas;
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
	sprintf (ourname, "PMS2d_%x", getpid ());
	
	usy_init ();
	msg_connect (MHandler, ourname);
	ds_Initialize ();
	if ((pid = ds_LookupPlatform (argv[2])) == BadPlatform)
	{
		fprintf (stderr, "Unknown platform: %s\n", argv[2]);
		exit (1);
	}
/*
 * Build the ugly fixed field list
 */
	fids[0] = F_Lookup ("2dp_lwc");
	fids[1] = F_Lookup ("2dp_lwc_particles");
	fids[2] = F_Lookup ("2dp_rainr");
	fids[3] = F_Lookup ("2dp_iwc");
	fids[4] = F_Lookup ("2dp_iwc_particles");
	fids[5] = F_Lookup ("2dp_reflectivity");
	fids[6] = F_Lookup ("2dp_lw_intercept");
	fids[7] = F_Lookup ("2dp_lw_slope");
	fids[8] = F_Lookup ("2dp_iw_intercept");
	fids[9] = F_Lookup ("2dp_iw_slope");
	fids[10] = F_Lookup ("2dp_mvd");
	fids[11] = F_Lookup ("2dp_ol_time");
	fids[12] = F_Lookup ("2dp_samp_volume");

	fids[13] = F_Lookup ("2dc_lwc");
	fids[14] = F_Lookup ("2dc_lwc_particles");
	fids[15] = F_Lookup ("2dc_rainr");
	fids[16] = F_Lookup ("2dc_iwc");
	fids[17] = F_Lookup ("2dc_iwc_particles");
	fids[18] = F_Lookup ("2dc_reflectivity");
	fids[19] = F_Lookup ("2dc_lw_intercept");
	fids[20] = F_Lookup ("2dc_lw_slope");
	fids[21] = F_Lookup ("2dc_iw_intercept");
	fids[22] = F_Lookup ("2dc_iw_slope");
	fids[23] = F_Lookup ("2dc_mvd");
	fids[24] = F_Lookup ("2dc_ol_time");
	fids[25] = F_Lookup ("2dc_samp_volume");

	fids[26] = F_Lookup ("attenuation");
	fids[27] = F_Lookup ("avg_time");
	fids[28] = F_Lookup ("tas");
/*
 * Initial DC.
 */
	dc = MakeDC (pid, fids, 29);
	nsamp = 0;
/*
 * Now we plow.
 */
	while (fread ((char *) &hdr, sizeof (hdr), 1, fp))
	{
	/*
	 * Get the yymmdd portion of the time
	 */
		yymmdd = (int) hdr.date;
		if (yymmdd < 0)
			break;
		
		year = yymmdd / 10000;
		month = (yymmdd / 100) % 100;
		day = yymmdd % 100;

		TC_ZtAssemble (&zt, year, month, day, 0, 0, 0, 0);
	/*
	 * Now add in the seconds portion
	 */
		sec = (int) hdr.time;
		if (sec < 0)
			break;

		usec = (int)((hdr.time - sec) * 1000000);

		zt.zt_Sec += sec;
		zt.zt_MicroSec = usec;
	/*
	 * Stuff the probe data into the data chunk.
	 */
		for (fld = 0; fld < 26; fld++)
			dc_AddScalar (dc, &zt, nsamp % WSAMP, fids[fld], 
				      (void *)(hdr.probedata + fld));
	/*
	 * Stuff the three shared fields into the data chunk.
	 */
		dc_AddScalar (dc, &zt, nsamp % WSAMP, fids[26], 
			      (void *)&hdr.attenuation);
		dc_AddScalar (dc, &zt, nsamp % WSAMP, fids[27], 
			      (void *)&hdr.avg_time);
		dc_AddScalar (dc, &zt, nsamp % WSAMP, fids[28], 
			      (void *)&hdr.tas);
	/*
	 * Perhaps store.
	 */
		if ((++nsamp % WSAMP) == 0)
		{
			ds_Store (dc, (nsamp == WSAMP), NULL, 0);
			dc_DestroyDC (dc);
			dc = MakeDC (pid, fids, 29);
		}
	/*
	 * Read and ignore (for now?) the spectrum data
	 */
		for (i = 0; i < (int)(hdr.rec_count - 1); i++)
			fread (rec, sizeof (rec), 1, fp);
	}

	if (nsamp % WSAMP)
		ds_Store (dc, FALSE, 0, 0);
	
	dc_DestroyDC (dc);
}






DataChunk *
MakeDC (pid, fids, fcount)
PlatformId	pid;
FieldId	*fids;
int	fcount;
/*
 * Make the DC.
 */
{
	DataChunk *dc = dc_CreateDC (DCC_Scalar);
	dc->dc_Platform = pid;
	dc_SetScalarFields (dc, fcount, fids);
	dc_SetBadval (dc, -999.0);
	return (dc);
}



int 
MHandler (msg)
Message *msg;
{
}
