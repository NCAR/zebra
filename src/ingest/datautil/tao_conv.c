/*
 * Hack funky pmel-format tao files into the data store.
 */

# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <netcdf.h>


/*
 * Grungy hardcoded list of stations.
 */
static char *Bouys[] =
{
	"0n125w",	"0n110w",	"0n140w",	"0n143e",
	"0n154e",	"0n155w",	"0n156e",	"0n158e",
	"0n160e",	"0n170w",
	"0n165e",	"0n170e",	"0n180w",	"0n80e",
	"0n95w",	"2n110w",	"2n125w",	"2n137e",
	"2n140w",	"2n147e",	"2n155w",	"2n156e",
	"2n165e",	"2n170w",	"2n180w",
	"2n95w",	"2s110w",	"2s125w",	"2s140w",
	"2s155w",	"2s156e",	"2s165e",	"2s170w",
	"2s180w",	"2s95w",	"5n110w",	"5n125w",
	"5n137e",	"5n140w",	"5n147e",	"5n155w",
	"5n156e",	"5n165e",	"5n170w",	"5n180w",
	"5s110w",	"5s125w",	"5s140w",	"5s155w",
	"5s156e",	"5s165e",	"5s170w",	"5s180w",
	"8n110w",	"8n125w",	"8n155w",	"8n165e",
	"8n170w",	"8s110w",	"8s125w",	"8s155w",
	"8s165e",	"8s170w",	"9n140w",
};

# define NBOUY (sizeof (Bouys)/sizeof (Bouys[0]))
PlatformId MainPlat, SubPlats[NBOUY];
Location BouyLocs[NBOUY];

/*
 * Info describing the data as it's coming in.
 */
int NTime;	/* How many samples we have. */
ZebTime *Times;	/* Time array		*/

/*
 * Stuff for the current netcdf source.
 */
int NCid;
long *TaoTime, *TaoTime2;	/* Tao funky julian times	*/
int NFTime;
int VTime, VTime2, VLat, VLon, DTime;	/* NetCDF vars, dims */


/*
 * Field info.
 */
struct FPair
{
	char *TaoName;		/* Name in the tao file		*/
	char *ZebName;		/* Name that we prefer		*/
};

static struct FPair WPairs[] =
{
	{ "WU_422",	"u_wind" },
	{ "WV_423",	"v_wind" },
	{ "WS_401",	"wspd" },
	{ "WD_410",	"wdir" },
	{ "RH_910",	"rh" }
};
# define NWFIELD (sizeof(WPairs)/sizeof(WPairs[0]))

static struct FPair TPairs[] =
{
	{ "T_20",	"sst" },
	/* { "P_1",	"pres" }, */
	{ "AT_21",	"tdry" },
};
# define NTFIELD (sizeof(TPairs)/sizeof(TPairs[0]))


# define MaxField 20
FieldId Fids[MaxField];
float *Data[MaxField];

/*
 * Forwards.
 */
int MHandler ();
void LoadFirst FP ((char *));
void CloseTaoFile FP ((void));
void ConvertTime FP ((ZebTime *, unsigned long, unsigned long));
void LoadStation FP ((char *, int));
void InitArray FP ((float *));


main ()
/*
 * Plow through a shitload of data.
 */
{
	int fld, station;
	DataChunk *dc;
/*
 * Hook in.
 */
	usy_init ();
	msg_connect (MHandler, "tao_conv");
	ds_Initialize ();
/*
 * Load up the initial station.
 */
	LoadFirst (Bouys[0]);
/*
 * Set up the data space.
 */
	for (fld = 0; fld < NWFIELD; fld++)
	{
		Fids[fld] = F_Lookup (WPairs[fld].ZebName);
		Data[fld] = (float *) malloc (NTime*NBOUY*sizeof (float));
		InitArray (Data[fld]);
	}
	for (fld = 0; fld < NTFIELD; fld++)
	{
		Fids[fld+NWFIELD] = F_Lookup (TPairs[fld].ZebName);
		Data[fld+NWFIELD] = (float *)
			malloc (NTime*NBOUY*sizeof (float));
		InitArray (Data[fld+NWFIELD]);
	}
/*
 * Look up our platforms.
 */
	if ((MainPlat = ds_LookupPlatform ("tao")) == BadPlatform)
	{
		printf ("Can't find tao platform!\n");
		exit (1);
	}
	for (station = 0; station < NBOUY; station++)
	{
		char fullname[20];
		sprintf (fullname, "tao/%s", Bouys[station]);
		if ((SubPlats[station] = ds_LookupPlatform (fullname)) ==
		    BadPlatform)
		{
			printf ("Subplat %s missing\n", fullname);
			exit (1);
		}
	}
/*
 * Initialize the data chunk.
 */
	dc = dc_CreateDC (DCC_IRGrid);
	dc->dc_Platform = MainPlat;
/*
 * Plow through the stations and load up the data.
 */
	for (station = 0; station < NBOUY; station++)
		LoadStation (Bouys[station], station);
/*
 * Assemble the info into the data chunk.
 */
	dc_IRSetup (dc, NBOUY, SubPlats, BouyLocs, NWFIELD + NTFIELD, Fids);
	dc_SetBadval (dc, 1e35);	/* What they seem to use */
	for (fld = 0; fld < NWFIELD; fld++)
		dc_IRAddMultGrid (dc, Times, 0, NTime, Fids[fld], Data[fld]);
	for (fld = 0; fld < NTFIELD; fld++)
		dc_IRAddMultGrid (dc, Times, 0, NTime, Fids[fld + NWFIELD],
				  Data[fld + NWFIELD]);
/*
 * Store the bastard.
 */
	ds_Store (dc, TRUE, 0, 0);
}





void
LoadFirst (name)
char *name;
/*
 * Pull in the first bouy and use it to initialize the time array.
 */
{
	int i;
/*
 * Get the file open.
 */
	if (! OpenTaoFile ("w", name))
		exit (1);
/*
 * Go through and convert all the times.
 */
	NTime = NFTime;
	Times = (ZebTime *) malloc (NTime * sizeof (ZebTime));
	for (i = 0; i < NTime; i++)
		ConvertTime (Times + i, TaoTime[i], TaoTime2[i]);
/*
 * Close.
 */
	CloseTaoFile ();
}



int
OpenTaoFile (prefix, name)
char *prefix, *name;
/*
 * Open up this file.
 */
{
	char fname[80];
	int zero = 0;
/*
 * Generate the file name and open it up.
 */
	sprintf (fname, "%s%s.cdf", prefix, name);
	if ((NCid = ncopen (fname, NC_NOWRITE)) < 0)
	{
		printf ("Can't open %s\n", fname);
		return (0);
	}
/*
 * Start finding variables and dimensons.
 */
	VTime = ncvarid (NCid, "time");
	VTime2 = ncvarid (NCid, "time2");
	VLat = ncvarid (NCid, "lat");
	VLon = ncvarid (NCid, "lon");
	DTime = ncdimid (NCid, "time");
	ncdiminq (NCid, DTime, 0, (long *) &NFTime);
/*
 * Allocate the times and read them in.
 */
	TaoTime = (long *) malloc (NFTime * sizeof (long));
	TaoTime2 = (long *) malloc (NFTime * sizeof (long));
	ncvarget (NCid, VTime, (long *) &zero, (long *) &NFTime, TaoTime);
	ncvarget (NCid, VTime2, (long *) &zero, (long *) &NFTime, TaoTime2);
}





void
CloseTaoFile ()
/*
 * Close out an open file.
 */
{
	ncclose (NCid);
	free (TaoTime);
	free (TaoTime2);
}






void
ConvertTime (zt, jday, mstime)
	ZebTime *zt;		/* where to store ZebTime */
	unsigned long jday, mstime;	/* Julian day to convert  */
/*
 * Convert Julian day to ZebTime
 *
 * Julian day - 2448622 = day of year 1992 (1..366)
 * Julian day - 2448988 = day of year 1993 (1..365)
 * so Julian day 2448989 is Jan 1, 1993
 */
{
	static ZebTime base = { 0, 0 };

	if (!base.zt_Sec)
		TC_ZtAssemble (&base, 93, 1, 1, 0, 0, 0, 0);
	zt->zt_Sec = base.zt_Sec + ((jday - 2448989) * 24 * 60 * 60);
	zt->zt_Sec += mstime/1000;
	zt->zt_MicroSec = 0;
}




void
LoadStation (name, offset)
char *name;
int offset;
/*
 * Load up this station.
 */
{
	int fld, t, vars[MaxField], ndrop = 0;
	long zero = 0;
/*
 * Start with the W file.
 */
	printf ("Processing %s...", name);
	fflush (stdout);
	if (! OpenTaoFile ("w", name))
		exit (1);
	printf ("w "); fflush (stdout);
/*
 * Map out the fields.
 */
	for (fld = 0; fld < NWFIELD; fld++)
		if ((vars[fld] = ncvarid (NCid, WPairs[fld].TaoName)) < 0)
		{
			printf ("%s lacks  %s\n", name, WPairs[fld].TaoName);
			return;
		}
/*
 * Get the location.
 */
	ncvarget1 (NCid, VLat, &zero, &BouyLocs[offset].l_lat);
	ncvarget1 (NCid, VLon, &zero, &BouyLocs[offset].l_lon);
	BouyLocs[offset].l_lon *= -1;	/* Seems backward in file?!? */
	BouyLocs[offset].l_alt = 0.0;
/*
 * Now plow through it all.
 */
	for (t = 0; t < NFTime; t++)
	{
		int dest;
		long coords[4];
	/*
	 * Find the time in our data array.
	 */
		if ((dest = FindSlot (TaoTime[t], TaoTime2[t])) < 0)
		{
			ndrop++;
			continue;
		}
	/*
	 * Read in each field and stash it appropriately.
	 */
		coords[0] = t;
		coords[1] = coords[2] = coords[3] = 0;
		for (fld = 0; fld < NWFIELD; fld++)
			ncvarget1 (NCid, vars[fld], coords,
				   Data[fld] + offset + dest*NBOUY);
	}
	printf ("[%d/%d] ", ndrop, NFTime);
	CloseTaoFile ();
/*
 * Similar thing with the t file.
 */
	if (! OpenTaoFile ("t", name))
		exit (1);
	printf ("t "); fflush (stdout);
/*
 * Map out the fields.
 */
	for (fld = 0; fld < NTFIELD; fld++)
		if ((vars[fld] = ncvarid (NCid, TPairs[fld].TaoName)) < 0)
		{
			printf ("%s lacks  %s\n", name, TPairs[fld].TaoName);
			return;
		}
/*
 * Now plow through it all.
 */
	ndrop = 0;
	for (t = 0; t < NFTime; t++)
	{
		int dest;
		long coords[4];
	/*
	 * Find the time in our data array.
	 */
		if ((dest = FindSlot (TaoTime[t], TaoTime2[t])) < 0)
		{
			ndrop++;
			continue;
		}
	/*
	 * Read in each field and stash it appropriately.
	 */
		coords[0] = t;
		coords[1] = coords[2] = coords[3] = 0;
		for (fld = 0; fld < NTFIELD; fld++)
			ncvarget1 (NCid, vars[fld], coords,
				   Data[fld+NWFIELD] + offset + dest*NBOUY);
	}
	printf ("[%d/%d]\n", ndrop, NFTime);
	CloseTaoFile ();
}





int FindSlot (t, t2)
/*
 * Find the place to put this time.
 */
{
	int top = NTime - 1, bottom = 0;
	ZebTime zt;

	ConvertTime (&zt, t, t2);

	while (top > bottom + 1)
	{
		int mid = (top + bottom)/2;
		long toff = (long) Times[mid].zt_Sec;
		if (toff == zt.zt_Sec) /* Might as well try */
			return (mid);
		else if (toff < zt.zt_Sec)
			bottom = mid;
		else
			top = mid;
	}
	return (-1);
}


MHandler () {}





void
InitArray (array)
float *array;
/*
 * Stuff in lots of bad value flags.
 */
{
	const int max = NTime*NBOUY;
	register int i;

	for (i = 0; i < max; i++)
		*array++ = 1e35;
}
