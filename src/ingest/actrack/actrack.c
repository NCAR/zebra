/* $Id: actrack.c,v 1.3 2001-01-08 17:17:33 granger Exp $ */

/*
 * Ingest track locations from the command line.
 *
 * Large parts lifted from uav_ingest.
 */


#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>

#include <defs.h>
#include <ingest.h>

#define MatchAbbrev(arg,opt) \
        ((strlen(arg) >= 2) && (!strncmp((arg),(opt),strlen(arg))))

#define INGEST_NAME	"track"
#define BADVALUE	-99999.0

MAKE_RCSID(
   "$Id: actrack.c,v 1.3 2001-01-08 17:17:33 granger Exp $")


struct Options
{
    char *vor;
    float azimuth;
    float range;
    int input;		/* take argument lines from standar input */
};


struct field {
	char *name;
	char *units;
	char *longname;
	FieldId id;
};


int Input (int argc, char *argv[], struct Options *opt);


static struct field *
InitFields(nfield)
int *nfield;
{
	static int inited = FALSE;
	static struct field Fields[] = 
	{
		{ "y2k",   "seconds", "Seconds since 2000" }
	};
	static const int NFields = ((sizeof(Fields))/sizeof(Fields[0]));
	struct field *f;

	if (inited)
		return;

	for (f = Fields; f - Fields < NFields; ++f)
		f->id = F_DeclareField (f->name, f->longname, f->units);
	*nfield = NFields;
	return ((struct field *)Fields);
}


static void
Usage (char *prog)
{
    printf ("Usage: %s <platform> <lat> <lon> <alt> [time]\n",prog);
    printf ("   %s -help\n",prog);
    printf ("   %s -input\n",prog);
    printf ("        Read data from stdin in command-line format.\n");
    printf ("   %s <platform> -vor <name> <azimuth> <range> <alt> [time]\n",
	    prog);
    printf ("        Enter vor-relative coordinates instead of lat/lon\n");
    printf ("        See vor -help for units\n");
    printf ("Where the default field units are as follows:\n");
    printf ("   lat      degrees North\n");
    printf ("   lon      degrees East\n");
    printf ("   alt      kilometers above Mean Sea Level\n");
    printf ("   time     if given as one numer: seconds since Unix epoch\n");
    printf ("            if form hh:mm:[ss], relative to current time\n");
    IngestUsage();
}



/* ParseCommandLineOptions --------------------------------------------
 *    Set global variables from command-line options, leaving only
 *    the expected file and field names in the arg list
 */
static void
ParseCommandLineOptions(int *argc, char *argv[], struct Options *opt)
{
	int i, j;
	opt->vor = 0;
	opt->input = 0;
/*
 * First parse any of the general ingest options
 */
	IngestParseOptions(argc, argv, Usage);
/*
 * Now check for any of our own flags on the command line
 */
	i = 1;
	while (i < *argc)
	{
		if (MatchAbbrev(argv[i],"-vor"))
		{
		    if (i + 3 >= *argc)
		    {
			Usage (argv[0]);
		    }
		    opt->vor = argv[i+1];
		    opt->azimuth = atof(argv[i+2]);
		    opt->range = atof(argv[i+3]);
		    IngestRemoveOptions(argc, argv, i, 4);
		}
		else if (MatchAbbrev(argv[i],"-input"))
		{
		    opt->input = 1;
		    IngestRemoveOptions(argc, argv, i, 1);
		}
#ifdef notdef
		else if (MatchAbbrev(argv[i],"-file"))
		{
		    if (i + 1 >= *argc)
		    {
			Usage (argv[0]);
		    }
		    opt->file = argv[i+1];
		    IngestRemoveOptions(argc, argv, i, 2);
		}
#endif
		else
		    ++i;
	}
}



#ifdef notdef
static int
message (msg)
struct message *msg;
/*
 * More than likely we're being told to kill ourselves
 */
{
	struct mh_template *mh = (struct mh_template *)msg->m_data;

	switch (msg->m_proto)
	{
	   case MT_MESSAGE:
		if (mh->mh_type == MH_DIE)
		{
			Finish (GlobalSource);
			exit (0);
		}
	}
	IngestLog (EF_PROBLEM, "errant message ignored");
	return (0);
}
#endif


void
PutTrack (PlatformId pid, ZebTime *zt, float lat, float lon, float alt)
{
    DataChunk *dc;
    Location loc;
    int nfield, i;
    FieldId fids[DC_MaxField];
    struct field *f;
    ZebTime y2k;
    float y2ksince;

    TC_ZtAssemble (&y2k, 2000, 1, 1, 0, 0, 0, 0);
    if (! (dc = dc_CreateDC (DCC_Scalar)))
    {
	IngestLog (EF_EMERGENCY, 
		   "could not create datachunk; no data stored");
	exit (4);
    }
    dc->dc_Platform = pid;
    f = InitFields (&nfield);
    for (i = 0; i < nfield; ++i)
	fids[i] = f[i].id;
    dc_SetScalarFields (dc, nfield, fids);
    dc_SetBadval (dc, BADVALUE);
    loc.l_lat = lat;
    loc.l_lon = lon;
    loc.l_alt = alt;
    dc_SetLocAltUnits (dc, AU_kmMSL);
    y2ksince = zt->zt_Sec - y2k.zt_Sec;
    dc_AddScalar (dc, zt, 0, f[0].id, &y2ksince);
    dc_SetLoc (dc, 0, &loc);
    IngestLog (EF_DEVELOP, "platform: %s; lat: %g; lon: %g; alt: %g; y2k: %g",
	       ds_PlatformName(pid), lat, lon, alt, y2ksince);
    ds_StoreBlocks (dc, FALSE, NULL, 0);
    dc_DestroyDC (dc);
}



int
main (int argc, char *argv[])
{
    struct Options options;
    char buf[512];

    ParseCommandLineOptions(&argc, argv, &options);
    /*
     * Initialize usy, message, DataStore, and fields all at once, but
     * don't use the Ingest module's default handler, since we have
     * some cleanup to do
     */
    sprintf (buf, "%s-%d", INGEST_NAME, getpid());
    IngestInitialize (buf);
#ifdef notdef
    msg_AddProtoHandler (MT_MESSAGE, message);
    msg_ELPrintMask (EF_ALL);
#endif

    if (! options.input)
    {
	return Input (argc, argv, &options);
    }

    /* Loop through input lines and parse as command lines */
    while (gets(buf))
    {
	struct Options sopt;
	int i = 1;
	char *opt[64];

	/* printf ("%s\n", buf); */
	opt[0] = argv[0];
	opt[1] = strtok(buf, " ");
	while ((opt[++i] = strtok (0, " ")))
	    ;
	ParseCommandLineOptions(&i, opt, &sopt);
	Input (i, opt, &sopt);
    }
    return 0;
}



int
Input (int argc, char *argv[], struct Options *opt)
{
    char *platform;
    PlatformId pid;
    float lat, lon, alt;
    ZebTime zt;
    int i;

    if ((opt->vor && argc < 3) || (!opt->vor && argc < 5))
    {
	fprintf (stderr, "%s: wrong number of arguments\n", argv[0]);
	Usage(argv[0]);
	exit(1);
    }
    i = 1;
    platform = argv[i++];
    if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
    {
	IngestLog (EF_PROBLEM, "bad platform '%s'", platform);
	exit (2);
    }

    if (opt->vor)
    {
	double dlat, dlon;
	if (VOR_Convert (opt->vor, opt->azimuth, opt->range,
			 &dlat, &dlon))
	{
	    lat = dlat;
	    lon = dlon;
	}
	else
	{
	    IngestLog (EF_PROBLEM, "vor conversion failed.");
	    exit (1);
	}
    }
    else
    {
	lat = atof(argv[i++]);
	lon = atof(argv[i++]);
    }
    alt = atof(argv[i++]);

    /* either use the current time or use the argument time */
    if (i < argc)
    {
	char *s = argv[i++];

	/* check for a colon indicating hh:mm form */
	int hh, mm, ss = 0;
	if (sscanf (s, "%d:%d:%d", &hh, &mm, &ss) >= 2)
	{
	    int year, month, day;
	    time_t t = time(0);
	    TC_SysToZt (t, &zt);
	    TC_ZtSplit (&zt, &year, &month, &day, 0, 0, 0, 0);
	    TC_ZtAssemble (&zt, year, month, day, hh, mm, ss, 0);
	    /* if re-assmbled time more than 6 hours into the future,
	     * then it was probably meant to be on yesterday's date. */
	    if (zt.zt_Sec > t + 6*3600)
		zt.zt_Sec -= 24*3600;
	}
	else
	{
	    /* force decimal interpretation of time number */
	    long t = strtol(s, 0, 10);
	    if (t == 0)
	    {
		IngestLog (EF_PROBLEM, 
			   "probable problem: time '%s' parsed as zero", s);
		exit (3);
	    }
	    TC_SysToZt (t, &zt);
	}
    }
    else
    {
	TC_SysToZt (time(0), &zt);
    }

    PutTrack (pid, &zt, lat, lon, alt);

    /*
     * Finally echo the command line which will input this same data again.
     */
    printf ("%s ", platform);
    if (opt->vor)
    {
	printf ("-vor %s %g %g ", opt->vor, opt->azimuth, opt->range);
    }
    else
    {
	printf ("%g %g ", lat, lon);
    }
    printf ("%g %lu ", alt, zt.zt_Sec);
    /*
     * Add computed lat/lon on end for extra information
     */
    if (opt->vor)
    {
	printf ("%g %g ", lat, lon);
    }
    printf ("\n");
    return 0;
}

