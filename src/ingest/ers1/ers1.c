/*
 * Ingest ERS-1 satellite data
 */
# include <unistd.h>
# include <sys/types.h>
# include <sys/stat.h>
# include <fcntl.h>
# include <math.h>

# include <config.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>

# define DEG_TO_RAD(deg) ((deg)*0.017453292) /* Deg to Rad conv. */

/*
 * The ERS-1 "value added" files have the following record format:
 */
# define PtBlock	19		/* Number of points in one rec	*/
typedef struct _ERSRecord
{
	int	er_time;		/* Time -- sec since beg of year */
	short	er_lat[PtBlock];	/* Latitudes			*/
	short	er_lon[PtBlock];	/* Longitudes			*/
	short	er_speed[PtBlock];	/* Wind speed			*/
	short	er_dir[PtBlock];	/* Direction			*/
	short	er_woflag[PtBlock];
	short	er_namb[PtBlock];
	short	er_mlespd[PtBlock][4];
	short	er_mledir[PtBlock][4];
} ERSRecord;

/*
 * Here we define the grid we want to make.
 */
# define LON_MIN 135.0
# define LON_MAX 175.0
# define LAT_MIN -20.0
# define LAT_MAX 10.0


/*
 * ERS1 resolution seems to be about 1 degree in latitude, .3 degree
 * in longitude.  Try, for now, for about .25 final resolution and
 * see what we get.
 */
# define GRID_NX 160
# define GRID_NY 120
float Grid[GRID_NX * GRID_NY];
# define GRID_XS ((LON_MAX - LON_MIN)/GRID_NX)
# define GRID_YS ((LAT_MAX - LAT_MIN)/GRID_NY)

# define MIN_POINTS	50	/* How many before it's worth our while? */

/*
 * Description of our result grid.
 */
Location Origin = { LAT_MIN, LON_MIN, 0.0 };
RGrid Rg = { GRID_XS, GRID_YS, 0.0, GRID_NX, GRID_NY, 1};

/*
 * Here we accumulate info for the bints routine.
 */
# define MAXPT 10000
float Xsta[MAXPT];
float Ysta[MAXPT];
float U[MAXPT];
float V[MAXPT];

float Dz[MAXPT];	/* bints scratch area */
float Dzr[GRID_NX*GRID_NY];	/* bints scratch */
/*
 * Parameters for bints.
 */
const int B_IP = 2;
const float B_R = 2.0;
const float B_RMX = 2.0;
const int B_NQD = 0;
const int B_NFILT = 0;
const float BADVAL = -9999.9;


/*
 * Forwards.
 */
int GetRevEntry FP ((FILE *, ZebTime *, int *, int *));
void GrabData FP ((PlatformId, int, ZebTime *, int, int));

int Handler () {}

/*
 * Quick swap.
 */
# ifdef __GNUC__
inline
# endif
short
swap2 (v)
short v;
{
        short ret = v;
	unsigned char c, *vp = (unsigned char *) &ret;
        c = vp[0];
        vp[0] = vp[1];
        vp[1] = c;
        return (ret);
}

               
# ifdef __GNUC__
inline
# endif
int
swap4 (v)
int v;
{
        int ret = v;
	unsigned char c, *vp = (unsigned char *) &ret;
        c = vp[0];
        vp[0] = vp[3];
        vp[3] = c;
        c = vp[1];
        vp[1] = vp[2];
        vp[2] = c;
        return (ret);
}




main (argc, argv)
int argc;
char **argv;
/*
 * ers1 revfile datafile platform
 */
{
	PlatformId pid;
	FILE *revfile;
	int datafile, beginr, endr;
	ZebTime zt;
	float xsc, ysc;
/*
 * Checking.
 */
	if (argc != 4)
	{
		fprintf (stderr, "Usage: ers1 revfile datafile platform\n");
		exit (1);
	}
/*
 * Open files.
 */
	if ((revfile = fopen (argv[1], "r")) == NULL)
	{
		perror (argv[1]);
		exit (1);
	}
	if ((datafile = open (argv[2], O_RDONLY)) < 0)
	{
		perror (argv[2]);
		exit (1);
	}
/*
 * Plug in.
 */
	usy_init ();
	msg_connect (Handler, "ers1");
	ds_Initialize ();
        if ((pid = ds_LookupPlatform (argv[3])) == BadPlatform)
        {
                fprintf (stderr, "Bad platform: %s", argv[3]);
                exit (1);
        }
/*
 * Tweak the grid spacing info.
 */
	cvt_Origin ((LAT_MIN + LAT_MAX)/2, LON_MIN);
	cvt_ToXY ((LAT_MIN + LAT_MAX)/2 + 1.0, LON_MIN + 1.0, &xsc, &ysc);
	Rg.rg_Xspacing *= xsc;
	Rg.rg_Yspacing *= ysc;
	printf ("Spacings: %.2f %.2f\n", Rg.rg_Xspacing, Rg.rg_Yspacing);
/*
 * Blast.
 */
	while (GetRevEntry (revfile, &zt, &beginr, &endr))
	{
		char at[60];
		TC_EncodeTime (&zt, TC_Full, at);
		printf ("%s %d -> %d\n", at, beginr, endr);
		GrabData (pid, datafile, &zt, beginr, endr);
	}
        exit (0);
}






int
GetRevEntry (fp, zt, begin, end)
FILE *fp;
ZebTime *zt;
int *begin, *end;
/*
 * Extract needed info from the "revcat" file.
 */
{
	char revline[120];
	SValue v;
/*
 * Read the next line from the file.
 */
	if (! fgets (revline, 120, fp))
		return (FALSE);
/*
 * Pull out the info.  Those nice ERS1 folks stored their times in *almost*
 * the zeb ascii format...
 */
	revline[13] = ',';
	revline[22] = '\0';
	uit_parse_date (revline + 4, &v, TRUE);
	TC_UIToZt (&v.us_v_date, zt);
	*begin = atoi (revline + 80);
	*end = atoi (revline + 87);
}





void
GrabData (pid, fd, zt, begin, end)
PlatformId pid;
int fd, begin, end;
ZebTime *zt;
/*
 * Pull in a sweep from the file.
 */
{
	int pt, recno, npt = 0, gnx = GRID_NX, gny = GRID_NY, nbad = 0;
	ERSRecord rec;
        DataChunk *dc;
        FieldId fids[2];
	float speed, dir;
/*
 * Move to the first record.
 */
	lseek (fd, (begin - 1)*sizeof (ERSRecord), SEEK_SET);
/*
 * Start reading.
 */
        for (recno = begin; recno <= end; recno++)
        {
                if (read (fd, &rec, sizeof (rec)) < sizeof (rec))
                {
                        fprintf (stderr, "Read error!\n");
                        return;
                }
        /*
         * Pass through the points.
         */
                for (pt = 0; pt < PtBlock; pt++)
                {
                /*
                 * Convert values and see that they are in our area
                 * of interest.
                 */
                        float lat = swap2(rec.er_lat[pt])/100.0;
                        float lon = swap2(rec.er_lon[pt])/100.0 + 180.0;
                        if (lon > 180.0)
                                lon -= 360.0;
                        if (lat < LAT_MIN || lat > LAT_MAX ||
                                        lon < LON_MIN || lon > LON_MAX)
                                continue;
                /*
                 * OK we got one.  Fix up the coords for bints.
                 */
                        speed = swap2(rec.er_speed[pt])/100.0;
                        dir = swap2(rec.er_dir[pt])/100.0 + 180.0;
			U[npt] = -speed * sin (DEG_TO_RAD (dir));
			V[npt] = -speed * cos (DEG_TO_RAD (dir));
                        Xsta[npt] = (lon - LON_MIN)/GRID_XS;
                        Ysta[npt] = (lat - LAT_MIN)/GRID_YS;
		/*
		 * They have a FUNKY way of representing bad data.
		 */
			if (speed == 51 || dir == 510)
				nbad++;
			else
				npt++;
                }
        }
/*
 * If we didn't get enough points just blow it off.
 */
        if (npt < MIN_POINTS)
	{
                printf ("-> %d points insufficient, blowing off this one\n",
				npt);
                return;
        }
/*
 * Fix up a data chunk.
 */
        dc = dc_CreateDC (DCC_RGrid);
        dc->dc_Platform = pid;
        fids[0] = F_Lookup ("u_wind");
        fids[1] = F_Lookup ("v_wind");
        dc_RGSetup (dc, 2, fids);
	dc_SetBadval (dc, BADVAL);
/*
 * OK we have to interpolate this thing.
 */
	printf ("-> %d points ", npt);
	if (nbad > 0)
		printf ("(+ %d bad) ", nbad);
	printf ("in range. Be patient...");
        fflush (stdout);
        bints_ (Grid, &gnx, &gny, Xsta, Ysta, U, Dz, Dzr, &npt,
                        &B_IP, &B_R, &B_RMX, &B_NQD, &B_NFILT, &BADVAL);
        dc_RGAddGrid (dc, 0, fids[0], &Origin, &Rg, zt, Grid, 0);
        printf ("u..."); fflush (stdout);
        bints_ (Grid, &gnx, &gny, Xsta, Ysta, V, Dz, Dzr, &npt,
                        &B_IP, &B_R, &B_RMX, &B_NQD, &B_NFILT, &BADVAL);
        dc_RGAddGrid (dc, 0, fids[1], &Origin, &Rg, zt, Grid, 0);
        printf ("v.\n"); fflush (stdout);
/*
 * Store and dump it.
 */
        ds_Store (dc, FALSE, 0, 0);
        dc_DestroyDC (dc);
}
