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

MAKE_RCSID ("$Id: ers1.c,v 1.3 1994-07-13 17:56:57 sobol Exp $");
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
int GetRevEntry FP ((FILE *, ZebTime *, ZebTime *, int *, int *));
void GrabData FP ((PlatformId, int, ZebTime *, ZebTime *, int, int, int *,
 int *, int *));
void StoreData FP ((PlatformId, ZebTime *, int *, int *));

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
	int datafile, beginr, endr, npt, nbad, tnpt;
	ZebTime zt, tmp_time;
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
	npt = 0;
	nbad = 0;
	while (GetRevEntry (revfile, &zt, &tmp_time, &beginr, &endr))
	{
		char at[60];
		TC_EncodeTime (&zt, TC_Full, at);
		printf ("%s %d -> %d\n", at, beginr, endr);
		tnpt = 0;
		GrabData (pid, datafile, &zt, &tmp_time, beginr, endr,
		 &npt, &nbad, &tnpt);
		if ((tnpt < MIN_POINTS) && (npt >= MIN_POINTS))
		{
			StoreData (pid, &zt, &npt, &nbad);
			npt = 0;
			nbad = 0;
		}
	}
/*
 * If we didn't get enough points just blow it off.
 */
        if (npt < MIN_POINTS)
	{
                printf ("-> %d points insufficient, blowing off this one\n",
				npt);
		exit (0);
        }
	StoreData (pid, &zt, &npt, &nbad);
        exit (0);
}






int
GetRevEntry (fp, zt, tmp_time, begin, end)
FILE *fp;
ZebTime *zt, *tmp_time;
int *begin, *end;
/*
 * Extract needed info from the "revcat" file.
 */
{
	char revline[120];
	SValue v;
	int year;
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
/*
 * ris - 6/25/94 - what we really need is the time associated with the first 
 *  good data point which we get from the data file.  This is documented as 
 *  # of seconds since the first day of the year, but this was not correct
 *  when dealing with 1993 files.  Instead we will use # of seconds since
 *  the beginning of 1992.  So for now we will get the number of seconds
 *  from 1/1/70 to 1/1/92 so that we can add to it later.
 */
	TC_ZtAssemble (tmp_time, 92, 1, 1, 0, 0, 0, 0);

	*begin = atoi (revline + 80);
	*end = atoi (revline + 87);
}





void
GrabData (pid, fd, zt, tmp_time, begin, end, npt, nbad, tnpt)
PlatformId pid;
int fd, begin, end;
ZebTime *zt, *tmp_time;
int *npt, *nbad, *tnpt;
/*
 * Pull in a sweep from the file.
 */
{
	int pt, recno, time;
	ERSRecord rec;
	float speed, dir, lat, lon;
	char at[60];
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
                        lat = swap2(rec.er_lat[pt])/100.0;
                        lon = swap2(rec.er_lon[pt])/100.0 + 180.0;
                        if (lon > 180.0)
                                lon -= 360.0;
                        if (lat < LAT_MIN || lat > LAT_MAX ||
                                        lon < LON_MIN || lon > LON_MAX)
                                continue;
                /*
                 * OK we got one.  Fix up the coords for bints.
		 *  If this is the first one set up the correct zebtime.
                 */
			if (*npt == 0)
			{
				time = swap4(rec.er_time); 
				zt->zt_Sec = tmp_time->zt_Sec + time;
				TC_EncodeTime (zt, TC_Full, at);
				printf ("Time set to %s \n", at);
			}

                        speed = swap2(rec.er_speed[pt])/100.0;
                        dir = swap2(rec.er_dir[pt])/100.0 + 180.0;
		/*
		 * They have a FUNKY way of representing bad data.
		 */
			if (speed == 51 || dir == 510)
			{
				(*nbad)++;
			}
			else
			{
				U[*npt] = -speed * sin (DEG_TO_RAD (dir));
				V[*npt] = -speed * cos (DEG_TO_RAD (dir));
                	        Xsta[*npt] = (lon - LON_MIN)/GRID_XS;
                        	Ysta[*npt] = (lat - LAT_MIN)/GRID_YS;
				(*npt)++;
				(*tnpt)++;
				if (*npt == MAXPT)
				{
					StoreData (pid, zt, npt, nbad);
					*npt = 0;
					*tnpt = 0;
					*nbad = 0;
				}
			}
                }
        }
        return;
}


void
StoreData (pid, zt, npt, nbad)
PlatformId pid;
ZebTime *zt;
int *npt, *nbad;
/*
 * Store the data gathered in this sweep.
 */
{
        DataChunk *dc;
        FieldId fids[2];
	int gnx = GRID_NX, gny = GRID_NY;
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
	printf ("-> %d points ", *npt);
	if (*nbad > 0)
		printf ("(+ %d bad) ", *nbad);
	printf ("in range. Be patient...");
        fflush (stdout);
        bints_ (Grid, &gnx, &gny, Xsta, Ysta, U, Dz, Dzr, npt,
                        &B_IP, &B_R, &B_RMX, &B_NQD, &B_NFILT, &BADVAL);
        dc_RGAddGrid (dc, 0, fids[0], &Origin, &Rg, zt, Grid, 0);
        printf ("u..."); fflush (stdout);
        bints_ (Grid, &gnx, &gny, Xsta, Ysta, V, Dz, Dzr, npt,
                        &B_IP, &B_R, &B_RMX, &B_NQD, &B_NFILT, &BADVAL);
        dc_RGAddGrid (dc, 0, fids[1], &Origin, &Rg, zt, Grid, 0);
        printf ("v.\n"); fflush (stdout);
/*
 * Store and dump it.
 */
        ds_Store (dc, FALSE, 0, 0);
        dc_DestroyDC (dc);
}
