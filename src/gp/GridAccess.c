/*
 * Routines for pulling grids out of MUDRAS files.
 */
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/pd.h"
# include "../include/DataStore.h"
# include "GraphProc.h"
# include "rg_status.h"

static char *rcsid = "$Id: GridAccess.c,v 1.4 1990-12-05 10:58:24 corbet Exp $";


/*
 * MUDRAS kludge common block.
 */
extern struct MudHdr
{
	short mc_id[510];
	char  mc_namf[200];
	float mc_sclfld[25];
	int mc_nfl;
	float mc_csp[9];
	int mc_ncx[3];
	float mc_orlat[3], mc_orlon[3];
	int mc_ivdate, mc_ivtime;
} 
# ifdef titan
	_VOLUME;
# define volume_ _VOLUME
# else
	volume_;
# endif

# ifdef titan
/*
 * The string descriptor format for the titan.
 */
struct titan_descr
{
	char *string;
	int len;
};
# endif

/*
 * Turn degrees/minutes/seconds into a single floating point value.
 */
# define CVTLL(a) ((a)[0] + ((a)[1]/60.0) + ((a)[2]/3600.0))

/*
 * We keep one file open, since it is reasonably likely that we will want
 * to get to it more than once in a row.
 */
static char Open_file[200] = { 0 };
static bool IsOpen = FALSE;

/*
 * Temporary kludge for finding mudras files.
 */
static struct mudkludge
{
	int	time;
	char	*file;
	bool	hdr_valid;
	struct MudHdr header;
} CP3files[] =
{
	{ 203408,	"17jcp3v1.mud", FALSE, { 0 } },
	{ 203851,	"17jcp3v2.mud", FALSE, { 0 } },
	{ 204334,	"17jcp3v3.mud", FALSE, { 0 } },
	{ 204817,	"17jcp3v4.mud", FALSE, { 0 } },
	{ 205300,	"17jcp3v5.mud", FALSE, { 0 } },
	{ 205743,	"17jcp3v6.mud", FALSE, { 0 } },
	{ 210226,	"17jcp3v7.mud", FALSE, { 0 } },
	{ 210709,	"17jcp3v8.mud", FALSE, { 0 } },
	{ 211152,	"17jcp3v9.mud", FALSE, { 0 } },
	{ 211635,	"17jcp3v10.mud", FALSE, { 0 } },
	{ 212118,	"17jcp3v11.mud", FALSE, { 0 } },
	{ 212601,	"17jcp3v12.mud", FALSE, { 0 } },
	{ 213044,	"17jcp3v13.mud", FALSE, { 0 } },
	{ 213527,	"17jcp3v14.mud", FALSE, { 0 } },
	{ 214010,	"17jcp3v15.mud", FALSE, { 0 } },
	{ 214453,	"17jcp3v16.mud", FALSE, { 0 } },
	{ 214936,	"17jcp3v17.mud", FALSE, { 0 } },
	{ 215418,	"17jcp3v18.mud", FALSE, { 0 } },
	{ 215901,	"17jcp3v19.mud", FALSE, { 0 } },
	{ 220344,	"17jcp3v20.mud", FALSE, { 0 } },
	{ 220827,	"17jcp3v21.mud", FALSE, { 0 } },
	{ 221350,	"17jcp3v22.mud", FALSE, { 0 } },
	{ 221840,	"17jcp3v23.mud", FALSE, { 0 } },
	{ 222326,	"17jcp3v24.mud", FALSE, { 0 } },
	{ 222813,	"17jcp3v25.mud", FALSE, { 0 } },
	{ 223300,	"17jcp3v26.mud", FALSE, { 0 } },
	{ 223746,	"17jcp3v27.mud", FALSE, { 0 } },
	{ 224200,	"17jcp3v28.mud", FALSE, { 0 } },
	{ 224700,	"17jcp3v29.mud", FALSE, { 0 } },
	{ 225200,	"17jcp3v30.mud", FALSE, { 0 } },
	{ 225600,	"17jcp3v31.mud", FALSE, { 0 } },
	{ 230100,	"17jcp3v32.mud", FALSE, { 0 } },
	{ 231000,	"just another kludge", FALSE, { 0 } },
	{ 0,		0		}
};




static struct mudkludge
TrecFiles[] =
{
	{ 193732,	"17jtrecv1.mud", FALSE, { 0 } },
	{ 195141,	"17jtrecv2.mud", FALSE, { 0 } },
	{ 200551,	"17jtrecv3.mud", FALSE, { 0 } },
	{ 201959,	"17jtrecv4.mud", FALSE, { 0 } },
	{ 203408,	"17jtrecv5.mud", FALSE, { 0 } },
	{ 204817,	"17jtrecv6.mud", FALSE, { 0 } },
	{ 210226,	"17jtrecv7.mud", FALSE, { 0 } },
	{ 211635,	"17jtrecv8.mud", FALSE, { 0 } },
	{ 212118,	"17jtrecv9.mud", FALSE, { 0 } },
	{ 212601,	"17jtrecv10.mud", FALSE, { 0 } },
	{ 213044,	"17jtrecv11.mud", FALSE, { 0 } },
	{ 213527,	"17jtrecv12.mud", FALSE, { 0 } },
	{ 214010,	"17jtrecv13.mud", FALSE, { 0 } },
	{ 214453,	"17jtrecv14.mud", FALSE, { 0 } },
	{ 214936,	"17jtrecv15.mud", FALSE, { 0 } },
	{ 215419,	"17jtrecv16.mud", FALSE, { 0 } },
	{ 215901,	"17jtrecv17.mud", FALSE, { 0 } },
	{ 221350,	"17jtrecv18.mud", FALSE, { 0 } },
	{ 221840,	"17jtrecv19.mud", FALSE, { 0 } },
	{ 223300,	"17jtrecv20.mud", FALSE, { 0 } },
	{ 224000,	"end kludge" }, 
	{ 0, 0				}
};




static struct mudkludge
DDFiles[] =
{
	{ 213000,	"o17j1530.mud", FALSE, { 0 } },
	{ 214500,	"o17j1545.mud", FALSE, { 0 } },
	{ 220000,	"o17j1600.mud", FALSE, { 0 } },
	{ 221000,	"o17j1610.mud", FALSE, { 0 } },
	{ 222000,	"o17j1620.mud", FALSE, { 0 } },
	{ 222800,	"o17j1628.mud", FALSE, { 0 } },
	{ 223600,	"Another kludge", FALSE, { 0 } },
	{ 0, 0				}
};



/*
 * NOAA C.
 */
static struct mudkludge 
NCFiles[] =
{
	{ 213000,	"1530nc.mud", FALSE, { 0 } },
	{ 214500,	"1545nc.mud", FALSE, { 0 } },
	{ 220000,	"1600nc.mud", FALSE, { 0 } },
	{ 221000,	"1610nc.mud", FALSE, { 0 } },
	{ 222000,	"1620nc.mud", FALSE, { 0 } },
	{ 222800,	"1628nc.mud", FALSE, { 0 } },
	{ 224500,	"1645nc.mud", FALSE, { 0 } },
	{ 225900,	"1659nc.mud", FALSE, { 0 } },
	{ 0, 0 }
};


/*
 * NOAA D.
 */
static struct mudkludge 
NDFiles[] =
{
	{ 213000,	"1530nd.mud", FALSE, { 0 } },
	{ 214500,	"1545nd.mud", FALSE, { 0 } },
	{ 220000,	"1600nd.mud", FALSE, { 0 } },
	{ 221000,	"1610nd.mud", FALSE, { 0 } },
	{ 222000,	"1620nd.mud", FALSE, { 0 } },
	{ 222800,	"1628nd.mud", FALSE, { 0 } },
	{ 224500,	"1645nd.mud", FALSE, { 0 } },
	{ 225900,	"1659nd.mud", FALSE, { 0 } },
	{ 0, 0 }
};



/*
 * Temp stuff to pull in GALE data.
 */
static struct mudkludge 
GaleFiles[] =
{
	{ 000100,	"d1260000.mud",	FALSE, { 0 } },
	{ 000500,	"d1260005.mud",	FALSE, { 0 } },
	{ 001000,	"d1260000.mud",	FALSE, { 0 } },
	{ 001500,	"d1260005.mud",	FALSE, { 0 } },
	{ 002000,	"d1260000.mud",	FALSE, { 0 } },
	{ 002500,	"d1260005.mud",	FALSE, { 0 } },
	{ 003000,	"d1260000.mud",	FALSE, { 0 } },
	{ 003500,	"d1260005.mud",	FALSE, { 0 } },
	{ 004000,	"d1260000.mud",	FALSE, { 0 } },
	{ 004500,	"d1260005.mud",	FALSE, { 0 } },
	{ 005000,	"d1260000.mud",	FALSE, { 0 } },
	{ 005500,	"d1260005.mud",	FALSE, { 0 } },
	{ 010000,	"d1260100.mud",	FALSE, { 0 } },
	{ 010500,	"d1260105.mud",	FALSE, { 0 } },
	{ 011000,	"d1260100.mud",	FALSE, { 0 } },
	{ 011500,	"d1260105.mud",	FALSE, { 0 } },
	{ 012000,	"d1260100.mud",	FALSE, { 0 } },
	{ 012500,	"d1260105.mud",	FALSE, { 0 } },
	{ 013000,	"d1260100.mud",	FALSE, { 0 } },
	{ 013500,	"d1260105.mud",	FALSE, { 0 } },
	{ 014000,	"d1260100.mud",	FALSE, { 0 } },
	{ 014500,	"d1260105.mud",	FALSE, { 0 } },
	{ 015000,	"d1260100.mud",	FALSE, { 0 } },
	{ 015500,	"d1260105.mud",	FALSE, { 0 } },
	{ 020000,	"d1260200.mud",	FALSE, { 0 } },
	{ 020500,	"d1260205.mud",	FALSE, { 0 } },
	{ 021000,	"d1260200.mud",	FALSE, { 0 } },
	{ 021500,	"d1260205.mud",	FALSE, { 0 } },
	{ 022000,	"d1260200.mud",	FALSE, { 0 } },
	{ 022500,	"d1260205.mud",	FALSE, { 0 } },
	{ 023000,	"d1260200.mud",	FALSE, { 0 } },
	{ 023500,	"d1260205.mud",	FALSE, { 0 } },
	{ 024000,	"d1260200.mud",	FALSE, { 0 } },
	{ 024500,	"d1260205.mud",	FALSE, { 0 } },
	{ 025000,	"d1260200.mud",	FALSE, { 0 } },
	{ 025500,	"d1260205.mud",	FALSE, { 0 } },
	{ 030000,	"d1260300.mud",	FALSE, { 0 } },
	{ 030500,	"d1260305.mud",	FALSE, { 0 } },
	{ 031000,	"d1260300.mud",	FALSE, { 0 } },
	{ 031500,	"d1260305.mud",	FALSE, { 0 } },
	{ 032000,	"d1260300.mud",	FALSE, { 0 } },
	{ 032500,	"d1260305.mud",	FALSE, { 0 } },
	{ 033000,	"d1260300.mud",	FALSE, { 0 } },
	{ 033500,	"d1260305.mud",	FALSE, { 0 } },
	{ 0, 0}
};


/*
 * More kludges.  This one maps nice names onto what we find in the file.
 */
static struct fld_map
{
	char	*fm_user;
	char	*fm_mudras;
} CP3Fmap[] =
{
	{ "reflectivity",	"DZNE"	},
	{ "dz",			"DZNE"	},
	{ "DZ",			"DZNE"	},
	{ "velocity",		"VFNE"	},
	{ "vel",		"VFNE"	},
	{ "u_wind",		"U COMP" },
	{ "v_wind",		"V COMP" },
	{ 0, 0 }
};

/*
 * The TREC field map.
 */
static struct fld_map TrecFmap[] =
{
	{ "u_wind",	"UEDIT"	},
	{ "v_wind",	"VEDIT" },
	{ 0, 0 }
};


/*
 * The NOAA C/D field map.
 */
static struct fld_map NOAAFmap[] =
{
	{ "reflectivity",	"DZ"	},
	{ "dz",			"DZ"	},
	{ "DZ",			"DZ"	},
	{ "velocity",		"VE"	},
	{ "vel",		"VE"	},
	{ 0, 0 }
};




static struct fld_map GaleFmap[] =
{
	{ "reflectivity",	"DZ"	},
	{ "velocity",		"VE"	},
	{ 0, 0 }
};




/*
 * Meta-kludge map.
 */
static struct meta_kludge
{
	char	*mk_platform;
	struct mudkludge *mk_mud;
	struct fld_map *mk_flds;
	float	mk_lat, mk_lon;		/* Origin location	*/
} Kmap[] =
{
	{ "cp3",	CP3files,	CP3Fmap, 39.7647, -104.8731 },
	{ "trec",	TrecFiles,	TrecFmap, 39.7647, -104.8731 },
	{ "cp3dd",	DDFiles,	CP3Fmap, 39.7647, -104.8731 },
	{ "dualdop",	DDFiles,	CP3Fmap, 39.7647, -104.8731 },
	{ "noaac",	NCFiles,	NOAAFmap, 0.0, 0.0 },
	{ "noaad",	NDFiles,	NOAAFmap, 0.0, 0.0 },
	{ "gale",	GaleFiles,	GaleFmap, 0.0, 0.0 },
	{ 0, 0}
};



# ifdef notdef
/*
 * Grid accessing macro.
 */
# define GRID(g,j,i)	((grid)[(i)* (*ydim) + (j)])
# endif
# define BADVAL	-32768.0

# ifdef titan
#	define do_rgrid DO_RGRID
# else
#	define do_rgrid do_rgrid_
# endif
extern int	do_rgrid ();

/*
 * Our routines.
 */
# ifdef __STDC__
	static bool ga_OpenMudrasFile (char *);
	static bool ga_FetchData (int, int, float *, int, int);
	static void ga_CloseMudrasFile (void);
	static char *ga_MapField (char *, struct fld_map *);
	static bool ga_Regularize (DataObject *);
# else
	static bool ga_OpenMudrasFile ();
	static bool ga_FetchData ();
	static void ga_CloseMudrasFile ();
	static char *ga_MapField ();
	static bool ga_Regularize ();
# endif


/*
 * Hardwared Fortran LUN.
 */
# define MUDRASLUN 10



float *
ga_MudrasGrid (plot_time, platform, fld, alt, xdim, ydim, x0, y0, x1, y1)
time	*plot_time;
char 	*platform, *fld;
int	*xdim, *ydim, *alt;
float	*x0, *y0, *x1, *y1;
/*
 * Pull in a grid from a MUDRAS file.
 */
{
	struct mudkludge *ckp;
	struct meta_kludge *mkp;
	char fname[80], *ibuf;
	int plane = 1, fldn = 1, len;
	float *grid, badflag = -32768, x, y;
	float lat, lon, falt = ((float) *alt)/1000.0;
/*
 * See if we recognize the platform.
 */
	for (mkp = Kmap; mkp->mk_platform; mkp++)
		if (! strcmp (mkp->mk_platform, platform))
			break;
	if (! mkp->mk_platform)
	{
		msg_ELog (EF_PROBLEM, "I can't handle platform %s (yet)",
			platform);
		return (0);
	}
/*
 * Find the file of interest.
 */
	if (plot_time->ds_hhmmss < mkp->mk_mud[0].time)
		return (0);
	for (ckp = mkp->mk_mud; ckp->time; ckp++)
		if (ckp->time > plot_time->ds_hhmmss)
			break;
	if (! ckp->time)
		return (0);
	ckp--;
/*
 * Open this file.
 */
	sprintf (fname, "../data/%s", ckp->file);
	if (! ga_OpenMudrasFile (fname))
	{
		msg_ELog (EF_PROBLEM, "Open error on %s", fname);
		return (0);
	}

	msg_ELog(EF_DEBUG, "vol: axes %d %d %d, date %d %d Z %.2f %.2f %.2f",
		volume_.mc_ncx[0],
		volume_.mc_ncx[1], volume_.mc_ncx[2], volume_.mc_ivdate,
		volume_.mc_ivtime, volume_.mc_csp[6], volume_.mc_csp[7],
		volume_.mc_csp[8]);

/*
 * Stash the header if necessary.
 */
	if (! ckp->hdr_valid)
	{
		ckp->header = volume_;
		ckp->hdr_valid = TRUE;
	}
/*
 * Find the field.
 */
	fld = ga_MapField (fld, mkp->mk_flds);
	len = strlen (fld);
	for (fldn = 0; fldn < volume_.mc_nfl; fldn++)
		if (! strncmp (fld, volume_.mc_namf + 8*fldn, len))
			break;
	if (fldn >= volume_.mc_nfl)
	{
		msg_ELog (EF_PROBLEM, "Unable to find field '%s'", fld);
		return (0);
	}
	fldn++;	/* based at 1 */
/*
 * Allocate our memory.
 */
	*xdim = volume_.mc_ncx[0];
	*ydim = volume_.mc_ncx[1];
	grid = (float *) malloc ((*xdim)*(*ydim)*sizeof (float));
/*
 * Figure out which plane to grab.
 */
	plane = (int) ((falt - volume_.mc_csp[6])/volume_.mc_csp[8] + 1.5);
	if (plane < 1)
		plane = 1;
	else if (plane > volume_.mc_ncx[2])
		plane = volume_.mc_ncx[2];
	*alt = (int) (1000*(volume_.mc_csp[6] + (plane-1)*volume_.mc_csp[8]));
/*
 * Snarf some data.
 */
	if (! ga_FetchData (plane, fldn, grid, *xdim, *ydim))
	{
		free (grid);
		msg_ELog (EF_PROBLEM, "Error getting MUDRAS grid");
		return (0);
	}
/*
 * Find the origin coordinates.  If it's non-zero in our local array, use
 * it; otherwise assume that the file can be trusted.
 */
	if (lat = mkp->mk_lat)
		lon = mkp->mk_lon;
	else
	{
		lat = CVTLL (volume_.mc_orlat);
		lon = CVTLL (volume_.mc_orlon);
		if (lon > 0)
			lon = -lon;
	}
/*
 * Fix up the coordinates.
 */
	cvt_ToXY (lat, lon, &x, &y);
	*x0 = x + volume_.mc_csp[0];
	*x1 = x + volume_.mc_csp[1];
	*y0 = y + volume_.mc_csp[3];
	*y1 = y + volume_.mc_csp[4];

	msg_ELog (EF_DEBUG, "Coords are (%.2f %.2f) to (%.2f %.2f)", *x0, *y0,
		*x1, *y1);

/*
 * All done.
 */
# ifdef notdef
	ga_CloseMudrasFile ();
# endif
	return (grid);
}





static bool
ga_OpenMudrasFile (name)
char *name;
/*
 * Open this file.
 */
{
	int mlun = MUDRASLUN, print = 0, status;
	char sci[10], sub[10], vol[10];
# ifdef titan
	struct titan_descr scid, subd, vold, named;
# endif
/*
 * If we already have this file open, life is easy.
 */
	if (IsOpen)
	{
		if (! strcmp (name, Open_file))
			return (TRUE);
		ga_CloseMudrasFile ();
	}
# ifdef titan
	named.string = name; named.len = strlen (name);
	scid.string = sci; scid.len = 10;
	subd.string = sub; subd.len = 10;
	vold.string = vol; vold.len = 10;
	CDOPNR (&mlun, &named, &scid, &subd, &vold, &print, &status);
# else
	cdopnr_ (&mlun, name, sci, sub, vol, &print, &status, strlen (name),
		10, 10, 10);
# endif
	return (IsOpen = (status == 0));
}





static bool
ga_FetchData (plane, fld, grid, xd, yd)
int plane, fld, xd, yd;
float *grid;
/*
 * Get a grid from this file.
 */
{
	int mlun = MUDRASLUN, nid = 510, nx, ny, axis = 3, status;
	float badflag = -32768;
	char *ibuf;
/*
 * Allocate the temporary array that fetchd needs.
 */
	ibuf = malloc (xd * yd * sizeof (short));
/*
 * Now get the data.
 */
# ifdef titan
	FETCHD
# else
	fetchd_
# endif
		(&mlun, volume_.mc_id, &nid, &plane, &fld, ibuf, grid, 
			&nx, &ny, &axis, &badflag, &status);
/*
 * Return storage and quit.
 */
	free (ibuf);
	return (status == 0);
}






static void
ga_CloseMudrasFile ()
/*
 * Close the currently open file.
 */
{
	int mlun = MUDRASLUN, status;

# ifdef titan
	CDCLOS (&mlun, &status);
# else
	cdclos_ (&mlun, &status);
# endif
	IsOpen = FALSE;
}






void
ga_RotateGrid (src, dst, x, y)
float *src, *dst;
int x, y;
/*
 * Rotate this grid into column-major order.
 */
{
	int row, col;
	float *sp, *dp;
/*
 * Copy each row into the destination.
 */
 	sp = src;
	for (row = 0; row < y; row++)
	{
		dp = dst + row;
		for (col = 0; col < x; col++)
		{
			*dp = *sp++;
			dp += y;
		}
	}
}






static char *
ga_MapField (name, fmap)
char *name;
struct fld_map *fmap;
/*
 * Map this user name onto a mudras name, if possible.
 */
{
	for (; fmap->fm_user; fmap++)
		if (! strcmp (name, fmap->fm_user))
			return (fmap->fm_mudras);
	return (name);
}






bool
ga_GridBBox (plot_time, platform, x0, y0, x1, y1)
time	*plot_time;
char 	*platform;
float	*x0, *y0, *x1, *y1;
/*
 * Find the bounding box for this grid.
 */
{
	struct mudkludge *ckp;
	struct meta_kludge *mkp;
	char fname[80];
	float x, y, lat, lon;
/*
 * See if we recognize the platform.
 */
	for (mkp = Kmap; mkp->mk_platform; mkp++)
		if (! strcmp (mkp->mk_platform, platform))
			break;
	if (! mkp->mk_platform)
	{
		msg_ELog (EF_PROBLEM, "I can't handle platform %s (yet)",
			platform);
		return (0);
	}
/*
 * Find the file of interest.
 */
	if (plot_time->ds_hhmmss < mkp->mk_mud[0].time)
		return (0);
	for (ckp = mkp->mk_mud; ckp->time; ckp++)
		if (ckp->time > plot_time->ds_hhmmss)
			break;
	if (! ckp->time)
		return (0);
	ckp--;
/*
 * If the current header information is valid, we can just use it; otherwise
 * we have to open the file.
 */
	if (! ckp->hdr_valid)
	{
		sprintf (fname, "../data/%s", ckp->file);
		if (! ga_OpenMudrasFile (fname))
		{
			msg_ELog (EF_PROBLEM, "Open error on %s", fname);
			return (0);
		}
		ckp->hdr_valid = TRUE;
		ckp->header = volume_;
	}
/*
 * Find the origin coordinates.  If it's non-zero in our local array, use
 * it; otherwise assume that the file can be trusted.
 */
	if (lat = mkp->mk_lat)
		lon = mkp->mk_lon;
	else
	{
		lat = CVTLL (ckp->header.mc_orlat);
		lon = CVTLL (ckp->header.mc_orlon);
		if (lon > 0)
			lon = -lon;
	}
/*
 * Fix up the coordinates.
 */
	cvt_ToXY (lat, lon, &x, &y);
	*x0 = x + ckp->header.mc_csp[0];
	*x1 = x + ckp->header.mc_csp[1];
	*y0 = y + ckp->header.mc_csp[3];
	*y1 = y + ckp->header.mc_csp[4];
/*
 * All done.
 */
	/* ga_CloseMudrasFile (); */
	return (TRUE);
}






bool
ga_AvailableAlts (plot_time, platform, heights, nh)
time *plot_time;
char *platform;
float *heights;
int *nh;
/*
 * Obtain the list of available heights for this platform at this time.
 */
{
	Location loc;
	RGrid rg;
	PlatformId pid;
	int i;
/*
 * Make sure this is a real platform.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "AvailableAlts on bad plat '%s'",
				platform);
		return (FALSE);
	}
/*
 * Check it out.
 */
	if (! ds_GetRgridParams (pid, plot_time, &loc, &rg))
		return (FALSE);
/*
 * Now calculate the heights.
 */
	*nh = rg.rg_nZ;
	for (i = 0; i < rg.rg_nZ; i++)
		heights[i] = loc.l_alt + i*rg.rg_Zspacing;
	return (TRUE);
}





float *
ga_GetGrid (plot_time, platform, fname, xdim, ydim, x0, y0, x1, y1, alt)
time	*plot_time;
char 	*platform, *fname;
int	*xdim, *ydim;
float	*x0, *y0, *x1, *y1, *alt;
{
	PlatformId	pid;
	DataObject	*dobj;
	RGrid *rg;
	float *ret;
	time realtime;
# ifdef notdef
/*
 * If the data store does not recognize our platform, assume mudras.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		int ialt = (int) *alt;
		float * ret = ga_MudrasGrid (plot_time, platform, fname,
				&ialt, xdim, ydim, x0, y0, x1, y1);
		*alt = (float) ialt;
		return (ret);
	}
# endif
/*
 * Look up our platform.
 */
	if ((pid = ds_LookupPlatform (platform)) == BadPlatform)
	{
		msg_ELog (EF_PROBLEM, "Bad platform '%s'", platform);
		return (0);
	}
/*
 * Find out when we can really get data.
 */
	if (! ds_DataTimes (pid, plot_time, 1, DsBefore, &realtime))
	{
		msg_ELog (EF_INFO, "No data available at all for %s",platform);
		return (0);
	}
	msg_ELog (EF_DEBUG, "Plot time %d %d -> %d %d", plot_time->ds_yymmdd,
		plot_time->ds_hhmmss, realtime.ds_yymmdd, realtime.ds_hhmmss);
/*
 * Do a DS get for this data.
 */
	if ((dobj = ds_GetData (pid, &fname, 1, &realtime, &realtime,
				Org2dGrid, *alt, BADVAL)) == 0)
	{
		msg_ELog (EF_PROBLEM, "Get failed on %s/%s at %d %06d",
			platform, fname, realtime.ds_yymmdd, 
			realtime.ds_hhmmss);
		return (0);
	}
	*alt = dobj->do_loc.l_alt;
/*
 * Now turn this grid into a regular one if necessary.
 */
	if (dobj->do_org == OrgIRGrid)
		if (! ga_Regularize (dobj))
		{
			ds_FreeDataObject (dobj);
			return (0);
		}
/*
 * Pull out the info and return it.  For now we yank out the data and 
 * assume that it can be freed later.
 */
	rg = &dobj->do_desc.d_rgrid;
	*xdim = rg->rg_nX;
	*ydim = rg->rg_nY;
	cvt_ToXY (dobj->do_loc.l_lat, dobj->do_loc.l_lon, x0, y0);
	*x1 = *x0 + (rg->rg_nX - 1)*rg->rg_Xspacing;
	*y1 = *y0 + (rg->rg_nY - 1)*rg->rg_Yspacing;
	ret = dobj->do_data[0];
	dobj->do_flags &= ~DOF_FREEDATA;
	ds_FreeDataObject (dobj);
	msg_ELog (EF_DEBUG, "Returned grid (%.2f %.2f) t (%.2f %.2f)",
		*x0, *y0, *x1, *y1);
	return (ret);
}




bool
ga_Regularize (dobj)
DataObject *dobj;
/*
 * Turn an irregular grid into a regular one.
 */
{
	int		RGRID (), i, j, status;
	float		spline_eval (), *grid, *xpos, *ypos, xp, yp;
	float		xmin = 9999.0, ymin = 9999.0, *scratch;
	float		xmax = -9999.0, ymax = -9999.0, badflag, *dp;
	void		spline ();
	RGrid		rg;
	IRGrid		*irg = &dobj->do_desc.d_irgrid;
/*
 * Be really sure this is an irregular one.
 */
	if (dobj->do_org != OrgIRGrid)
	{
		msg_ELog (EF_PROBLEM, "Attempt to regularize non-IRGrid");
		return (FALSE);
	}
/*
 * Wire the dimension of the grid, and get some more memory.
 */
	rg.rg_nX = rg.rg_nY = 16;			/* XXX */
	grid = (float *) malloc (rg.rg_nX * rg.rg_nY * sizeof (float));
/*
 * Do a pass over the locations, and set everything up.
 */
	xpos = (float *) malloc (irg->ir_npoint * sizeof (float));
	ypos = (float *) malloc (irg->ir_npoint * sizeof (float));
	dp = dobj->do_data[0];
	for (i = 0; i < irg->ir_npoint; i++)
	{
	/*
	 * Turn this location into XY space, and see if it stretches our 
	 * limits.
	 */
	 	cvt_ToXY (irg->ir_loc[i].l_lat, irg->ir_loc[i].l_lon,
				xpos + i, ypos + i);
# ifdef notdef
	msg_ELog (EF_DEBUG, "Data %d at %d %d, %.2f", i, (int) xpos[i], 
			(int) ypos[i], dp[i]);
		if ((i % 4) == 0)
			sleep (1);
# endif
		if (xpos[i] < xmin)
			xmin = xpos[i];
		if (xpos[i] > xmax)
			xmax = xpos[i];
		if (ypos[i] < ymin)
			ymin = ypos[i];
		if (ypos[i] > ymax)
			ymax = ypos[i];
	}
/*
 * Store some of the new position info.
 */
	rg.rg_Xspacing = (xmax - xmin)/(rg.rg_nX - 1);
	rg.rg_Yspacing = (ymax - ymin)/(rg.rg_nY - 1);
	cvt_ToLatLon (xmin, ymin, &dobj->do_loc.l_lat, &dobj->do_loc.l_lon);
/*
 * Fill the grid with bad value flags
 */
	for (i = 0; i < rg.rg_nX*rg.rg_nY; i++)
		grid[i] = BADVAL;
/*
 * Use RGRID to generate gridded data
 */
	badflag = BADVAL;
	scratch = (float *) malloc (rg.rg_nX * rg.rg_nY * sizeof (float));
	msg_ELog (EF_DEBUG,
		"Call rgrid, %d x %d, np %d, (%.2f %.2f) to (%.2f %.2f)",
		rg.rg_nX, rg.rg_nY, irg->ir_npoint, xmin, ymin, xmax, ymax);
	status = do_rgrid (grid, &rg.rg_nX, &rg.rg_nY, &irg->ir_npoint,
		dobj->do_data[0], &badflag, 
		xpos, ypos, &xmin, &ymin, &xmax, &ymax, scratch);
/*
 * Clean up.
 */
	free (scratch);
	free (xpos);
	free (ypos);
/*
 * See what happened here.
 */
	switch (status)
	{
	    case RG_OK:
		break;
	    case RG_NOTENUFPTS:
		msg_ELog (EF_PROBLEM, 
			"Not enough good points to generate a grid");
		break;
	    case RG_COLLINEAR:
		msg_ELog (EF_PROBLEM,
			"Points are collinear, unable to generate a grid");
		break;
	    default:
		msg_ELog (EF_PROBLEM,
			"Unknown status 0x%x returned by RGRID", status);
	}
/*
 * Finish fixing up the data object, and return.
 */
	dobj->do_org = Org2dGrid;
	if (dobj->do_flags & DOF_FREEDATA)
		free (dobj->do_data[0]);
	dobj->do_data[0] = grid;
	dobj->do_flags |= DOF_FREEDATA;
	dobj->do_desc.d_rgrid = rg;
	return (TRUE);
}
