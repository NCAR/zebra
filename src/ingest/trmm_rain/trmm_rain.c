/*
 * Ingest module for TRMM rain gauge data
 */
/*
 *		Copyright (C) 1993 UCAR
 *	University Corporation for Atmospheric Research
 *		   All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced
 * or used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 * 
 * This software and any accompanying written materials are provided "as is"
 * without warranty of any kind.  UCAR expressly disclaims all warranties of
 * any kind, either express or implied, including but not limited to the
 * implied warranties of merchantibility and fitness for a particular purpose.
 * UCAR does not indemnify any infringement of copyright, patent, or trademark
 * through use or modification of this software.  UCAR does not provide 
 * maintenance or updates for its software.
 */

# ifndef lint
	static char *rcsid = "$Id: trmm_rain.c,v 1.2 1993-05-19 23:10:54 granger Exp $";
# endif

# include <time.h>
# include <errno.h>
# include <stdio.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>
# include <ingest.h>

# define BADVAL	-9999.0

static FILE	*Infile;
static int	Year;
static time_t	Yearsec;
static FieldId	Fid;
static DataChunk	*Dc;

static void	InitDC FP ((void));
static void	GrabData FP ((void));
static time_t	YearSeconds FP ((int));


static void
Usage (prog)
char *prog;
{
	printf ("Usage: %s [ingest options] filename\n", prog);
	IngestUsage ();
}
	


main (argc, argv)
int	argc;
char	**argv;
{
/*
 * Parse general ingest options
 */
	IngestParseOptions (&argc, argv, Usage);
/*
 * Get our program-specific options
 */
	if (argc != 2)
	{
		Usage (argv[0]);
		exit (1);
	}
/*
 * Open the input file
 */
	Infile = fopen (argv[1], "r");

	if (! Infile)
	{
		printf ("Error %d opening  '%s'!\n", errno, argv[1]);
		exit (1);
	}
/*
 * Connect to the data store, message, etc., and initialize our data chunk
 */
	IngestInitialize ("TRMM_Rain");
	InitDC ();
/*
 * Build the data chunk then write it out
 */
	GrabData ();

	if (! ds_Store (Dc, FALSE, (dsDetail *) 0, 0))
		IngestLog (EF_EMERGENCY, "%s: Failure storing data", 
			   ds_PlatformName (Dc->dc_Platform));
	else
		IngestLog (EF_INFO, "Successful completion");

	exit (0);
}


static void
InitDC ()
/*
 * Create our data chunk and initialize based on the first line of the input
 * file
 */
{
	int	azim, range, s;
	char	sitename[40];
	PlatformId	pid;
	Location	loc;
	struct tm	tm;
/*
 * Table of sites
 */
	struct _sites
	{
		char	name[5];
		char	platname[4];
		float	lat, lon;
	} Sites[] = 
	{
		{"Anna", "ann", -12.9142, 131.6743},
		{"Batc", "bat", -13.0517, 131.0217},
		{"Bath", "bah", -11.7761, 130.6147},
		{"Bell", "bel", -12.7597, 130.8817},
		{"Berr", "ber", -12.4572, 130.9253},
		{"Chan", "chp", -13.1665, 130.1185},
		{"Char", "car", -12.4160, 130.6246},
		{"Dum ", "dum", -12.6415, 130.3773},
		{"Gard", "gap", -11.3988, 130.4198},
		{"Good", "gom", -13.2119, 131.3715},
		{"Gunn", "gup", -12.2431, 131.0419},
		{"Hump", "hum", -12.6082, 131.2894},
		{"Kool", "kol", -12.3921, 131.1741},
		{"La B", "lab", -13.1127, 130.4893},
		{"Litc", "lit", -13.4338, 130.4800},
		{"Mand", "man", -12.4431, 130.7603},
		{"McMi", "mil", -12.5400, 131.0782},
		{"Mt. ", "mbu", -13.2295, 131.1372},
		{"Old ", "ops", -12.3510, 131.8110},
		{"Pick", "pic", -11.7671, 130.8759},
		{"Poin", "pst", -12.5851, 131.7522},
		{"Snak", "sbo", -11.4287, 130.6626},
		{"Wool", "wol", -12.3743, 131.4614},
		{"", "", 0.0000, 0.0000}
		
	};
/*
 * Get the site name, year, and radar-relative azimuth and range from the
 * first line of the file.  We don't use the azimuth and range since we
 * have lat/lon locations below.
 */
	fscanf (Infile, " %39c%d %d %d", sitename, &Year, &azim, &range);
	Yearsec = YearSeconds (Year);
/*
 * Find this one in the site list
 */
	for (s = 0; Sites[s].name[0]; s++)
		if (! strncmp (sitename, Sites[s].name, 4))
			break;
	
	if (! Sites[s].name[0])
	{
		IngestLog (EF_EMERGENCY, "Bad site string '%s'", sitename);
		exit (1);
	}
/*
 * Create the data chunk and put in the platform ID, location, field ID, and
 * bad value flag
 */
	Dc = dc_CreateDC (DCC_Scalar);

	if ((Dc->dc_Platform = ds_LookupPlatform (Sites[s].platname)) == 
	    BadPlatform)
	{
		IngestLog (EF_EMERGENCY, "Cannot get  platform ID for '%s'",
			   Sites[s].platname);
		exit (1);
	}

	IngestLog (EF_INFO, "Ingesting data for %s", Sites[s].platname);

	loc.l_lat = Sites[s].lat;
	loc.l_lon = Sites[s].lon;
	loc.l_alt = 0.00;	/* We don't have altitudes available */
	dc_SetStaticLoc (Dc, &loc);

	Fid = F_DeclareField ("rainr", "Rain gauge rates", "mm/hr");
	dc_SetScalarFields (Dc, 1, &Fid);

	dc_SetBadval (Dc, BADVAL);
}



static void
GrabData ()
/*
 * Put all the data into the data chunk
 */
{
	ZebTime	t;
	int	ndx = 0, prevday = 0, num, jday, hour, minute, second;
	float	rate;

	while ((num = fscanf (Infile, "%d %d:%d:%d %f", &jday, &hour, &minute, 
		       &second, &rate)) == 5)
	{
	/*
	 * Handle year change if necessary
	 */
		if (jday < prevday)
			Yearsec = YearSeconds (++Year);

		prevday = jday;
	/*
	 * Build the time, truncating seconds to zero
	 */
		t.zt_Sec = Yearsec + (jday - 1) * 24 * 3600 + hour * 3600 +
			minute * 60;
		t.zt_MicroSec = 0;
	/*
	 * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	 * According to email from Matthias Steiner, times are LOCAL,
	 * so here we must adjust final time from local to GMT, assuming
	 * the email is correct.  Anybody know the timezone of Darwin?
	 * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	 */



	/*
	 * Throw this sample into the data chunk
	 */
		dc_AddScalar (Dc, &t, ndx++, Fid, &rate);
		if ((ndx % 250) == 0)
			IngestLog (EF_DEBUG, "%d points read", ndx);
	}

	if (num > 0)
		IngestLog (EF_PROBLEM, "Stopping at bad data line");

	IngestLog (EF_INFO, "%d good data points", ndx);
}



static time_t
YearSeconds (year)
/*
 * Turn the year into a UNIX time
 */
{
	struct tm	t;

	t.tm_year = year - 1900;
	t.tm_sec = t.tm_min = t.tm_hour = t.tm_mon = 0;
	t.tm_mday = 1;
	t.tm_zone = (char *) 0;
	t.tm_wday = t.tm_isdst = t.tm_yday = 0;
	return (timegm (&t));
}

