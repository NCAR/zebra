/*
 * Ingest CLASS data into the system.
 *
 * Usage:
 *	class_ingest file f1 f2 ... fn
 *
 */
/*		Copyright (C) 1987-92 by UCAR
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
# include <copyright.h>
# include <ctype.h>
# include <ui_error.h>
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"
# include "../include/DataStore.h"
# include "fields.h"

static char *rcsid = "$Id: class_ingest.c,v 2.2 1992-03-31 23:10:39 burghart Exp $";

# ifdef __STDC__
	static int	incoming (struct message *);
	static void	Times (void);
	static void	Locations (void);
# else
	static int	incoming ();
	static void	Times ();
	static void	Locations ();
# endif

/*
 * Our data object
 */
DataObject Dobj;

# define SND	"snd"
# define BUFLEN	1024
# define BADVAL -9999.0

int 	Npts;
float	Pres[BUFLEN], QPres[BUFLEN], Buf[BUFLEN];
int	BadPts[BUFLEN], NBad;




main (argc, argv)
int argc;
char **argv;
{
	char	plat[30], *site, *snd_site ();
	int	i, f;
/*
 * Basic arg check.
 */
	if (argc < 3)
	{
		printf ("Usage: class_ingest file fields\n");
		exit (1);
	}
/*
 * Hook into the world.
 */
ERRORCATCH
	usy_init ();
	msg_connect (incoming, "class_ingest");
	ds_Initialize ();
/*
 * Initialize data object stuff we know now
 */
	Dobj.do_org = OrgScalar;
	Dobj.do_badval = BADVAL;
	Dobj.do_flags = 0;
/*
 * Load the sounding file (0 indicates we're loading a CLASS format file)
 */
	snd_load_file (argv[1], 0, SND);
/*
 * Get the site name and figure out the platform
 */
	site = snd_site (SND);

	if (sscanf (site, "FIXED, %s", plat) == 1)
		/* do nothing */;
	if (sscanf (site, "FIXED  %s", plat) == 1)
		/* do nothing */;
	else if (strncmp (site, "MOBILE", 6) == 0)
		strcpy (plat, "mobile");
	else
		strcpy (plat, site);
/*
 * Make the platform name lower case
 */
	for (i = 0; i < strlen (site); i++)
		plat[i] = tolower (plat[i]);
/*
 * Set the platform in the data object, making sure it's OK along the way
 */
	if ((Dobj.do_id = ds_LookupPlatform (plat)) == BadPlatform)
		ui_error ("Unknown platform: %s", plat);
/*
 * Get the times and locations
 */
	Times ();
	Locations ();
/*
 * Get pressure and pressure quality fields to build a data removal
 * list
 */
	snd_get_data (SND, Pres, BUFLEN, fd_num ("pres"), BADVAL);
	snd_get_data (SND, QPres, BUFLEN, fd_num ("qpres"), BADVAL);

	NBad = 0;
	for (i = 0; i < Npts; i++)
		if (Pres[i] == BADVAL || QPres[i] == BADVAL || 
			Pres[i] == 0.0 || 
			(QPres[i] > 1.5 && QPres[i] != 77 && QPres[i] != 88))
			BadPts[NBad++] = i;
/*
 * Grab each field
 */
	argc -= 2;
	argv += 2;

	Dobj.do_nfield = argc;

	for (f = 0; f < Dobj.do_nfield; f++)
	{
		Dobj.do_fields[f] = usy_string (argv[f]);
		snd_get_data (SND, Buf, BUFLEN, fd_num (argv[f]), BADVAL);
	/*
	 * Remove the bad points
	 */
		for (i = 0; i < NBad; i++)
			Buf[BadPts[i]] = BADVAL;

		Dobj.do_data[f] = (float *) malloc (Npts * sizeof (float));
		memcpy (Dobj.do_data[f], Buf, Npts * sizeof (float));
	}
/*
 * Send everything to the data store
 */
	ds_PutData (&Dobj, TRUE);
ON_ERROR
	exit (1);
ENDCATCH
}



static void
Times ()
/*
 * Get the times of the sounding points
 */
{
	time	start, t, snd_time ();
	int	i, hours, minutes, seconds, delta, snd_get_data ();
/*
 * Get the start time and the time data for the sounding
 */
	start = snd_time (SND);

	Npts = Dobj.do_npoint = snd_get_data (SND, Buf, BUFLEN, 
		fd_num ("time"), BADVAL);
/*
 * Allocate the time array for the data object
 */
	Dobj.do_times = (time *) malloc (Npts * sizeof (time));
/*
 * Convert the sounding times, which are in seconds from sounding launch,
 * into absolute times and put them into the data object
 */
	for (i = 0; i < Npts; i++)
	{
		t = start;

		if (Buf[i] >= 0)
		{
			hours = (int)(Buf[i] / 3600);
			minutes = (int)((Buf[i] - 3600 * hours) / 60);
			seconds = (int)(Buf[i] - 3600 * hours - 60 * minutes);
			delta = 10000 * hours + 100 * minutes + seconds;
			pmu_dadd (&t.ds_yymmdd, &t.ds_hhmmss, delta);
		}

		Dobj.do_times[i] = t;
	}

	Dobj.do_begin = start;
	Dobj.do_end = Dobj.do_times[Npts-1];
}




static void
Locations ()
/*
 * Get the locations of the sounding points
 */
{
	float	snd_s_lat (), snd_s_lon (), snd_s_alt ();
	int	i, snd_get_data ();
/*
 * Put in the site location
 */
	Dobj.do_loc.l_lat = snd_s_lat (SND);
	Dobj.do_loc.l_lon = snd_s_lon (SND);
	Dobj.do_loc.l_alt = 0.001 * snd_s_alt (SND);
/*
 * Allocate the location array for the data object
 */
	Dobj.do_aloc = (Location *) malloc (Npts * sizeof (Location));
/*
 * Get the latitude data
 */
	snd_get_data (SND, Buf, BUFLEN, fd_num ("latitude"), BADVAL);

	for (i = 0; i < Npts; i++)
		Dobj.do_aloc[i].l_lat = Buf[i];
/*
 * Get the longitude data
 */
	snd_get_data (SND, Buf, BUFLEN, fd_num ("longitude"), BADVAL);

	for (i = 0; i < Npts; i++)
		Dobj.do_aloc[i].l_lon = Buf[i];
/*
 * Get the altitude data, converting from m to km
 */
	snd_get_data (SND, Buf, BUFLEN, fd_num ("altitude"), BADVAL);

	for (i = 0; i < Npts; i++)
		Dobj.do_aloc[i].l_alt = 0.001 * Buf[i];
}



		
static int
incoming (msg)
struct message *msg;
/*
 * Deal with incoming messages.
 */
{
	switch (msg->m_proto)
	{
	   case MT_TIMER:
	   	tl_DispatchEvent ((struct tm_time *) msg->m_data);
		break;
	}
	return (0);
}
