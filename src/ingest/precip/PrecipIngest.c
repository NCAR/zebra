/*
 * Convert a composite precip data file to Zeb data store files. 
 */
static char *rcsid = "$Id: PrecipIngest.c,v 1.2 1994-02-02 20:11:51 burghart Exp $";
/*		Copyright (C) 1987,88,89,90,91,92 by UCAR
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

# include <config.h>
# include <copyright.h>

# include <errno.h>
# include <stdio.h>
# include <string.h>
# include <math.h>

# include <defs.h>
# include <message.h>
# include <DataStore.h>


# define MAXPLATS	3000
# define SAMPS_PER_DAY	96		/* 15 minute samples */
# define BADVAL		-9999.0

/*
 * The big space where we put everything from the data file
 */
struct
{
	float		rain[SAMPS_PER_DAY];
	short		qual_code[SAMPS_PER_DAY];
	short		qc_flag[SAMPS_PER_DAY];
} FileData[MAXPLATS];

/*
 * Possible data sites
 */
struct
{
	char	*name;
	int	namelen;
	PlatformId	pid;
	Location	loc;
} Sites[MAXPLATS];

/*
 * Global stuff.
 */
static int	NumSites = 0;	/* Number of platforms.		*/
static char	PlatformName[80];	/* Name of the platform.	*/
static PlatformId	Pid;

/*
 * prototypes
 */
static void	IngestFile FP ((char *));
static void	LoadConfig FP ((char *));
static char *	GetNextString FP ((char *, char *));
static int	Die FP ((int));
static int	GetSiteIndex FP ((char *, double, double));
static int	NextLine FP ((FILE *, char *, char *, float *, float *, 
			float *, short *, short *));





main (argc, argv)
int	argc;
char	**argv;
{
	int	i;
/*
 * Check arguments.
 */
	if (argc < 4)
	{
		printf ("Usage: %s <config_file> <platform> <data_file>\n",
			argv[0]);
		exit (1);
	}
/*
 * Initialize
 */
	msg_connect (Die, "PrecipIngest");
	usy_init ();
	ds_Initialize ();
	msg_ELog (EF_INFO, "Ingesting '%s'", argv[3]);
/*
 * Check the main platform.
 */
	strcpy (PlatformName, argv[2]);
	if ((Pid = ds_LookupPlatform (PlatformName)) == BadPlatform)
	{
		msg_ELog (EF_EMERGENCY, "No platform '%s' in data store", 
			PlatformName);
		Die (1);
	}
/*
 * Load the configuration and do the work
 */
	LoadConfig (argv[1]);
	IngestFile (argv[3]);
	Die (0);
}




static void
IngestFile (fname)
char	*fname;
/*
 * Start converting some data.
 */
{
	FILE	*infile;
	char	sitename[40], origtimestr[18], timestr[18];
	float	*timeslice, lat, lon;
	int	lines = 0, s, l, sndx, year, month, day, hour, minute, second;
	ZebTime	starttime, time;
	DataChunk	*dc;
	PlatformId	sub_pids[MAXPLATS];
	Location	locs[MAXPLATS];
	FieldId		fids[3];
/*
 * Read the input file.
 */
	if ((infile = fopen (fname, "r")) == 0)
	{
		msg_ELog (EF_EMERGENCY, "Error %d opening '%s'", errno, fname);
		Die (1);
	}
/*
 * Snag all the data
 */
	while (NextLine (infile, timestr, sitename, &lat, &lon, 
		FileData[lines].rain, FileData[lines].qual_code, 
		FileData[lines].qc_flag))
	{
	/*
	 * Test the time
	 */
		if (lines == 0)
		{
		/*
		 * Save the original time string and make it a ZebTime, too
		 */
			int	year, month, day, hour, minute, second;

			strcpy (origtimestr, timestr);

			if (sscanf (timestr, "%d/%d/%d %d:%d:%d", &year, 
				&month, &day, &hour, &minute, &second) != 6)
			{
				msg_ELog (EF_EMERGENCY, "Bad time string '%s'",
					timestr);
				Die (1);
			}

			TC_ZtAssemble (&starttime, year, month, day, hour, 
				minute, second, 0);
		}

		if (strcmp (origtimestr, timestr))
		{
			msg_ELog (EF_EMERGENCY, 
				"Unexpected time change from '%s' to '%s'",
				origtimestr, timestr);
			Die (1);
		}
	/*
	 * Find the subplatform ID for this site
	 */
		if ((sndx = GetSiteIndex (sitename, lat, lon)) < 0)
		{
			msg_ELog (EF_INFO, "Dropping line for '%s'", sitename);
			continue;
		}

		sub_pids[lines] = Sites[sndx].pid;
		locs[lines] = Sites[sndx].loc;

		lines++;
		if (! (lines % 50))
			msg_ELog (EF_DEBUG, "%d data lines read", lines);
	}
/*
 * Close the file
 */
	fclose (infile);
/*
 * Three fields (as described in precip_comp)
 */
	fids[0] = F_DeclareField ("precip", "precipitaion", "mm");
	fids[1] = F_DeclareField ("qual_code", "Qualification code", "code");
	fids[2] = F_DeclareField ("qc_flag", "QC Flag", "code");
/*
 * Allocate a chunk big enough to hold one time slice of data
 */
	timeslice = (float *) malloc (lines * sizeof (float));
/*
 * Create a data chunk.
 */	
	dc = dc_CreateDC (DCC_IRGrid);
	dc->dc_Platform = Pid;
	dc_IRSetup (dc, lines, sub_pids, locs, 3, fids); 
	dc_SetBadval (dc, BADVAL);
/*
 * Loop through all the samples for the day, building the data chunk
 */
	for (s = 0; s < SAMPS_PER_DAY; s++)
	{
		time = starttime;
		time.zt_Sec += s * (86400 / SAMPS_PER_DAY);
	/*
	 * Precip for this sample
	 */
		for (l = 0; l < lines; l++)
			timeslice[l] = FileData[l].rain[s];

		dc_IRAddGrid (dc, &time, s, fids[0], timeslice);
	/*
	 * Qualification code for this sample
	 */
		for (l = 0; l < lines; l++)
			timeslice[l] = (float) FileData[l].qual_code[s];

		dc_IRAddGrid (dc, &time, s, fids[1], timeslice);
	/*
	 * QC flag for this sample
	 */
		for (l = 0; l < lines; l++)
			timeslice[l] = (float) FileData[l].qc_flag[s];

		dc_IRAddGrid (dc, &time, s, fids[2], timeslice);
	}
/*
 * Store and release the data chunk
 */
	ds_Store (dc, FALSE, NULL, 0);
	dc_DestroyDC (dc);
}




static void
LoadConfig (fname)
char	*fname;
/*
 * Add another sub-platform to the list.
 */
{
	int	p;
	FILE	*configfile;
	char	line[128], subplatname[20], sitename[60], fullname[80], *lp;
	float	lat, lon, alt;
/*
 * Open the config file
 */
	if ((configfile = fopen (fname, "r")) == 0)
	{
		msg_ELog ("Error %d opening config file '%s'", errno, fname);
		Die (1);
	}
/*
 * Read each line, which should consist of:
 *	<subplatform name> <site name> <lat> <lon> <alt> [<comment>]
 * OR
 *	! <comment>
 * 
 */
	while (fgets (line, sizeof (line), configfile))
	{
	/*
	 * Strip leading whitespace
	 */
		for (lp = line; *lp == ' ' || *lp == '\t'; lp++)
			/* nothing */;
	/*
	 * Ignore comments or empty lines
	 */
		if (*lp == '!' || *lp == '\0')
			continue;
	/*
	 * Get the platform name and site name, then the latitude and longitude
	 */
		if ((lp = GetNextString (subplatname, lp)) == 0 ||
			(lp = GetNextString (sitename, lp)) == 0 ||
			sscanf (lp, "%f %f %f", &lat, &lon, &alt) != 3)
		{
			msg_ELog (EF_EMERGENCY, "Bad config line '%s'", line);
			Die (1);
		}
	/*
	 * Store this one away
	 */
		if ((p = NumSites++) >= MAXPLATS)
		{
			msg_ELog (EF_EMERGENCY, "Too many sub-platforms");
			Die (1);
		}

		Sites[p].name = strdup (sitename);
		Sites[p].namelen = strlen (sitename);
		Sites[p].loc.l_lat = lat;
		Sites[p].loc.l_lon = lon;
		Sites[p].loc.l_alt = alt;

		sprintf (fullname, "%s/%s", PlatformName, subplatname);
		Sites[p].pid = ds_LookupPlatform (fullname);
		if (Sites[p].pid == BadPlatform)
		{
			msg_ELog (EF_EMERGENCY, "Bad platform '%s'", fullname);
			Die (1);
		}

		if (! (NumSites % 50))
			msg_ELog (EF_DEBUG, "%d config lines read", NumSites);
	}
}




static char*
GetNextString (ret, text)
char	*ret, *text;
/*
 * Extract the next string from 'text', either quoted or terminated by
 * white space.	The string is written into 'ret', and the return value
 * of the function is a pointer to the first character after the string
 * (success) or null (failure).
 */
{
	char	*endquote, *whitespace;
/*
 * Remove leading white space
 */
	while (*text == ' ' || *text == '\t')
		text++;
/*
 * Quoted string?
 */
	if (*text == '\"' || *text == '\'')
	{
		if ((endquote = strchr (text + 1, *text)) == 0)
			return ((char *) 0);

		strncpy (ret, text + 1, endquote - text - 1);
		ret[endquote - text - 1] = '\0';

		return (endquote + 1);
	}
/*
 * No quotes, so delineate by whitespace
 */
	if (sscanf (text, "%s", ret) == 1)
		return (text + strlen (ret));
	else
		return ((char *) 0);
}


	

static int
Die (status)
int	status;
/*
 * Die gracefully.
 */
{
	msg_ELog (EF_INFO, "Exiting.");
	exit (status);
}




static int
GetSiteIndex (sitename, lat, lon)
char	*sitename;
double	lat, lon;
/*
 * Return an index in the site list, given a site name and location.
 * On error, return -1.
 */
{
	int	i;
	int	inlen = strlen (sitename), complen;

	for (i = 0; i < NumSites; i++)
	{
		complen = (Sites[i].namelen < inlen) ? 
			Sites[i].namelen : inlen;
		if (! strncasecmp (sitename, Sites[i].name, complen))
		{
		/*
		 * Since we can have multiple name matches, we have to make
		 * sure locations match, too
		 */
			float	latdiff = fabs (lat - Sites[i].loc.l_lat);
			float	londiff = fabs (lon - Sites[i].loc.l_lon);

			if (latdiff < 0.011 && londiff < 0.011)
				return (i);
		}
	}
/*
 * Uh oh...
 */
	msg_ELog (EF_PROBLEM, "No match for '%s' at %.2f,%.2f", sitename, 
		lat, lon);
	return (-1);
}




static int
NextLine (infile, timestr, sitename, lat, lon, precip, qual_code, qc_flag)
FILE	*infile;
char	*timestr, *sitename;
float	*lat, *lon, *precip;
short	*qual_code, *qc_flag;
/*
 * Read the next line from 'infile', splitting up the bits & pieces of it
 */
{
	char	line[80 + 12 * SAMPS_PER_DAY];
	int	i, f, flag;
	static char	flagtable[] = "UGBDNXE........M";

	if (! fgets (line, sizeof (line), infile))
		return (0);

	strncpy (timestr, line, 17);
	timestr[17] = '\0';

	strncpy (sitename, line + 29, 10);
	sitename[10] = '\0';

	sscanf (line + 40, "%f %f", lat, lon);

	for (i = 0; i < SAMPS_PER_DAY; i++)
	{
	/*
	 * Actual precip value
	 */
		precip[i] = atof (line + 66 + 12 * i);
		if (precip[i] < 0.0)
			precip[i] = BADVAL;
	/*
	 * Qualification code
	 */
		qual_code[i] = atoi (line + 75 + 12 * i);
	/*
	 * Convert the ASCII character QC flag into the equivalent BUFR
	 * integer code
	 */
		flag = (int) line[77 + 12 * i];
		for (f = 0; flagtable[f] && flag != flagtable[f]; f++)
			/* nothing */;

		qc_flag[i] = f;
	}
}
