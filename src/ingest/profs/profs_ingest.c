/*
 * Ingest PROFS ASCII data into the system.
 *
 * Usage:
 *	profs_ingest <profs_file>
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
# include <errno.h>
# include <math.h>
# include "ingest.h"

# ifndef lint
MAKE_RCSID ("$Id: profs_ingest.c,v 1.1 1992-09-11 06:39:42 granger Exp $")
# endif

# define DEG_TO_RAD(x)	((x)*0.017453293)

void	Stations FP ((void));
void	Fields FP ((void));
void	DoData FP ((void));

/*
 * Global stuff
 */
# define MAX_FIELD 40
# define MAX_PLAT 100

FILE		*Infile;

int	Nsta;
int	Nfld;
int	Nextra;			/* extra field count */
FieldId	Fid[MAX_FIELD];
float	Scale[MAX_FIELD];
float	Offset[MAX_FIELD];
float	Bad[MAX_FIELD];
int	Wspd_ndx = -1, Wdir_ndx = -1;	/* indices for wspd and wdir fields */
char	Scratch[256];
PlatformId	Plat;
PlatformId	SubPlat[MAX_PLAT];
Location	Loc[MAX_PLAT];

# define DC_BADVAL -9999.0


/*
 * Conversion table for changing known PROFS fields into known zeb fields
 */
struct _Pfld
{
	char	*profs_name;
	char	*zeb_name;
	float	scale;
	float	offset;
} Pfld_table[] = 
{
	{"Temp", "tdry", 0.555555, -17.777778},	/* F to C */
	{"Dew_Pt", "dp", 0.555555, -17.777778},	/* F to C */
	{"Press", "pres", 1.0, 0.0},
	{"Wnd_Azm", "wdir", 1.0, -180.0},
	{"Wnd_Spd", "wspd", 0.514791, 0.0},	/* kts to m/s */
	{"Precip", "raina", 25.4, 0.0},		/* inches to mm */
	{NULL, NULL, 0.0, 0.0}
};





main (argc, argv)
int argc;
char **argv;
{
/*
 * Basic arg check.
 */
	IngestParseOptions(&argc, argv, NULL);
	if (argc < 2)
	{
		printf ("Usage: %s <data_file>\n", argv[0]);
		exit (1);
	}
/*
 * Hook into the world.
 */
	IngestInitialize ("PROFSIngest");
/*
 * More initialization.
 */
	if ((Plat = ds_LookupPlatform ("profs")) == BadPlatform)
	{
		IngestLog (EF_PROBLEM, "Unknown platform: 'profs'");
		exit (1);
	}
/*
 * Open file and read first line
 */
	if ((Infile = fopen (argv[1], "r")) == NULL)
	{
		IngestLog (EF_PROBLEM, "Error %d opening '%s'", errno, 
			argv[1]);
		exit (1);
	}

	fscanf (Infile, "%d %d", &Nsta, &Nfld);
	fgets (Scratch, sizeof (Scratch), Infile); /* Dump rest of the line */
/*
 * Figure out our stations and fields.
 */
	Stations ();
	Fields ();
/*
 * Get the data
 */
	DoData ();
	exit (0);
}




void
Stations ()
/*
 * Do station-oriented initialization.
 */
{
	int	slist[100], sta, num;
	char	id[4] = "xxx", garbage[2], sitename[40], pname[20];
	Location	*loc;
/*
 * Read the station info from the file
 */
	for (sta = 0; sta < Nsta; sta++)
	{
	/*
	 * Grab the info for this station
	 */
		loc = Loc + sta;

		fscanf (Infile, "%d %f %f %f%3c%2c", &num, &loc->l_lat, 
			&loc->l_lon, &loc->l_alt, id, garbage);
		loc->l_alt *= 0.001;	/* convert alt to km */

		fgets (sitename, sizeof (sitename), Infile);
	/*
	 * Make sure that this station is known to the system.
	 */
		sprintf (pname, "profs/%s", id);
		if ((SubPlat[sta] = ds_LookupPlatform (pname)) == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "Unknown subplat '%s'", pname);
			continue;
		}
	}
}




void
Fields ()
/*
 * Deal with the field list
 */
{
	int	i, fld;
	char	fname[MAX_FIELD][8];
/*
 * Field names
 */
	for (fld = 0; fld < Nfld; fld++)
	{
	/*
	 * Read the field name and trim trailing spaces
	 */
		fscanf (Infile, "%7c", fname[fld]);
		fname[fld][7] = '\0';

		for (i = 6; fname[fld][i] == ' '; i--)
			fname[fld][i] = '\0';
	}

	fgets (Scratch, sizeof (Scratch), Infile); /* Finish the line */
/*
 * Units (ignore)
 */
	fgets (Scratch, sizeof (Scratch), Infile); /* Finish the line */
/*
 * Bad value flags
 */
	for (fld = 0; fld < Nfld; fld++)
		fscanf (Infile, "%7f", Bad + fld);

	fgets (Scratch, sizeof (Scratch), Infile); /* Finish the line */
/*
 * Declare the fields
 */
	for (fld = 0; fld < Nfld; fld++)
	{
	/*
	 * See if this field is in our conversion list
	 */
		for (i = 0; Pfld_table[i].profs_name; i++)
			if (! strcmp (fname[fld], Pfld_table[i].profs_name))
				break;

		if (Pfld_table[i].profs_name)
		{
			strcpy (fname[fld], Pfld_table[i].zeb_name);
			Scale[fld] = Pfld_table[i].scale;
			Offset[fld] = Pfld_table[i].offset;
		}
		else
		{
			Scale[fld] = 1.0;
			Offset[fld] = 0.0;
		}
	/*
	 * Look up (or declare) the field
	 */
		Fid[fld] = F_Lookup (fname[fld]);
	/*
	 * Keep track of wind speed and wind direction
	 */
		if (! strcmp (fname[fld], "wspd"))
			Wspd_ndx = fld;
		else if (! strcmp (fname[fld], "wdir"))
			Wdir_ndx = fld;
	}
/*
 * Declare u_wind and v_wind if we have wspd and wdir
 */
	Nextra = 0;

	if (Wspd_ndx >= 0 && Wdir_ndx >= 0)
	{
		Fid[Nfld] = F_Lookup ("u_wind");
		Fid[Nfld + 1] = F_Lookup ("v_wind");
		Nextra = 2;
	}
}




void
DoData ()
/*
 * Read and store the data
 *
 * Right now, the files we're getting contain only one sample.  I imagine
 * that will change at some point.
 */
{
	int	year, month, day, hour, minute, second, fld, sta, num;
	float	*data, *dp, wspd, wdir, u_wind, v_wind;
	ZebTime	t;
	DataChunk	*dc;
/*
 * Get the time
 */
	fscanf (Infile, "%d/%d/%d %d:%d:%d", &year, &month, &day, &hour, 
		&minute, &second);

	TC_ZtAssemble (&t, year, month - 1, day, hour, minute, second, 0);
/*
 * Create and initialize the data chunk
 */
	dc = dc_CreateDC (DCC_IRGrid);
	dc->dc_Platform = Plat;
	dc_IRSetup (dc, Nsta, SubPlat, Loc, Nfld + Nextra, Fid);
	dc_SetBadval (dc, DC_BADVAL);
/*
 * Allocate the data array
 */
	data = (float *) malloc ((Nfld + Nextra) * Nsta * sizeof (float));
/*
 * Read the data
 */
	for (sta = 0; sta < Nsta; sta++)
	{
		wspd = wdir = DC_BADVAL;

		dp = data + sta;

		fscanf (Infile, "%7d", &num);

		for (fld = 0; fld < Nfld; fld++)
		{
			if (fscanf (Infile, "%7f", dp) != 1)
			{
				IngestLog (EF_PROBLEM, "Short data line");
				exit (1);
			}
		/*
		 * Deal with bad values, scale & offset
		 */
			if (*dp == Bad[fld])
				*dp = DC_BADVAL;
			else
			{
				*dp = *dp * Scale[fld] + Offset[fld];
				if (fld == Wdir_ndx && *dp < 0.0)
					*dp += 360.0;
			}
		/*
		 * Save wspd and wdir
		 */
			if (fld == Wspd_ndx)
				wspd = *dp;
			else if (fld == Wdir_ndx)
				wdir = *dp;
		/*
		 * Move the data pointer
		 */
			dp += Nsta;
		}
	/*
	 * Calculate u_wind and v_wind if possible
	 */
		if (Nextra)
		{
			if (wspd != DC_BADVAL && wdir != DC_BADVAL)
			{
				u_wind = wspd * cos (DEG_TO_RAD (90 - wdir));
				v_wind = wspd * sin (DEG_TO_RAD (90 - wdir));
			}
			else
				u_wind = v_wind = DC_BADVAL;

			*dp = u_wind;
			dp += Nsta;
			*dp = v_wind;
			dp += Nsta;
		}
	}
/*
 * Put the data into the data chunk and store the data chunk
 */
	for (fld = 0; fld < Nfld + Nextra; fld++)
		dc_IRAddGrid (dc, &t, 0, Fid[fld], data + Nsta * fld);

	ds_Store (dc, FALSE, 0, 0);
/*
 * Clean up
 */
	dc_DestroyDC (dc);
	free (data);
}
