/*
 * Ingest data from slow tape ascii files.
 */
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
# include <stdio.h>
# include <math.h>
# define __EXTENSIONS__		/* Solaris string.h wants this for strtok_r */
# include <string.h>
# include <defs.h>
# include <message.h>
# include <DataStore.h>

# define DEG_TO_RAD(x)	((x) * 0.017453293)

# define BFACT		10
# define BSIZE		3440
/* # define N_RAW		40	/* Number of raw fields on the tape */
/* # define F_OFFSET	14	/* Offset to the first field we want */

/*
 * Field information.
 */
# define MAXP3FIELD 100
static int	Nfld, Nrec, DoRec[MAXP3FIELD], NRead;
static FieldId  Fids[MAXP3FIELD], RecFids[MAXP3FIELD];
static char	Format[MAXP3FIELD][16];

/*
 * Here is a list of the fields we care about, for one reason or another,
 * and the offset into the array of indirect field pointers.
 */
enum FieldIndex
{
	FI_Year = 0, FI_Month = 1, FI_Day = 2, FI_Hour = 3, FI_Minute = 4,
	FI_Second = 5, FI_SelINE = 6, FI_INE1Lat = 7, FI_INE1Lon = 8,
	FI_INE2Lat = 9, FI_INE2Lon = 10, FI_GPSLon = 11, FI_GPSLat = 12,
	FI_RAlt1 = 13, FI_RAlt2 = 14, FI_GPSAlt = 15, FI_Wspd = 16,
	FI_Wdir = 17, FI_UWind = 18, FI_VWind = 19, FI_ThetaE = 20,
	FI_ThetaEE = 21, FI_Pres = 22, FI_Temp = 23, FI_DP = 24,
	FI_LWater = 25, FI_PT = 26
};
# define N_INDEX 27	/* Keep this current! */

struct FMap
{
	char *fm_Name;
	enum FieldIndex fm_Index;
} FieldMap[] = {
	{ "Iyr",	FI_Year },
	{ "Imo",	FI_Month },
	{ "Idy", 	FI_Day },
	{ "Ihr",	FI_Hour },
	{ "Imn",	FI_Minute },
	{ "Isc",	FI_Second },
	{ "Nav",	FI_SelINE },
	{ "fla1",	FI_INE1Lat },
	{ "flo1",	FI_INE1Lon },
	{ "fla2",	FI_INE2Lat },
	{ "flo2",	FI_INE2Lon },
	{ "fla4",	FI_GPSLat },
	{ "flo4",	FI_GPSLon },
	{ "ra1",	FI_RAlt1 },
	{ "ra2",	FI_RAlt2 },
 	{ "ra3",	FI_GPSAlt },
	{ "wspd",	FI_Wspd },
	{ "wdir",	FI_Wdir },
	{ "pres",	FI_Pres },
	{ "tdry",	FI_Temp },
	{ "dp",		FI_DP },
	{ "lw",		FI_LWater },
/*
 * Derived fields here
 */
	{ "u_wind",	FI_UWind },
	{ "v_wind",	FI_VWind },
	{ "theta_e",	FI_ThetaE },
	{ "theta_ee",	FI_ThetaEE },
	{ "pt",		FI_PT },
};

static int FOffsets[N_INDEX];


/*
 * Routines.
 */
static int	rawget FP ((FILE *, float *));
static void	getfids FP ((char *));
static void	do_derivation FP ((float *));
int		Die FP ((void));
static void	SaveOffset FP ((char *, int));

/*
 * Globals.
 */

main (argc, argv)
int	argc;
char	**argv;
{
	int	i, rec, nsamp, first = 1;
	int	year, month, day, hour, minute, second;
	DataChunk	*dc;
	Location	origin;
	ZebTime		zt;
	FILE		*fptr;
	float		data[MAXP3FIELD];
	PlatformId	pid;
/*
 * Check arguments.
 */
	if (argc != 4)
	{
		printf ("Usage: st_ingest file platform field-file\n");
		exit (1);
	}
/*
 * Initialization.
 */
	msg_connect (Die, "st_ingest");
	usy_init ();
	ds_Initialize ();
/*
 * Open the file.
 */
	if ((fptr = fopen (argv[1], "r")) == NULL)
	{
		msg_ELog (EF_EMERGENCY, "Can't open file '%s'.", argv[1]);
		exit (1);
	}
/*
 * Check the platform. 
 */
	if ((pid = ds_LookupPlatform (argv[2])) == BadPlatform)
	{
		msg_ELog (EF_EMERGENCY, "Bad platform '%s'.", argv[2]);
		exit (1);
	}	
/*
 * Initialize FOffsets to bogus values
 */
	for (i = 0; i < N_INDEX; i++)
	    FOffsets[i] = -1;
/*
 * Set up fields and create an initial data chunk.
 */
	getfids (argv[3]);
	dc = dc_CreateDC (DCC_Scalar);
	dc->dc_Platform = pid;
	dc_SetScalarFields (dc, Nrec, RecFids);
	nsamp = 0;
/*
 * Plow through the data file.
 */
	while (data != NULL)
	{
		int navchoice;
	/*
	 * Get the raw data.
	 */
		if (! rawget (fptr, data))
		{
			msg_ELog (EF_INFO, "No more data.");
			break;
		}
	/*
	 * Derive some other fields
	 */
		do_derivation (data);		
	/*
	 * Extract a time.
	 */
		year = (int) data[FOffsets[FI_Year]];
		month = (int) data[FOffsets[FI_Month]];
		day = (int) data[FOffsets[FI_Day]];
		hour = (int) data[FOffsets[FI_Hour]];
		minute = (int) data[FOffsets[FI_Minute]];
		second = (int) data[FOffsets[FI_Second]];
		TC_ZtAssemble (&zt, year, month, day, hour, minute,
			second, 0); 
	/*
	 * Extract a location.  Use the "Nav" field to choose the best
	 * navigation, or default to "fla1" and "flo1".
	 */
		navchoice = (FOffsets[FI_SelINE] >= 0) ? 
		    (int)data[FOffsets[FI_SelINE]] : 1;
		
		switch (navchoice)
		{
		    case 1:
			origin.l_lat = data[FOffsets[FI_INE1Lat]];
			origin.l_lon = data[FOffsets[FI_INE1Lon]];
			origin.l_alt = data[FOffsets[FI_RAlt1]] * 0.001;
			break;

		    case 2:
			origin.l_lat = data[FOffsets[FI_INE2Lat]];
			origin.l_lon = data[FOffsets[FI_INE2Lon]];
			origin.l_alt = data[FOffsets[FI_RAlt2]] * 0.001;
			break;

		    case 4:
			origin.l_lat = data[FOffsets[FI_GPSLat]];
			origin.l_lon = data[FOffsets[FI_GPSLon]];
			origin.l_alt = data[FOffsets[FI_GPSAlt]] * 0.001;
			break;

		    default:
			msg_ELog (EF_PROBLEM, "Unexpected nav value %f",
				    data[FOffsets[FI_SelINE]]);
			continue;
		}
	/*
 	 * Do the rest of the fields.
	 */
		rec = 0;
		for (i = 0; i < Nfld; i++)
			if (DoRec[i])
				dc_AddScalar (dc, &zt, nsamp, RecFids[rec++],
						data + i);  
		dc_SetLoc (dc, nsamp, &origin);
		nsamp++;
	/*
	 * If we have a good amount of data, store it and start over.
	 */
		if (nsamp >= 500)
		{
			ds_Store (dc, first, NULL, 0);
			first = 0;
			dc_DestroyDC (dc);
			dc = dc_CreateDC (DCC_Scalar);
			dc->dc_Platform = pid;
			dc_SetScalarFields (dc, Nrec, RecFids);
			nsamp = 0;
		}
	}
/*
 * Store any remaining data.
 */
	if (nsamp > 0)
		ds_Store (dc, first, NULL, 0);
	exit (0);
}


int
Die ()
/*
 * Die gracefully.
 */
{
	exit (0);
}



static void
getfids (file)
char *file;
/*
 * Get id's for Zeb fields.
 */
{
	FILE *fp;
	char line[120];
# ifdef GRUNGY_OLD_STUFF
/*
 * Fields from the tape
 */
	fids[0] = F_Lookup ("gx1");
	fids[1] = F_Lookup ("gy1");
	fids[2] = F_Lookup ("gx2");
	fids[3] = F_Lookup ("gy2");
	fids[4] = F_Lookup ("gx4");
	fids[5] = F_Lookup ("gy4");
	fids[6] = F_Lookup ("heading");
	fids[7] = F_Lookup ("ra1");
	fids[8] = F_Lookup ("ra2");
	fids[9] = F_Lookup ("ra3");
	fids[10] = F_Lookup ("pitch");
	fids[11] = F_Lookup ("roll");
	fids[12] = F_Lookup ("pres");
	fids[13] = F_Lookup ("temp");
	fids[14] = F_Lookup ("dp");
	fids[15] = F_Lookup ("gs");
	fids[16] = F_Lookup ("tk");
	fids[17] = F_Lookup ("wspd");
	fids[18] = F_Lookup ("wdir");
	fids[19] = F_Lookup ("rd");
	fids[20] = F_Lookup ("rs");
	fids[21] = F_Lookup ("ru");
	fids[22] = F_Lookup ("w_wind");
	fids[23] = F_Lookup ("ui");
	fids[24] = F_Lookup ("lw");
	fids[25] = F_Lookup ("tas");
	Nfld = 26;
# endif
/*
 * Open the field description file.
 */
	if ((fp = fopen (file, "r")) == NULL)
	{
		perror (file);
		exit (1);

	}
/*
 * Now go through and read it.
 */
	Nfld = 0;
	while (fgets (line, 120, fp))
	{
		char *cp, *units, *format, *desc, *record;
	/*
	 * Ignore comments and blank lines.
	 */
		line[strlen (line) - 1] = '\0';		/* zorch newline */
		if (cp = strchr (line, '#'))
			*cp = '\0';
		if (strlen (line) == 0)
			continue;
	/*
	 * OK, split the line at the first blank, and see if we're
	 * recording it.
	 */
		desc = line;
		(void) strtok_r (line, " \t", &desc);
		units = strtok_r (NULL, " \t", &desc);
		format = strtok_r (NULL, " \t", &desc);
		strcpy (Format[Nfld], format);
		record = strtok_r (NULL, " \t", &desc);
		DoRec[Nfld] = (*record != 'n' && *record != 'N');
	/*
	 * If it's to be recorded, and declare the field.
	 * Also stash aside the fid if it's one we need.
	 */
		if (DoRec[Nfld])
		{
			Fids[Nfld] = F_DeclareField (line, desc, units);
			RecFids[Nrec++] = Fids[Nfld];
		}
		SaveOffset (line, Nfld);
	/*
	 * Are we recording this one?
	 */
		Nfld++;
	}
	NRead = Nfld;
/*
 * Fields we derive
 */
	SaveOffset ("u_wind", Nfld);
	DoRec[Nfld] = TRUE;
	RecFids[Nrec++] = Fids[Nfld++] = F_DeclareField ("u_wind",
			"U wind component", "m/s");
	SaveOffset ("v_wind", Nfld);
	DoRec[Nfld] = TRUE;
	RecFids[Nrec++] = Fids[Nfld++] = F_DeclareField ("v_wind",
			"V wind component", "m/s");
	SaveOffset ("theta_e", Nfld);
	DoRec[Nfld] = TRUE;
	RecFids[Nrec++] = Fids[Nfld++] = F_DeclareField ("theta_e",
			"Equivalent Potential Temperature", "K");
	SaveOffset ("theta_ee", Nfld);
	DoRec[Nfld] = TRUE;
	RecFids[Nrec++] = Fids[Nfld++] = F_DeclareField ("theta_ee",
			"Equivalent Potential Temperature (Emanuel)", "K");
	SaveOffset ("pt", Nfld);
	DoRec[Nfld] = TRUE;
	RecFids[Nrec++] = Fids[Nfld++] = F_DeclareField ("pt",
			"Potential Temperature", "K");
}




static void
SaveOffset (fname, offset)
char *fname;
int offset;
/*
 * See if we need to stash aside a copy of this FID.
 */
{
	int i;

	for (i = 0; i < N_INDEX; i++)
		if (! strcmp (fname, FieldMap[i].fm_Name))
		{
			FOffsets[FieldMap[i].fm_Index] = offset;
			return;
		}
}






static int
rawget(fptr, data)
FILE	*fptr;
float	*data;
{
	int		bsize, i;
	static int	count = 0 /* = BFACT */;
/* 
 * Should we expect an rtape header? 
 */
# ifdef notdef
	if (count >= BFACT)
	{
		count = 0;
		if (fscanf (fptr, "%d", &bsize) < 1) 
		{
			return (FALSE);
		}
    		if (bsize != BSIZE)
		{
      			msg_ELog (EF_PROBLEM,"Got blocksize %d, expected %d",
				bsize, BSIZE);
			return (FALSE);
    		}
  	}
# endif
/* 
 * Get the variables.
 */
	for (i = 0; i < NRead; i++)
	{
	    char cval;
	    int ival, got;
	/*
	 * Determine the type of this field from the last character of its 
	 * format string
	 */
	    char ftype = Format[i][strlen( Format[i] ) - 1];
	    
	    switch (ftype)
	    {
	      case 'c':
		got = fscanf (fptr, Format[i], &cval);
		data[i] = (float)cval;
		break;
	      case 'd':
		got = fscanf (fptr, Format[i], &ival);
		data[i] = (float)ival;
		break;
	      case 'f':
		got = fscanf (fptr, Format[i], data + i);
		break;
	      default:
		msg_ELog (EF_PROBLEM, "Unable to parse field with format '%s'",
			  Format[i]);
		return (FALSE);
	    }
	    

	    if (got < 1)
	    {
		msg_ELog (EF_INFO, "End of data. Ingested %d records",
			  count);
		return (FALSE);
	    }
	}
/*
 * Increment buffer count.
 */
	count++;
	return (TRUE);
}


static void
do_derivation (data)
float	*data;
/*
 * Derive some fields
 */
{
	float	wspd, wdir, t, t_k, p, dp, l;
	extern float bolton (float press, float temp, float dewpoint);
	extern float raf_thetae (float t, float p, float dp, float l);
/*
 * u wind and v wind
 */
	wspd = data[FOffsets[FI_Wspd]];
	wdir = data[FOffsets[FI_Wdir]];

	data[FOffsets[FI_UWind]] = wspd * cos (DEG_TO_RAD (270 - wdir));
	data[FOffsets[FI_VWind]] = wspd * sin (DEG_TO_RAD (270 - wdir));
/*
 * equivalent potential temperature (standard calculation)
 */
	p = data[FOffsets[FI_Pres]];
	t = data[FOffsets[FI_Temp]];
	dp = data[FOffsets[FI_DP]];

	data[FOffsets[FI_ThetaE]] = bolton (p, t, dp);
/*
 * Kerry Emanuel's equivalent potential temperature (includes liquid water 
 * component)
 */
	if (FOffsets[FI_LWater] >= 0)
	{
	    l = data[FOffsets[FI_LWater]];	/* liquid water content	*/
	    data[FOffsets[FI_ThetaEE]] = raf_thetae (p, t, dp, l);
	}
	else
	    data[FOffsets[FI_ThetaEE]] = -999.0;
/*
 * Potential temperature (per formula from Jorgensen, FASTEX, 1/97)
 */
	data[FOffsets[FI_PT]] = (t + 273.16)*pow (1000.0/p, 0.286714);
}
