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
# include <defs.h>
# include <message.h>
# include <DataStore.h>

# define DEG_TO_RAD(x)	((x) * 0.017453293)

# define BFACT		10
# define BSIZE		3440
# define MAXFLD		100
# define N_RAW		40	/* Number of raw fields on the tape */
# define F_OFFSET	14	/* Offset to the first field we want */

static int	Nfld;


/*
 * Routines.
 */
static int	rawget FP ((FILE *, float *));
static void	getfids FP ((FieldId *));
static void	do_derivation FP ((float *));
int		Die FP ((void));

/*
 * Globals.
 */

main (argc, argv)
int	argc;
char	**argv;
{
	int	i;
	int	year, month, day, hour, minute, second;
	DataChunk	*dc;
	Location	origin;
	ZebTime		zt;
	FILE		*fptr;
	FieldId		fids[MAXFLD];
	float		data[MAXFLD];
	PlatformId	pid;
/*
 * Check arguments.
 */
	if (argc < 3)
	{
		printf ("Usage: st_ingest file platform");
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
 * Set up fields.
 */
	getfids (fids);
/*
 * Plow through the data file.
 */
	while (data != NULL)
	{
	/*
	 * Create the data chunk.
	 */
		dc = dc_CreateDC (DCC_Scalar);
		dc->dc_Platform = pid;
		dc_SetScalarFields (dc, Nfld, fids);
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
		year = (int) data[3];
		month = (int) data[1];
		day = (int) data[2];
		hour = (int) data[4];
		minute = (int) data[5];
		second = (int) data[6];
		TC_ZtAssemble (&zt, year, month, day, hour, minute,
			second, 0); 
	/*
	 * Extract a location.
	 */
		if (data[7] == 1)
		{
			origin.l_lat = data[8];
			origin.l_lon = data[9];
			origin.l_alt = data[23];
		}
		else if (data[7] == 2)
		{
			origin.l_lat = data[10];
			origin.l_lon = data[11];
			origin.l_alt = data[23];
		}
		else if (data[7] == 4)
		{
			origin.l_lat = data[12];
			origin.l_lon = data[13];
			origin.l_alt = data[23];
		}
		else
		{
		    msg_ELog (EF_PROBLEM, "Unexpected nav value %f", data[7]);
		    continue;
		}
	/*
 	 * Do the rest of the fields.
	 */
		for (i = 0; i < Nfld; i++)
			dc_AddScalar (dc, &zt, 0, fids[i], 
				data + F_OFFSET + i);  
		dc_SetLoc (dc, 0, &origin);
	/*
	 * Store the data. 
	 */
		ds_Store (dc, FALSE, NULL, 0);
		dc_DestroyDC (dc);
	}
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
getfids (fids)
FieldId	*fids;
/*
 * Get id's for Zeb fields.
 */
{
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
/*
 * Fields we derive
 */
	fids[Nfld++] = F_Lookup ("u_wind");
	fids[Nfld++] = F_Lookup ("v_wind");
	fids[Nfld++] = F_Lookup ("theta_e");
	fids[Nfld++] = F_Lookup ("theta_ee");	/* Kerry Emanuel's theta_e */
}



static int
rawget(fptr, data)
FILE	*fptr;
float	*data;
{
	int		bsize, i;
	static int	count = BFACT;
/* 
 * Should we expect an rtape header? 
 */
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
/* 
 * Get the variables.
 */
	for (i = 0; i < N_RAW; i++)
		fscanf (fptr, "%g", data + i);
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
	int	dndx = N_RAW;
	extern float	bolton(), raf_thetae();
/*
 * u wind and v wind
 */
	wspd = data[31];
	wdir = data[32];

	data[dndx++] = wspd * cos (DEG_TO_RAD (270 - wdir));
	data[dndx++] = wspd * sin (DEG_TO_RAD (270 - wdir));
/*
 * equivalent potential temperature (standard calculation)
 */
	p = data[26];
	t = data[27];
	dp = data[28];

	data[dndx++] = bolton (p, t, dp);
/*
 * Kerry Emanuel's equivalent potential temperature (includes liquid water 
 * component)
 */
	l = data[38];	/* liquid water content	*/
	data[dndx++] = raf_thetae (p, t, dp, l);
}
