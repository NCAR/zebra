/*
 * Ingest a day of PAM data into the system.
 *
 * Usage:
 *	daypam_ingest yymmdd zeb-platform project f1 f2 ... fn
 *
 */
/*		Copyright (C) 1987,88,89,90,91 by UCAR
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
# include <math.h>
# include "defs.h"
# include "message.h"
# include "timer.h"
# include <DataStore.h>
# include <ui_error.h>
# include <mda.h>
# include <station.h>

MAKE_RCSID ("$Id: daypam_ingest.c,v 1.4 1993-07-01 20:15:47 granger Exp $")

static int incoming FP ((struct message *));
void	Stations FP ((char *));
void	Fields FP ((int, char **));
void	DoSnarf FP ((int));
void	DoRain FP ((int, UItime, UItime));
void	DoPres FP ((int, UItime, UItime));
void	CalculateRain FP ((int fndx, int ngrab, float *dp, float *mdp));

/*
 * Types of field "modifications" we can apply
 */
typedef enum
{
	None = 0,
	RainAccum,
	RainRate,
	PresCorr
} modtype;

/*
 * Structure to describe a field modification
 */
struct
{
	modtype	mod;	/* Modification to apply			 */
	field	mfld;	/* id of field needed for mod			 */
	int	offset;	/* time offset (mins) for RainAccum and RainRate */
	float	refalt;	/* reference altitude for PresCorr		 */
} Mods[40];


# define MAX_FIELD 40
# define MAX_PLAT 100
# define BADVAL -9999.0
# define DAYPAMDIR "/home/granger/ppf"

int	Nsta;			/* Number of stations */
int	Sample;			/* Sample rate in seconds */
int	Ngrab;			/* Number of samples in a single day */

struct dstream *Ds, *Ms;
int Report;
int Nds, NField;
station Mda_plats[MAX_PLAT];
field Mda_flds[MAX_FIELD];
float *Data, *Mdata;		/* The data array */
FieldId ZebFields[MAX_FIELD];
PlatformId Plat;
PlatformId SubPlats[MAX_PLAT];
Location Locs[MAX_PLAT];
float *FData[MAX_FIELD];


main (argc, argv)
int argc;
char **argv;
{
	UItime t;
	char ourname[40];
/*
 * Basic arg check.
 */
	if (argc < 5)
	{
		printf ("Usage: %s yymmdd platform project fields\n", argv[0]);
#ifdef notdef
		printf ("If 'yymmdd' is '-', the date will be retrieved from\n");
		printf ("the Zeb Timer.\n");
#endif
		exit (1);
	}
/*
 * Hook into the world.
 */
	usy_init ();
	sprintf (ourname, "%sIngest", argv[2]);
	msg_connect (incoming, ourname);
	ds_Initialize ();
	mda_declare_file (DAYPAMDIR, MDA_TYPE_DATABASE, MDA_F_PAM,
		"pam", argv[3]);
#ifdef notdef
/*
 * Figure out the time we will use from the command line
 * and force an init.
 */
	if (!strcmp(argv[1],"-"))
	{
		tl_GetTime (&t);
	}
	else
	{
		t.ds_yymmdd = atoi(argv[1]);
		t.ds_hhmmss = 0;
	}
#endif
	tl_GetTime (&t);
	mda_do_init (t.ds_yymmdd, t.ds_hhmmss, argv[3]);
/*
 * More initialization.
 */
	if ((Plat = ds_LookupPlatform (argv[2])) == BadPlatform)
	{
		printf ("Unknown platform: %s\n", argv[2]);
		exit (1);
	}
/*
 * Figure out our stations and fields.
 */
	Stations (argv[2]);
	Fields (argc - 4, argv + 4);
/*
 * Now we go for it.
 */
	DoSnarf (atoi (argv[1]));

	exit(0);
}




void
Stations (plat)
char *plat;
/*
 * Do station-oriented initialization.
 */
{
	int slist[100], sta, elev, stest, rtest, ngood = 0;
	Location *loc;
	char pname[60];
/*
 * Get our station list and the sample/report information for the first
 * station
 */
	sta_g_slist (slist, &Nsta);
	sta_g_rate (slist[0], &Sample, &Report);
/*
 * Go through and fill in all of our irgrid info.
 */
	for (sta = 0; sta < Nsta; sta++)
	{	
		sprintf (pname, "%s/%s", plat, sta_g_name (slist[sta]));
	/*
	 * Only keep stations that have the same sample time as the first
	 * station
	 */
		sta_g_rate (slist[sta], &stest, &rtest);
		if (stest != Sample)
		{
			msg_ELog (EF_DEBUG, "%s dropped; diff. sample time",
				pname);
			continue;
		}
	/*
	 * Make sure that this station is known to the system.
	 */
		if ((SubPlats[sta] = ds_LookupPlatform (pname)) == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "DS doesn't know station %s",
				pname);
			continue;
		}
		slist[ngood++] = slist[sta];
	/*
	 * Get the rest of the info.
	 */
		loc = Locs + sta;
		sta_g_position (slist[sta], &loc->l_lat, &loc->l_lon, &elev);
		if (loc->l_lon > 0)
			loc->l_lon *= -1;	/* Assume west hemisphere */
		loc->l_alt = ((float) elev)/1000.0;
		Mda_plats[sta] = slist[sta];
	}
/*
 * Adjust the station count to reflect dropped stations
 */
	Nsta = ngood;
/*
 * Convert Sample and Report to seconds
 */
	Sample /= NTICKSEC; 
	Report /= NTICKSEC;

	msg_ELog (EF_DEBUG, "Sample: %i secs,  Report: %i secs,  # stations: %i",
		  Sample, Report, Nsta);
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




void
Fields (nf, fields)
int nf;
char **fields;
/*
 * Deal with the list of fields, and make our dstream structures.
 */
{
	int fld, sta, ominutes, osecs, alt;
	struct dstream *dsp;
	float *datap;
	char rtype, *fname, *zebname, *strchr ();
/*
 * Go through our field list, and validate that we understand each one.
 */
	NField = 0;
	for (fld = 0; fld < nf; fld++)
	{
	/*
 	 * Allow mda/zeb notation.
	 */
		fname = fields[fld];
		if (zebname = strchr (fname, '/'))
			*zebname++ = '\0';
		else
			zebname = fname;

		ZebFields[NField] = F_Lookup (zebname);
		printf ("Field %d: %s (%d)\n", NField, zebname, 
			ZebFields[NField]);
	/*
	 * Initialize to no field modification
	 */
		Mods[fld].mod = None;
	/*
	 * Deal with fields raina<num> and rainr<num>
	 */
		if (sscanf (fname, "rain%c%d", &rtype, &ominutes) == 2)
		{
			osecs = ominutes * 60;

			if ((osecs % Sample) != 0)	
			{
				printf ("Rain offset time %d is not ", osecs);
				printf ("a multiple of the sample time %d\n", 
					Sample);
				continue;
			}

			Mda_flds[NField] = fld_number ("raina");

			switch (rtype)
			{
			    case 'a':
				Mods[fld].mod = RainAccum;
				break;
			    case 'r':
				Mods[fld].mod = RainRate;
				break;
			    default:
				msg_ELog (EF_PROBLEM, "Bad field '%s'", fname);
				continue;
			}
			Mods[fld].offset = ominutes;
			msg_ELog (EF_DEBUG, "mod field %s: %s, offset %i mins",
				  fname, 
				  (Mods[fld].mod == RainAccum)?
				     "rain rate":"rain accumulation",
				  ominutes);
		}
	/*
	 * Deal with corrected pressure (cpres<num>)
	 */
		else if (sscanf (fname, "cpres%d", &alt) == 1)
		{
			Mda_flds[NField] = fld_number ("pres");
			Mods[fld].mod = PresCorr;
			Mods[fld].refalt = (float) alt;
			msg_ELog (EF_DEBUG, 
				  "mod field %s: corrected pressure, ref altitude %f", 
				  fname, Mods[fld].refalt);
		}
	/*
	 * Otherwise assume it's a normal field
	 */
	 	else if (! (Mda_flds[NField] = fld_number (fname)))
		{
			msg_ELog (EF_PROBLEM, "Bad field '%s'", fname);
			continue;
		}
		NField++;
	}
/*
 * Allocate the space we will need for the data.
 */
	Ngrab = 86400 / Sample;		/* samples per day */
	Nds = Nsta * NField;
	Data = (float *) malloc (Nds * Ngrab * sizeof (float));
	Ds = (struct dstream *) malloc (Nds * sizeof (struct dstream));
/*
 * Now we go through and fill in all of the dstreams.
 */
	dsp = Ds;
	for (fld = 0; fld < NField; fld++)
	{
		datap = Data + fld * Ngrab * Nsta;
		FData[fld] = datap;
		for (sta = 0; sta < Nsta; sta++)
		{
			dsp->ds_plat = Mda_plats[sta];
			dsp->ds_field = Mda_flds[fld];
			dsp->ds_data = datap++;
			dsp->ds_stride = Nsta;
			dsp++;
		}
	}
/*
 * Allocate space for a field to be used for field modification
 */
	Mdata = (float *) malloc (Ngrab * Nsta * sizeof (float));
	Ms = (struct dstream *) 
		malloc (Nsta * sizeof (struct dstream));
/*
 * Build the dstreams for the modification field
 */
	dsp = Ms;
	datap = Mdata;
	for (sta = 0; sta < Nsta; sta++)
	{
		dsp->ds_plat = Mda_plats[sta];
		dsp->ds_data = datap++;
		dsp->ds_stride = Nsta;
		dsp++;
	}
	msg_ELog (EF_DEBUG, "samples per day: %i,   # fields: %i",
		  Ngrab, NField);
}




void
DoSnarf (yymmdd)
int	yymmdd;
/*
 * Handle the data snarf for this day.
 */
{	
	int	i, fld;
	int	samp_mmss = (Sample/60)*100 + Sample % 60;
	UItime	begin, end;
	ZebTime	*zt;
	DataChunk *dc;
/*
 * Set up begin and end times
 */
	begin.ds_yymmdd = yymmdd;
	begin.ds_hhmmss = 0;

	end = begin;
	pmu_dadd (&end.ds_yymmdd, &end.ds_hhmmss, 240000);
	pmu_dsub (&end.ds_yymmdd, &end.ds_hhmmss, samp_mmss);
/*
 * Get the data from MDA.
 */
	ERRORCATCH
		mda_fetch (Nds, Ds, &begin, &end, BADVAL, 0);
	ON_ERROR
		ui_epop ();
		return;
	ENDCATCH
/*
 * Test: see if we got all bad flags.  If so, we refuse to do any more.
 */
	for (i = 0; i < Ngrab * Nds; i++)
		if (Data[i] != BADVAL)
			break;
	if (i >= Ngrab * Nds)
		msg_ELog (EF_INFO, "No PAM data for %d!", yymmdd);
/*
 * Do modifications if necessary
 */
	for (fld = 0; fld < NField; fld++)
	{
		switch (Mods[fld].mod)
		{
		    case None:
			break;
		    case RainAccum:
		    case RainRate:
			DoRain (fld, begin, end);
			break;
		    case PresCorr:
			DoPres (fld, begin, end);
			break;
		    default:
			printf ("*BUG* Bad mod type %d\n", Mods[fld].mod);
			exit (1);
		}
	}	
/*
 * Allocate and build the array of times
 */
	zt = (ZebTime *) malloc (Ngrab * sizeof (ZebTime));

	for (i = 0; i < Ngrab; i++)
	{
		TC_UIToZt (&begin, zt + i);
		pmu_dadd (&begin.ds_yymmdd, &begin.ds_hhmmss, samp_mmss);
	}
/*
 * Create and fill in the data chunk.
 */
	dc = dc_CreateDC (DCC_IRGrid);
	dc->dc_Platform = Plat;
	dc_IRSetup (dc, Nsta, SubPlats, Locs, NField, ZebFields);
	dc_SetBadval (dc, BADVAL);
	for (i = 0; i < NField; i++)
		dc_IRAddMultGrid (dc, zt, 0, Ngrab, ZebFields[i], FData[i]);
/*
 * Put it into the data store.
 */
	ds_Store (dc, TRUE, 0, 0);
	dc_DestroyDC (dc);
	free (zt);
}




void
DoRain (fndx, begin, end)
int	fndx;
UItime	begin, end;
/*
 * Find the rain accumulation or rate for the fndx'th field
 */
{
	int	adj = (Mods[fndx].offset/60)*10000 +(Mods[fndx].offset%60)*100;
	int	samp_mmss = (Sample/60)*100 + Sample % 60;
	int	nadj;
	char 	fldname[10];
	float	*dp;
	int	sta, i;
	struct dstream *dsp;
	UItime	mid;

	sprintf(fldname,"rain%c%i",(Mods[fndx].mod == RainAccum)?'a':'r',
		Mods[fndx].offset);
/*
 * Adjust the begin and end times to the beginning of our delta period
 */
	msg_ELog (EF_DEBUG, "calculating %s, time: %06d %06d,", 
		  fldname, begin.ds_yymmdd, begin.ds_hhmmss);
	mid = begin;
	pmu_dsub (&begin.ds_yymmdd, &begin.ds_hhmmss, adj);
	pmu_dsub (&end.ds_yymmdd, &end.ds_hhmmss, adj);
	msg_ELog (EF_DEBUG, "time - %d ==> %06d %06d", adj, 
		  begin.ds_yymmdd, begin.ds_hhmmss);
	pmu_dsub (&mid.ds_yymmdd, &mid.ds_hhmmss, samp_mmss);
	nadj = Mods[fndx].offset * 60 / Sample;
/*
 * Put the field into the dstreams
 */
	dsp = Ms;
	for (sta = 0; sta < Nsta; sta++)
	{
		dsp->ds_field = fld_number ("raina");
		dsp++;
	}
/*
 * We have to fetch the data in two sections.  First fetch
 * the data which falls on yesterday.  If this fetch causes an error,
 * we must assume yesterday's data is not available and the corresponding
 * values for today must be marked bad.  The number of values we'll get
 * should be the number of samples during the offset period, i.e. 'nadj'
 */
	msg_ELog (EF_DEBUG, "getting %i samples from prev day, up to %06d %06d for %s",
		  nadj, mid.ds_yymmdd, mid.ds_hhmmss, fldname);
	dp = FData[fndx];
	ERRORCATCH
		mda_fetch (Nsta, Ms, &begin, &mid, BADVAL, 0);
	        CalculateRain (fndx, nadj, dp, Mdata);
	ON_ERROR
		fprintf(stderr,	"*** using bad values for %i %s samples\n",
			nadj, fldname);
		msg_ELog (EF_PROBLEM,
			  "data fetch failed, %s, %06d %06d - %06d %06d, %s",
			  fldname, begin.ds_yymmdd, begin.ds_hhmmss, 
			  mid.ds_yymmdd, mid.ds_hhmmss, "using bad values");
		for (i = 0; i < nadj * Nsta; i++)
			*dp++ = BADVAL;
	ENDCATCH
/*
 * Now we can grab the rest of the data needed, i.e. all the data from today
 */
	msg_ELog (EF_DEBUG, "getting rest of samples, %i, up to %06d %06d, for %s",
		  Ngrab - nadj, end.ds_yymmdd, end.ds_hhmmss, fldname);
        mid.ds_yymmdd = end.ds_yymmdd;
	mid.ds_hhmmss = 0;
	dp = FData[fndx] + (nadj * Nsta);
	ERRORCATCH
		mda_fetch (Nsta, Ms, &mid, &end, BADVAL, 0);
	        CalculateRain (fndx, Ngrab - nadj, dp, Mdata);
	ON_ERROR
		msg_ELog (EF_PROBLEM,
			  "data fetch failed, %s, %06d %06d - %06d %06d, %s",
			  fldname, mid.ds_yymmdd, mid.ds_hhmmss, 
			  end.ds_yymmdd, end.ds_hhmmss, "using bad values");
		for (i = 0; i < (Ngrab - nadj) * Nsta; i++)
			*dp++ = BADVAL;
	ENDCATCH
}




void
CalculateRain(fndx, ngrab, dp, mdp)
	int fndx;	/* Index of field we're calculating */
	int ngrab;	/* Number of samples in dp[] and mdp[] */
	float *dp;	/* Pointer to field data where results will be stored */
	float *mdp;	/* Offset data values */
{
	int	do_rate;
	float	factor;
	int	i;
	char	msg[100];
/*
 * Set up a multiplication factor based on whether we need to convert to
 * rain rate or not
 */
	do_rate = (Mods[fndx].mod == RainRate);
	factor = do_rate ? 60.0 / (float)(Mods[fndx].offset) : 1.0;
/*
 * Find the deltas and multiply by the factor from above
 */
	for (i = 0; i < Nsta * ngrab; i++)
	{
		if (*dp == BADVAL || *mdp == BADVAL)
		{
			*dp++ = BADVAL;
			mdp++;
			continue;
		}

		sprintf (msg, "now: %5.1f, then: %5.1f, ", *dp, *mdp);
		*dp = *dp - *mdp++;
		*dp *= factor;
	/*
	 * Filter out negative values, which are clearly bad.  Also get
	 * rid of zeros -- makes the plots look nicer.
	 */
		if (*dp <= 0.0)
			*dp = BADVAL;
		dp++;
		msg_ELog (EF_DEBUG, "%s --- fixed: %.1f", msg, *(dp-1));
	}
}




void
DoPres (fndx, begin, end)
int	fndx;
UItime	begin, end;
/*
 * Perform a pressure correction
 */
{
	int	sta, samp;
	float	*p, *vt, num, den;
	float	ref = Mods[fndx].refalt;
	struct dstream *dsp;
/*
 * Put 'vt' into the dstreams
 */
	dsp = Ms;
	for (sta = 0; sta < Nsta; sta++)
	{
		dsp->ds_field = fld_number ("vt");
		dsp++;
	}
/*
 * Grab the data
 */
	mda_fetch (Nsta, Ms, &begin, &end, BADVAL, 0);
/*
 * Do the correction
 */
	p = FData[fndx];
	vt = Mdata;

	for (samp = 0; samp < Ngrab; samp++)
	{
		for (sta = 0; sta < Nsta; sta++)
		{
			if (*p == BADVAL || *vt == BADVAL)
			{
				*p++ = BADVAL;
				vt++;
				continue;
			}
		/*
		 * This algorithm comes from Herzegh's memo dated 18 Mar 88.
		 * The mean virtual temperature between the station altitude 
		 * and the reference altitude is calculated assuming a 
		 * constant lapse rate of 0.0065 deg/m, since it's difficult 
		 * to come up with a true virtual temperature at the 
		 * reference altitude. 
		 */
			num = 2.0 * 9.81 * (1000.0*Locs[sta].l_alt - ref);
			den = 287.0 * (2 * (273.15 + *vt++) +
					(1000.0*Locs[sta].l_alt - ref)*0.0065);
			*p++ *= exp (num/den);
		}
	}
}
