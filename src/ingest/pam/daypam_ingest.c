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
# include "../include/defs.h"
# include "../include/message.h"
# include "../include/timer.h"
# include <DataStore.h>
# include <ui_error.h>
# include <mda.h>
# include <station.h>

MAKE_RCSID ("$Id: daypam_ingest.c,v 1.1 1992-11-10 19:21:57 burghart Exp $")

static int incoming FP ((struct message *));
void	Stations FP ((char *));
void	Fields FP ((int, char **));
void	DoSnarf FP ((int));
void	DoRain FP ((int, time, time));
void	DoPres FP ((int, time, time));


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
	modtype	mod;	/* Modification to apply		*/
	field	mfld;	/* id of field needed for mod		*/
	int	offset;	/* time offset (hhmm) for RainAccum and RainRate */
	float	refalt;	/* reference altitude for PresCorr	*/
} Mods[40];


/*
 * Number of stations
 */
int	Nsta;

/*
 * Data for interfacing to various packages.
 */
# define MAX_FIELD 40
# define MAX_PLAT 100
struct dstream *Ds, *Ms;
int Sample, Report, Ngrab, Nds, NField;
station Mda_plats[MAX_PLAT];
field Mda_flds[MAX_FIELD];
float *Data, *Mdata;		/* The data array */
FieldId ZebFields[MAX_FIELD];
PlatformId Plat;
PlatformId SubPlats[MAX_PLAT];
Location Locs[MAX_PLAT];
ZebTime Time;
float *FData[MAX_FIELD];

# define BADVAL -9999.0




main (argc, argv)
int argc;
char **argv;
{
	time t;
	char ourname[40];
/*
 * Basic arg check.
 */
	if (argc < 5)
	{
		printf ("Usage: %s yymmdd platform project fields\n", argv[0]);
		exit (1);
	}
/*
 * Hook into the world.
 */
	usy_init ();
	sprintf (ourname, "%sIngest", argv[2]);
	msg_connect (incoming, ourname);
	ds_Initialize ();
	mda_declare_file ("/data/ppf", MDA_TYPE_DATABASE, MDA_F_PAM,
		"pam", argv[3]);
/*
 * Figure our current time and force an init.
 */
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
		}


	/*
	 * Deal with corrected pressure (cpres<num>)
	 */
		else if (sscanf (fname, "cpres%d", &alt) == 1)
		{
			Mda_flds[NField] = fld_number ("pres");
			Mods[fld].mod = PresCorr;
			Mods[fld].refalt = (float) alt;
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
}




void
DoSnarf (yymmdd)
int	yymmdd;
/*
 * Handle the data snarf for this day.
 */
{	
	int	i, fld, samp_mmss = (Sample/60)*100 + Sample % 60;
	time	begin, end;
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
time	begin, end;
/*
 * Find the rain accumulation or rate for the fndx'th field
 */
{
	/*int	adj = Mods[fndx].offset / 60 * 100 + Mods[fndx].offset % 60;*/
	int	adj = (Mods[fndx].offset/60)*10000 +(Mods[fndx].offset%60)*100;
	int	sta, i, do_rate;
	float	*dp, *mdp, factor;
	struct dstream *dsp;
/*
 * Adjust the begin and end times to the beginning of our delta
 * period
 */
	msg_ELog (EF_DEBUG, "time: %06d %06d,", begin.ds_yymmdd,
			begin.ds_hhmmss);
	pmu_dsub (&begin.ds_yymmdd, &begin.ds_hhmmss, adj);
	pmu_dsub (&end.ds_yymmdd, &end.ds_hhmmss, adj);
	msg_ELog (EF_DEBUG, "time - %d: %06d %06d", adj, begin.ds_yymmdd,
		begin.ds_hhmmss);
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
 * Grab the data
 */
	mda_fetch (Nsta, Ms, &begin, &end, BADVAL, 0);
/*
 * Set up a multiplication factor based on whether we need to convert to
 * rain rate or not
 */
	do_rate = (Mods[fndx].mod == RainRate);
	factor = do_rate ? 60.0 / (float)(Mods[fndx].offset) : 1.0;
/*
 * Find the deltas and multiply by the factor from above
 */
	dp = FData[fndx];
	mdp = Mdata;

	for (i = 0; i < Nsta * Ngrab; i++)
	{
		if (*dp == BADVAL || *mdp == BADVAL)
		{
			*dp++ = BADVAL;
			mdp++;
			continue;
		}

		msg_ELog (EF_DEBUG, "now: %.1f, then: %.1f, ", *dp, *mdp);
		*dp = *dp - *mdp++;
		*dp *= factor;
	/*
	 * Filter out negative values, which are clearly bad.  Also get
	 * rid of zeros -- makes the plots look nicer.
	 */
		if (*dp <= 0.0)
			*dp = BADVAL;
		dp++;
		msg_ELog (EF_DEBUG, "  fixed: %.1f", *(dp-1));
	}
}




void
DoPres (fndx, begin, end)
int	fndx;
time	begin, end;
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
