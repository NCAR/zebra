/*
 * Copyright (C) 1987,88,89,90,91 by UCAR University Corporation for
 * Atmospheric Research All rights reserved
 *
 * No part of this work covered by the copyrights herein may be reproduced or
 * used in any form or by any means -- graphic, electronic, or mechanical,
 * including photocopying, recording, taping, or information storage and
 * retrieval systems -- without permission of the copyright owner.
 */

/*****************************************************************************/

/*
 * dsprint -
 *
 * Stolen from NEXUS 26 Jan 1995 and moved to base Zebra distribution.
 * The original NEXUS code is ifdef'ed out with the NEXUS symbol.
 */

/***************************************************************************/

#include <stdio.h>		/* system lib includes	 */
#include <string.h>
#include <signal.h>
#include <ctype.h>
#include <sys/types.h>
# include <time.h>

#include <errno.h>
#include <math.h>

#include <defs.h>		/* Zeb lib includes	 */
#include <message.h>
#include <DataStore.h>
#include <timer.h>

#ifdef NEXUS
#include "nexus.h"
#else
/* from nexus.h */
#define MISSVAL		88888.0	/* NEXUS-specific missing data flags	 */
#endif /* NEXUS */

MAKE_RCSID("$Id: dsprint.c,v 1.14 2002-09-17 18:04:36 granger Exp $")

/*************************************************************
 ANSI C function prototypes
*************************************************************/

/* zeb stuff: */

int MHandler    FP ((Message *));

void field_init FP ((ZebTime *));	/* our data store init. */

/* utility: */

void getopts     FP ((int argc, char *argv[]));	/* grab cmd. line options */

/*************************************************************
  GLOBAL VARIABLES
*************************************************************/

char           *plat_name = NULL;	/* platform name */
PlatformId      pid;		/* platform id for data store catalog */
DataOrganization dorg;		/* platform data organization */
DataClass       dclass;		/* platform data class */
char           *myname;		/* this process name */
char           *field_param;

char           *time_string = NULL;	/* if sample time is specified */

#if defined(SVR4) || defined(SYSV) || defined(linux)
# define USE_STRERROR
#else
extern char    *sys_errlist[];
#endif

enum
{
    OBS_BEGIN, OBS_END, OBS_RETRIEVE, OBS_LIST, OBS_FIRST_GOOD, 
    OBS_LAST_GOOD, OBS_INTERVAL
}
action;		/* type of transaction */

 /*
  * If field limits are specified, store here, and also save the limit field
  * definition
  */

double          lower_limit,
                upper_limit;
FieldId         limit_fid;
char            limit_field[50];

#define MAX_OBS 1000

#define MAX_FIDS 100

#define HEADER_FIELD_WIDTH 10;

char           *field_spec[MAX_FIDS];
char           *field_name[MAX_FIDS];
char           *field_format[MAX_FIDS];
FieldId         fids[MAX_FIDS];
int             field_size[MAX_FIDS];

int             fid_count = 0;

int             print_header = 1;

char            print_age = 0;	/* set if we want theage of the data samples
				 * printed on the data line */
char            print_zt = 0;	/* set if we want the zeb time stamp printed
				 * on the data line */
char            print_stars = 0;/* set if we want stars printed in place of
				 * badval */
char            print_missing = 0;	/* set if we want missingval printed */

int maximum_seconds = 0;
/* if non-zero, keep printing samples from observations until maximum */

/*************************************************************************** */

int
main (argc, argv)
int argc;
char *argv[];
{

    char           *bufptr;
    int             buflen;
    DataChunk      *dc = NULL;
    DataChunk      *dc_limit = NULL;
    DataPtr         ptr;
    ZebTime         sample_time;
    ZebTime         times[MAX_OBS];
    ZebTime         this_time;
    struct timezone tz;
    struct timeval  tv;
    struct tm      *t;

    int             i,
                    j,
                    k,
                    n_times,
                    npts;
    float           val,
                    badval,
                    missval,
                    limit_val;
    char            format_string[200];
    int             start,
                    end;
    int             within_limits;

    missval = MISSVAL;

    /* Hook into zeb. */

    myname = argv[0];

    getopts (argc, argv);

    if (!msg_connect (MHandler, myname)) {
	fprintf (stderr,
	    "%s: Message Handler connect failure, aborting...\n",
	    myname);
	exit (0);
    }
    if (gettimeofday (&tv, &tz)) {
#ifdef USE_STRERROR
	printf ("couldn't get timeofday, errno %d %s\n", errno, strerror(errno));
#else
	printf ("couldn't get timeofday, errno %d %s\n", errno, sys_errlist[errno]);
#endif
	exit (0);
    }
    if (time_string) {
      if (! TC_DecodeTime(time_string, &sample_time))
	{

	    msg_ELog (EF_PROBLEM, "incorrect date,time format\n");
	    exit (0);
	}
    } else {

	TC_SysToZt (tv.tv_sec, &sample_time);
    }

#ifdef NEXUS
    msg_join (NEXUS_GRP);
#endif

    /* data store initialize */
    ds_Initialize ();

    if ((pid = ds_LookupPlatform (plat_name)) == BadPlatform) {
	msg_ELog (EF_PROBLEM,
	    " %s: Unknown platform: %s", myname, plat_name);
	exit (0);
    }
    dorg = ds_PlatformDataOrg (pid);
    if ((dorg == OrgScalar) || (dorg == OrgFixedScalar))
	dclass = DCC_Scalar;
    else
	dclass = DCC_Transparent;

    /* Load up the list of observations before the chosen time. */
    if (!(n_times = ds_GetObsTimes (pid, &sample_time, times, 
				    MAX_OBS, NULL)))
    {
      msg_ELog(EF_PROBLEM, 
	       "unable to find observation before %s",
	       TC_AscTime(&sample_time, TC_Full));
      exit (0);
    }

    switch (action) {

      case OBS_LIST:

	for (i = 0; i < n_times; i++)
	{
	  printf ("%s\n", TC_AscTime(times+i, TC_Full));
	}
	break;

      case OBS_RETRIEVE:
      case OBS_BEGIN:
      case OBS_END:
      case OBS_FIRST_GOOD:
      case OBS_LAST_GOOD:
      case OBS_INTERVAL:

	switch (dclass) {
	  case DCC_Scalar:
	    field_init (&times[0]);
	    if (maximum_seconds > 0)
	    {
	      ZTime first_time = sample_time;
	      first_time.zt_Sec -= maximum_seconds;
	      dc = ds_Fetch (pid, DCC_Scalar, &first_time, &sample_time,
			     (FieldId *) fids, fid_count,
			     (dsDetail *) NULL, 0);
	    }
	    else
	    {
	    dc = ds_FetchObs (pid, DCC_Scalar,
		times,
		(FieldId *) fids, fid_count,
		(dsDetail *) NULL, 0);
	    /* if there are limits on this fetch, get the limiting field */
	    if (action == OBS_INTERVAL) {
		dc_limit = ds_FetchObs (pid, DCC_Scalar,
		    times,
		    (FieldId *) & limit_fid, 1,
		    (dsDetail *) NULL, 0);
	    }
	    }
	    break;
	  case DCC_Transparent:
	    dc = ds_Fetch (pid, DCC_Transparent,
		times, times,
		(FieldId *) NULL, 0,
		(dsDetail *) NULL, 0);
	    break;
	default:
	  break;
	}

	/*
	 * if time was specified, and obs not available at this time, then
	 * complain and exit
	 */

	if (!dc || (time_string && ! TC_Eq(sample_time, times[0])))
	{
	  msg_ELog (EF_PROBLEM,
		    "unable to find entry for %s at %s", 
		    plat_name, TC_AscTime(&sample_time, TC_Full));
	  exit (0);
	}
	if (dc) {

	    npts = dc_GetNSample (dc);
	    badval = dc_GetBadval (dc);

	    switch (action) {
	      case OBS_RETRIEVE:
		start = 0;
		end = npts;
		break;

	      case OBS_INTERVAL:
		start = 0;
		end = npts;
		break;

	      case OBS_BEGIN:
		start = 0;
		end = 1;
		break;

	      case OBS_END:
		start = npts - 1;
		end = npts;
		break;

	      case OBS_FIRST_GOOD:
		i = 0;
		do {
		    for (j = 0; j < fid_count; j++) {
			val = dc_GetScalar (dc, i, fids[j]);
			if (val == badval) {
			    i++;
			    break;
			}
		    }
		} while (j < fid_count && i < npts);
		start = i;
		end = i + 1 < npts ? i + 1 : i;
		break;

	      case OBS_LAST_GOOD:
		i = npts - 1;
		do {
		    for (j = 0; j < fid_count; j++) {
			val = dc_GetScalar (dc, i, fids[j]);
			if (val == badval) {
			    i--;
			    break;
			}
		    }
		} while (j < fid_count && i > -1);
		start = i;
		end = i + 1 > 0 ? i + 1 : i;
		break;
	    }

	    switch (dclass) {
	      case DCC_Scalar:
		if (print_header) {
		    if (print_age)
			printf ("%7s ", "age");

		    if (print_zt)
			printf ("%21s ", "time");

		    for (j = 0; j < fid_count; j++) {
			sprintf (format_string, "%%%ds ", field_size[j]);
			printf (format_string, field_name[j]);
		    }
		    if (fid_count)
			printf ("\n");
		}
		for (i = start; i < end; i++) {

		    /*
		     * if this is a limited search, then check to see that we
		     * are within limits
		     */
		    within_limits = 1;

		    if (action == OBS_INTERVAL) {
			limit_val = dc_GetScalar (dc_limit, i, limit_fid);
			if (limit_val == missval)
			    within_limits = 0;
			else if (limit_val < lower_limit || 
				 limit_val > upper_limit)
			    within_limits = 0;
		    }
		    if (within_limits) {

			/*
			 * if any fields are missing vals, don't print them
			 * unless print_missing is set
			 */
			if (!print_missing) {
			    for (j = 0; j < fid_count; j++)
				if (dc_GetScalar (dc, i, fids[j]) == missval)
				    break;

			    if (j != fid_count)
				continue;
			}
			if (print_age) {
			    dc_GetTime (dc, i, &this_time);
			    printf ("%7.1f ", 
				    (tv.tv_sec - this_time.zt_Sec) / 60.0);
			}
			if (print_zt) {
			    dc_GetTime (dc, i, &this_time);
			    if (print_zt == 1)
			      printf ("     %8ld %06ld ", 
				      (long)this_time.zt_Sec,
				      (long)this_time.zt_MicroSec);
			    else
			      printf ("%21s ", TC_AscTime(&this_time, TC_Full));
			}
			for (j = 0; j < fid_count; j++) {
			    val = dc_GetScalar (dc, i, fids[j]);
			    if (val != badval || !print_stars) {
				sprintf (format_string, "%s ", field_format[j]);
				printf (format_string, val);
			    } else {

				/*
				 * have received badval, so print stars
				 * instead
				 */
				for (k = 0; k < field_size[j]; k++)
				    printf ("*");
				printf (" ");
			    }
			}
			printf ("\n");
		    }
		}
		break;
	      case DCC_Transparent:
		ptr = dc_GetSample (dc, 0, &buflen);
		if (ptr) {
		    fwrite (ptr, buflen, 1, stdout);
		} else {
		    msg_ELog (EF_PROBLEM,
			      "can't access sample 0 in data chunk");
		    exit (0);
		}
		break;
	    }
	}
	break;

    }

    exit (0);
    return (0);
}

/**************************************************************************/
/*
 * field_init() - Set up the field name and formatting information
 */
void
field_init (ztime)
    ZebTime        *ztime;
{
    int             i;
    char            size[200];

    int             k,
                    nf;
    char           *ptr;
    char            ts[80];

    ptr = strtok (field_param, ":");

    while (ptr) {
	field_spec[fid_count] = malloc (strlen (ptr) + 1);
	strcpy (field_spec[fid_count], ptr);
	fid_count++;
	ptr = strtok (NULL, ":");
    }
    /* If no fields specified, then print out all available fields */
    if (fid_count == 0) {
	nf = MAX_FIDS;
	if (ds_GetFields (pid, ztime, &nf, fids))
	  fid_count = nf;
	for (k = 0; k < fid_count; k++) {
	    field_name[k] = malloc (strlen (F_GetName (fids[k])) + 1);
	    strcpy (field_name[k], F_GetName (fids[k]));
	    field_format[k] = malloc (7);
	    strcpy (field_format[k], "%10.7g");
	}

    } else {
	for (k = 0; k < fid_count; k++) {
	    field_format[k] = malloc (strlen (field_spec[k]) + 1);
	    strcpy (field_format[k], "%10.7g");
	    field_name[k] = malloc (strlen (field_spec[k]) + 1);
	    ptr = strtok (field_spec[k], "(");
	    if (!ptr) ptr = field_spec[k];
	    strcpy (field_name[k], ptr);

	    if (ptr != field_spec[k])
	    {
	      ptr = strtok (NULL, ")");
	      strcpy (field_format[k], ptr);
	    }
	    fids[k] = F_Lookup (field_name[k]);
	}
    }
    for (k = 0; k < fid_count; k++) {

	field_size[k] = HEADER_FIELD_WIDTH;

	if (ptr = strchr (field_format[k], '%')) {

	    i = 0;
	    ptr++;
	    while (isdigit (*ptr)) {
		size[i] = *ptr;
		i++;
		ptr++;

	    }
	    size[i] = 0;
	    field_size[k] = atoi (size);

	}
    }

    /* establish the limit fiels as well */
    if (action == OBS_INTERVAL)
	limit_fid = F_Lookup (limit_field);

}

/****************************************************************************/
int
MHandler (msg)
Message *msg;
/*
 * process incoming zeb IPC messages.
 */

{
    struct mh_template *tmpl;
    int             save_verbose;
    char            station_alt[100];

    switch (msg->m_proto) {
      case MT_MESSAGE:
	tmpl = (struct mh_template *) msg->m_data;
	if (tmpl->mh_type == MH_SHUTDOWN)
	    exit (0);
	break;

#ifdef NEXUS
      case MT_NEXUS:
	tmpl = (struct mh_template *) msg->m_data;
	switch (tmpl->mh_type) {
	  case MH_DEBUG:
	    break;

	  case MH_MURDER:
	    exit (0);
	  default:
	    break;
	}
#endif /* NEXUS */
      default:
	break;
    }

    return (0);
}


void
usage (myname)
char *myname;
{
	printf ("usage: %s -p <plat> <action> %s\n",
		 myname, "[options]");
	printf ("where:\n");
	printf ("   <plat> is the platform of interest, and\n");
	printf ("   <action> must be one of the following:\n");
	printf ("      -r    Retrieve the observation\n");
	printf ("      -b    Beginning of observation\n");
	printf ("      -e    End of observation\n");
	printf ("      -x <n> Print from this number of seconds before\n");
	printf ("      -l    List observations\n");
	printf ("      -g <first|last>\n");
	printf ("            Either the first or last good observation\n");
	printf ("      -i field:start:stop\n");
	printf ("available options:\n");
	printf ("   -f field(format):...\n");
	printf ("   -t dd-mmm-yy,hh:mm:ss\n");
	printf ("            Select the observation to print by given time.\n");
	printf ("   -n       Do not print the header line.\n");
	printf ("   -a       Print the age of the samples.\n");
	printf ("   -d       Print the sample time in date and time format.\n");
	printf ("   -z       Print zebra time as seconds and microsecs.\n");
	printf ("   -s       Print stars in place of bad values.\n");
	printf ("   -m       Print missing data.\n");
	printf ("   -h       This message.\n");
}


/******************************************************************************

 Function: get command line options

 Called by: main

******************************************************************************/


void
getopts (argc, argv)
int argc;
char *argv[];
{
    int             c;
    int             err = 0;
    extern char    *optarg;
    extern int      optind;
    int             iaction = 0;
    char           *str;
    char            limit_string[100];
    double          tempd;

    while ((c = getopt (argc, argv, "p:rx:lf:t:benazdg:smi:h")) != -1) {
	switch (c) {

	  case 'h':
	    usage (argv[0]);
	    exit (0);

	  case 'm':
	    print_missing = 1;
	    break;

	  case 's':
	    print_stars = 1;
	    break;

	  case 'z':
	    print_zt = 1;
	    break;

	  case 'd':
	    print_zt = 2;
	    break;

	  case 'a':
	    print_age = 1;
	    break;

	  case 'b':
	    action = OBS_BEGIN;
	    iaction++;
	    break;

	  case 'e':
	    action = OBS_END;
	    iaction++;
	    break;

	  case 'i':
	    action = OBS_INTERVAL;
	    iaction++;
	    /* next token must contain field:limit1:limit2 */
	    strcpy (limit_string, optarg);

	    /* get the limit field name */
	    str = strtok (limit_string, ":");
	    if (!str) {
		err++;
		break;
	    }
	    strcpy (limit_field, str);

	    /* get the lower limit */
	    str = strtok (NULL, ":");
	    if (!str) {
		err++;
		break;
	    }
	    lower_limit = atof (str);

	    /* get the upper limit */
	    str = strtok (NULL, ":");
	    if (!str) {
		err++;
		break;
	    }
	    upper_limit = atof (str);

	    /* swap if reversed order */
	    if (lower_limit > upper_limit) {
		tempd = upper_limit;
		upper_limit = lower_limit;
		lower_limit = tempd;
	    }
	    break;

	  case 'p':
	    plat_name = optarg;	/* the ptu data store platform name */
	    break;

	  case 'r':
	    action = OBS_RETRIEVE;
	    iaction++;
	    break;

	  case 'x':
	    action = OBS_RETRIEVE;
	    iaction++;
	    maximum_seconds = atoi(optarg);
	    break;

	  case 'l':
	    action = OBS_LIST;
	    iaction++;
	    break;

	  case 'f':
	    field_param = optarg;
	    break;

	  case 't':
	    time_string = optarg;
	    break;

	  case 'n':
	    print_header = 0;
	    break;

	  case 'g':
	    if (strcmp (optarg, "last") == 0) {
		action = OBS_LAST_GOOD;
		iaction++;
	    } else {
		action = OBS_FIRST_GOOD;
		iaction++;
	    }
	    break;

	  default:
	    err++;

	}

    }				/* endw */

    if (!plat_name || (iaction != 1))
	err++;

    if (err) {
	usage (myname);
	exit (9);
    }
}
