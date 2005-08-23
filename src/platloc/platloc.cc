//
// platloc: show the latest location of a platform before a user-selected time
// (default now)
//
//		Copyright (C) 2003 by UCAR
//	University Corporation for Atmospheric Research
//		   All rights reserved
//
// No part of this work covered by the copyrights herein may be reproduced
// or used in any form or by any means -- graphic, electronic, or mechanical,
// including photocopying, recording, taping, or information storage and
// retrieval systems -- without permission of the copyright owner.
// 
// This software and any accompanying written materials are provided "as is"
// without warranty of any kind.  UCAR expressly disclaims all warranties of
// any kind, either express or implied, including but not limited to the
// implied warranties of merchantibility and fitness for a particular purpose.
// UCAR does not indemnify any infringement of copyright, patent, or trademark
// through use or modification of this software.  UCAR does not provide 
// maintenance or updates for its software.
//
# include <stdio.h>
# include <unistd.h>
# include <stdlib.h>
# include <time.h>

# include <message.h>
# include <DataStore.h>

void usage(const char* progname);
int msg_handler(Message *msg);

main(int argc, char* argv[])
{
    char* myname = argv[0];
    time_t requestTime = time(0);
    int simpleOutput = 0;

    int c;
    while ((c = getopt(argc, argv, "hst:")) >= 0)
    {
	char* endptr;
	
	switch (c)
	{
	  case 'h':
	    usage(myname);
	    exit(0);
	  case 's':
	    simpleOutput = 1;
	    break;
	  case 't':
	    //
	    // Set a requested data time other than the default of "now"
	    //
	    requestTime = strtol(optarg, &endptr, 0);
	    if (endptr == optarg)
	    {
		fprintf(stderr, "Bad time integer '%s'\n", optarg);
		exit(1);
	    }
	    break;
	}
    }

    if (optind != (argc - 1))
    {
	usage(myname);
	exit(1);
    }

    char* platname = argv[optind];

    //
    // Connect to message system and data store
    //
    if (! msg_connect(msg_handler, myname) || ! ds_Initialize())
    {
	fprintf(stderr, "Error connecting to message or datastore\n");
	exit(1);
    }

    //	
    // Find our platform
    //
    int nplats;
    PlatformId* pids = ds_SearchPlatforms(platname, &nplats, 0, 0);
    if (nplats == 0)
    {
	fprintf(stderr, "Unknown platform '%s'\n", platname);
	exit(1);
    }
    PlatformId pid = pids[0];

    //
    // Find the latest data time at or before the requested time
    //
    ZebraTime rtime, dtime;
    rtime.zt_Sec = requestTime;
    rtime.zt_MicroSec = 0;

    if (! ds_DataTimes(pid, &rtime, 1, DsBefore, &dtime))
    {
	fprintf(stderr, "No data times for %s before %d\n", platname, 
		requestTime);
	exit(1);
    }

    DataChunk *dc = ds_Fetch (pid, DCID_Location, &dtime, &dtime, 
			      0, 0, 0, 0);

    if (! dc)
    {
	fprintf(stderr, "Error getting data for %s at %d\n", platname,
		requestTime);
	exit(1);
    }

    Location loc;
    dc_GetLoc (dc, 0, &loc);
    if (simpleOutput)
	printf("%.4f\t%.4f\t%d\n", loc.l_lat, loc.l_lon, dtime.zt_Sec);
    else
	printf("lat/lon: %.4f/%.4f at %d\n", loc.l_lat, loc.l_lon, 
	       dtime.zt_Sec);
}


void
usage(const char* progname)
{
    printf("Usage: %s [options] platname\n", progname);
    printf("Options:\n");
    printf("   -h             print this usage information and exit\n");
    printf("   -t <time>      specify the desired data time, in seconds\n");
    printf("                  since 1 January 1970, 00:00 UTC.  Default\n");
    printf("                  is the current time.\n");
    printf("   -s             simple output (lat<tab>lon<tab>data time)\n");
    printf("If a position for the platform is available at or before the\n");
    printf("selected time, the latitude/longitude and actual data time are\n");
    printf("printed and a zero status code is returned.  The output looks\n");
    printf("like:\n\n");
    printf("\tlat/lon: 32.1234/-103.4567 at 1052174285\n\n");
    printf("On error, a message will be printed to standard error,\n");
    printf("and a non-zero status code will be generated\n");
}



int 
msg_handler(Message *msg)
{
    fprintf(stderr, "Dropping unexpected message!\n");
    return 0;
}
