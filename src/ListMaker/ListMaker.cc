//
// DSListSource: provide a list of data samples for a given platform, 
// delivered as a series of Zebra MT_COMMAND messages to the destination
// process.
//
// Usage: DSListSource <myname> <destname> <platname> <start_time>
//

# include <stdio.h>
# include <message.h>
# include <DataStore.h>


static int msghandler( Message *msg );


main( int argc, char *argv[] )
{
//
// Get args
//
    if (argc < 4)
    {
	printf( "Usage: %s <myname> <destname> <platname> [<start_time>]\n",
		argv[0] );
	exit( 1 );
    }

    char *myname = argv[1];
    char *destname = argv[2];
    char *platname = argv[3];

    ZebraTime since;

    since.zt_Sec = 0;
    since.zt_MicroSec = 0;
    if (argc == 5)
    {
	double when = atof( argv[4] );

	since.zt_Sec = (long)when;
	since.zt_MicroSec = (long)(1.0e6 * (when - since.zt_Sec));
    }
//
// Establish Zebra connection
//
    if (! msg_connect( msghandler, myname ))
    {
	printf( "ListMaker failed to connect to message service\n" );
	exit( 1 );
    }

    if (! ds_Initialize())
    {
	msg_ELog( EF_PROBLEM, "Failed to initialize datastore");
	exit( 1 );
    }
//
// Get PlatformId
//
    PlatformId pid = ds_LookupPlatform( platname );
    
    if (pid == BadPlatform)
    {
	msg_ELog( EF_PROBLEM, "Bad platform '%s'", platname );
	exit( 1 );
    }
//
// Step through the files in chronological order at and after the chosen time
//
    const DataFile* df;

    for (df = ds_FindDFAfter (SRC_ALL, pid, &since); df; df = ds_NextFile (df))
    {
	char *attrs = "";
	char tstring[32];
	int nsamps, s;
	const int maxsamps = 128;   // in one file
	const ZebraTime* begin = &(df->df_core.dfc_begin);
	ZebraTime times[maxsamps];
	Location locs[maxsamps];
    //
    // Get times and angles for all the samples in this file
    //
     	nsamps = ds_GetObsSamples (pid, begin, times, locs, maxsamps);
    //
    // For each sample in this file...
    //
	for (s = 0; s < nsamps; s++)
	{
	    char attrs[32];
	    char *scantype;
	    DataChunk *dc;
	    double dtime = times[s].zt_Sec + 1.0e-6 * times[s].zt_MicroSec;
	//
	// Skip this sample if it's at or before the specified start time
	//
	    if (TC_Eq (times[s], since))
		continue;
	//
	// Get scan type for this sample
	//
	    dc = ds_Fetch (pid, DCC_Location, times + s, times + s, 0, 0, 
			   0, 0);
	    if (! (scantype = dc_GetSampleAttr (dc, 0, "scan_type")))
		scantype = "unknown";
	    
	    sprintf (attrs, "%.1f %s", locs[s].l_alt, scantype);
	//
	// build a message of:
	//    <time_string> <attribute_string> <time_as_double>
	// and send it to the destination process
	//
	    char tstring[32], msg[128];
	    
	    TC_EncodeTime (times + s, TC_Full, tstring);
	    sprintf( msg, "%s|%s|%.6lf", tstring, attrs, dtime );

	    cp_Exec( destname, msg );
	}
    }
    msg_disconnect();
}



static int
msghandler( Message *msg )
{
    if (msg->m_proto == MT_FINISH)
    {
	msg_disconnect();
	exit( 0 );
    }
    
    return( 0 );
}
