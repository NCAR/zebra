/*
 * Quick and dirty field derivation verification
 */

# include <math.h>	/* for fabs() */
# include <message.h>
# include "DataStore.h"
# include "apple.h"

RCSID("$Id: T_Derivations.c,v 3.1 1997-11-21 20:36:36 burghart Exp $")

# define NTIMES 100

/*
 * Precomputed theta_e, in degrees Fahrenheit, using the formulas in 
 * Derivations.def as of 13 November 1997, and a platform derivation file
 * for sgpaerich1C1.a1 containing:
 *
 *	[T][degC] = thermistor0;
 *	[P][hPa] = pressure;
 *	[rh][%] = humidity_relative;
 */
float Theta_e[NTIMES] = 
{
    111.095, 111.210, 110.781, 108.229, 108.690, 110.013, 108.886, 107.996,
    108.721, 108.440, 108.193, 108.264, 108.486, 108.054, 106.258, 105.315,
    105.065, 104.572, 104.542, 104.886, 104.565, 103.867, 104.561, 104.258,
    103.487, 103.032, 103.338, 103.630, 103.486, 103.397, 102.381, 102.492,
    102.453, 102.357, 102.218, 101.839, 102.046, 102.102, 102.013, 101.945,
    101.877, 101.747, 101.762, 101.741, 101.985, 102.167, 101.706, 101.831,
    101.444, 101.148, 101.173, 101.023,  99.838,  99.986, 100.479,  99.831,
     99.773, 100.050,  99.274,  99.772,  99.571,  99.151, 102.549, 100.412,
     99.777,  99.808,  99.927,  99.144,  99.073,  98.956, 103.249, 101.006,
     99.831,  99.982,  99.419,  98.781,  99.121,  99.090,  98.815,  99.060,
    114.589, 117.663, 117.849, 120.679, 125.095, 136.355, 127.397, 135.804,
    146.128, 140.321, 150.268, 155.865, 169.791, 165.534, 163.253, 193.044,
    190.484, 200.631, 208.224, 220.511
};



static int
T_Derivations ()
{
    DataChunk *dc;
    PlatformId pid = ds_LookupPlatform ("sgpaerich1C1.a1");
    FieldId want_fld = F_Field (0, "theta_e", 0, "F");
    ZebraTime now, times[NTIMES];
    float *data;
    int i, err;
/*
 * Get the times for the last 100 samples in the sgpaerich1C1.a1 file
 */
    TC_SysToZt (time(0), &now);
    if (ds_DataTimes (pid, &now, NTIMES, DsBefore, times) != NTIMES)
    {
	msg_ELog (EF_PROBLEM, "Didn't get the expected %d sample times!",
		  NTIMES);
	return (1);
    }
/*
 * Get the (derived) theta_e data
 */
    dc = ds_Fetch (pid, DCC_NSpace, &(times[NTIMES - 1]), &(times[0]), 
		   &want_fld, 1, NULL, 0);
    data = (float*) dc_GetScalarData (dc, 0, want_fld);
/*
 * Compare to our precomputed values
 */
    err = 0;
    for (i = 0; i < NTIMES; i++)
    {
	if (fabs (data[i] - Theta_e[i]) > 0.001)
	{
	    msg_ELog (EF_PROBLEM, "Wrong theta_e at %d: %.3f != %.3f",
		      i, data[i], Theta_e[i]);
	    err++;
	}
    }

    return (err);
}

		

TestRoutine DerivationTests[] = 
{
	{ "derivations", FTUnknown, DCC_None, TR_BEGIN, T_Derivations,
	  "check for correct field derivation" },
	END_TESTS
};



