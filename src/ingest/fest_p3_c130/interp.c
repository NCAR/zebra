/*
 * $Id: interp.c,v 1.1 1992-11-18 00:32:23 granger Exp $
 *
 * Fills in missing samples of 1-second frequency by interpolating
 * locations and filling in fields with bad values.
 * Required by c130_ingest and p3dat_ingest.
 */

#include "ingest.h"

MAKE_RCSID("$Id: interp.c,v 1.1 1992-11-18 00:32:23 granger Exp $")

/*----------------------------------------------------------------
 * A generic location interpolator for aircraft data.
 * Given the last correct time and location before a time gap and
 * the first correct time and location after the gap, samples 
 * are inserted for each missing second using bad values for the
 * fields and interpolated locations.  Upon entry, 'sample' is
 * the number of the latest sample in the DataChunk.  On return,
 * sample has increased by the number of samples written to the
 * DataChunk by this routine.  Errors are logged from here.
 */
void
InterpolateGap(dc, btime, blocn, etime, elocn, sample)
	DataChunk *dc;
	ZebTime *btime;
	Location *blocn;
	ZebTime *etime;
	Location *elocn;
	int *sample;
{
	FieldId *fields;
	int nfields;
	int ns;			/* # of samples we need to fill in */
	float latstep;		/* step in latitude per fill-in */
	float lonstep;		/* step in longitude per fill-in */
	float altstep;		/* step in altitude per filled-in sample */
	int i;
	int fld;
	Location locn;
	ZebTime when;
	float badval;		/* The value which will be inserted into all of
				 * the fields in the filled-in samples */
	char ctime[30];

	ns = etime->zt_Sec - btime->zt_Sec - 1;
	if (ns < 1)
		return;

	fields = dc_GetFields(dc, &nfields);

	latstep = (elocn->l_lat - blocn->l_lat)/(float)(ns + 1);
	lonstep = (elocn->l_lon - blocn->l_lon)/(float)(ns + 1);
	altstep = (elocn->l_alt - blocn->l_alt)/(float)(ns + 1);

	when.zt_MicroSec = 0;
	when.zt_Sec = btime->zt_Sec + 1;
	locn.l_lat = blocn->l_lat + latstep;
	locn.l_lon = blocn->l_lon + lonstep;
	locn.l_alt = blocn->l_alt + altstep;
	badval = dc_GetBadval(dc);
	IngestLog(EF_DEBUG,
		  "%s from %lu to %lu, %i fill-ins, %s %6.5f, %s %6.5f, %s %6.4f",
		  "Interpolating", btime->zt_Sec, etime->zt_Sec,
		  ns, "latstep", latstep, "lonstep", lonstep, "altstep", altstep);
	for (i = 0; i < ns; ++i)
	{
		TC_EncodeTime(&when, TC_TimeOnly, ctime);
		IngestLog(EF_DEBUG | ((*sample % 500) ? 0 : EF_INFO),
			  "%s %5i, %s, %.2f lat, %.2f lon, alt %5.3f km",
			  "Interpolated sample",
			  *sample, ctime, 
			  locn.l_lat, locn.l_lon, locn.l_alt);

		for (fld = 0; fld < nfields; ++fld)
		{
			dc_AddScalar(dc, &when, *sample, fields[fld], &badval);
		}
		dc_SetLoc(dc, *sample, &locn);
		
		++(when.zt_Sec);
		locn.l_lat += latstep;
		locn.l_lon += lonstep;
		locn.l_alt += altstep;
		++(*sample);
	}
}
