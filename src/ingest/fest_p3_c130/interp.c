/*
 * $Id: interp.c,v 1.2 1992-12-09 23:21:23 granger Exp $
 *
 * Fills in missing samples of 1-second frequency by interpolating
 * locations and filling in fields with bad values.
 * Required by c130_ingest and p3dat_ingest.
 */

#include "ingest.h"

MAKE_RCSID("$Id: interp.c,v 1.2 1992-12-09 23:21:23 granger Exp $")

/*----------------------------------------------------------------
 * A generic location interpolator for aircraft data.
 * Given the last correct time and location before a time gap and
 * the first correct time and location after the gap, samples 
 * are inserted for each missing second using bad values for the
 * fields and interpolated locations.  Upon entry, 'sample' is
 * the number of the latest sample in the DataChunk.  On return,
 * sample has increased by the number of samples written to the
 * DataChunk by this routine.  Errors are logged from here.
 *
 * New concern: it is now possible that we'll get passed bad value
 * flags for locn fields.  If so, bad values are used for all of the
 * interpolated values for that field.
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

	badval = dc_GetBadval(dc);
	when.zt_MicroSec = 0;
	when.zt_Sec = btime->zt_Sec;
#	define BEGIN_LOCN(fld) 	(((blocn->fld == badval) || \
				  (elocn->fld == badval)) ? badval : blocn->fld)
	locn.l_lat = BEGIN_LOCN(l_lat);
	locn.l_lon = BEGIN_LOCN(l_lon);
	locn.l_alt = BEGIN_LOCN(l_alt);
	IngestLog(EF_DEBUG,
		  "%s from %lu to %lu, %i fill-ins, %s %6.5f, %s %6.5f, %s %6.4f",
		  "Interpolating", btime->zt_Sec, etime->zt_Sec,
		  ns, "latstep", latstep, "lonstep", lonstep, "altstep", altstep);
	for (i = 0; i < ns; ++i)
	{
		++(when.zt_Sec);
		for (fld = 0; fld < nfields; ++fld)
		{
			dc_AddScalar(dc, &when, *sample, fields[fld], &badval);
		}

		locn.l_lat = (locn.l_lat == badval) ? badval : locn.l_lat+latstep;
		locn.l_lon = (locn.l_lon == badval) ? badval : locn.l_lon+lonstep;
		locn.l_alt = (locn.l_alt == badval) ? badval : locn.l_alt+altstep;
		dc_SetLoc(dc, *sample, &locn);

		TC_EncodeTime(&when, TC_TimeOnly, ctime);
		IngestLog(EF_DEBUG | ((*sample % 500) ? 0 : EF_INFO),
			  "%s %5i, %s, %.2f lat, %.2f lon, alt %5.3f km",
			  "Interpolated sample",
			  *sample, ctime, 
			  locn.l_lat, locn.l_lon, locn.l_alt);
		
		++(*sample);
	}
}


