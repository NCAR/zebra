
#include <stdio.h>
#include <message.h>
#include <timer.h>
#include "DataStore.h"
#include "apple.h"



void
Announce(header)
const char *header;
/*
 * Announce beginning of test sequence to event logger and to stdout
 */
{
	msg_ELog (EF_TEST, "%s", (char *) header);
}



PlatformId
NewPlatform (tp)
struct TestPlatform *tp;
{
	PlatformId pid = MakePlatform (tp);
	CleanPlatform (pid);
	return (pid);
}



PlatformId
MakePlatform (tp)
struct TestPlatform *tp;
{
	PlatformId pid;

	if ((pid = ds_LookupPlatform (tp->name)) != BadPlatform)
		return (pid);
	return (DefinePlatform (tp));
}



PlatformId
DefinePlatform (tp)
struct TestPlatform *tp;
/*
 * Given test platform structure, define it.
 */
{
	PlatClassRef pc = ds_NewClass (tp->name);
	PlatClassId cid;

	msg_ELog (EF_DEBUG, "defining platform '%s'", tp->name);
	ds_AssignClass (pc, tp->org, tp->ftype, tp->mobile);
	ds_SetMaxSample (pc, tp->maxsamples);
	ds_SetComment (pc, "automatically defined class");
	cid = ds_DefineClass (pc);
	return (ds_DefinePlatform (cid, ds_ClassName(cid)));
}



void
CleanPlatform (pid)
PlatformId pid;
{
	if (pid != BadPlatform)
	{
		ZebTime now;
		msg_ELog (EF_DEBUG, "cleaning platform '%s'", 
			  ds_PlatformName (pid));
		tl_Time (&now);
		now.zt_MicroSec = 0;
		ds_DeleteData (pid, &now);
	}
}



PlatformId
NeedPlatform (name)
const char *name;
/*
 * Look for this platform name in our table and try to define it, returning
 * its platform id.  Someday maybe we could just make up a name based on the
 * needed characteristics and not require a table.
 */
{
	PlatformId pid;
	int i;

	/*
	 * Try to find it in the table
	 */
	pid = BadPlatform;
	for (i = 0; i < NUM_PLATFORMS; ++i)
	{
		if (! strcmp (TestPlatforms[i].name, name))
			break;
	}
	if (i >= NUM_PLATFORMS)
	{
		if ((pid = ds_LookupPlatform (name)) == BadPlatform)
		{
			msg_ELog (EF_PROBLEM, "needed plat unknown: '%s'", 
				  name);
		}
	}
	else if (TestPlatforms[i].platid == NoPlatform)
	{
		if ((pid = ds_LookupPlatform (name)) == BadPlatform)
		{
			pid = DefinePlatform (TestPlatforms+i);
		}
		if (pid != BadPlatform)	/* clean platforms for first use */
		{
			CleanPlatform (pid);
		}
		TestPlatforms[i].platid = pid;
	}
	else
	{
		pid = TestPlatforms[i].platid;
	}
	return (pid);
}


int
T_CompareDataPrec(float *src1, float *src2, int size, float epsilon)
{
	int i;

	fflush (stdout);
	if (!src1 || !src2)
		return (1);
	for (i = 0; i < size; ++i)
		if (src1[i] - src2[i] > epsilon ||
		    src1[i] - src2[i] > epsilon) break;
	if (i < size)
	    msg_ELog (EF_PROBLEM, "  failed at %i, %f != %f", 
		      i, src1[i], src2[i]);
	else
	    msg_ELog (EF_DEVELOP, "  succeeded at %i", i);
	return ((i < size) ? 1 : 0);
}



int
T_CompareData(src1, src2, size)
float *src1, *src2;
int size;
{
    return T_CompareDataPrec (src1, src2, size, 0);
}



void
T_DumpData (retrieve, n, len, fname) 
float *retrieve;
int n;
int len;
char *fname;
{
	int i, j;

	if (! DumpDataChunks)
		return ;
	printf ("First %d '%s' values (len = %d):\n", n, fname, len);
	for (i = 0; (i < n) && (i < len); ++i)
		printf ("%9.2f%c", retrieve[i], (i+1)%7?' ':'\n');
	printf ("\nLast %d '%s' values (len = %d):\n", n, fname, len);
	for (i = 0, j = len-n+((n<=len)?0:(n-len));
	     (i < n) && (j < len); ++i, ++j)
		printf ("%9.2f%c", retrieve[j], (i+1)%7?' ':'\n');
	printf ("\n");
}




int
T_VerifyObs (pid, begin, end, nsamp)
PlatformId pid;
ZebTime *begin;
ZebTime *end;
int nsamp;
/*
 * Verify that there is an observation over the given times with the given
 * number of samples for the given platform.  Return 0 if everything passes.
 */
{
	int err = 0;
	int nlook, nfound;
	ZebTime *times;
	/*
	 * Look for one more than we expect so we can detect too many.
	 */
	nlook = nsamp + 1;
	times = (ZebTime *) malloc (nlook * sizeof(ZebTime));
	nfound = ds_GetObsSamples (pid, begin, times, NULL, nlook);
	if (nfound != nsamp)
	{
		++err;
		msg_ELog (EF_PROBLEM, "GetObsSamples: %d samples, %d expected",
			  nfound, nsamp);
	}
	if (! TC_Eq (*begin, times[0]))
	{
		++err;
		msg_ELog (EF_PROBLEM, "GetObsSamples: begin times not equal");
	}
	if (end && (! TC_Eq (*end, times[nfound])))
	{
		++err;
		msg_ELog (EF_PROBLEM, "GetObsSamples: end times not equal");
	}
	free (times);
	return (err);
}




void
T_ReceiveNotify (pid, param, when, nsample, ucode)
PlatformId pid;
int param;
ZebTime *when;
int nsample;
UpdCode ucode;
{
	char tbuf[64];
	const char *platname;
	/*
	 * Print lines of the form:
	 *
	 * <platform> <filename> <datadir> <time> <nsamples> <ucode>
	 */
	TC_EncodeTime (when, TC_Full, tbuf);
	platname = ds_PlatformName (pid);
	msg_ELog (EF_PROBLEM,  "NOTIFY %s %s %i %s\n", 
		platname ? platname : "NULL", 
		tbuf, nsample, (ucode == UpdOverwrite) ? "owr" :
		((ucode == UpdInsert) ? "ins" : "app"));
}



int
T_ExpectNotify (pid, when, nsamp, ucode)
PlatformId pid;
ZebTime *when;
int nsamp;
UpdCode ucode;
/*
 * Expect and detect a notification from the daemon on this platform.
 * If 'when' is NULL it will not be checked.  Return the assigned
 * log message expect id.
 */
{
	char regexp[128];
	int eid;

	if (ds_IsStandalone())
		return (0);
	/*
	 * Build the regular expression to find.
	 */
	sprintf (regexp, "NOTIFY %s %s %i %s",
		 ds_PlatformName (pid), ".*", nsamp,
		 (ucode == UpdOverwrite) ? "owr" :
		 ((ucode == UpdInsert) ? "ins" : "app"));
	eid = TX_Catch (regexp);
	return (eid);
}



int
T_CatchNotify ()
/*
 * Verify that an expected notification on a platform was received.
 * Return non-zero if not.
 */
{
	if (ds_IsStandalone())
		return (0);
	/*
	 * We may need to wait for the daemon.
	 */
	while (! TX_Pending ())
	{
		if (msg_poll (5) == MSG_TIMEOUT)
			break;
	}
	return (TX_Caught ());
}




#ifdef ds_Store
#undef ds_Store
#endif
#ifdef ds_Fetch
#undef ds_Fetch
#endif
#ifdef ds_FetchObs
#undef ds_FetchObs
#endif

zbool
T_Store (dc, newfile, details, ndetail)
DataChunk *dc;
int newfile;
dsDetail *details;
int ndetail;
{
	int ret;

	ret = ds_Store (dc, newfile, details, ndetail);
	if (! ret)
	{
		++Errors;
		msg_ELog (EF_PROBLEM, "ds_Store failed for platform %s",
			  ds_PlatformName (dc->dc_Platform));
	}
	return (ret);
}




DataChunk *
T_Fetch (pid,c,begin,when,fields,nfield,details,ndetail)
PlatformId pid;
DataClass c;
ZebTime *begin;
ZebTime *when;
FieldId *fields;
int nfield;
dsDetail *details;
int ndetail;
{
	DataChunk *ret;

	ret = ds_Fetch (pid,c,begin,when,fields,nfield,details,ndetail);
	if (! ret)
	{	++Errors;
		msg_ELog (EF_PROBLEM, "ds_Fetch failed for platform %s",
			  ds_PlatformName (pid));
	}
	return (ret);
}




DataChunk *
T_FetchObs (pid,c,when,fields,nfield,details,ndetail)
PlatformId pid;
DataClass c;
ZebTime *when;
FieldId *fields;
int nfield;
dsDetail *details;
int ndetail;
{
	DataChunk *ret;

	ret = ds_FetchObs (pid,c,when,fields,nfield,details,ndetail);
	if (! ret)
	{
		++Errors;
		msg_ELog (EF_PROBLEM, "ds_FetchObs failed for platform %s",
			  ds_PlatformName (pid));
	}
	return (ret);
}



zbool
TP_Store (dc, newfile, details, ndetail)
DataChunk *dc;
int newfile;
dsDetail *details;
int ndetail;
{
	int ret;

	TP_Push ("Store");
	ret = T_Store (dc, newfile, details, ndetail);
	TP_Pop ();
	return (ret);
}




DataChunk *
TP_Fetch (pid,c,begin,when,fields,nfield,details,ndetail)
PlatformId pid;
DataClass c;
ZebTime *begin;
ZebTime *when;
FieldId *fields;
int nfield;
dsDetail *details;
int ndetail;
{
	DataChunk *ret;

	TP_Push ("Fetch");
	ret = T_Fetch (pid,c,begin,when,fields,nfield,details,ndetail);
	TP_Pop ();
	return (ret);
}




DataChunk *
TP_FetchObs (pid,c,when,fields,nfield,details,ndetail)
PlatformId pid;
DataClass c;
ZebTime *when;
FieldId *fields;
int nfield;
dsDetail *details;
int ndetail;
{
	DataChunk *ret;

	TP_Push ("FetchObs");
	ret = T_FetchObs (pid,c,when,fields,nfield,details,ndetail);
	TP_Pop ();
	return (ret);
}


