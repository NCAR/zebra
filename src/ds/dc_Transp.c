/*
 * The definition of the transparent data object class.
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
# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "DataChunk.h"
# include "DataChunkP.h"
MAKE_RCSID ("$Id: dc_Transp.c,v 1.4 1992-01-22 23:22:58 corbet Exp $")

/*
 * TODO:
 *	Easy way to add lots of samples at once
 *	Time-sorting of added samples?
 *	Location frob
 */

static DataChunk 	*Dc_TrCreate FP((DataClass));
static void 		dc_TrDump FP((DataChunk *));
static PlatformId	*dc_MakePlats FP((DataChunk *));
static void		dc_MorePlats FP((DataChunk *, int));
static void		dc_MoreLocs FP((DataChunk *, int));
/*
 * The basic methods structure.
 */
# define SUPERCLASS DCC_Raw

RawDCClass TranspMethods =
{
	"Transparent",
	SUPERCLASS,		/* Superclass			*/
	Dc_TrCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	dc_TrDump,		/* Dump				*/
};


/*
 * Structures for keeping track of sampling information in transparent
 * data objects.
 *
 * The following is used to keep track on one sample.  This can become
 * inefficient for large data chunks containing lots of small samples,
 * such as a day's worth of PAM data.  If that becomes a problem, this
 * will have to become smarter.
 */
typedef struct _TransSample
{
	ZebTime	ats_Time;	/* Time of this sample		*/
	int	ats_Offset;	/* Offset into data array	*/
	int	ats_Len;	/* Length of this sample	*/
} TransSample;

/*
 * The actual AuxData structure which deals in samples is this:
 */
typedef struct _AuxTrans
{
	short	at_NSample;	/* Number of samples in this DC		*/
	short	at_NSampAlloc;	/* Space allocated for this many	*/
	TransSample at_Samples[1];	/* Description of each sample	*/
	Location at_SLoc;	/* Location for static platforms	*/
} AuxTrans;


/*
 * AuxData codes.
 */
# define ST_SAMPLES	1	/* Sample locations and sizes.		*/
# define ST_PLATFORMS	2	/* Optional platform list		*/
# define ST_LOCATIONS	3	/* Locations for mobile plats		*/

/*
 * Local routines.
 */
static AuxTrans * dc_TrMoreSamples FP ((DataChunk *, AuxTrans *, int));
static int	dc_TrMoreData FP ((DataChunk *, int));


static DataChunk *
Dc_TrCreate (class)
DataClass class;
/*
 * Create an transparent data object.
 */
{
	DataChunk *dc;
	AuxTrans *tp;
/*
 * Start by creating a superclass data object.
 */
	dc = dc_CreateDC (SUPERCLASS);
/*
 * Allocate an initial AuxData structure with space to hold one sample.
 * It might be better, in the long run, to hold off on this until somebody
 * starts adding data.
 */
	tp = ALLOC (AuxTrans);
	tp->at_NSample = 0;
	tp->at_NSampAlloc = 1;
	dc_AddADE (dc, (DataPtr) tp, DCC_Transparent, ST_SAMPLES,
			sizeof (AuxTrans), TRUE);
/*
 * Done.
 */
	dc->dc_Class = class;
	return (dc);
}





int
dc_GetNSample (dc)
DataChunk *dc;
/*
 * Return the number of samples to be found in this data chunk.
 */
{
	AuxTrans *tp;
/*
 * Checking.
 */
	if (! dc_IsSubClassOf (dc->dc_Class, DCC_Transparent))
	{
		msg_ELog (EF_PROBLEM, "Tried to get NSample of class %d",
				dc->dc_Class);
		return (0);
	}
/*
 * Find our data and return the info.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return (0);
	return (tp->at_NSample);
}





void
dc_SetStaticLoc (dc, loc)
DataChunk *dc;
Location *loc;
/*
 * Set the location for a static data chunk.
 */
{
	AuxTrans *tp;
/*
 * Checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "SetStaticLoc"))
		return;
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return;
	tp->at_SLoc = *loc;
}




void
dc_SetLoc (dc, sample, loc)
DataChunk *dc;
int sample;
Location *loc;
/*
 * Set the location for this sample.
 */
{
	AuxTrans *tp;
	Location *loclist;
/*
 * Checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "SetLoc"))
		return;
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return;
/*
 * The sample has to exist first.
 */
	if (sample < 0 || sample >= tp->at_NSample)
	{
		msg_ELog (EF_PROBLEM, "Try to set loc on sample %d of %d",
			sample, tp->at_NSample);
		return;
	}
/*
 * Look for the sample list; if it does not yet exist, create it.
 */
	loclist = (Location *) dc_FindADE (dc, DCC_Transparent,ST_LOCATIONS,0);
	if (loclist == NULL)
	{
		int len = tp->at_NSample * sizeof (Location);
		loclist = (Location *) malloc(len);
		memset (loclist, 0, len);
		dc_AddADE (dc, loclist, DCC_Transparent, ST_LOCATIONS, 
				len, TRUE);
	}
/*
 * Now we just store the location.
 */
	loclist[sample] = *loc;
}






void
dc_AddSample (dc, t, data, len)
DataChunk *dc;
ZebTime *t;
DataPtr data;
int len;
/*
 * Add some data to this data chunk.
 * Entry:
 *	DC	is a datachunk which is a subclass of Transparent
 *	T	is the time of the new sample
 *	DATA	is the new sample data.  If DATA is NULL, the data array in
 *		the data chunk will remain uninitialized.
 *	LEN	is the length of DATA
 * Exit:
 *	The new sample has been added to this data chunk.
 */
{
	AuxTrans *tp;
	int offset, ns;
/*
 * The obbligatory class check.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "Add sample"))
		return;
/*
 * Find our data.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return;
	}
/*
 * If our chunk lacks space for the sample, add it now.
 */
	if (tp->at_NSample >= tp->at_NSampAlloc)
		tp = dc_TrMoreSamples (dc, tp, 1);
/*
 * Create more data space in the data chunk itself.
 */
	offset = dc_TrMoreData (dc, len);
/*
 * Fill thing in, and we are done.
 */
	ns = tp->at_NSample++;
	tp->at_Samples[ns].ats_Time = *t;
	tp->at_Samples[ns].ats_Offset = offset;
	tp->at_Samples[ns].ats_Len = len;
	if (data && len > 0)
		memcpy ((char *) dc->dc_Data + offset, data, len);
}





static AuxTrans *
dc_TrMoreSamples (dc, tp, n)
DataChunk *dc;
AuxTrans *tp;
int n;
/*
 * Augment the description space of this data chunk to be able to hold
 * at least "n" more samples.
 */
{
	int nnew = tp->at_NSampAlloc + 2*n, len;
	AuxTrans *newtp;
/*
 * Allocate a new set of data, adjust it, and tweak the accounting.
 */
	len = sizeof (AuxTrans) + (nnew - 1)*sizeof (TransSample);
	newtp = (AuxTrans *) realloc (tp, len);
	newtp->at_NSampAlloc = nnew;
	dc_ChangeADE (dc, (DataPtr) newtp, DCC_Transparent, ST_SAMPLES, len);
/*
 * Make sure there is platform and location space too.
 */
	dc_MorePlats (dc, nnew);
	dc_MoreLocs (dc, nnew);
	return (newtp);
}




static void
dc_MorePlats (dc, n)
DataChunk *dc;
int n;
/*
 * Make sure we have space to store this many platform ID's.
 */
{
	PlatformId *list;
	int old, samp;
/*
 * Get the current list.  If it doesn't exist, there is no work to do.
 */
	if ((list = (PlatformId *) dc_FindADE (dc, DCC_Transparent,
						ST_PLATFORMS, &old)) == NULL)
		return;
/*
 * Make the list bigger, and default the entries to the base platform.
 */
	old /= sizeof (PlatformId);
	list = (PlatformId *) realloc (list, n*sizeof (PlatformId));
	for (samp = old; samp < n; samp++)
		list[samp] = dc->dc_Platform;
	dc_ChangeADE (dc, (DataPtr) list, DCC_Transparent, ST_PLATFORMS,
			n*sizeof (PlatformId));
}





static void
dc_MoreLocs (dc, n)
DataChunk *dc;
int n;
/*
 * Make sure we have space to store this many locations.
 */
{
	Location *locs;
	int old, samp;
/*
 * Get the current list.  If it doesn't exist, there is no work to do.
 */
	if ((locs = (Location *) dc_FindADE (dc, DCC_Transparent,
						ST_LOCATIONS, &old)) == NULL)
		return;
/*
 * Make the list bigger.
 */
	old /= sizeof (Location);
	locs = (Location *) realloc (locs, n*sizeof (Location));
	dc_ChangeADE (dc, (DataPtr) locs, DCC_Transparent, ST_LOCATIONS,
			n*sizeof (Location));
}





static int
dc_TrMoreData (dc, len)
DataChunk *dc;
int len;
/*
 * Increase the available data space by LEN.  The return value is the
 * offset to the beginning of the new space.  This routine exists to make
 * it easy to do caching of data space later on, if we want, to avoid
 * reallocs.
 */
{
	int offset = dc->dc_DataLen;

/*
 * Check to see that they really want more stuff, then add it if so.
 */
	if (len > 0)
		Dc_RawAdd (dc, len);
	return (offset);
}






bool
dc_GetTime (dc, sample, t)
DataChunk *dc;
int sample;
ZebTime *t;
/*
 * Return the time of the given sample.
 */
{
	AuxTrans *tp;
/*
 * The obbligatory class check.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "Get sample"))
		return (FALSE);
/*
 * Find our data.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return (FALSE);
	}
/*
 * Make sure the sample exists.  If so, return the info.
 */
	if (sample < 0 || sample >= tp->at_NSample)
		return (FALSE);
	*t = tp->at_Samples[sample].ats_Time;
	return (TRUE);
}





DataPtr
dc_GetSample (dc, sample, len)
DataChunk *dc;
int sample, *len;
/*
 * Locate a sample within this data chunk.
 * If SAMPLE exists then
 *	The return value is a pointer to the beginning of the sample data
 *	if LEN is non-NULL, it is set to the length of the sample
 * else
 *	The return value is NULL.
 */
{
	AuxTrans *tp;
/*
 * The obbligatory class check.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "Get sample"))
		return (NULL);
/*
 * Find our data.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return (NULL);
	}
/*
 * Make sure the sample exists.  If so, return the info.
 */
	if (sample < 0 || sample >= tp->at_NSample)
		return (NULL);
	if (len)
		*len = tp->at_Samples[sample].ats_Len;
	return ((DataPtr) ((char *) dc->dc_Data +
			tp->at_Samples[sample].ats_Offset));
}





static void
dc_TrDump (dc)
DataChunk *dc;
/*
 * Dump out this data chunk.
 */
{
	AuxTrans *tp;
	int i;
	char atime[40];
/*
 * The obbligatory class check.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "Get sample"))
		return;
/*
 * Find our data.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return;
	}
/*
 * Go for it.
 */
	printf ("TRANSPARENT class, %d samples of %d allocated\n",
			tp->at_NSample, tp->at_NSampAlloc);
	for (i = 0; i < tp->at_NSample; i++)
	{
		TransSample *ts = tp->at_Samples + i;
		TC_EncodeTime (&ts->ats_Time, TC_Full, atime);
		printf ("\t%2d at %s, len %d offset %d\n", i,
			atime, ts->ats_Len, ts->ats_Offset);
	}
}





void
dc_SetPlat (dc, sample, plat)
DataChunk *dc;
int sample;
PlatformId plat;
/*
 * Set the platform ID for this sample.
 */
{
	PlatformId *pids;
	int len, nsamp;
/*
 * Checking.
 */
	if (! dc_IsSubClassOf (dc->dc_Class, DCC_Transparent))
	{
		msg_ELog (EF_PROBLEM, "Tried to get NSample of class %d",
				dc->dc_Class);
		return;
	}
/*
 * Make sure this sample exists.
 */
	nsamp = dc_GetNSample (dc);
	if (sample >= nsamp || sample < 0)
	{
		msg_ELog (EF_PROBLEM, "Attempt to set plat on samp %d, max %d",
				sample, nsamp);
		return;
	}
/*
 * Retrieve the platform list.  If it does not exist, then we need to
 * create one.
 */
	if ((pids = (PlatformId *) dc_FindADE (dc, DCC_Transparent,
						ST_PLATFORMS, &len)) == NULL)
		pids = dc_MakePlats (dc);
/*
 * Now we just store the value.
 */
	pids[sample] = plat;
}





static PlatformId *
dc_MakePlats (dc)
DataChunk *dc;
/*
 * Create a platform list for this data chunk.
 */
{
	PlatformId *list;
	int samp, nsamp = dc_GetNSample (dc);
/*
 * Allocate the platform list, and initialize it to the base platform of
 * this data chunk.
 */
	list = (PlatformId *) malloc (nsamp * sizeof (PlatformId));
	for (samp = 0; samp < nsamp; samp++)
		list[samp] = dc->dc_Platform;
/*
 * Add it to the data chunk.
 */
	dc_AddADE (dc, (DataPtr) list, DCC_Transparent, ST_PLATFORMS,
		nsamp*sizeof (PlatformId), TRUE);
	return (list);
}





PlatformId
dc_GetPlat (dc, sample)
DataChunk *dc;
int sample;
/*
 * Get the platform associated with this sample.
 */
{
	PlatformId *list;
/*
 * Checking.
 */
	if (! dc_IsSubClassOf (dc->dc_Class, DCC_Transparent))
	{
		msg_ELog (EF_PROBLEM, "Tried to get NSample of class %d",
				dc->dc_Class);
		return (0);
	}
	if (sample < 0 || sample >= dc_GetNSample (dc))
		return (BadPlatform);
/*
 * Fetch the platformID list.  If it doesn't exist, then everything is
 * owned by the base platform.
 */
	if ((list = (PlatformId *) dc_FindADE (dc, DCC_Transparent,
						ST_PLATFORMS, NULL)) == NULL)
		return (dc->dc_Platform);
	return list[sample];
}






void
dc_GetLoc (dc, sample, loc)
DataChunk *dc;
int sample;
Location *loc;
/*
 * Get the location corresponding to this sample.
 */
{
	Location *locs;
	AuxTrans *tp;
/*
 * The obbligatory class check.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "GetLoc"))
		return;
/*
 * Find our data.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return;
	}
/*
 * Look for the location list.  If it is not there, we return the static
 * location.
 */
	if (! (locs = (Location *) dc_FindADE (dc, DCC_Transparent,
			ST_LOCATIONS, (int *) 0)))
	{
		*loc = tp->at_SLoc;
		return;
	}
/*
 * For sample-indexed locations, make sure the sample is not bogus.
 */
	if (sample < 0 || sample >= tp->at_NSample)
	{
		msg_ELog (EF_PROBLEM, "Try to getLoc on sample %d of %d",
			sample, tp->at_NSample);
		return;
	}
/*
 * Return the info.
 */
	*loc = locs[sample];
}






void
dc_AdjustSample (dc, sample, newsize)
DataChunk *dc;
int sample, newsize;
/*
 * Adjust the size of this sample.
 */
{
	AuxTrans *tp;
	int i, diff, oldlen = dc->dc_DataLen;
	TransSample *ts;
/*
 * The obbligatory class check.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent,"Adjust sample"))
		return;
/*
 * Find our data.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return;
	}
/*
 * Make sure the sample exists.
 */
	if (sample < 0 || sample >= tp->at_NSample)
	{
		msg_ELog (EF_PROBLEM, "Adjust on nonexistent sample %d",
				sample);
		return;
	}
/*
 * If we are making it smaller (will probably never happen), just adjust
 * the size and quit.  This leaves a hole in the allocated data, but it
 * is not worth the trouble to copy it.
 */
	ts = tp->at_Samples + sample;
	if (newsize < ts->ats_Len)
	{
		ts->ats_Len = newsize;
		return;
	}
/*
 * We are expanding.  Allocate the new space and shift everything down.  We
 * could get badly burned by a braindamaged malloc here, so use bcopy for 
 * now, which claims to do this right.
 */
	diff = newsize - ts->ats_Len;
	Dc_RawAdd (dc, diff);
	if ((sample + 1) < tp->at_NSample)
	{
		bcopy ((char *) dc->dc_Data + ts[1].ats_Offset,
			(char *) dc->dc_Data + ts[1].ats_Offset + diff,
			oldlen - ts[1].ats_Offset);
		for (i = sample + 1; i < tp->at_NSample; i++)
			tp->at_Samples[i].ats_Offset += diff;
	}
	ts->ats_Len = newsize;
}



# ifdef notdef

int
dc_SetUniformSamples (dc, nsample, size)
DataChunk *dc;
int nsample, size;
/*
 * Set this DC up to contain NSAMPLE additional samples, all of the
 * given SIZE.  The return value is the offset to the first of the
 * new samples.
 */
{
	AuxTrans *tp;
	int offset, samp;
/*
 * Sanity Claus.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "SetupSamples"))
		return (0);
/*
 * Find our data.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return;
	}
/*
 * If our chunk lacks space for the sample, add it now.
 */
	if ((tp->at_NSample + nsample) >= tp->at_NSampAlloc)
		tp = dc_TrMoreSamples (dc, tp, tp->at_NSample + nsample - 
						tp->tp_at_NSampAlloc);
/*
 * Create more data space in the data chunk itself.
 */
	offset = dc_TrMoreData (dc, nsample * size);
/*
 * Now set up all of the sample pointers.
 */
	for (samp = 0; samp < nsample; samp++)
	{
		int index = tp->at_NSample + samp;
		


# endif
