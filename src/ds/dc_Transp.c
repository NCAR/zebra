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
# ifdef notdef
#ifdef SVR4
# include <string.h>
#endif
# endif

MAKE_RCSID ("$Id: dc_Transp.c,v 1.14 1994-01-26 11:24:33 granger Exp $")

/*
 * TODO:
 *	Easy way to add lots of samples at once
 *	Time-sorting of added samples?
 *	Location frob
 */

static DataChunk 	*Dc_TrCreate FP((DataClass));
static void 		dc_TrDump FP((DataChunk *));
/*
 * The basic methods structure.
 */
# define SUPERCLASS DCC_Raw

RawDCClass TranspMethods =
{
	"Transparent",
	SUPERCLASS,		/* Superclass			*/
	1,			/* Depth, Raw = 0		*/
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
	unsigned short at_NSample;	/* Number of samples in this DC	 */
	unsigned short at_NSampAlloc;	/* Space allocated for this many */
	unsigned short at_HintNSample;	/* estimated # of samples to store */
	unsigned short at_HintSampSize;	/* estimate of a single sample's size*/
	unsigned short at_HintUseAvgs;	/* use average sample size as needed */
	unsigned short at_SampOverhead;	/* sample size overhead of subclasses*/
	unsigned short at_SampDataSize;	/* hint for size of data in a sample */
	long at_NextOffset;		/* Next offset into buffered raw data,
					   equals dc_DataLen if no buffer */
	Location at_SLoc;		/* Location for static platforms */
	TransSample at_Samples[1];	/* Description of each sample	 */
} AuxTrans;


/*
 * AuxData codes.
 */
# define ST_SAMPLES	1	/* Sample locations and sizes.		*/
# define ST_PLATFORMS	2	/* Optional platform list		*/
# define ST_LOCATIONS	3	/* Locations for mobile plats		*/
/* XXX Make sure any others are less than ST_ATTR! */
# define ST_ATTR	1000	/* Per-sample attributes		*/


/*
 * Local routines.
 */
static PlatformId	*dc_MakePlats FP((DataChunk *));
static void		dc_MorePlats FP((DataChunk *, int));
static void		dc_MoreLocs FP((DataChunk *, int));
static AuxTrans * 	dc_TrMoreSamples FP ((DataChunk *, AuxTrans *, int));
static int		dc_TrMoreData FP ((DataChunk *, AuxTrans *, int));
static int		dc_PrintSaAttr FP ((char *key, void *value, int nval,
					    DC_ElemType type, void *arg));
static int		dc_TrCompareSamples FP((const void *, const void *));
static int		dc_TrGrowthHint FP((DataChunk *dc, AuxTrans *tp, int));
static int		dc_AvgSampleSize FP((DataChunk *dc, AuxTrans *tp));
static int		dc_SampleReserve FP((DataChunk *dc, AuxTrans *, int));



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
	dc = DC_ClassCreate (SUPERCLASS);
/*
 * Allocate an initial AuxData structure with space to hold one sample.
 * It might be better, in the long run, to hold off on this until somebody
 * starts adding data.
 */
	tp = ALLOC (AuxTrans);
	tp->at_NSample = 0;
	tp->at_NSampAlloc = 1;
	tp->at_HintNSample = 0;
	tp->at_HintSampSize = 0;
	tp->at_HintUseAvgs = 1;		/* Always default to trying averages */
	tp->at_SampOverhead = 0;
	tp->at_SampDataSize = 0;
	tp->at_NextOffset = dc->dc_DataLen;	/* where our data will start */
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
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "GetNSample"))
		return (0);
/*
 * Find our data and return the info.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return (0);
	return (tp->at_NSample);
}





void
dc_HintNSamples (dc, nsample, decrease)
DataChunk *dc;
int nsample;
bool decrease;
/* 
 * Provide a hint to the number of samples this chunk will contain.  
 * If 'decrease' is TRUE, then 'nsample' will be used as the new hint even
 * if it is smaller than the current hint.  If 'decrease' is FALSE, then
 * the hint is changed only if it is larger than the present hint.  No space
 * is allocated anywhere until the chunk has to grow to fit more data.  If this
 * number is reduced before more samples are allocated, the newer value will
 * be used when growth occurs.  If this number is reduced AFTER growth occurs,
 * the memory use will NOT be reduced.  If the hint is less than the number
 * of existing samples, then it has no effect.
 *
 * The decrease flag allows internal class methods to hint about the number
 * of samples without accidentally reducing a hint that the application may
 * have suggested.  Usually a class method calls this function only if it knows
 * for certain that the number of samples will increase to at least this much.
 */
{
	AuxTrans *tp;
/*
 * Checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "HintNSamples"))
		return;
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return;
	if (nsample >= 0 && (decrease || (nsample > tp->at_HintNSample)))
		tp->at_HintNSample = nsample;
}





void
dc_HintSampleSize (dc, sampsize, override)
DataChunk *dc;
int sampsize;
bool override;
/* 
 * Suggests an approximate size for each sample in a DataChunk.  The
 * estimate should include only the space required for the data.  Overhead
 * space for each class is calculated and added internally by each subclass.
 * If 'override' is TRUE or there is currently no estimate for sample size,
 * 'size' is used as the size hint.  Otherwise, when 'override' is FALSE and
 * a sample size exists, 'size' is ignored and the current hint is not
 * changed.  Usually the class knows better what the sample size will be,
 * and should be given preference with 'override' equal to FALSE.  However,
 * if the class knows nothing about the size, you may as well set the size
 * with 'override' equal to TRUE.
 *
 * As for HintNSamples, this does not affect memory allocation until the
 * DataChunk tries to grow to add more samples or expand an existing sample.
 * 
 * The actual hint size, the sum of the data hint and the overhead, is updated.
 */
{
	AuxTrans *tp;
/*
 * Checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "HintSampSize"))
		return;
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return;
	if ((sampsize >= 0) && (tp->at_SampDataSize == 0 || override))
	{
		tp->at_SampDataSize = sampsize;
		tp->at_HintSampSize = tp->at_SampDataSize+tp->at_SampOverhead;
	}
}




void
dc_HintMoreSamples (dc, nsample, decrease)
DataChunk *dc;
int nsample;
bool decrease;
/*
 * Hint that 'nsample' more samples are about to be added to the DataChunk.
 * Basically, this is just like HintNSamples, except it calculates the new
 * nsamples hint by adding nsample to the current number of samples in the
 * chunk.  Again, no memory is allocated.  If 'decrease' is FALSE, then the
 * nsample hint will change iff the addition of 'nsample' increases the hint.
 */
{
	AuxTrans *tp;

	if (! dc_ReqSubClassOf(dc->dc_Class,DCC_Transparent,"HintMoreSamples"))
		return;
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return;
	if (nsample > 0 && 
	    (decrease || (tp->at_HintNSample < tp->at_NSample + nsample)))
		tp->at_HintNSample = tp->at_NSample + nsample;
}




void
dc_HintSampleOverhead (dc, size)
DataChunk *dc;
int size;
/*
 * Adds the amount of overhead space that a datachunk class needs per sample.
 * This is called once per datachunk per class, such as at creation or during
 * some single initialization function.  It is not meant to be called
 * directly by an application.  If an application uses it, it does so at its
 * own risk.  (Nothing will break, since it's just a hint, but it may use more
 * space than it needs.)
 * 
 * The actual hint size, the sum of the data hint and the overhead, is updated.
 */
{
	AuxTrans *tp;

	if (!dc_ReqSubClassOf(dc->dc_Class,DCC_Transparent,"HintSmplOverhead"))
		return;
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return;
	if (size >= 0)
	{
		tp->at_SampOverhead += size;
		tp->at_HintSampSize = tp->at_SampDataSize+tp->at_SampOverhead;
	}
}


	

void
dc_HintUseAverages (dc, use)
DataChunk *dc;
bool use;
/*
 * Set the HintUseAvgs flag
 */
{
	AuxTrans *tp;

	if (!dc_ReqSubClassOf(dc->dc_Class,DCC_Transparent,"HintUseAverages"))
		return;
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return;
	tp->at_HintUseAvgs = use;
}


	

int
dc_NSamplesGrowthHint(dc, nnew)
DataChunk *dc;
int nnew;     /* minimum number of samples to add to this chunk, 0 is valid */
/*
 * Returns at least (tp->at_NSample+nnew), possibly tp->at_HintNSample, and no
 * greater than tp->at_NSampAlloc.  Preference is given to the hint if it is
 * larger than NSample.  At present, we default to NSampAlloc if no
 * hint, because we know that the growth of NSampAlloc is very slow and
 * incremental.  If NSampAlloc begins to grow faster, we may want to default
 * this to at_NSample instead, in case it's possible to allocate many more
 * samples than will actually be used.
 */
{
	AuxTrans *tp;

	if (!dc_ReqSubClassOf(dc->dc_Class,DCC_Transparent,"NSamplesGrowth"))
		return (0);
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
		return (nnew);
	return (dc_TrGrowthHint (dc, tp, nnew));
}




/*ARGSUSED*/
static int
dc_TrGrowthHint (dc, tp, nnew)
DataChunk *dc;
AuxTrans *tp;
int nnew;
{
	if (tp->at_HintNSample > tp->at_NSample + nnew)
		return (tp->at_HintNSample);
	else if (tp->at_NSampAlloc > tp->at_NSample + nnew)
		return (tp->at_NSampAlloc);
	else
		return (tp->at_NSample + nnew);
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
	dc_SetMLoc (dc, sample, 1, loc);
}




void
dc_SetMLoc (dc, begin, nsamp, loc)
DataChunk *dc;
int begin;
int nsamp;
Location *loc;
/*
 * Set the locations for a series of samples.  The locations are in the array
 * pointed to by 'loc'.
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
	if ((begin < 0) || (begin + nsamp > tp->at_NSample))
	{
		msg_ELog (EF_PROBLEM, "Try to set loc on sample %d of %d",
			  begin, tp->at_NSample);
		return;
	}
/*
 * Look for the sample list; if it does not yet exist, create it, using a hint
 * on the number of samples, if possible.
 */
	loclist = (Location *) dc_FindADE (dc, DCC_Transparent,ST_LOCATIONS,0);
	if (loclist == NULL)
	{
		int nloc, len;

		nloc = dc_TrGrowthHint (dc, tp, 0);
		len = nloc * sizeof (Location);
		loclist = (Location *) malloc(len);
		memset (loclist, 0, len);
		dc_AddADE (dc, loclist, DCC_Transparent, ST_LOCATIONS, 
				len, TRUE);
	}
/*
 * Now we just store the location.
 */
	memcpy ((char *)(loclist+begin), (char *)loc, nsamp*sizeof(Location));
}




static AuxTrans *
dc_NewSample (dc, method)
DataChunk *dc;
char *method;
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
 * Returns:
 *	The address of the new sample, or NULL on an error.
 */
{
	AuxTrans *tp;
/*
 * The obligatory class check.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, method))
		return NULL;
/*
 * Find our data.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "%s: Missing ST_SAMPLES!", method);
		return NULL;
	}
/*
 * If our chunk lacks space for another sample, add it now.
 */
	if (tp->at_NSample + 1 > tp->at_NSampAlloc)
		tp = dc_TrMoreSamples (dc, tp, 1);
/*
 * The rest is specific to the method that called us.
 */
	return (tp);
}




static int
dc_SampleReserve (dc, tp, len)
DataChunk *dc;
AuxTrans *tp;
int len;
{
	int reserve, avg;

	reserve = (len > tp->at_HintSampSize) ? len : tp->at_HintSampSize;
	if (tp->at_HintUseAvgs)
	{
		avg = dc_AvgSampleSize (dc, tp);
		reserve = (avg > reserve) ? avg : reserve;
	}
	return (reserve);
}




DataPtr
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
 * Returns:
 *	The address of the new sample, or NULL on an error.
 */
{
	AuxTrans *tp;
	int offset, ns;
	int reserve;

	if (!(tp = dc_NewSample (dc, "AddSample")))
		return NULL;
/*
 * Create more data space in the data chunk itself.  Even if they are not
 * requesting space for a whole sample according to our hint, reserve that
 * much space so that the next sample does not use space we may eventually
 * want.  The length of our sample will still be only the 'len' that was
 * requested.  We'll know how much space we actually have for this sample
 * by the offset of the next one (or by NextOffset if there is no next one).
 */
	reserve = dc_SampleReserve (dc, tp, len);
	offset = dc_TrMoreData (dc, tp, reserve);
/*
 * Fill the thing in, and we are done.
 */
	ns = tp->at_NSample++;
	tp->at_Samples[ns].ats_Time = *t;
	tp->at_Samples[ns].ats_Offset = offset;
	tp->at_Samples[ns].ats_Len = len;
	if (data && len > 0)
		memcpy ((char *) dc->dc_Data + offset, data, len);
	return ((DataPtr)((char *)dc->dc_Data + offset));
}





DataPtr
dc_AddAlignedSample (dc, t, data, len, align)
DataChunk *dc;
ZebTime *t;
DataPtr data;
int len;
int align;	/* size to align the new sample's offset with */
/*
 * Add some data to this data chunk.
 * Entry:
 *	DC	is a datachunk which is a subclass of Transparent
 *	T	is the time of the new sample
 *	DATA	is the new sample data.  If DATA is NULL, the data array in
 *		the data chunk will remain uninitialized.
 *	LEN	is the length of DATA
 *	ALIGN	is the size to which the sample offset must align
 * Exit:
 *	The new sample has been added to this data chunk.
 * Returns:
 *	The address of the new sample, or NULL on an error.
 */
{
	AuxTrans *tp;
	int offset, ns;
	int aligned;
	int reserve;

	if (!(tp = dc_NewSample (dc, "AddAlignedSample")))
		return NULL;
/*
 * Create more data space in the data chunk itself.  See the note in
 * dc_AddSample() about reserving space.  Alignment is done by finding out
 * the next offset, >= NextOffset, which is aligned on size in 'align'.  Then
 * enough space is requested for the space desired in 'reserve' and the
 * space we need to skip to align the sample's offset.
 */
	reserve = dc_SampleReserve (dc, tp, len);
	aligned = (int) ALIGN(tp->at_NextOffset,align);
	offset = dc_TrMoreData (dc,tp,reserve + (aligned - tp->at_NextOffset));
	offset = aligned;
/*
 * Fill the thing in, and we are done.
 */
	ns = tp->at_NSample++;
	tp->at_Samples[ns].ats_Time = *t;
	tp->at_Samples[ns].ats_Offset = offset;
	tp->at_Samples[ns].ats_Len = len;
	if (data && len > 0)
		memcpy ((char *) dc->dc_Data + offset, data, len);
	return ((DataPtr)((char *)dc->dc_Data + offset));
}




int
dc_ReserveStaticSpace (dc, len)
DataChunk *dc;
int len;
/*
 * Allocates space at the beginning of the raw data space for 'len' bytes.
 * I suppose we could copy existing samples forward, update offset, and
 * then insert the space, but instead we'll just make sure no samples exist
 * yet.  It is up to the subclass to reserve the space before any samples
 * are added.  Since the space is at the beginning, the transparent class
 * will never interfere with it, hence "static space".
 */
{
	AuxTrans *tp;
	int offset;

	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "StaticSpace"))
		return 0;
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return 0;
	}
	if (tp->at_NSample > 0)
	{
		msg_ELog (EF_PROBLEM, 
			  "reserving static space after %d samples added", 
			  tp->at_NSample);
		return 0;
	}
	offset = tp->at_NextOffset;
	Dc_RawAdd (dc, len);
	tp->at_NextOffset = dc->dc_DataLen;
	return (offset);
}





static AuxTrans *
dc_TrMoreSamples (dc, tp, n)
DataChunk *dc;
AuxTrans *tp;
int n;
/*
 * Augment the description space of this data chunk to be able to hold
 * at least "n" more samples.  If we have some hints available, use them
 * to allocate space accordingly, possibly more than "n" samples.
 */
{
	int nnew, len;
	AuxTrans *newtp;
/*
 * If we have a hint greater than the current number of samples, use it
 * to determine the new number.  Otherwise, rely on the growth function.
 */
	if (tp->at_HintNSample >= tp->at_NSampAlloc + n)
		nnew = tp->at_HintNSample;
	else
		nnew = tp->at_NSampAlloc + 2*n;
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
	if (old < n)
	{
		list = (PlatformId *) realloc (list, n*sizeof (PlatformId));
		for (samp = old; samp < n; samp++)
			list[samp] = dc->dc_Platform;
		dc_ChangeADE (dc, (DataPtr) list, DCC_Transparent, 
			      ST_PLATFORMS, n*sizeof (PlatformId));
	}
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
	if (old < n)
	{
		locs = (Location *) realloc (locs, n*sizeof (Location));
		dc_ChangeADE (dc, (DataPtr) locs, DCC_Transparent, 
			      ST_LOCATIONS, n*sizeof (Location));
	}
}




static int
dc_AvgSampleSize (dc, tp)
DataChunk *dc;
AuxTrans *tp;
{
	int first;
	int avg = 0;

	if (tp->at_NSample > 0)
	{
		first = tp->at_Samples[0].ats_Offset;
		avg = (tp->at_NextOffset - first) / tp->at_NSample;
	}
	return (avg);
}




static int
dc_TrMoreData (dc, tp, len)
DataChunk *dc;
AuxTrans *tp;
int len;
/*
 * Increase the available data space by LEN.  The return value is the
 * offset to the beginning of the new space.  This routine exists to make
 * it easy to do caching of data space later on, if we want, to avoid
 * reallocs.
 *
 * To cache data space, we use the NextOffset value to indicate the start
 * of available raw space.  If there is buffer space, then NextOffset <
 * DataLen.  If we must increase our space, we use any hints that are
 * available to do so.
 *
 * If len == -1, then buffer space is allocated according to the current
 * hints, but the offset pointer is not advanced.  This brings the
 * allocated memory in sync with the current hints.
 *
 * If NSamples reaches the hint, but we still fall short, then use the
 * difference between the average so far and the hint to calculate how much
 * space we should need to finish out the chunk.  This should sufficiently
 * account for the greater space requirements of aligning differently-typed
 * fields.
 */
{
	int offset = tp->at_NextOffset;
	int hint, add, avg;

	if ((len == -1) || (offset + len > dc->dc_DataLen))
	{
		if (len == -1)
			len = 0;
	/*
	 * Start with what we think we should be using for the size of
	 * a single sample, if anything.
	 */
		hint = dc_SampleReserve (dc, tp, 0);
		avg = dc_AvgSampleSize (dc, tp);
		add = hint;
	/*
	 * If we have some kind of hint, use it to make an educated guess 
	 * of the amount of space which will be needed for future samples.
	 * If the number of samples already exceeds our hint, the best we
	 * can guess to add is the size of one sample.
	 */
		if (hint > 0)
		{
			if ((tp->at_HintNSample >= tp->at_NSample) &&
			    (avg > hint))
			{
			/*
			 * The sample hint sizes were not enough to fit the
			 * hinted number of samples, so we probably fell
			 * short due to alignments.  Try to add the
			 * shortfall now.
			 */
				add = (avg - hint) * tp->at_HintNSample;
				add -= dc->dc_DataLen - offset;
			}
			else if (tp->at_HintNSample > tp->at_NSample)
			{
				add = (tp->at_HintNSample - tp->at_NSample)
					* hint;
			}
		}
	/*
	 * Make sure we at least make room for 'len' more bytes.
	 */
		if (add < offset + len - dc->dc_DataLen)
			add = offset + len - dc->dc_DataLen;
		if (add > 0)
			Dc_RawAdd (dc, add);
	}
/*
 * Since only the next len bytes will be used, advance NextOffset by len.
 */
	tp->at_NextOffset += len;
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
 * The obligatory class check.
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
 * The obligatory class check.
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





static int
dc_TrCompareSamples (a1, a2)
const void *a1;
const void *a2;
/*
 * Return -1 if time of s1 before s2, 0 if time of s1 == s2, and 
 * 1 if time of s1 after s2
 */
{
        TransSample *s1 = (TransSample *) a1;
        TransSample *s2 = (TransSample *) a2;

	if (TC_Less(s1->ats_Time, s2->ats_Time))
		return -1;
	else if (TC_Less(s2->ats_Time, s1->ats_Time))
		return 1;
	else
		return 0;
}




void
dc_SortSamples (dc)
DataChunk *dc;
/*
 * Re-order the samples in this datachunk so that they are in 
 * chronological order.
 *
 * All we need to do is sort the at_Samples array and make sure the
 * platform list, if any, is kept up to date.
 */
{
	AuxTrans *tp;
	PlatformId *list;
	int i;
	struct sortrecord {
		TransSample trans;
		PlatformId pid;
	} *sr;
/*
 * The obligatory class check.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "Sort samples"))
		return ;
/*
 * Find our data.
 */
	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return ;
	}
/*
 * If there are 1 or fewer samples, don't bother
 */
	if (tp->at_NSample <= 1)
		return ;
/*
 * If there is no platform list, then just sort the at_Samples array
 */
	list = (PlatformId *) dc_FindADE (dc, DCC_Transparent,
					  ST_PLATFORMS, NULL);
	if (!list)
	{
#ifdef SVR4
		qsort ((void *)(tp->at_Samples), (size_t) tp->at_NSample,
		       sizeof (TransSample), dc_TrCompareSamples);
#else
		qsort ((char *)(tp->at_Samples), (size_t) tp->at_NSample,
		       sizeof (TransSample), dc_TrCompareSamples);
#endif
		return ;
	}
/*
 * Otherwise combine the TransSample and PlatformId arrays into an array
 * of sort records, sort the records, and copy the samples and pids from
 * the sorted array of records.
 */
	sr = (struct sortrecord *)malloc(tp->at_NSample * 
					 sizeof(struct sortrecord));
	for (i = 0; i < tp->at_NSample; ++i)
	{
		sr[i].trans = tp->at_Samples[i];
		sr[i].pid = list[i];
	}
#ifdef SVR4
	qsort ((void *)sr, (size_t) tp->at_NSample,
	       sizeof (struct sortrecord), dc_TrCompareSamples);
#else
	qsort ((char *)sr, (size_t) tp->at_NSample,
	       sizeof (struct sortrecord), dc_TrCompareSamples);
#endif
	for (i = 0; i < tp->at_NSample; ++i)
	{
		tp->at_Samples[i] = sr[i].trans;
		list[i] = sr[i].pid;
	}
	free (sr);
}




static void
dc_TrDump (dc)
DataChunk *dc;
/*
 * Dump out this data chunk.
 */
{
	AuxTrans *tp;
	int i, len;
	char atime[40];
	PlatformId *list;
/*
 * The obligatory class check.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "Transp Dump"))
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
	list = (PlatformId *) dc_FindADE (dc, DCC_Transparent,
					  ST_PLATFORMS, NULL);
/*
 * Go for it.
 */
	printf ("TRANSPARENT class, ");
	printf ("%d samples, %d allocated, next off %d, ",
		tp->at_NSample, tp->at_NSampAlloc, tp->at_NextOffset);
	printf ("use avg: %s\n", tp->at_HintUseAvgs ? "true" : "false");
	printf ("Hints: nsamples %d, sample size %d, avg %d, ",
		tp->at_HintNSample, tp->at_HintSampSize, 
		dc_AvgSampleSize (dc, tp));
	printf ("data %d, subclass %d\n",
		tp->at_SampDataSize, tp->at_SampOverhead);
	for (i = 0; i < tp->at_NSample; i++)
	{
	/*
	 * Put out time and size info.
	 */
		TransSample *ts = tp->at_Samples + i;
		TC_EncodeTime (&ts->ats_Time, TC_Full, atime);
		printf ("\t%2d at %s, len %d offset %d", i,
			atime, ts->ats_Len, ts->ats_Offset);
		if (list)
			printf (", plat '%s'", ds_PlatformName (list[i]));
		printf ("\n");
	/*
	 * Print any sample attributes
	 */
		dc_ProcSampleAttrArrays (dc, i, NULL, dc_PrintSaAttr, NULL);
	}
}





static int
dc_PrintSaAttr (key, value, nval, type, arg)
char *key;
void *value;
int nval;
DC_ElemType type;
void *arg;
/*
 * Print out an attribute value.
 */
{
	int i;

	if (nval && (type == DCT_String))
	{
		printf ("\t\t%s --> '%s'\n", key, (char *)value);
		return (0);
	}
	printf ("\t\t%s --> ", key);
	for (i = 0; i < nval; ++i)
	{
		printf ("%s%s", dc_ElemToString(value, type),
			(i == nval - 1) ? "\n" : ", ");
		value = (char *)value + dc_SizeOfType (type);
	}
	if (nval == 0)
		printf ("\n");
	return (0);
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
	int samp, nsamp;
	AuxTrans *tp;

	if (! (tp = (AuxTrans *) dc_FindADE (dc, DCC_Transparent, ST_SAMPLES,
				(int *) 0)))
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return;
	}
/*
 * Find out if any hints available to suggest how many platforms to allocate
 */
	nsamp = dc_TrGrowthHint (dc, tp, 0);
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
 * The obligatory class check.
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
	int i, diff, oldlen;
	TransSample *ts;
	int next_sample;
/*
 * The obligatory class check.
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
 * We are expanding.  See if we already have enough space between the end
 * of this sample and the beginning of the next one (or where the next one
 * would begin if it existed: NextOffset).  If we do, all we have to do is
 * adjust our length.
 */
	next_sample = ((sample + 1) < tp->at_NSample) ? 
		tp->at_Samples[sample + 1].ats_Offset : tp->at_NextOffset;
	if (ts->ats_Offset + newsize <= next_sample)
	{
		ts->ats_Len = newsize;
		return;
	}
/*
 * Oh well, it was worth a try.  Now we have to make room.  Allocate the new
 * space and shift everything down.  We could get badly burned by a
 * braindamaged malloc here, so use bcopy for now, which claims to do this
 * right.
 */
	diff = newsize - ts->ats_Len;
	oldlen = dc_TrMoreData (dc, tp, diff);
	if ((sample + 1) < tp->at_NSample)
	{
#ifdef SVR4
		memcpy ((char *) dc->dc_Data + ts[1].ats_Offset + diff,
			(char *) dc->dc_Data + ts[1].ats_Offset,
			oldlen - ts[1].ats_Offset);
#else
		bcopy ((char *) dc->dc_Data + ts[1].ats_Offset,
			(char *) dc->dc_Data + ts[1].ats_Offset + diff,
			oldlen - ts[1].ats_Offset);
#endif
		for (i = sample + 1; i < tp->at_NSample; i++)
			tp->at_Samples[i].ats_Offset += diff;
	}
	ts->ats_Len = newsize;
}




void
dc_AddMoreSamples (dc, nsample, size)
DataChunk *dc;
int nsample;
int size;
/*
 * Allocate space for at least 'nsample' additional samples of size 'size'.  If
 * size is 0, the sample-size hint is used.  If 'nsample' is 0, the nsamples
 * hint is used.  No matter what, it tries to immediately allocate space 
 * according to the parameters or the current hints.  The nonzero parameters
 * are stored as hints for future reference.
 */
{
	AuxTrans *tp;
	int offset, samp;
/*
 * Sanity Claus.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class,DCC_Transparent,"AddMoreSamples"))
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
 * Store our parameters as hints for future reference, esp the sample size, but
 * only if they are nonzero.  Don't let nsample hint become smaller.
 */
	if ((nsample > 0) && (tp->at_NSample + nsample > tp->at_HintNSample))
		tp->at_HintNSample = tp->at_NSample + nsample;
	if (size > 0)
	{
		tp->at_SampDataSize = size;
		tp->at_HintSampSize = tp->at_SampDataSize+tp->at_SampOverhead;
	}
/*
 * If our chunk lacks space for this many more samples, call dc_TrMoreSamples
 * so that it allocates the space according to our new hint.
 */
	if (tp->at_HintNSample > tp->at_NSampAlloc)
		tp = dc_TrMoreSamples (dc, tp, 0);
/*
 * Allocate buffer space according to current hints.
 */
	if (tp->at_HintNSample > tp->at_NSample)
		(void) dc_TrMoreData (dc, tp, -1);
/*
 * All done.
 */
}





void
dc_SetSampleAttrArray (dc, sample, key, type, nval, values)
DataChunk *dc;
int sample;
char *key;
DC_ElemType type;
int nval;
void *values;
{
	if (! dc_ReqSubClassOf(dc->dc_Class,
			       DCC_Transparent, "SetSampleAttrArray"))
		return;
	dca_AddAttrArray (dc, DCC_Transparent, ST_ATTR + sample, 
			  key, type, nval, values);
}




void *
dc_GetSampleAttrArray (dc, sample, key, type, nval)
DataChunk *dc;
int sample;
char *key;
DC_ElemType *type;
int *nval;
{
	void *values;

	if (! dc_ReqSubClassOf(dc->dc_Class, DCC_Transparent,
			       "GetSampleAttrArray"))
		return NULL;
	if (values = dca_GetAttrArray (dc, DCC_Transparent, ST_ATTR + sample, 
				       key, type, nval))
		return (values);
	return (dc_GetGlobalAttrArray (dc, key, type, nval));
}




int
dc_ProcSampleAttrArrays (dc, sample, pattern, func, arg)
DataChunk *dc;
int sample;
char *pattern;
int (*func) (/* char *key, void *vals, int nval, DC_ElemType, void *arg */);
void *arg;
{
	if (! dc_ReqSubClassOf(dc->dc_Class, DCC_Transparent,
			       "ProcSampleAttrArrays"))
		return;
	return (dca_ProcAttrArrays (dc, DCC_Transparent, ST_ATTR + sample,
				    pattern, func, arg));
}




int
dc_GetNSampleAttrs (dc, sample)
DataChunk *dc;
int sample;
{
	if (! dc_ReqSubClassOf(dc->dc_Class,DCC_Transparent,"GetNSampleAttrs"))
		return;
	return (dca_GetNAttrs (dc, DCC_Transparent, ST_ATTR + sample));
}




void
dc_SetSampleAttr (dc, sample, key, value)
DataChunk *dc;
int sample;
char *key, *value;
/*
 * Add a per-sample attribute to this data chunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent,"SetSampleAttr"))
		return;
	dca_AddAttr (dc, DCC_Transparent, ST_ATTR + sample, key, value);
}



void
dc_RemoveSampleAttr (dc, sample, key)
DataChunk *dc;
int sample;
char *key;
/*
 * Remvoe a per-sample attribute from this data chunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, 
				DCC_Transparent, "RemoveSampleAttr"))
		return;
	dca_RemoveAttr (dc, DCC_Transparent, ST_ATTR + sample, key);
}




char *
dc_GetSampleAttr (dc, sample, key)
DataChunk *dc;
int sample;
char *key;
/*
 * Look up a per-sample attribute.
 */
{
	char *value;

	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent,"GetSampleAttr"))
		return(NULL);
	if (value = dca_GetAttr (dc, DCC_Transparent, ST_ATTR + sample, key))
		return (value);
	return (dc_GetGlobalAttr (dc, key));
}




void *
dc_GetSaAttrBlock (dc, sample, len)
DataChunk *dc;
int sample, *len;
/*
 * Get the per-sample attributes out as an opaque chunk.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent,
				"GetSaAttrBlock"))
		return(NULL);
	return (dca_GetBlock (dc, DCC_Transparent, ST_ATTR + sample, len));
}



void
dc_SetSaAttrBlock (dc, sample, block, len)
DataChunk *dc;
void *block;
int sample, len;
/*
 * Store a per-sample attribute block back.
 */
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent,
				"SetSaAttrBlock"))
		return;
	dca_PutBlock (dc, DCC_Transparent, ST_ATTR + sample, block, len);
}




char **
dc_GetSampleAttrList(dc, sample, pattern, values, natts)
DataChunk *dc;
int sample;
char *pattern;
char **values[];
int *natts;
{
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent,
				"GetSampleAttrList"))
		return (NULL);
	return(dca_GetAttrList(dc, DCC_Transparent, ST_ATTR + sample,
			       pattern, values, natts));
}



char **
dc_GetSampleAttrKeys (dc, sample, natts)
DataChunk *dc;
int sample;
int *natts;
{
/*
 * Returns a list of keys for the sample attributes for this sample.
 * Also puts into natt the number of global attributes for this dc.
 * The returned array of attribute keys is only valid until the next call
 * of any of the Get*AttrList or Get*AttrKeys functions.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_Transparent,
				"GetSampleAttrKeys"))
		return (NULL);
	return(dca_GetAttrList(dc, DCC_Transparent, ST_ATTR + sample,
			       NULL, NULL, natts));
}
 
