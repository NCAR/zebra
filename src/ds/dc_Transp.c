/*
 * The definition of the transparent data object class.
 */
static char *rcsid = "$Id: dc_Transp.c,v 1.1 1991-11-16 01:18:54 corbet Exp $";
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

/*
 * TODO:
 *	Easy way to add lots of samples at once
 *	Time-sorting of added samples?
 */

# ifdef __STDC__
	static DataChunk *Dc_TrCreate (DataChunkClass);
	static void dc_TrDump ();
# else
# endif
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
	time	ats_Time;	/* Time of this sample		*/
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
} AuxTrans;


/*
 * AuxData codes.
 */
# define ST_SAMPLES	1

/*
 * Local routines.
 */
# ifdef __STDC__
	static AuxTrans * dc_TrMoreSamples (DataChunk *, AuxTrans *, int);
	static int	dc_TrMoreData (DataChunk *, int);
# else
# endif


static DataChunk *
Dc_TrCreate (class)
DataChunkClass class;
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
	dc_AddADE (dc, (DataPointer) tp, DCC_Transparent, ST_SAMPLES,
			sizeof (AuxTrans), TRUE);
/*
 * Done.
 */
	dc->dc_Class = DCC_Transparent;
	return (dc);
}





int
dc_TrGetNSample (dc)
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
	{
		msg_ELog (EF_PROBLEM, "Missing ST_SAMPLES in dchunk!");
		return (0);
	}
	return (tp->at_NSample);
}






void
dc_TrAddSample (dc, t, data, len)
DataChunk *dc;
time *t;
DataPointer data;
int len;
/*
 * Add some data to this data chunk.
 * Entry:
 *	DC	is a datachunk which is a subclass of Transparent
 *	T	is the time of the new sample
 *	DATA	is the new sample data
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
	if (! ds_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "Add sample"))
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
	dc_ChangeADE (dc, (DataPointer) newtp, DCC_Transparent,ST_SAMPLES,len);
	return (newtp);
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

	Dc_RawAdd (dc, len);
	return (offset);
}






DataPointer
dc_TrGetSample (dc, sample, len)
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
	if (! ds_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "Get sample"))
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
 * Make sure the sample exists.  If so, return the info.
 */
	if (sample < 0 || sample >= tp->at_NSample)
		return (NULL);
	if (len)
		*len = tp->at_Samples[sample].ats_Len;
	return ((DataPointer) ((char *) dc->dc_Data +
			tp->at_Samples[sample].ats_Offset));
}





void
dc_TrDump (dc)
DataChunk *dc;
/*
 * Dump out this data chunk.
 */
{
	AuxTrans *tp;
	int i;
/*
 * The obbligatory class check.
 */
	if (! ds_ReqSubClassOf (dc->dc_Class, DCC_Transparent, "Get sample"))
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
		printf ("\t%2d at %06d %06d, len %d offset %d\n", i,
			ts->ats_Time.ds_yymmdd, ts->ats_Time.ds_hhmmss,
			ts->ats_Len, ts->ats_Offset);
	}
}
