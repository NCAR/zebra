/*
 * The definition of the MetData data chunk class.  This class adds the
 * concept of fields and stuff in general interpretable as meteorological
 * data.
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
# include "ds_fields.h"
# include "DataChunk.h"
# include "DataChunkP.h"
MAKE_RCSID ("$Id: dc_MetData.c,v 3.1 1992-05-27 17:24:03 corbet Exp $")

# define SUPERCLASS DCC_Transparent

/*
 * The default bad value flag.
 */
# define DefaultBadval -99999.9

/*
 * The structure which describes our fields.
 */
typedef struct _FldInfo
{
	int		fi_NField;
	FieldId		fi_Fields[DC_MaxField];
	bool		fi_Uniform;		/* Uniform length fields */
	int		fi_Size;		/* Size of uniform flds  */
	float		fi_Badval;		/* Bad value flag	*/
} FldInfo;

/*
 * If we have non-uniform fields, then the beginning piece of every sample
 * contains a list of the following:
 */
typedef struct _FieldTOC
{
	int	ft_Offset;		/* Offset of this field		*/
	int	ft_Len;			/* Length of this field		*/
} FieldTOC;


/*
 * Our class-specific AuxData structure types.
 */
# define ST_FLDINFO	1000


/*
 * Local routines.
 */
static DataChunk *dc_MDCreate FP((DataClass));
static int	dc_GetIndex FP((FldInfo *, FieldId));
static int	dc_ArrangeSpace FP((DataChunk *, ZebTime *, int, int, int,
			FldInfo *));
static void	dc_CopyData FP((DataChunk *, FldInfo *, int, int, int,
			DataPtr));
static void	dc_DumpMD FP((DataChunk *));
static void	dc_AddNonUniform FP((DataChunk *, FldInfo *, int, ZebTime *,
			int, int, int, DataPtr));

RawDCClass MetDataMethods =
{
	"MetData",
	SUPERCLASS,		/* Superclass			*/
	dc_MDCreate,
	InheritMethod,		/* No special destroy		*/
	0,			/* Add??			*/
	dc_DumpMD,		/* Dump				*/
};




/* ARGSUSED */
static DataChunk *
dc_MDCreate (class)
DataClass class;
/*
 * Create a chunk of this class.
 */
{
	DataChunk *dc;
/*
 * The usual.  Make a superclass chunk and tweak it to look like us.  We don't
 * add any field info here, because we don't know it yet.
 */
	dc = dc_CreateDC (SUPERCLASS);
	dc->dc_Class = DCC_MetData;
	return (dc);
}




void
dc_SetupFields (dc, nfield, fields)
DataChunk *dc;
int nfield;
FieldId *fields;
/*
 * Set up this DC to take non-uniform fields.
 */
{
	FldInfo *finfo;
	int fld;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "SetupFields"))
		return;
/*
 * Allocate and fill in our structure.
 */
	finfo = ALLOC (FldInfo);
	finfo->fi_NField = nfield;
	for (fld = 0; fld < nfield; fld++)
		finfo->fi_Fields[fld] = fields[fld];
	finfo->fi_Uniform = FALSE;
	finfo->fi_Badval = DefaultBadval;
	finfo->fi_Size = 0;
/* 
 * Attach it to the DC.
 */
	dc_AddADE (dc, (DataPtr) finfo, DCC_MetData, ST_FLDINFO, 
				sizeof (FldInfo), TRUE);
}



/* ARGSUSED */
void
dc_SetupUniformFields (dc, nsamples, nfield, fields, size)
DataChunk *dc;
int nsamples, nfield, size;
FieldId *fields;
/*
 * Get this data chunk set up to have uniform-length fields.
 * Entry:
 *	DC	is a new data chunk, which is a subclass of DCC_MetData
 *	NSAMPLES is the best guess at the number of samples which this
 *		data chunk will have.  It is not an upper limit.
 *	NFIELD	is the number of fields this data chunk will have.
 *	FIELDS	is the list of ID's for the fields in this DC.
 *	SIZE	is the uniform size that each field will have.
 * Exit:
 *	The data chunk has been configured for this mode of operation.
 */
{
	FldInfo *finfo;
	int fld;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "SetupFields"))
		return;
/*
 * Allocate and fill in our structure.
 */
	finfo = ALLOC (FldInfo);
	finfo->fi_NField = nfield;
	for (fld = 0; fld < nfield; fld++)
		finfo->fi_Fields[fld] = fields[fld];
	finfo->fi_Uniform = TRUE;
	finfo->fi_Size = size;
	finfo->fi_Badval = DefaultBadval;
/* 
 * Attach it to the DC.
 */
	dc_AddADE (dc, (DataPtr) finfo, DCC_MetData, ST_FLDINFO, 
				sizeof (FldInfo), TRUE);
/*
 * We don't use nsamples quite yet.
 */
	return;
}






double
dc_GetBadval (dc)
DataChunk *dc;
/*
 * Return the bad value flag stored in this DC.
 */
{
	FldInfo *finfo;
	char *abad;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetBadval"))
		return (0);
/*
 * Look first for a global attribute.
 */
	if (abad = dc_GetGlobalAttr (dc, "bad_value_flag"))
	{
		float fbad;
		sscanf (abad, "%f", &fbad);
		return (fbad);
	}
/*
 * Grab the field info structure, and return the bad value flag stored
 * therein.  If the structure doesn't exist, return the default.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
		return (DefaultBadval);
	return (finfo->fi_Badval);
}





void
dc_SetBadval (dc, badval)
DataChunk *dc;
float badval;
/*
 * Store the bad value flag into this DC.  This should be done after fields
 * are set up, but before data are added.
 */
{
	FldInfo *finfo;
	char sbad[20];
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "SetBadval"))
		return;
/*
 * Grab the field info structure, and set the bad value flag.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM,"Attempt to set bad flag with no fields");
		return;
	}
	finfo->fi_Badval = badval;
	if (dc_GetNSample (dc) > 0)
		msg_ELog (EF_PROBLEM, "Bad value flag set on non-empty DC");
/*
 * Also store it as a global attribute.
 */
	sprintf (sbad, "%f", badval);
	dc_SetGlobalAttr (dc, "bad_value_flag", sbad);
}





int
dc_GetNField (dc)
DataChunk *dc;
/*
 * Return the number of fields stored in this DC.
 */
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetNField"))
		return (0);
/*
 * Grab the field info structure and return the number.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
		return (0);
	return (finfo->fi_NField);
}




FieldId *
dc_GetFields (dc, nf)
DataChunk *dc;
int *nf;
/*
 * Return the list of fields in this DC.
 */
{
	FldInfo *finfo;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetFields"))
		return (0);
/*
 * Grab the field info structure and return the stuff.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
		return (0);
	if (nf)
		*nf = finfo->fi_NField;
	return (finfo->fi_Fields);
}





void
dc_AddMData (dc, t, field, size, start, nsamp, data)
DataChunk *dc;
ZebTime *t;
FieldId field;
int start, size, nsamp;
DataPtr data;
/*
 * Add data to this data chunk for one field.
 * Entry:
 *	DC	is a data chunk which is a subclass of MetData
 *	T	is an array of times for the incoming data
 *	FIELD	is the field for which this data is being added
 *	SIZE	is the size of one sample (all must be same size)
 *	START	is the index of the first sample to be modified.
 *	NSAMP	is the number of samples to add
 *	DATA	is the actual data.
 * Exit:
 *	The data has been added.
 */
{
	FldInfo *finfo;
	int findex;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "AddMData"))
		return;
/*
 * Grab the field info structure and return the number.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM, "Attempt to add data with no finfo");
		return;
	}
/*
 * Find the index of our field.
 */
	if ((findex = dc_GetIndex (finfo, field)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Attempt to add field %d not in finfo",
				field);
		return;
	}
/*
 * If we have non-uniform fields, things are more complicated, so we'll
 * hand it off.
 */
	if (! finfo->fi_Uniform)
		dc_AddNonUniform (dc, finfo, findex, t, size, start, nsamp,
					data);
/*
 * Otherwise arrange for space for this data and copy it over..
 */
	else
	{
		if (! dc_ArrangeSpace (dc, t, size, start, nsamp, finfo))
			return;
		dc_CopyData (dc, finfo, findex, start, nsamp, data);
	}
}






static void
dc_AddNonUniform (dc, finfo, findex, t, size, start, nsamp, data)
DataChunk *dc;
FldInfo *finfo;
int findex, size, start, nsamp;
ZebTime *t;
DataPtr data;
/*
 * Add this field to a DC with non-uniform data.
 */
{
	int samp, len, i;
	DataPtr space;
	FieldTOC *ft;

	for (samp = start; samp < start + nsamp; samp++)
	{
	/*
	 * If the sample exists, make sure this field is not already a 
	 * part of it, then expand to the size we need.
	 */
		if (space = dc_GetSample (dc, samp, &len))
		{
			ft = (FieldTOC *) space;
			ft[findex].ft_Offset = len;
			ft[findex].ft_Len = size;
			dc_AdjustSample (dc, samp, len + size);
			ft = (FieldTOC *) (space = dc_GetSample(dc,samp,&len));
		}
	/*
	 * Otherwise, create it.
	 */
		else
		{
			dc_AddSample (dc, t, NULL,
				size + finfo->fi_NField*sizeof (FieldTOC));
			ft = (FieldTOC *) (space = dc_GetSample(dc,samp,&len));
			for (i = 0; i < finfo->fi_NField; i++)
				ft[i].ft_Offset = ft[i].ft_Len = 0;
			ft[findex].ft_Offset = finfo->fi_NField*
						sizeof (FieldTOC);
			ft[findex].ft_Len = size;
		}
	/*
	 * Now copy in the data.
	 */
		if (data)
			memcpy ((char *) space + ft[findex].ft_Offset, data,
					size);
	/*
	 * On to the next one.
	 */
		t++;
		if (data)
			data = (char *) data + size;
	}
}







static int
dc_GetIndex (finfo, field)
FldInfo *finfo;
FieldId field;
/*
 * Return the index of this field for this DC.
 */
{
	int f;

	for (f = 0; f < finfo->fi_NField; f++)
		if (finfo->fi_Fields[f] == field)
			return (f);
	return (-1);
}






static int
dc_ArrangeSpace (dc, t, size, start, nsamp, finfo)
DataChunk *dc;
ZebTime *t;
int size, start, nsamp;
FldInfo *finfo;
/*
 * Get this data chunk set up to accept the new data (uniform fields).
 */
{
	int nexist, samp;
/*
 * Make sure the size is what we were promised.
 */
	if (size != finfo->fi_Size)
	{
		msg_ELog (EF_PROBLEM, "Size %d does not match finfo %d",
			size, finfo->fi_Size);
		return (FALSE);
	}
/*
 * If all of the samples exist already, then we need not do anything.  
 * (Though, to be proper, we should probably check the times, but I am
 *  lazy, for now.)
 */
	if ((nexist = dc_GetNSample (dc)) >= (start + nsamp))
		return (TRUE);
/*
 * If they don't all exist, there better not be gaps in the middle.
 */
	if (start > nexist)
	{
		msg_ELog (EF_PROBLEM, "Attempt to add samp %d after %d exist",
			start, nexist);
		return (FALSE);
	}
/*
 * Go through and add each one.
 */
	for (samp = start; samp < start + nsamp; samp++)
		if (samp >= nexist)
			(void) dc_AddSample (dc, t + samp - start, NULL,
						finfo->fi_NField*size);
	return (TRUE);
}






static void
dc_CopyData (dc, finfo, findex, start, nsamp, data)
DataChunk *dc;
FldInfo *finfo;
int findex, start, nsamp;
DataPtr data;
/*
 * Copy this data into the data chunk.
 */
{
	int samp, offset = finfo->fi_Size * findex;
/*
 * If no data, no work.
 */
	if (! data)
		return;
/*
 * Otherwise go through a sample at a time.
 */
	for (samp = 0; samp < nsamp; samp++)
	{
		DataPtr dest = dc_GetSample (dc, start + samp, NULL);
	/*
	 * This absolutely, undubitably, definately, certainly, without
	 * fail should never, ever happen, not even in a million years.
	 */
		if (! dest)
		{
			msg_ELog (EF_PROBLEM, "Sample %d disappeared anyway",
				samp + start);
			return;
		}
		memcpy ((char *) dest + offset, data, finfo->fi_Size);
		data = (char *) data + finfo->fi_Size;
	}
}






DataPtr
dc_GetMData (dc, sample, field, len)
DataChunk *dc;
int sample, *len;
FieldId field;
/*
 * Find a data sample in this DC.
 */
{
	FldInfo *finfo;
	FieldTOC *ft;
	int findex;
	DataPtr data;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetMData"))
		return (NULL);
/*
 * Grab the field info structure.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
	{
		msg_ELog (EF_PROBLEM, "Attempt to get data with no finfo");
		return (NULL);
	}
/*
 * Find the index of our field.
 */
	if ((findex = dc_GetIndex (finfo, field)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Attempt to get field %d not in finfo",
				field);
		return (NULL);
	}
/*
 * Find this sample.
 */
	if ((data = dc_GetSample (dc, sample, NULL)) == NULL)
		return (NULL);
/*
 * Now that we have everything, we're set.
 */
	if (finfo->fi_Uniform)
	{
		if (len)
			*len = finfo->fi_Size;
		return ((char *) data + findex*finfo->fi_Size);
	}
/*
 * In the non-uniform case, find the TOC and return the info from there.
 */
	ft = (FieldTOC *) data;
	if (ft[findex].ft_Len <= 0)
		return (NULL);
	if (len)
		*len = ft[findex].ft_Len;
	return ((char *) data + ft[findex].ft_Offset);
}





static void
dc_DumpMD (dc)
DataChunk *dc;
/*
 * Dump out this DC.
 */
{
	FldInfo *finfo;
	int i;
/*
 * The usual sanity checking.
 */
	if (! dc_ReqSubClassOf (dc->dc_Class, DCC_MetData, "GetMData"))
		return;
/*
 * Grab the field info structure.
 */
	if ((finfo = (FldInfo *) dc_FindADE (dc, DCC_MetData, ST_FLDINFO, 0))
						== NULL)
		return;
/*
 * Write.
 */
	printf ("METDATA class, %d fields, uniform %s, size %d\n\tFields: ",
		finfo->fi_NField, finfo->fi_Uniform ? "True" : "False",
		finfo->fi_Size);
	for (i = 0; i < finfo->fi_NField; i++)
		printf (" %s", F_GetName (finfo->fi_Fields[i]));
	printf ("\n");
}
