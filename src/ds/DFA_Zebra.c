/*
 * All the goodies for the zeb native file format.
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

# include <sys/types.h>
# include <fcntl.h>
# include <unistd.h>
# include <errno.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "znfile.h"
# include "ds_fields.h"

MAKE_RCSID ("$Id: DFA_Zebra.c,v 1.1 1992-06-10 16:00:00 corbet Exp $");


/*
 * The open file tag.
 */
typedef struct _znTag
{
	zn_Header	zt_Hdr;		/* The file header		*/
	int		zt_Fd;		/* Descriptor of open file	*/
	int		zt_Sync;	/* Which pieces need synching	*/
	int		zt_Write;	/* File open for write access	*/
	ZebTime		*zt_Time;	/* The time array		*/
	zn_Sample	*zt_Sample;	/* Sample array			*/
	FieldId		*zt_Fields;	/* The field array		*/
	zn_Field	*zt_FNames;	/* Names of the fields		*/
	PlatformId	*zt_Ids;	/* Station ID's (irgrid)	*/
	Location	*zt_Locs;	/* Location array		*/
	zn_Sample	*zt_Attr;	/* Per-sample attribute array	*/
	char		*zt_GlAttr;	/* Global attribute block	*/
} znTag;

/*
 * Sync flags.
 */
# define SF_HEADER	0x0001		/* Synchronize header		*/
# define SF_TIME	0x0002		/* Time array			*/
# define SF_SAMPLE	0x0004		/* Sample array			*/
# define SF_FIELD	0x0008		/* Field array			*/
# define SF_LOCATION	0x0010		/* Location array		*/
# define SF_ATTR	0x0020		/* Sample attributes		*/


/*
 * Used in zn_TimeIndex() -- the minimum time array size before it is
 * worth the effort to set up and execute a binary search.
 */
# define MINTIME 24


/*
 * The class/organization compatibility table.  If the desired class
 * and the given file organization appear together here, we can do it.
 */
static struct CO_Compat
{
	DataOrganization	c_org;
	DataClass		c_class;
} COCTable [] =
{
	{ OrgOutline,		DCC_Boundary	},
# ifdef notdef
	{ Org1dGrid,		DCC_RGrid	},
	{ Org2dGrid,		DCC_RGrid	},
	{ Org3dGrid,		DCC_RGrid	},
	{ OrgIRGrid,		DCC_IRGrid	},
	{ OrgIRGrid,		DCC_Scalar	},
	{ OrgScalar,		DCC_Scalar	},
	{ OrgScalar,		DCC_Location	},
# endif
};
# define N_COC (sizeof (COCTable)/sizeof (struct CO_Compat))


/*
 * Forwards.
 */
static void	zn_CFMakeFields FP ((znTag *, DataChunk *));
static void	zn_WriteSync FP ((znTag *));
static void	zn_CFMakeStations FP ((znTag *, DataChunk *));
static int	zn_FindDest FP ((znTag *, ZebTime *, WriteCode));
static void	zn_ExpandTOC FP ((znTag *));
static void	zn_OpenSlot FP ((znTag *, int));
static int	zn_WrBoundary FP ((znTag *, DataChunk *, int, zn_Sample *,
			 WriteCode, int));
static void	zn_GetFieldIndex FP ((znTag *, FieldId *, int, int *));
static void	zn_DataWrite FP ((znTag *, void *, int, zn_Sample *,
			WriteCode));
static void	zn_OFLoadStations FP ((znTag *));
static void	zn_PutAttrs FP ((znTag *, int, void *, int));
static int	zn_OrgClassCompat FP ((DataOrganization, DataClass));
static void	zn_ReadBoundary FP ((znTag *, DataChunk *, int, int));

static long	zn_GetFromFree FP ((znTag *, int, long, zn_Free *, long));
static long	zn_GetSpace FP ((znTag *, int));
static void	zn_GetFreeBlock FP ((znTag *, long, zn_Free *));
static void	zn_GetBlock FP ((znTag *, long, void *, int));
static void	zn_PutBlock FP ((znTag *, long, void *, int));
static void	zn_FreeSpace FP ((znTag *, long, int));







int
zn_CreateFile (df, dc, rtag)
DataFile *df;
DataChunk *dc;
char **rtag;
/*
 * Create a new zeb native file.
 */
{
	znTag *tag = ALLOC (znTag);
	zn_Header *hdr = &tag->zt_Hdr;
	int ssize, asize;
	void *ablock;
/*
 * Create the file itself before we go anywhere.
 */
	if ((tag->zt_Fd = open (df->df_name, O_RDWR|O_TRUNC|O_CREAT, 0666)) <0)
	{
		msg_ELog (EF_PROBLEM, "Can't create file %s (%d)",
			df->df_name, errno);
		free (tag);
		return (FALSE);
	}
/*
 * Initialize the file tag.
 */
	tag->zt_Sync = SF_HEADER;
	tag->zt_Sample = 0; tag->zt_Fields = 0;
	tag->zt_FNames = 0; tag->zt_Ids = 0; tag->zt_Locs = 0;
	tag->zt_Write = TRUE;
	tag->zt_Attr = 0;
	tag->zt_GlAttr = 0;
/*
 * Header initialization.  Once this is done we can start space allocation.
 */
	hdr->znh_Magic = ZN_MAGIC;
	hdr->znh_Free = -1;
	hdr->znh_NFree = hdr->znh_NFreeB = hdr->znh_Len = 0;
	hdr->znh_NSample = hdr->znh_NField = 0;
	hdr->znh_Org = PTable[df->df_platform].dp_org;
	hdr->znh_OffLoc = -1;
	hdr->znh_OffGlAttr = hdr->znh_OffAttr = -1;
	hdr->znh_GlAttrLen = 0;
/*
 * Allocate the space for the header itself and sync it out.
 */
	(void) zn_GetSpace (tag, sizeof (zn_Header)); /* Know it's at 0 */
	zn_WriteSync (tag);
/*
 * Allocate the time array.
 */
	tag->zt_Time = (ZebTime *) malloc (ZN_GRAIN * sizeof (ZebTime));
	hdr->znh_OffTime = zn_GetSpace (tag, ZN_GRAIN*sizeof (ZebTime));
	hdr->znh_NSampAlloc = ZN_GRAIN;
	tag->zt_Sync |= SF_HEADER | SF_TIME;
/*
 * Allocate and fill in the field info array.
 */
 	zn_CFMakeFields (tag, dc);
/*
 * Allocate the sample offset array.
 */
	ssize = ZN_GRAIN*hdr->znh_NField*sizeof (zn_Sample);
	tag->zt_Sample = (zn_Sample *) malloc (ssize);
	hdr->znh_OffSample = zn_GetSpace (tag, ssize);
	tag->zt_Sync |= SF_SAMPLE;
/*
 * Add other chunks to the file if appropriate.
 */
	if (dc->dc_Class == DCC_IRGrid)		/* IRGRID station array	*/
		zn_CFMakeStations (tag, dc);
	if (ds_IsMobile (dc->dc_Platform))	/* Locations		*/
	{
		tag->zt_Locs = (Location *) malloc (ZN_GRAIN*sizeof(Location));
		hdr->znh_OffLoc = zn_GetSpace (tag, ZN_GRAIN*sizeof(Location));
	}
	else
		dc_GetLoc (dc, 0, &hdr->znh_Loc);
/*
 * Global attributes too.
 */
	if ((ablock = dc_GetGlAttrBlock (dc, &asize)) != 0)
	{
		hdr->znh_OffGlAttr = zn_GetSpace (tag, asize);
		hdr->znh_GlAttrLen = asize;
		zn_PutBlock (tag, hdr->znh_OffGlAttr, ablock, asize);
		tag->zt_GlAttr = malloc (asize);
		memcpy (tag->zt_GlAttr, ablock, asize);
	}
/*
 * IMAGE, RGRID -- rgrid array
 */
	zn_WriteSync (tag);
	*rtag = (char *) tag;
	return (TRUE);
}





static void
zn_CFMakeFields (tag, dc)
znTag *tag;
DataChunk *dc;
/*
 * Initialize the fields of this file from the data chunk.
 */
{
	int i, nf;
	FieldId *fids, bfid;
/*
 * Make a special case for boundaries.
 */
	if (dc->dc_Class == DCC_Boundary)
	{
		bfid = F_Lookup ("boundary");
		fids = &bfid;
		nf = 1;
	}
	else
		fids = dc_GetFields (dc, &nf);
/*
 * Put the field array into the tag.
 */
	tag->zt_Fields = (FieldId *) malloc (nf * sizeof (FieldId));
	tag->zt_Sync |= SF_FIELD;
	tag->zt_Hdr.znh_OffField = zn_GetSpace (tag, nf * sizeof (zn_Field));
	tag->zt_Hdr.znh_NField = nf;
	tag->zt_FNames = (zn_Field *) malloc (nf * sizeof (zn_Field));
/*
 * Go through and fill it all in.
 */
	for (i = 0; i < nf; i++)
	{
		tag->zt_Fields[i] = fids[i];
		strcpy (tag->zt_FNames[i], F_GetName (fids[i]));
	}
}








static void
zn_WriteSync (tag)
znTag *tag;
/*
 * Synchronize parts of this file which have been modified.
 */
{
	zn_Header *hdr = &tag->zt_Hdr;
/*
 * Dump out the various tables.
 */
	if (tag->zt_Sync & SF_HEADER)
		zn_PutBlock (tag, 0, &tag->zt_Hdr, sizeof (zn_Header));
	if (tag->zt_Sync & SF_TIME)
		zn_PutBlock (tag, hdr->znh_OffTime, tag->zt_Time,
			hdr->znh_NSample*sizeof (ZebTime));
	if (tag->zt_Sync & SF_SAMPLE)
		zn_PutBlock (tag, hdr->znh_OffSample, tag->zt_Sample,
			hdr->znh_NField*hdr->znh_NSample*sizeof (zn_Sample));
	if (tag->zt_Sync & SF_LOCATION)
		zn_PutBlock (tag, hdr->znh_OffLoc, tag->zt_Locs,
			hdr->znh_NSample*sizeof (Location));
	if (tag->zt_Sync & SF_FIELD)
		zn_PutBlock (tag, hdr->znh_OffField, tag->zt_FNames,
			hdr->znh_NField*sizeof (zn_Field));
	if (tag->zt_Sync & SF_ATTR)
		zn_PutBlock (tag, hdr->znh_OffAttr, tag->zt_Attr,
			hdr->znh_NSample*sizeof (zn_Sample));
	tag->zt_Sync = 0;
}





static void
zn_CFMakeStations (tag, dc)
znTag *tag;
DataChunk *dc;
/*
 * Put the station array into this file.
 */
{
	int nplat = dc_IRGetNPlatform (dc), sta;
	zn_Header *hdr = &tag->zt_Hdr;
	zn_Station *zst;
/*
 * Allocate all the space we need.
 */
	tag->zt_Ids = (PlatformId *) malloc (nplat*sizeof (PlatformId));
	tag->zt_Locs = (Location *) malloc (nplat*sizeof (Location));
	zst = (zn_Station *) malloc (nplat*sizeof (zn_Station));
	hdr->znh_OffStation = zn_GetSpace (tag, nplat*sizeof (zn_Station));
/*
 * Gather together the info we need.
 */
	dc_IRGetPlatforms (dc, tag->zt_Ids, tag->zt_Locs);
	for (sta = 0; sta < nplat; sta++)
	{
		strcpy (zst[sta].zns_Name, ds_PlatformName (tag->zt_Ids[sta]));
		zst[sta].zns_Loc = tag->zt_Locs[sta];
	}
/*
 * Put this out to the file and we are done.
 */
	zn_PutBlock (tag, hdr->znh_OffStation, zst, nplat*sizeof (zn_Station));
	free (zst);
}







int
zn_QueryTime (file, begin, end, nsample)
char *file;
ZebTime *begin, *end;
int *nsample;
/*
 * Tell the daemon about what is in this file, anyway.
 */
{
	znTag tag;
	zn_Header hdr;
/*
 * Open up the file.
 */
	if ((tag.zt_Fd = open (file, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, file);
		return (FALSE);
	}
/*
 * Pull in the header, followed by the times.
 */
	zn_GetBlock (&tag, 0, &hdr, sizeof (hdr));
	*nsample = hdr.znh_NSample;
	zn_GetBlock (&tag, hdr.znh_OffTime, begin, sizeof (ZebTime));
	zn_GetBlock (&tag, hdr.znh_OffTime +
		(hdr.znh_NSample - 1)*sizeof(ZebTime), end, sizeof (ZebTime));
/*
 * Done.
 */
	close (tag.zt_Fd);
	return (TRUE);
}





int
zn_GetIRGLoc (dfindex, locs)
int dfindex;
Location *locs;
/*
 * Get the station locations in this file.
 */
{
	znTag *tag;
/*
 * Make sure the file is open.
 */
	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (FALSE);
	memcpy (locs, tag->zt_Locs, tag->zt_Hdr.znh_NStation*sizeof(Location));
	return (TRUE);
}




static int
zn_TimeIndex (tag, t)
znTag *tag;
ZebTime *t;
/*
 * Find the offset to the first sample in this file before or at the 
 * given time.
 */
{
	int i;
	zn_Header *hdr = &tag->zt_Hdr;
/*
 * Check the extreme cases.
 */
	if (TC_Less (*t, tag->zt_Time[0]))
		return (-1);
	else if (TC_LessEq (tag->zt_Time[hdr->znh_NSample - 1], *t))
		return (hdr->znh_NSample - 1);
/*
 * Do a search for the time.  For short time arrays, we don't bother
 * with the binary search.
 */
	if (hdr->znh_NSample < MINTIME)
	{
		for (i = hdr->znh_NSample - 1; i >= 0; i--)
			if (TC_LessEq (tag->zt_Time[i], *t))
				return (i);
	}
/*
 * Longer arrays (and longer can be some thousands) benefit from
 * the binary search.
 */
	else
	{
		int top = hdr->znh_NSample - 1, bottom = 0;
		while (top > bottom + 1)
		{
			int mid = (top + bottom)/2;
			if (TC_LessEq (tag->zt_Time[mid], *t))
			{
				if (TC_Eq (tag->zt_Time[mid], *t))
					return (mid);
				bottom = mid;
			}
			else
				top = mid;
		}
		return (TC_LessEq (tag->zt_Time[top], *t) ? top : bottom);
	}
	return (0); /* never reached -- shuts up saber */
}








zn_PutSample (dfile, dc, sample, wc)
int dfile, sample;
DataChunk *dc;
WriteCode wc;
/*
 * Dump a sample's worth of data into the file.
 */
{
	int fsample, nfield, *index = 0, alen;
	FieldId *fids;
	znTag *tag;
	ZebTime t;
	zn_Sample *samp;
	zn_Header *hdr;
	void *ablock;
	char atime[30];
/*
 * Open up the file.
 */
	if (! dfa_OpenFile (dfile, TRUE, (void *) &tag))
		return (0);
	hdr = &tag->zt_Hdr;
/*
 * Figure out where this sample is to be written.
 */
	dc_GetTime (dc, sample, &t);
	fsample = zn_FindDest (tag, &t, wc);
	TC_EncodeTime (&t, TC_TimeOnly, atime);
	msg_ELog (EF_DEBUG, "PutSample, code %d, fsample %d t %s", wc,
		fsample, atime);
/*
 * Get our field array into order.
 */
	if (dc->dc_Class != DCC_Boundary)
	{
		fids = dc_GetFields (dc, &nfield);
		index = (int *) malloc (nfield*sizeof (int));
		zn_GetFieldIndex (tag, fids, nfield, index);
	}
	samp = tag->zt_Sample + (fsample*hdr->znh_NField);
/*
 * Now it is a matter of writing out the data according to the type of
 * things.
 */
	switch (hdr->znh_Org)
	{
	   case OrgOutline:
	   	zn_WrBoundary (tag, dc, sample, samp, wc, 0);
		msg_ELog (EF_DEBUG, " Bnd ended up %d at %ld", samp->znf_Size,
			samp->znf_Offset);
		break;
	}
/*
 * We also have to add the time to the time array.  We flush the individual
 * time out here rather than dirty up and sync the entire array.
 */
	/* dc_GetTime (dc, sample, tag->zt_Time + fsample); */
	tag->zt_Time[fsample] = t;
	zn_PutBlock (tag, hdr->znh_OffTime + fsample*sizeof (ZebTime),
			tag->zt_Time + fsample, sizeof (ZebTime));
/*
 * Save out the sample array.
 */
	zn_PutBlock (tag, hdr->znh_OffSample +
			fsample*hdr->znh_NField*sizeof (zn_Sample),
			tag->zt_Sample + fsample*hdr->znh_NField,
			hdr->znh_NField*sizeof (zn_Sample));
/*
 * If there are sample attributes, save them.
 */
	if (ablock = dc_GetSaAttrBlock (dc, sample, &alen))
		zn_PutAttrs (tag, fsample, ablock, alen);
/*
 * Done.
 */
 	if (index)
		free (index);
	zn_WriteSync (tag);
	return (1);
}





static void
zn_PutAttrs (tag, sample, ablock, alen)
znTag *tag;
int sample, alen;
void *ablock;
/*
 * Dump this attribute block out to the file.
 */
{
	zn_Header *hdr = &tag->zt_Hdr;
	zn_Sample *zs;
/*
 * The real key here is whether there is already an attribute array in
 * this file or not.  If not, we need to make one.
 */
	if (hdr->znh_OffAttr < 0)
	{
		int size = hdr->znh_NSampAlloc*sizeof (zn_Sample);
		hdr->znh_OffAttr = zn_GetSpace (tag, size);
		tag->zt_Attr = (zn_Sample *) malloc (size);
		memset (tag->zt_Attr, 0, size);
		tag->zt_Sync |= SF_ATTR;
	}
/*
 * If there is already an attribute array, free it up.
 */
	zs = tag->zt_Attr + sample;
	if (zs->znf_Size > 0)
		zn_FreeSpace (tag, zs->znf_Offset, zs->znf_Size);
/*
 * Allocate the new space and fill it in.
 */
	zs->znf_Size = alen;
	zs->znf_Offset = zn_GetSpace (tag, alen);
	zn_PutBlock (tag, zs->znf_Offset, ablock, alen);
/*
 * Sync out this attribute entry.
 */
	zn_PutBlock (tag, hdr->znh_OffAttr + sample*sizeof (zn_Sample),
		zs, sizeof (zn_Sample));
}







static void
zn_GetFieldIndex (tag, fids, nfield, index)
znTag *tag;
FieldId *fids;
int nfield, *index;
/*
 * Figure out file field corresponds to each dc field.  This routine
 * will add the fields to the file if necessary.
 */
{
	int nnew = 0, dcfield, ffield, newsize, sample, field;
	zn_Header *hdr = &tag->zt_Hdr;
	zn_Sample *new, *np, *op;
/*
 * Pass through everything and match them up.
 */
	for (dcfield = 0; dcfield < nfield; dcfield++)
	{
		for (ffield = 0; ffield < hdr->znh_NField; ffield++)
			if (fids[dcfield] == tag->zt_Fields[ffield])
				break;
		if (ffield < hdr->znh_NField)
			index[dcfield] = ffield;
		else
		{
			index[dcfield] = -1;
			nnew++;
		}
	}
/*
 * If we found all of our fields, we can quit.
 */
 	if (! nnew)
		return;
/*
 * Otherwise we are going to have to expand the sample table.
 */
	newsize = hdr->znh_NSampAlloc*(hdr->znh_NField+nnew)*sizeof(zn_Sample);
	np = new = (zn_Sample *) malloc (newsize);
	zn_FreeSpace (tag, hdr->znh_OffSample,
		hdr->znh_NSampAlloc*hdr->znh_NField*sizeof (zn_Sample));
	hdr->znh_OffSample = zn_GetSpace (tag, newsize);
	tag->zt_Sync |= SF_SAMPLE | SF_HEADER;
/*
 * Copy the old sample info to the new, and add the new fields.
 */
	op = tag->zt_Sample;
	for (sample = 0; sample < hdr->znh_NSample; sample++)
	{
		for (field = 0; field < hdr->znh_NField; field++)
			*np++ = *op++;
		for (field = 0; field < nnew; field++)
		{
			np->znf_Size = np->znf_Offset = 0;
			np++;
		}
	}
	free (tag->zt_Sample);
	tag->zt_Sample = new;
/*
 * Expand the field name table.
 */
	tag->zt_FNames = (zn_Field *) realloc (tag->zt_FNames,
				(hdr->znh_NField + nnew)*sizeof (zn_Field));
	zn_FreeSpace (tag, hdr->znh_OffField,
				hdr->znh_NField*sizeof (zn_Field));
	hdr->znh_OffField = zn_GetSpace (tag, 
				(hdr->znh_NField + nnew)*sizeof (zn_Field));
/*
 * Now we can map the remaining fields.
 */
	for (dcfield = 0; dcfield < nfield; dcfield++)
		if (index[dcfield] < 0)
		{
			strcpy (tag->zt_FNames[hdr->znh_NField],
					F_GetName (fids[dcfield]));
			index[dcfield] = hdr->znh_NField++;
		}
/*
 * Write out the new field name table (sync won't do that) and we are 
 * done.
 */
	zn_PutBlock (tag, hdr->znh_OffField, tag->zt_FNames,
					hdr->znh_NField*sizeof (zn_Field));
}






static int
zn_FindDest (tag, t, wc)
znTag *tag;
ZebTime *t;
WriteCode wc;
/*
 * Figure out just where we will be writing this piece of data.
 */
{
	int sample;
	switch (wc)
	{
	/*
	 * The append case is relatively simple.
	 */
	   case wc_Append:
	   	zn_ExpandTOC (tag);
		return (tag->zt_Hdr.znh_NSample - 1);
	/*
	 * For the overwrite case, we just find the unlucky sample
	 * and return that.
	 */
	   case wc_Overwrite:
		sample = zn_TimeIndex (tag, t);
		return (sample);
	/*
	 * For the insert case, we find the place to do the insertion,
	 * and open up the space.
	 */
	   case wc_Insert:
		sample = zn_TimeIndex (tag, t) + 1;
		zn_OpenSlot (tag, sample);
		return (sample);
	}
	return (-99999);	/* Should never get here */
}





static void
zn_ExpandTOC (tag)
znTag *tag;
/*
 * Make the contents information in this file one entry bigger.
 */
{
	zn_Header *hdr = &tag->zt_Hdr;
	int newns, oldns = hdr->znh_NSampAlloc;
/*
 * The easy case is when the tables are already big enough.
 */
	if (hdr->znh_NSample < hdr->znh_NSampAlloc)
	{
		hdr->znh_NSample++;
		tag->zt_Sync |= SF_HEADER;
		return;
	}
/*
 * Otherwise we have to make everything bigger.  Start with the time array.
 */
	newns = (hdr->znh_NSampAlloc += ZN_GRAIN);
	tag->zt_Time = (ZebTime *) realloc (tag->zt_Time, 
					newns*sizeof (ZebTime));
	zn_FreeSpace (tag, hdr->znh_OffTime, oldns*sizeof (ZebTime));
	hdr->znh_OffTime = zn_GetSpace (tag, newns*sizeof (ZebTime));
/*
 * Now the sample information array.
 */
	tag->zt_Sample = (zn_Sample *) realloc (tag->zt_Sample,
				hdr->znh_NField*newns*sizeof (zn_Sample));
	zn_FreeSpace (tag, hdr->znh_OffSample,
				hdr->znh_NField*oldns*sizeof (zn_Sample));
	hdr->znh_OffSample = zn_GetSpace (tag,
				hdr->znh_NField*newns*sizeof (zn_Sample));
	tag->zt_Sync |= SF_HEADER | SF_SAMPLE;
/*
 * The location array if need be.
 */
	if (hdr->znh_OffLoc >= 0)
	{
		tag->zt_Locs = (Location *) realloc (tag->zt_Locs,
					newns*sizeof (Location));
		zn_FreeSpace (tag, hdr->znh_OffLoc, oldns*sizeof (Location));
		hdr->znh_OffLoc = zn_GetSpace (tag, newns*sizeof (Location));
		tag->zt_Sync |= SF_LOCATION;
	}
/*
 * Attributes.
 */
	if (hdr->znh_OffAttr >= 0)
	{
		tag->zt_Attr = (zn_Sample *) realloc (tag->zt_Attr,
					newns*sizeof (zn_Sample));
		zn_FreeSpace (tag, hdr->znh_OffAttr, oldns*sizeof (zn_Sample));
		hdr->znh_OffAttr = zn_GetSpace (tag, newns*sizeof (zn_Sample));
		memset (tag->zt_Attr + oldns, 0, ZN_GRAIN*sizeof (zn_Sample));
		zn_PutBlock (tag, hdr->znh_OffAttr, tag->zt_Attr,
				newns*sizeof (zn_Sample));
	}
	hdr->znh_NSample++;
}




static void
zn_OpenSlot (tag, sample)
znTag *tag;
int sample;
/*
 * Open up a slot before the given sample.
 */
{
	zn_Header *hdr = &tag->zt_Hdr;
	int nmove = hdr->znh_NSample - sample + 1;
/*
 * Get some new space in the file.
 */
	zn_ExpandTOC (tag);
/*
 * Shift the various tables around.
 */
	bcopy (tag->zt_Time + sample, tag->zt_Time + sample + 1,
			nmove*sizeof (ZebTime));
	bcopy (tag->zt_Sample + sample*hdr->znh_NField,
		tag->zt_Sample + (sample + 1)*hdr->znh_NField,
		nmove*hdr->znh_NField*sizeof (zn_Sample));
	tag->zt_Sync |= SF_HEADER | SF_TIME | SF_SAMPLE;
/*
 * If there are locations do them too.
 */
	if (hdr->znh_OffLoc >= 0)
	{
		bcopy (tag->zt_Locs + sample, tag->zt_Locs + sample + 1,
			nmove*sizeof (Location));
		tag->zt_Sync |= SF_LOCATION;
	}
/*
 * If attributes, also them.
 */
	if (hdr->znh_OffAttr >= 0)
	{
		bcopy (tag->zt_Attr + sample, tag->zt_Attr + sample + 1,
			nmove*sizeof (zn_Sample));
		tag->zt_Sync |= SF_ATTR;
		tag->zt_Attr[sample].znf_Size = 0;
		tag->zt_Attr[sample].znf_Offset = 0;
	}
/* XXX OTHER TABLES TOO (attr). */
}






static int
zn_WrBoundary (tag, dc, sample, samp, wc, index)
znTag *tag;
DataChunk *dc;
int sample, index;
zn_Sample *samp;
WriteCode wc;
/*
 * Write out a boundary sample.
 */
{
	Location *locs;
	int nloc;
/*
 * Pull out the boundary itself, then dump it out.
 */
	locs = dc_BndGet (dc, sample, &nloc);
	zn_DataWrite (tag, locs, nloc*sizeof (Location), samp + index, wc);
	return (1);
}






static void
zn_DataWrite (tag, data, size, samp, wc)
znTag *tag;
void *data;
int size;
zn_Sample *samp;
WriteCode wc;
/*
 * Write this chunk of data out to the file.
 */
{
/*
 * If this is an overwrite, we need to figure out what to do with the
 * space.  If the new size is not bigger than the old, we can recycle;
 * otherwise we need to free it and start over.
 */
	if (wc == wc_Overwrite && samp->znf_Size > 0)
	{
		if (samp->znf_Size < size)	/* Start over */
		{
			zn_FreeSpace (tag, samp->znf_Offset, samp->znf_Size);
			samp->znf_Offset = zn_GetSpace (tag, size);
		}
		else if (samp->znf_Size > size)	/* Free remainder */
			zn_FreeSpace (tag, samp->znf_Offset + size,
					samp->znf_Size - size);
		/* Else equal -- do nothing */
	}
/*
 * Otherwise this is a new allocation.
 */
	else
		samp->znf_Offset = zn_GetSpace (tag, size);
/*
 * Now we just write.
 */
	samp->znf_Size = size;
	zn_PutBlock (tag, samp->znf_Offset, data, size);	
}







zn_Close (ctag)
void *ctag;
/*
 * Close this file.
 */
{
	znTag *tag = (znTag *) ctag;
/*
 * Close the file itself.
 */
	if (tag->zt_Write)
		zn_WriteSync (tag);
	close (tag->zt_Fd);
/*
 * Now free up all the storage.
 */
	free (tag->zt_Time);
	if (tag->zt_Sample)
		free (tag->zt_Sample);
	if (tag->zt_Fields)
		free (tag->zt_Fields);
	if (tag->zt_FNames)
		free (tag->zt_FNames);
	if (tag->zt_Ids)
		free (tag->zt_Ids);
	if (tag->zt_Locs)
		free (tag->zt_Locs);
	if (tag->zt_GlAttr)
		free (tag->zt_GlAttr);
	if (tag->zt_Attr)
		free (tag->zt_Attr);
	free (tag);
	return (0);
}





int
zn_Open (df, write, rtag)
DataFile *df;
bool write;
void **rtag;
/*
 * Open an existing data file.
 */
{
	znTag *tag = ALLOC (znTag);
	zn_Header *hdr = &tag->zt_Hdr;
	int nsa, field;
/*
 * Open the file.
 */
	if ((tag->zt_Fd = open (df->df_name, write ? O_RDWR : O_RDONLY)) < 0)
	{
		free (tag);
		msg_ELog (EF_PROBLEM, "Can't open %s (%d)", df->df_name,errno);
		return (FALSE);
	}
	zn_GetBlock (tag, 0, &tag->zt_Hdr, sizeof (zn_Header));
/*
 * Pull in the time and sample arrays.
 */
	nsa = hdr->znh_NSampAlloc;
	tag->zt_Time = (ZebTime *) malloc(nsa*sizeof (ZebTime));
	zn_GetBlock (tag, hdr->znh_OffTime, tag->zt_Time, nsa*sizeof(ZebTime));
	tag->zt_Sample = (zn_Sample *)
			malloc (nsa*hdr->znh_NField*sizeof (zn_Sample));
	zn_GetBlock (tag, hdr->znh_OffSample, tag->zt_Sample,
			nsa*hdr->znh_NField*sizeof (zn_Sample));
/*
 * Pull in the fields and convert them to fids.
 */
	tag->zt_FNames = (zn_Field *) malloc(hdr->znh_NField*sizeof(zn_Field));
	zn_GetBlock (tag, hdr->znh_OffField, tag->zt_FNames,
				hdr->znh_NField*sizeof (zn_Field));
	tag->zt_Fields = (FieldId *) malloc (hdr->znh_NField*sizeof (FieldId));
	for (field = 0; field < hdr->znh_NField; field++)
		tag->zt_Fields[field] = F_Lookup (tag->zt_FNames[field]);
/*
 * If there is a station array, pull it in.
 */
	if (hdr->znh_Org == OrgIRGrid)
		zn_OFLoadStations (tag);
/*
 * Otherwise look to see if there is a location array.  (i.e. irgrids are
 * never mobile.)
 */
	if (ds_IsMobile (df->df_platform))
	{
		tag->zt_Locs = (Location *) malloc (nsa*sizeof (Location));
		zn_GetBlock (tag, hdr->znh_OffLoc, tag->zt_Locs,
				nsa*sizeof (Location));
	}
/*
 * If we have global attributes, get them.
 */
	if (hdr->znh_GlAttrLen > 0)
	{
		tag->zt_GlAttr = malloc (hdr->znh_GlAttrLen);
		zn_GetBlock (tag, hdr->znh_OffGlAttr, tag->zt_GlAttr,
			hdr->znh_GlAttrLen);
	}
	else
		tag->zt_GlAttr = 0;
/*
 * Also the sample attribute index.
 */
	if (hdr->znh_OffAttr >= 0)
	{
		tag->zt_Attr = (zn_Sample *) malloc (nsa*sizeof (zn_Sample));
		zn_GetBlock (tag, hdr->znh_OffAttr, tag->zt_Attr,
				nsa*sizeof (zn_Sample));
	}
	else
		tag->zt_Attr = 0;
/*
 * Done.
 */
	tag->zt_Sync = 0;
	tag->zt_Write = write;
	*rtag = (void *) tag;
	return (TRUE);
}





static void
zn_OFLoadStations (tag)
znTag *tag;
/*
 * Pull in the station array for this file.
 */
{
	zn_Header *hdr = &tag->zt_Hdr;
	int sta, nsta = hdr->znh_NStation;
	zn_Station *zs = (zn_Station *) malloc (nsta*sizeof (zn_Station));
/*
 * Pull in the station array.
 */
	zn_GetBlock (tag, hdr->znh_OffStation, zs, nsta*sizeof (zn_Station));
/*
 * Allocate a station ID array and translate all the names.
 */
	tag->zt_Ids = (PlatformId *) malloc (nsta*sizeof (PlatformId));
	tag->zt_Locs = (Location *) malloc (nsta*sizeof (Location));
	for (sta = 0; sta < nsta; sta++)
	{
		tag->zt_Ids[sta] = ds_LookupPlatform (zs[sta].zns_Name);
		if (tag->zt_Ids[sta] == BadPlatform)
			msg_ELog (EF_PROBLEM, "File station %s unknown",
					zs[sta].zns_Name);
		tag->zt_Locs[sta] = zs[sta].zns_Loc;
	}
	free (zs);
}





zn_MakeFileName (dir, platform, zt, dest)
char *dir, *platform, *dest;
ZebTime *zt;
/*
 * Generate a file name.
 */
{
	date t;

	TC_ZtToUI (zt, &t);
	sprintf (dest, "%s/%s.%06d.%04d.znf", dir, platform, t.ds_yymmdd,
		t.ds_hhmmss / 100);
}






zn_Sync (ctag)
void *ctag;
/*
 * Synchronize this file with what is on disk.
 */
{
	znTag *tag = (znTag *) ctag;
	zn_Header *hdr = &tag->zt_Hdr;
	int oldnsa = hdr->znh_NSampAlloc, oldnf = hdr->znh_NField, nsa;
/*
 * Pull in the new header.  If the sizes of tables have changed, we need
 * to reallocate them.
 */
	zn_GetBlock (tag, 0, hdr, sizeof (zn_Header));
	nsa = hdr->znh_NSampAlloc;
	if (oldnsa != hdr->znh_NSampAlloc)
	{
		tag->zt_Time = (ZebTime *) realloc (tag->zt_Time,
				nsa*sizeof (ZebTime));
		tag->zt_Sample = (zn_Sample *) realloc (tag->zt_Sample,
				nsa*hdr->znh_NField*sizeof (zn_Sample));
		if (tag->zt_Locs)
			tag->zt_Locs = (Location *) realloc (tag->zt_Locs,
				nsa*sizeof (Location));
		if (tag->zt_Attr)
			tag->zt_Attr = (zn_Sample *) realloc (tag->zt_Attr,
				nsa*sizeof (zn_Sample));
	}
/*
 * If there are new fields we have other stuff to do.
 */
	if (oldnf != hdr->znh_NField)
	{
		if (oldnsa == hdr->znh_NSampAlloc)
			tag->zt_Sample = (zn_Sample *) realloc (tag->zt_Sample,
				nsa*hdr->znh_NField*sizeof (zn_Sample));
		tag->zt_Fields = (FieldId *) realloc (tag->zt_Fields,
				hdr->znh_NField*sizeof (FieldId));
		tag->zt_FNames = (zn_Field *) realloc (tag->zt_FNames,
				hdr->znh_NField*sizeof (zn_Field));
	}
/*
 * Now, finally, we can read in the new info.
 */
	zn_GetBlock (tag, hdr->znh_OffTime, tag->zt_Time, nsa*sizeof(ZebTime));
	zn_GetBlock (tag, hdr->znh_OffSample, tag->zt_Sample,
				nsa*hdr->znh_NField*sizeof (zn_Sample));
	if (tag->zt_Locs)
		zn_GetBlock (tag, hdr->znh_OffLoc, tag->zt_Locs,
						nsa*sizeof (Location));
	if (tag->zt_Attr)
		zn_GetBlock (tag, hdr->znh_OffAttr, tag->zt_Attr,
						nsa*sizeof (zn_Sample));
/*
 * Deal with fields if necessary.
 */
	if (hdr->znh_NField != oldnf)
	{
		int fld;
		zn_GetBlock (tag, hdr->znh_OffField, tag->zt_FNames,
					hdr->znh_NField*sizeof (zn_Field));
		for (fld = 0; fld < hdr->znh_NField; fld++)
			tag->zt_Fields[fld] = F_Lookup (tag->zt_FNames[fld]);
	}
	return (TRUE);
}





DataChunk *
zn_Setup (gp, fields, nfield, class)
GetList *gp;
FieldId *fields;
int nfield;
DataClass class;
/*
 * Get set up to pull out some data.
 */
{
	znTag *tag;
	DataChunk *dc;
/*
 * Get the file open for starters.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (NULL);
/*
 * Make sure this is a combination we can do.
 */
	if (! zn_OrgClassCompat (tag->zt_Hdr.znh_Org, class))
	{
		msg_ELog (EF_PROBLEM, "Class/org mismatch");
		return (NULL);
	}
/*
 * Create the data chunk.
 */
	dc = dc_CreateDC (class);
/*
 * If there are any global attributes in this file, put them in the
 * data chunk.
 */
	if (tag->zt_GlAttr)
		dc_SetGlAttrBlock (dc, tag->zt_GlAttr,
					tag->zt_Hdr.znh_GlAttrLen);
/*
 * Do class-specific setup.
 */

/*
 * Done.
 */
	return (dc);
}






static int
zn_OrgClassCompat (org, class)
DataOrganization org;
DataClass class;
/*
 * Return TRUE iff these two are compatible.
 */
{
	int i;
/*
 * Go through and see if we find the combination in the table.
 */
	for (i = 0; i < N_COC; i++)
		if (class == COCTable[i].c_class && org == COCTable[i].c_org)
			return (TRUE);
	return (FALSE);
}






int
zn_GetData (dc, gp, details, ndetail)
DataChunk *dc;
GetList *gp;
dsDetail *details;
int ndetail;
/*
 * Extract some data from the file.
 */
{
	znTag *tag;
	float badval;
	FieldId *fids;
	int nfield, tbegin, tend, dcsamp = dc_GetNSample (dc), samp;
	SValue v;
/*
 * Get the file open for starters.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (0);
/*
 * If bad value flags are relevant, make sure they get what they want.
 */
	if (dc_IsSubClassOf (dc->dc_Class, DCC_MetData))
	{
		badval = ds_GetDetail ("badval", details, ndetail, &v) ?
				v.us_v_float : 99999.9;
		dc_SetBadval (dc, badval);
		fids = dc_GetFields (dc, &nfield);
	}
/*
 * Get the time indices.
 */
	tbegin = zn_TimeIndex (tag, &gp->gl_begin);
	tend = zn_TimeIndex (tag, &gp->gl_end);
/*
 * Now things get organization-specific.
 */
	switch (dc->dc_Class)
	{
	   case DCC_Boundary:
	   	zn_ReadBoundary (tag, dc, tbegin, tend);
		break;

	   default:
	   	msg_ELog (EF_PROBLEM, "Strange...class %d in GetData",
				dc->dc_Class);
		return (FALSE);
	}
/*
 * Get per-sample attributes if need be.
 */
	if (tag->zt_Attr)
	{
		char ablock[1024];
		for (samp = tbegin; samp <= tend; samp++, dcsamp++)
		{
			zn_Sample *attr = tag->zt_Attr + samp;
			if (attr->znf_Size <= 0)
				continue;
			zn_GetBlock (tag, attr->znf_Offset, ablock,
						attr->znf_Size);
			dc_SetSaAttrBlock (dc, dcsamp, ablock, attr->znf_Size);
		}
	}
	return (TRUE);
}





static void
zn_ReadBoundary (tag, dc, tbegin, tend)
znTag *tag;
DataChunk *dc;
int tbegin, tend;
/*
 * Pull in the boundaries between these samples.
 */
{
	Location *locs;
	int max = 0, samp;
/*
 * Find the biggest boundary we need to pull in, and allocate sufficient
 * memory for that.
 */
	for (samp = tbegin; samp < tend; samp++)
		if (tag->zt_Sample[samp].znf_Size > max)
			max = tag->zt_Sample[samp].znf_Size;
	locs = (Location *) malloc (max);
/*
 * Now we just pass through and do it.
 */
	for (samp = tbegin; samp <= tend; samp++)
	{
		zn_Sample *zs = tag->zt_Sample + samp;
		zn_GetBlock (tag, zs->znf_Offset, locs, zs->znf_Size);
		dc_BndAdd (dc, tag->zt_Time + samp, dc->dc_Platform, locs,
				zs->znf_Size/sizeof (Location));
	}
}





int
zn_InqNPlat (dfindex)
int dfindex;
/*
 * Return the number of platforms here.
 */
{
	znTag *tag;

	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (0);
	return (tag->zt_Hdr.znh_Org == OrgIRGrid ?
			tag->zt_Hdr.znh_NStation : 1);
}






int
zn_Times (index, when, which, n, dest)
int index, n;
ZebTime *when, *dest;
TimeSpec which;
/*
 * Find out when data is available.
 */
{
	znTag *tag;
	zn_Header *hdr;
	int t, i;
/*
 * Get the file open.
 */
	if (! dfa_OpenFile (index, FALSE, (void *) &tag))
		return (0);
	hdr = &tag->zt_Hdr;
	t = zn_TimeIndex (tag, when);
/*
 * Copy out the info.
 */
	if (which == DsBefore)
		for (i = 0; t >= 0 && i < n; i++)
		{
			*dest = tag->zt_Time[t];
			dest++;
			t--;
		}
	else if (which == DsAfter)
	{
		t++;
		for (i = 0; t < hdr->znh_NSample && i < n; i++)
		{
			*dest = tag->zt_Time[t];
			dest--;
			t++;
		}
	}
	return (i);
}





int
zn_GetObsSamples (dfile, times, locs, max)
int dfile, max;
ZebTime *times;
Location *locs;
/*
 * Return sample info.
 */
{
	znTag *tag;
	int i;
/*
 * Open the file.
 */
	if (! dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);
/*
 * Now we blast through and copy out as many as we can.
 */
	for (i = 0; i < tag->zt_Hdr.znh_NSample && i < max; i++)
	{
		*times++ = tag->zt_Time[i];
		*locs++ = tag->zt_Locs ? tag->zt_Locs[i] : tag->zt_Hdr.znh_Loc;
	}
	return (i);
}







int
zn_Fields (dfile, t, nfield, fids)
int dfile, *nfield;
ZebTime *t;
FieldId *fids;
/*
 * Return the fields available at this time.
 */
{
	znTag *tag;
	zn_Header *hdr;
	int max = *nfield, fld;
/*
 * Question: should this routine return (1) the list of all known fields, 
 * 	     or (2) just the fields which are present at the given time?
 *	     For now (1) is implemented.
 *
 * Open the file.
 */
	if (! dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);
	hdr = &tag->zt_Hdr;
/*
 * Copy out the fields.
 */
	*nfield = 0;
	for (fld = 0; fld < hdr->znh_NField; fld++)
	{
		fids[*nfield] = tag->zt_Fields[fld];
		(*nfield)++;
	}
	return (TRUE);
}








/*
 * Low-level space allocation.
 */
static long
zn_GetSpace (tag, size)
znTag *tag;
int size;
{
	zn_Free fb;
	long prev = -1, free;
	zn_Header *hdr = &tag->zt_Hdr;
/*
 * Scan through the free list to see if there is anything usable.
 */
	for (free = hdr->znh_Free; free > 0; free = fb.znf_Next)
	{
	/*
	 * Pull up this block and see if it is big enough.
	 */
		zn_GetFreeBlock (tag, free, &fb);
		if (fb.znf_Size >= size)
			return (zn_GetFromFree (tag, size, free, &fb, prev));
		prev = free;
	}
/*
 * That didn't work, so we'll just allocate it at the end.
 */
	free = hdr->znh_Len;
	hdr->znh_Len += size;
	tag->zt_Sync |= SF_HEADER;
	return (free);
}





static long
zn_GetFromFree (tag, size, offset, fb, prev)
znTag *tag;
int size;
long offset, prev;
zn_Free *fb;
/*
 * Allocate a portion of this free block.
 */
{
	zn_Free prevfb;
	zn_Header *hdr = &tag->zt_Hdr;
	
	tag->zt_Sync |= SF_HEADER;
/*
 * The easy case is if we are just taking a piece of it -- allocate that 
 * chunk at the end and tweak the size.
 */
	if ((size + sizeof (zn_Free)) <= fb->znf_Size)
	{
		long ret = offset + fb->znf_Size - size;
		fb->znf_Size -= size;
		zn_PutBlock (tag, offset, fb, sizeof (zn_Free));
		hdr->znh_NFreeB -= size;
		return (ret);
	}
/*
 * We're going to have to hand over the block entirely.  Pull it out of the
 * free chain.
 */
	if (prev >= 0)
	{
		zn_GetFreeBlock (tag, prev, &prevfb);
		prevfb.znf_Next = fb->znf_Next;
		zn_PutBlock (tag, prev, &prevfb, sizeof (zn_Free));
	}
	else
		hdr->znh_Free = fb->znf_Next;
/*
 * Finish stats and return.
 */
 	hdr->znh_NFree--;
	hdr->znh_NFreeB -= fb->znf_Size;
	return (offset);
}





static void
zn_GetFreeBlock (tag, offset, fb)
znTag *tag;
long offset;
zn_Free *fb;
/*
 * Pull in a free space block.
 */
{
	zn_GetBlock (tag, offset, &fb, sizeof (zn_Free));
	if (fb->znf_FMagic != ZN_FREE_MAGIC)
		msg_ELog (EF_PROBLEM, "Corrupt free block, off %ld mag 0x%x",
			offset, fb->znf_FMagic);
}




static void
zn_GetBlock (tag, offset, dest, len)
znTag *tag;
long offset;
void *dest;
int len;
/*
 * Pull in a chunk of data from this file.
 */
{
	lseek (tag->zt_Fd, offset, SEEK_SET);
	if (read (tag->zt_Fd, dest, len) != len)
		msg_ELog (EF_PROBLEM, "ZN Read error %d", errno);
}






static void
zn_PutBlock (tag, offset, data, len)
znTag *tag;
long offset;
void *data;
int len;
/*
 * Write a chunk of data to this file.
 */
{
	lseek (tag->zt_Fd, offset, SEEK_SET);
	if (write (tag->zt_Fd, data, len) != len)
		msg_ELog (EF_PROBLEM, "ZN Write error %d", errno);
}




static void
zn_FreeSpace (tag, offset, len)
znTag *tag;
long offset;
int len;
/*
 * Free up a chunk of space in the file.
 */
{
	long before = -1, after = -1, free;
	zn_Header *hdr = &tag->zt_Hdr;
	zn_Free fb, afterfb, pfb;
/*
 * Pass through the free list and see if there are free blocks that adjoin
 * this one at either end.
 */
	for (free = hdr->znh_Free; free >= 0; free = fb.znf_Next)
	{
		zn_GetFreeBlock (tag, free, &fb);
		if ((free + fb.znf_Size) == offset)
			before = free;
		else if (offset + len == free)
			after = free;
	}
/*
 * If there is a free block ahead of this one, we merge them.
 */
	if (before)
	{
		zn_GetFreeBlock (tag, before, &fb);
		fb.znf_Size += len;
		offset = before;
	}
/*
 * Otherwise we make a new one free chunk.
 */
	else
	{
		fb.znf_FMagic = ZN_FREE_MAGIC;
		fb.znf_Size = len;
		fb.znf_Next = hdr->znh_Free;
		hdr->znh_Free = offset;
		hdr->znh_NFree++;
	}
/*
 * In either case, we now have a legit free block at "offset" describing
 * our space.  Now, if there is an after block, we need to expand this
 * free block, and remove the after block.
 */
	if (after)
	{
	/*
	 * Expand the new free block.
	 */
		zn_GetFreeBlock (tag, after, &afterfb);
		fb.znf_Size += afterfb.znf_Size;
	/*
	 * Go through and find the block ahead of the "after" block in the
	 * list.
	 */
	 	if (after == hdr->znh_Free)
			hdr->znh_Free = afterfb.znf_Next;
		else
		{
		 	for (free = hdr->znh_Free; free >= 0;
					free = pfb.znf_Next)
			{
				zn_GetFreeBlock (tag, free, &pfb);
				if (pfb.znf_Next == after)
					break;
			}
			pfb.znf_Next = afterfb.znf_Next;
			zn_PutBlock (tag, free, &pfb, sizeof (pfb));
		}
		hdr->znh_NFreeB--;
	}
/*
 * All done.  Clean things up and quit.
 */
	zn_PutBlock (tag, offset, &fb, sizeof (fb));
	tag->zt_Sync |= SF_HEADER;
	hdr->znh_NFreeB += len;
}
