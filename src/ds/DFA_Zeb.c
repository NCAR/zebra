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

MAKE_RCSID ("$Id: DFA_Zeb.c,v 1.14 1993-06-01 15:11:46 corbet Exp $");


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
	FieldId		*zt_Fids;	/* The field array		*/
	zn_Field	*zt_Fields;	/* Names of the fields		*/
	PlatformId	*zt_Ids;	/* Station ID's (irgrid)	*/
	Location	*zt_Locs;	/* Location array		*/
	zn_Sample	*zt_Attr;	/* Per-sample attribute array	*/
	char		*zt_GlAttr;	/* Global attribute block	*/
	RGrid		*zt_Rg;		/* Rgrid desc array		*/
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
# define SF_RGRID	0x0040		/* RGrid array			*/


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
	{ Org1dGrid,		DCC_RGrid	},
	{ Org2dGrid,		DCC_RGrid	},
	{ Org3dGrid,		DCC_RGrid	},
	{ OrgIRGrid,		DCC_IRGrid	},
	{ OrgIRGrid,		DCC_Scalar	},
	{ OrgTransparent,	DCC_Transparent	},
	{ OrgScalar,		DCC_Scalar	},
	{ OrgScalar,		DCC_Location	},
	{ OrgFixedScalar,	DCC_Scalar	},
	{ OrgFixedScalar,	DCC_Location	},
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
static int	zn_WrGrid FP ((znTag *, DataChunk *, int, int, zn_Sample *, 
			WriteCode, int *, FieldId *, int));
static int	zn_WrIRGrid FP ((znTag *, DataChunk *, int, int, zn_Sample *, 
			WriteCode, int *, FieldId *, int));
static int	zn_WrScalar FP ((znTag *, DataChunk *, int, int, zn_Sample *, 
			WriteCode, int *, FieldId *, int));
static int	zn_WrFixScalar FP ((znTag *, DataChunk *, int, int,
			zn_Sample *, WriteCode, int *, FieldId *, int));
static void 	zn_WrLocInfo FP ((znTag *, int, Location *, RGrid *));
static int	zn_WrTrans FP ((znTag *, DataChunk *, int, int, zn_Sample *, 
			WriteCode));
static void	zn_GetFieldIndex FP ((znTag *, FieldId *, int, int *, int));
static void	zn_DataWrite FP ((znTag *, void *, int, zn_Sample *,
			WriteCode));
static void	zn_OFLoadStations FP ((znTag *));
static void	zn_PutAttrs FP ((znTag *, int, void *, int));
static int	zn_OrgClassCompat FP ((DataOrganization, DataClass));
static void	zn_ReadBoundary FP ((znTag *, DataChunk *, int, int));
static void	zn_ReadGrid FP ((znTag *, DataChunk *, int, int, int,
			dsDetail *, int, double));
static void	zn_ReadIRG FP ((znTag *, DataChunk *, int, int, int, double));
static void	zn_ReadScalar FP ((znTag *, DataChunk *, int, int, int,
			double));
static void	zn_ReadTrans FP ((znTag *, DataChunk *, int, int, int));
static void	zn_ReadLocation FP ((znTag *, DataChunk *, int, int));
static void	zn_RdRGridOffset FP ((RGrid *, Location *, long *, int *,
			dsDetail *, int));
static void	zn_DoBadval FP ((float *, int, double, double));
static void	zn_SetBad FP ((float *, int, double));

static long	zn_GetFromFree FP ((znTag *, int, long, zn_Free *, long));
static long	zn_GetSpace FP ((znTag *, int));
static void	zn_GetFreeBlock FP ((znTag *, long, zn_Free *));
static void	zn_GetBlock FP ((znTag *, long, void *, int));
static void	zn_PutBlock FP ((znTag *, long, void *, int));
static void	zn_FreeSpace FP ((znTag *, long, int));

static int 	zn_SASize FP ((zn_Header *, int));
static zn_Sample * zn_FindSampStr FP ((znTag *, int));






int
zn_CreateFile (fname, df, dc, rtag)
char *fname;
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
	bool grid;
	void *ablock;
/*
 * Create the file itself before we go anywhere.
 */
	if ((tag->zt_Fd = open (fname, O_RDWR|O_TRUNC|O_CREAT, 0666)) <0)
	{
		msg_ELog (EF_PROBLEM, "Can't create file %s (%d)",
			fname, errno);
		free (tag);
		return (FALSE);
	}
/*
 * Initialize the file tag.
 */
	tag->zt_Sync = SF_HEADER;
	tag->zt_Sample = 0; tag->zt_Fids = 0;
	tag->zt_Fields = 0; tag->zt_Ids = 0; tag->zt_Locs = 0;
	tag->zt_Write = TRUE;
	tag->zt_Attr = 0;
	tag->zt_GlAttr = 0;
	tag->zt_Rg = 0;
/*
 * Header initialization.  Once this is done we can start space allocation.
 */
	hdr->znh_Magic = ZN_MAGIC;
	hdr->znh_Free = -1;
	hdr->znh_NFree = hdr->znh_NFreeB = hdr->znh_Len = 0;
	hdr->znh_NSample = hdr->znh_NField = 0;
	hdr->znh_Org = ds_PlatformDataOrg (df->df_platform);
	hdr->znh_OffLoc = -1;
	hdr->znh_OffGlAttr = hdr->znh_OffAttr = -1;
	hdr->znh_GlAttrLen = 0;
	hdr->znh_OffRg = -1;
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
	ssize = zn_SASize (hdr, ZN_GRAIN);
/*	ssize = ZN_GRAIN*hdr->znh_NField*sizeof (zn_Sample); */
	tag->zt_Sample = (zn_Sample *) malloc (ssize);
	hdr->znh_OffSample = zn_GetSpace (tag, ssize);
	tag->zt_Sync |= SF_SAMPLE;
/*
 * Does this file involve grids?
 */
	grid = hdr->znh_Org == Org1dGrid || hdr->znh_Org == Org2dGrid ||
			hdr->znh_Org == Org3dGrid || hdr->znh_Org == OrgImage;
/*
 * Add other chunks to the file if appropriate.
 */
	if (dc->dc_Class == DCC_IRGrid)		/* IRGRID station array	*/
		zn_CFMakeStations (tag, dc);
	if (grid || ds_IsMobile (dc->dc_Platform)) /* Locations		*/
	{
		tag->zt_Locs = (Location *) malloc (ZN_GRAIN*sizeof(Location));
		hdr->znh_OffLoc = zn_GetSpace (tag, ZN_GRAIN*sizeof(Location));
		tag->zt_Sync |= SF_LOCATION;
	}
	else
		dc_GetLoc (dc, 0, &hdr->znh_Loc);
/*
 * Grid dimensions.
 */
	if (grid)
	{
		tag->zt_Rg = (RGrid *) malloc (ZN_GRAIN*sizeof (RGrid));
		hdr->znh_OffRg = zn_GetSpace (tag, ZN_GRAIN*sizeof (RGrid));
		tag->zt_Sync |= SF_RGRID;
	}
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
 * Sync up and we are done.
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
	float badval = 99999.9;
/*
 * Make a special case for boundaries. (XXX)
 */
	if (dc->dc_Class == DCC_Boundary || dc->dc_Class == DCC_Transparent)
	{
		bfid = F_Lookup ("boundary");
		fids = &bfid;
		nf = 1;
	}
	else
	{
		fids = dc_GetFields (dc, &nf);
		badval = dc_GetBadval (dc);
	}
/*
 * Put the field array into the tag.
 */
	tag->zt_Fids = (FieldId *) malloc (nf * sizeof (FieldId));
	tag->zt_Sync |= SF_FIELD;
	tag->zt_Hdr.znh_OffField = zn_GetSpace (tag, nf * sizeof (zn_Field));
	tag->zt_Hdr.znh_NField = nf;
	tag->zt_Fields = (zn_Field *) malloc (nf * sizeof (zn_Field));
/*
 * Go through and fill it all in.
 */
	for (i = 0; i < nf; i++)
	{
		tag->zt_Fids[i] = fids[i];
		strcpy (tag->zt_Fields[i].zf_Name, F_GetName (fids[i]));
		tag->zt_Fields[i].zf_Format = DF_Float;
		tag->zt_Fields[i].zf_Badval = badval;
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
			hdr->znh_NSampAlloc*sizeof (ZebTime));
	if (tag->zt_Sync & SF_SAMPLE)
		zn_PutBlock (tag, hdr->znh_OffSample, tag->zt_Sample,
		       zn_SASize (hdr, hdr->znh_NSampAlloc));
	if (tag->zt_Sync & SF_LOCATION)
		zn_PutBlock (tag, hdr->znh_OffLoc, tag->zt_Locs,
			hdr->znh_NSampAlloc*sizeof (Location));
	if (tag->zt_Sync & SF_FIELD)
		zn_PutBlock (tag, hdr->znh_OffField, tag->zt_Fields,
			hdr->znh_NField*sizeof (zn_Field));
	if (tag->zt_Sync & SF_ATTR)
		zn_PutBlock (tag, hdr->znh_OffAttr, tag->zt_Attr,
			hdr->znh_NSampAlloc*sizeof (zn_Sample));
	if (tag->zt_Sync & SF_RGRID)
		zn_PutBlock (tag, hdr->znh_OffRg, tag->zt_Rg,
			hdr->znh_NSampAlloc*sizeof (RGrid));
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
	hdr->znh_NStation = nplat;
	tag->zt_Sync |= SF_HEADER;
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
	if (dc->dc_Class != DCC_Boundary && dc->dc_Class != DCC_Transparent)
	{
		fids = dc_GetFields (dc, &nfield);
		index = (int *) malloc (nfield*sizeof (int));
		zn_GetFieldIndex (tag, fids, nfield, index, TRUE);
	}
	/* samp = tag->zt_Sample + (fsample*hdr->znh_NField); */
	samp = zn_FindSampStr (tag, fsample);
/*
 * Now it is a matter of writing out the data according to the type of
 * things.
 */
	switch (hdr->znh_Org)
	{
	   case OrgOutline:
	   	zn_WrBoundary (tag, dc, sample, samp, wc, 0);
		break;

	   case Org1dGrid:
	   case Org2dGrid:
	   case Org3dGrid:
	   	zn_WrGrid (tag, dc, fsample, sample, samp, wc, index,
				fids, nfield);
		break;

	   case OrgIRGrid:
	   	zn_WrIRGrid (tag, dc, fsample, sample, samp, wc, index,
				fids, nfield);
		break;

	   case OrgScalar:
	   	zn_WrScalar (tag, dc, fsample, sample, samp, wc, index,
				fids, nfield);
	   	break;

	   case OrgFixedScalar:
	   	zn_WrFixScalar (tag, dc, fsample, sample, samp, wc, index,
				fids, nfield);
		break;

	   case OrgTransparent:
	   	zn_WrTrans (tag, dc, fsample, sample, samp, wc);
		break;
	}
/*
 * We also have to add the time to the time array.  We flush the individual
 * time out here rather than dirty up and sync the entire array.
 */
	tag->zt_Time[fsample] = t;
	zn_PutBlock (tag, hdr->znh_OffTime + fsample*sizeof (ZebTime),
			tag->zt_Time + fsample, sizeof (ZebTime));
/*
 * Save out the sample array.
 */
	zn_PutBlock (tag, hdr->znh_OffSample + zn_SASize (hdr, fsample),
			samp, zn_SASize (hdr, 1));
/*
 * If there are sample attributes, save them; otherwise erase any existing
 * ones (ablock == NULL)
 */
	ablock = dc_GetSaAttrBlock (dc, sample, &alen);
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
 * Dump this attribute block out to the file.  If ABLOCK is NULL, erase
 * any existing attribute block.
 */
{
	zn_Header *hdr = &tag->zt_Hdr;
	zn_Sample *zs;
/*
 * If no attribute block and no attribute array, we're outta here
 */
	if ((hdr->znh_OffAttr < 0) && (ablock == NULL))
		return;
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
	if (ablock)
	{
		zs->znf_Size = alen;
		zs->znf_Offset = zn_GetSpace (tag, alen);
		zn_PutBlock (tag, zs->znf_Offset, ablock, alen);
	}
	else
	{
		zs->znf_Size = 0;
		zs->znf_Offset = 0;
	}
/*
 * Sync out this attribute entry.
 */
	zn_PutBlock (tag, hdr->znh_OffAttr + sample*sizeof (zn_Sample),
		zs, sizeof (zn_Sample));
}







static void
zn_GetFieldIndex (tag, fids, nfield, index, create)
znTag *tag;
FieldId *fids;
int nfield, *index, create;
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
			if (fids[dcfield] == tag->zt_Fids[ffield])
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
 	if (! nnew || ! create || hdr->znh_Org == OrgFixedScalar)
		return;
/*
 * Otherwise we are going to have to expand the sample table.
 * Sample table size problems below, but doesn't matter (for now) since
 * fixed case bails out above.
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
	tag->zt_Fields = (zn_Field *) realloc (tag->zt_Fields,
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
			strcpy (tag->zt_Fields[hdr->znh_NField].zf_Name,
					F_GetName (fids[dcfield]));
			tag->zt_Fields[hdr->znh_NField].zf_Format = DF_Float;
			index[dcfield] = hdr->znh_NField++;
		}
/*
 * Write out the new field name table (sync won't do that) and we are 
 * done.
 */
	zn_PutBlock (tag, hdr->znh_OffField, tag->zt_Fields,
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
 *
 * (12/92 jc) try doubling the TOC size instead of just slowly growing it,
 * so as to reduce fragmentation problems.
 */
/*	newns = (hdr->znh_NSampAlloc += ZN_GRAIN); */
	newns = (hdr->znh_NSampAlloc *= 2);
	tag->zt_Time = (ZebTime *) realloc (tag->zt_Time, 
					newns*sizeof (ZebTime));
	zn_FreeSpace (tag, hdr->znh_OffTime, oldns*sizeof (ZebTime));
	hdr->znh_OffTime = zn_GetSpace (tag, newns*sizeof (ZebTime));
	tag->zt_Sync |= SF_TIME;
/*
 * Now the sample information array.
 */
	tag->zt_Sample = (zn_Sample *) realloc (tag->zt_Sample,
			zn_SASize (hdr, newns));
	zn_FreeSpace (tag, hdr->znh_OffSample, zn_SASize (hdr, oldns));
	hdr->znh_OffSample = zn_GetSpace (tag, zn_SASize (hdr, newns));
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
 * RGRid arrays, if need be.
 */
	if (hdr->znh_OffRg >= 0)
	{
		tag->zt_Rg = (RGrid *) realloc (tag->zt_Rg,
					newns*sizeof (RGrid));
		zn_FreeSpace (tag, hdr->znh_OffRg, oldns*sizeof (RGrid));
		hdr->znh_OffRg = zn_GetSpace (tag, newns*sizeof (RGrid));
		tag->zt_Sync |= SF_RGRID;
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
		memset (tag->zt_Attr + oldns, 0,
				(newns - oldns)*sizeof (zn_Sample));
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
	bcopy (zn_FindSampStr (tag, sample),
		zn_FindSampStr (tag, sample + 1),
		zn_SASize (hdr, nmove));
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
 * Same with rgrids.
 */
	if (hdr->znh_OffRg >= 0)
	{
		bcopy (tag->zt_Rg + sample, tag->zt_Rg + sample + 1,
			nmove*sizeof (RGrid));
		tag->zt_Sync |= SF_RGRID;
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
}




static int
zn_WrGrid (tag, dc, fsample, dcsample, samp, wc, index, fids, nfield)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, *index, nfield;
zn_Sample *samp;
WriteCode wc;
FieldId *fids;
/*
 * Write out a grid sample.
 */
{
	int fld, len;
	float *data;
	RGrid rg;
	Location loc;
/*
 * Write out the data for each field.
 */
	for (fld = 0; fld < nfield; fld++)
	{
		data = dc_RGGetGrid (dc, dcsample, fids[fld], &loc, &rg, &len);
		zn_DataWrite (tag, data, len, samp + index[fld], wc);
	}
/*
 * Save out the location info.
 */
	zn_WrLocInfo (tag, fsample, &loc, &rg);
}






static int
zn_WrIRGrid (tag, dc, fsample, dcsample, samp, wc, index, fids, nfield)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, *index, nfield;
zn_Sample *samp;
WriteCode wc;
FieldId *fids;
/*
 * Write out an irregular grid sample.
 */
{
	int fld, len;
	float *data;
/*
 * Write out the data for each field.
 */
	for (fld = 0; fld < nfield; fld++)
	{
		data = dc_IRGetGrid (dc, dcsample, fids[fld]);
		zn_DataWrite (tag, data,tag->zt_Hdr.znh_NStation*sizeof(float),
				samp + index[fld], wc);
	}
}




static int
zn_WrScalar (tag, dc, fsample, dcsample, samp, wc, index, fids, nfield)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, *index, nfield;
zn_Sample *samp;
WriteCode wc;
FieldId *fids;
/*
 * Write out some scalar data.
 */
{
	int fld, len;
	float data;
/*
 * Write out the data for each field.
 */
	for (fld = 0; fld < nfield; fld++)
	{
		data = dc_GetScalar (dc, dcsample, fids[fld]);
		zn_DataWrite (tag, &data, sizeof(float), samp + index[fld],wc);
	}
/*
 * Put out the location if necessary.
 */
	if (ds_IsMobile (dc->dc_Platform))
	{
		dc_GetLoc (dc, dcsample, tag->zt_Locs + fsample);
		zn_PutBlock (tag, tag->zt_Hdr.znh_OffLoc + 
			fsample*sizeof (Location), tag->zt_Locs + fsample,
			sizeof (Location));
	}
}






static int
zn_WrFixScalar (tag, dc, fsample, dcsample, samp, wc, index, fids, nfield)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, *index, nfield;
zn_Sample *samp;
WriteCode wc;
FieldId *fids;
/*
 * Write out some fixed-field scalar data.
 */
{
	int fld, len;
	float *fdata, bad = dc_GetBadval (dc);
	zn_Header *hdr = &tag->zt_Hdr;
/*
 * Get an array to hold all the data for this sample.
 */
	fdata = (float *) malloc (hdr->znh_NField * sizeof (float));
	if (wc == wc_Append)
		for (fld = 0; fld < hdr->znh_NField; fld++)
			fdata[fld] = bad;
	else
		zn_GetBlock (tag, samp->znf_Offset, fdata, samp->znf_Size);
/*
 * Fill in the data for each field.
 */
	for (fld = 0; fld < nfield; fld++)
		fdata[fld] = dc_GetScalar (dc, dcsample, fids[fld]);
	zn_DataWrite (tag, fdata, hdr->znh_NField*sizeof (float), samp, wc);
/*
 * Put out the location if necessary.
 */
	if (ds_IsMobile (dc->dc_Platform))
	{
		dc_GetLoc (dc, dcsample, tag->zt_Locs + fsample);
		zn_PutBlock (tag, tag->zt_Hdr.znh_OffLoc + 
			fsample*sizeof (Location), tag->zt_Locs + fsample,
			sizeof (Location));
	}
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





static int
zn_WrTrans (tag, dc, fsample, dcsample, samp, wc)
znTag *tag;
DataChunk *dc;
int fsample, dcsample;
zn_Sample *samp;
WriteCode wc;
/*
 * Write out a boundary sample.
 */
{
	DataPtr data;
	int len;
/*
 * Pull out the data itself, then dump it out.
 */
	data = dc_GetSample (dc, dcsample, &len);
	zn_DataWrite (tag, data, len, samp, wc);
/*
 * Put out the location if necessary.
 */
	if (ds_IsMobile (dc->dc_Platform))
	{
		dc_GetLoc (dc, dcsample, tag->zt_Locs + fsample);
		zn_PutBlock (tag, tag->zt_Hdr.znh_OffLoc + 
			fsample*sizeof (Location), tag->zt_Locs + fsample,
			sizeof (Location));
	}
	return (1);
}






static void
zn_WrLocInfo (tag, sample, loc, rg)
znTag *tag;
int sample;
Location *loc;
RGrid *rg;
/*
 * Save this location info in the file.
 */
{
	zn_Header *hdr = (zn_Header *) &tag->zt_Hdr;

	tag->zt_Locs[sample] = *loc;
	zn_PutBlock (tag, hdr->znh_OffLoc + sample*sizeof (Location), 
			loc, sizeof (Location));

	tag->zt_Rg[sample] = *rg;
	zn_PutBlock (tag, hdr->znh_OffRg + sample*sizeof (RGrid), 
			rg, sizeof (RGrid));
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
	free (tag->zt_Time);		/* Time array -- always there	*/
	if (tag->zt_Sample)		/* Sample array			*/
		free (tag->zt_Sample);
	if (tag->zt_Fids)		/* Field ID's			*/
		free (tag->zt_Fids);
	if (tag->zt_Fields)		/* Field descriptions		*/
		free (tag->zt_Fields);
	if (tag->zt_Ids)		/* Station ID's			*/
		free (tag->zt_Ids);
	if (tag->zt_Locs)		/* Locations			*/
		free (tag->zt_Locs);
	if (tag->zt_GlAttr)		/* Global attributes		*/
		free (tag->zt_GlAttr);
	if (tag->zt_Attr)		/* Sample attributes		*/
		free (tag->zt_Attr);
	if (tag->zt_Rg)			/* Rgrid dimensions		*/
		free (tag->zt_Rg);
	free (tag);
	return (0);
}





int
zn_Open (fname, df, write, rtag)
char *fname;
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
	bool grid;
/*
 * Open the file.
 */
	memset (tag, 0, sizeof (znTag));
	if ((tag->zt_Fd = open (fname, write ? O_RDWR : O_RDONLY)) < 0)
	{
		free (tag);
		msg_ELog (EF_PROBLEM, "Can't open %s (%d)", fname, errno);
		return (FALSE);
	}
	zn_GetBlock (tag, 0, &tag->zt_Hdr, sizeof (zn_Header));
/*
 * Pull in the time and sample arrays.
 */
	nsa = hdr->znh_NSampAlloc;
	tag->zt_Time = (ZebTime *) malloc(nsa*sizeof (ZebTime));
	zn_GetBlock (tag, hdr->znh_OffTime, tag->zt_Time, nsa*sizeof(ZebTime));
	tag->zt_Sample = (zn_Sample *) malloc (zn_SASize (hdr, nsa));
	zn_GetBlock (tag, hdr->znh_OffSample, tag->zt_Sample,
			zn_SASize (hdr, nsa));
/*
 * Pull in the fields and convert them to fids.
 */
	tag->zt_Fields = (zn_Field *) malloc(hdr->znh_NField*sizeof(zn_Field));
	zn_GetBlock (tag, hdr->znh_OffField, tag->zt_Fields,
				hdr->znh_NField*sizeof (zn_Field));
	tag->zt_Fids = (FieldId *) malloc (hdr->znh_NField*sizeof (FieldId));
	for (field = 0; field < hdr->znh_NField; field++)
		tag->zt_Fids[field] = F_Lookup (tag->zt_Fields[field].zf_Name);
/*
 * If there is a station array, pull it in.
 */
	if (hdr->znh_Org == OrgIRGrid)
		zn_OFLoadStations (tag);
/*
 * Does this file involve grids?
 */
	grid = hdr->znh_Org == Org1dGrid || hdr->znh_Org == Org2dGrid ||
			hdr->znh_Org == Org3dGrid || hdr->znh_Org == OrgImage;
/*
 * Look for a location array.
 */
	if (grid || ds_IsMobile (df->df_platform))
	{
		tag->zt_Locs = (Location *) malloc (nsa*sizeof (Location));
		zn_GetBlock (tag, hdr->znh_OffLoc, tag->zt_Locs,
				nsa*sizeof (Location));
	}
/*
 * Grid dims.
 */
	if (grid)
	{
		tag->zt_Rg = (RGrid *) malloc (nsa*sizeof (RGrid));
		zn_GetBlock (tag, hdr->znh_OffRg, tag->zt_Rg,
				nsa*sizeof (RGrid));
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
	sprintf (dest, "%s.%06d.%04d.znf", platform, t.ds_yymmdd,
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
 *
 * In retrospect, the use of "realloc" below is probably not the right
 * way to do this, since all of the arrays get read in completely 
 * anyway.  Probably we are doing a lot of copying for nothing.
 */
	zn_GetBlock (tag, 0, hdr, sizeof (zn_Header));
	nsa = hdr->znh_NSampAlloc;
	if (oldnsa != hdr->znh_NSampAlloc)
	{
		tag->zt_Time = (ZebTime *) realloc (tag->zt_Time,
				nsa*sizeof (ZebTime));
		tag->zt_Sample = (zn_Sample *) realloc (tag->zt_Sample,
				zn_SASize (hdr, nsa));
		if (tag->zt_Locs)
			tag->zt_Locs = (Location *) realloc (tag->zt_Locs,
				nsa*sizeof (Location));
		if (tag->zt_Attr)
			tag->zt_Attr = (zn_Sample *) realloc (tag->zt_Attr,
				nsa*sizeof (zn_Sample));
		if (tag->zt_Rg)
			tag->zt_Rg = (RGrid *) realloc (tag->zt_Rg,
				nsa*sizeof (RGrid));
	}
/*
 * Check to see if somebody added per-sample attributes when we weren't
 * looking.
 */
	if (! tag->zt_Attr && hdr->znh_OffAttr > 0)
		tag->zt_Attr = (zn_Sample *) malloc (nsa*sizeof (zn_Sample));
/*
 * If there are new fields we have other stuff to do.
 */
	if (oldnf != hdr->znh_NField)
	{
		if (oldnsa == hdr->znh_NSampAlloc)
			tag->zt_Sample = (zn_Sample *) realloc (tag->zt_Sample,
				zn_SASize (hdr, nsa));
		tag->zt_Fids = (FieldId *) realloc (tag->zt_Fids,
				hdr->znh_NField*sizeof (FieldId));
		tag->zt_Fields = (zn_Field *) realloc (tag->zt_Fields,
				hdr->znh_NField*sizeof (zn_Field));
	}
/*
 * Now, finally, we can read in the new info.
 */
	zn_GetBlock (tag, hdr->znh_OffTime, tag->zt_Time, nsa*sizeof(ZebTime));
	zn_GetBlock (tag, hdr->znh_OffSample, tag->zt_Sample,
				zn_SASize (hdr, nsa));
	if (tag->zt_Locs)
		zn_GetBlock (tag, hdr->znh_OffLoc, tag->zt_Locs,
						nsa*sizeof (Location));
	if (tag->zt_Attr)
		zn_GetBlock (tag, hdr->znh_OffAttr, tag->zt_Attr,
						nsa*sizeof (zn_Sample));
	if (tag->zt_Rg)
		zn_GetBlock (tag, hdr->znh_OffRg, tag->zt_Rg,
						nsa*sizeof (RGrid));
/*
 * Deal with fields if necessary.
 */
	if (hdr->znh_NField != oldnf)
	{
		int fld;
		zn_GetBlock (tag, hdr->znh_OffField, tag->zt_Fields,
					hdr->znh_NField*sizeof (zn_Field));
		for (fld = 0; fld < hdr->znh_NField; fld++)
			tag->zt_Fids[fld] = F_Lookup (
						tag->zt_Fields[fld].zf_Name);
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
	zn_Header *hdr;
/*
 * Get the file open for starters.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (NULL);
	hdr = &tag->zt_Hdr;
/*
 * Make sure this is a combination we can do.
 */
	if (! zn_OrgClassCompat (hdr->znh_Org, class))
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
		dc_SetGlAttrBlock (dc, tag->zt_GlAttr, hdr->znh_GlAttrLen);
/*
 * Store the static location out of the header if called for.
 */
	if (hdr->znh_OffLoc < 0)
		dc_SetStaticLoc (dc, &hdr->znh_Loc);
/*
 * Do class-specific setup.
 */
	switch (class)
	{
	   case DCC_RGrid:
	   	dc_RGSetup (dc, nfield, fields);
		break;

	   case DCC_IRGrid:
	   	dc_IRSetup (dc, hdr->znh_NStation, tag->zt_Ids, tag->zt_Locs,
				nfield, fields);
		break;

	   case DCC_Scalar:
	   	dc_SetScalarFields (dc, nfield, fields);
		break;
	}
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
	bool metdata = dc_IsSubClassOf (dc->dc_Class, DCC_MetData);
	SValue v;
/*
 * Get the file open for starters.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (0);
/*
 * Met data stuff
 */
	if (metdata)
	{
		badval = dc_GetBadval (dc);
	/*
	 * Complain for now if the user requests a bad value flag 
	 * and it doesn't match the one from the file
	 *
	 * Why does this have to be this way???????
	 */
		if (ds_GetDetail ("badval", details, ndetail, &v) &&
			v.us_v_float != badval)
		{
			msg_ELog (EF_PROBLEM, "User/file badval mismatch!");
			return (0);
		}
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

	   case DCC_RGrid:
	   	zn_ReadGrid (tag, dc, dcsamp, tbegin, tend, details, ndetail,
				badval);
		break;

	   case DCC_IRGrid:
	   	zn_ReadIRG (tag, dc, dcsamp, tbegin, tend, badval);
		break;

	   case DCC_Scalar:
	   	zn_ReadScalar (tag, dc, dcsamp, tbegin, tend, badval);
		break;

	   case DCC_Location:
	   	zn_ReadLocation (tag, dc, tbegin, tend);
		break;

	   case DCC_Transparent:
	   	zn_ReadTrans (tag, dc, dcsamp, tbegin, tend);
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
	for (samp = tbegin; samp <= tend; samp++)
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
	free (locs);
}





static void
zn_ReadScalar (tag, dc, dcsamp, tbegin, tend, badval)
znTag *tag;
DataChunk *dc;
int dcsamp, tbegin, tend;
float badval;
{
	int sample, *index, nfield, fld, offset = 0, plat;
	FieldId *fids;
	zn_Sample *zs;
	zn_Header *hdr = &tag->zt_Hdr;
	float *data = (float *) malloc ((tend - tbegin + 1)*sizeof (float));
	float *dp;
	bool fixed = (hdr->znh_Org == OrgFixedScalar);
/*
 * If we are pulling a single station out of an irgrid, figure out 
 * what the offset will be.
 */
	if (hdr->znh_Org == OrgIRGrid)
	{
		for (plat = 0; plat < hdr->znh_NStation; plat++)
			if (tag->zt_Ids[plat] == dc->dc_Platform)
				break;
		if (plat >= hdr->znh_NStation)
		{
			msg_ELog (EF_PROBLEM, "Plat %s missing from IRGrid",
				ds_PlatformName (dc->dc_Platform));
			free (data);
			return;
		}
		offset = plat*sizeof (float);
	}
/*
 * Figure out where our fields are.
 */
	fids = dc_GetFields (dc, &nfield);
	index = (int *) malloc (nfield*sizeof (int));
	zn_GetFieldIndex (tag, fids, nfield, index, FALSE);
/*
 * Go through and get the entire set of data for the given field.
 */
	for (fld = 0; fld < nfield; fld++)
	{
	/*
	 * Now we get each sample.
	 */
		dp = data;
	 	for (sample = tbegin; sample <= tend; sample++)
		{
			zs = zn_FindSampStr (tag, sample) + 
						(fixed ? 0 : index[fld]);
			if (index[fld] < 0 || zs->znf_Size <= 0)
				*dp = badval;
			else if (fixed)
				zn_GetBlock (tag, zs->znf_Offset + 
					index[fld]*sizeof (float), dp,
					sizeof (float));
			else
				zn_GetBlock (tag, zs->znf_Offset + offset,
					dp, sizeof (float));
			dp++;
		}
	/*
	 * Dump it into the data chunk.
	 */
	 	dc_AddMultScalar (dc, tag->zt_Time + tbegin, dcsamp,
			tend - tbegin + 1, fids[fld], data);
	}
/*
 * Time to deal with locations.  If it is static, life is easy.
 */
	if (! ds_IsMobile (dc->dc_Platform))
		dc_SetStaticLoc (dc, &hdr->znh_Loc);
/*
 * Otherwise we need to copy the locs over.
 */
	else
		for (sample = tbegin; sample <= tend; sample++)
			dc_SetLoc (dc, dcsamp + sample - tbegin, 
				tag->zt_Locs + sample);
/*
 * Clean up and we are done.
 */
	free (index);
	free (data);
}





static void
zn_ReadTrans (tag, dc, dcsamp, tbegin, tend)
znTag *tag;
DataChunk *dc;
int dcsamp, tbegin, tend;
{
	int sample, alen = -1, len;
	zn_Sample *zs;
	zn_Header *hdr = &tag->zt_Hdr;
	DataPtr data = 0;
/*
 * Pull it in one sample at a time.
 */
	for (sample = tbegin; sample <= tend; sample++)
	{
		zs = tag->zt_Sample + sample*hdr->znh_NField;

		if ((len = zs->znf_Size) > 0)
		{
		/*
		 * Make sure we have enough scratch space.
		 */
		 	if (len > alen)
			{
				if (data)
					free (data);
				data = (DataPtr) malloc (alen = len);
			}
		/*
		 * Now pull in the stuff and add it to the DC.
		 */
			zn_GetBlock (tag, zs->znf_Offset, data, len);
			dc_AddSample (dc, tag->zt_Time + sample, data, len);
		}
	}
/*
 * Time to deal with locations.
 */
	if (ds_IsMobile (dc->dc_Platform))
		for (sample = tbegin; sample <= tend; sample++)
			dc_SetLoc (dc, dcsamp + sample - tbegin, 
				tag->zt_Locs + sample);
/*
 * Clean up and we are done.
 */
	if (data)
		free (data);
}





static void
zn_ReadLocation (tag, dc, tbegin, tend)
znTag *tag;
DataChunk *dc;
int tbegin, tend;
/*
 * Pull in location-only data.
 */
{
	int sample;
/*
 * Just pull in the locations.
 */
	if (ds_IsMobile (dc->dc_Platform))
		for (sample = tbegin; sample <= tend; sample++)
			dc_LocAdd (dc, tag->zt_Time + sample,
				tag->zt_Locs + sample);
}



static void
zn_ReadIRG (tag, dc, dcsamp, tbegin, tend, badval)
znTag *tag;
DataChunk *dc;
int dcsamp, tbegin, tend;
float badval;
/*
 * Pull in some irregular grid data.
 */
{
	int size, sample, *index, nfield, fld;
	FieldId *fids;
	zn_Sample *zs;
	zn_Header *hdr = &tag->zt_Hdr;
	float *data = (float *) malloc (hdr->znh_NStation*sizeof (float));
/*
 * Figure out where our fields are.
 */
	fids = dc_GetFields (dc, &nfield);
	index = (int *) malloc (nfield*sizeof (int));
	zn_GetFieldIndex (tag, fids, nfield, index, FALSE);
	zs = tag->zt_Sample + tbegin*hdr->znh_NField;
/*
 * Now pull in the stuff.
 */
	for (sample = tbegin; sample <= tend; sample++)
	{
		for (fld = 0; fld < nfield; fld++)
		{
		/*
		 * Make sure the data exists, and grab it.
		 */
		 	if (index[fld] < 0 || zs[index[fld]].znf_Size <= 0)
				zn_SetBad (data, hdr->znh_NStation, badval);
			else
			 	zn_GetBlock (tag, zs[index[fld]].znf_Offset,
					data, hdr->znh_NStation*sizeof(float));
		/*
		 * Store it into the dc.
		 */
			dc_IRAddGrid (dc, tag->zt_Time + sample, dcsamp,
				fids[fld], data);
		}
	/*
	 * Move on.
	 */
		dcsamp++;
		zs += hdr->znh_NField;
	}
	free (index);
	free (data);
}





static void
zn_SetBad (data, npoint, badval)
float *data, badval;
int npoint;
/*
 * Set this array to the bad value flag.
 */
{
	for (; npoint > 0; npoint--)
		*data++ = badval;
}




static void
zn_ReadGrid (tag, dc, dcsamp, tbegin, tend, details, ndetail, badval)
znTag *tag;
DataChunk *dc;
int dcsamp, tbegin, tend, ndetail;
dsDetail *details;
float badval;
/*
 * Pull some grids in.
 */
{
	long offset;
	int size, sample, *index, nfield, fld;
	FieldId *fids;
	RGrid rg;
	Location origin;
	zn_Sample *zs;
	zn_Header *hdr = &tag->zt_Hdr;
	float *data;
/*
 * Figure out where our fields are.
 */
	fids = dc_GetFields (dc, &nfield);
	index = (int *) malloc (nfield*sizeof (int));
	zn_GetFieldIndex (tag, fids, nfield, index, FALSE);
	zs = tag->zt_Sample + tbegin*hdr->znh_NField;
/*
 * Now pull in the stuff.
 */
	for (sample = tbegin; sample <= tend; sample++)
	{
	/*
	 * Consider subsectioning.
	 */
		rg = tag->zt_Rg[sample];
		origin = tag->zt_Locs[sample];
	 	zn_RdRGridOffset (&rg, &origin, &offset, &size,
				details, ndetail);
	/*
	 * Blast through the fields and get the data.
	 */
	 	for (fld = 0; fld < nfield; fld++)
		{
		/*
		 * Make sure the data is here.
		 */
			if (index[fld] < 0 || zs[index[fld]].znf_Size <= 0)
				continue;
		/*
		 * Set up the data chunk to accept it, then read it in.  The
		 * two-step process is there to avoid an extra copy of the
		 * data.
		 */
			dc_RGAddGrid (dc, dcsamp, fids[fld], &origin, &rg,
				tag->zt_Time + sample, (float *) 0, size);
			data = dc_GetMData (dc, dcsamp, fids[fld], 0);
			zn_GetBlock (tag, zs[index[fld]].znf_Offset + offset,
					data, size);
		/*
		 * Apply the bad value flag.
		 */
		 	zn_DoBadval (data, size/sizeof (float),
				tag->zt_Fields[index[fld]].zf_Badval, badval);
		}
		dcsamp++;
		zs += hdr->znh_NField;
	}
	free (index);
}






static void
zn_RdRGridOffset (rg, loc, offset, size, details, ndetail)
RGrid *rg;
Location *loc;
long *offset;
int *size, ndetail;
dsDetail *details;
/*
 * Figure offsets and sizes 
 */
{
	SValue v;
	int level;
/*
 * If they have not specified an altitude, set params to read the whole
 * damn thing.
 */
	if (! ds_GetDetail ("altitude", details, ndetail, &v))
	{
		*offset = 0;
		*size = rg->rg_nX*rg->rg_nY*rg->rg_nZ*sizeof (float); /*XXX*/
		return;
	}
/*
 * OK, we need to figure out where they want to read from.
 */
	level = (v.us_v_float - loc->l_alt)/rg->rg_Zspacing + 0.5;
	if (level < 0)
		level = 0;
	else if (level >= rg->rg_nZ)
		level = rg->rg_nZ - 1;
/*
 * Set params and we're done.
 */
	loc->l_alt += level*rg->rg_Zspacing;
	rg->rg_nZ = 1;
	*offset = level*rg->rg_nX*rg->rg_nY*sizeof (float);
	*size = rg->rg_nX*rg->rg_nY*sizeof (float);

}





int
zn_InqRGrid (dfile, origin, rg)
int dfile;
Location *origin;
RGrid *rg;
/*
 * Return the coord info for this grid.
 */
{
	znTag *tag;

	if (! dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);
	if (! tag->zt_Locs || ! tag->zt_Rg)
		return (0);
	*rg = tag->zt_Rg[0];	/* XXX Best we can do */
	*origin = tag->zt_Locs[0];
	return (TRUE);
}






static void
zn_DoBadval (data, len, old, new)
float *data, old, new;
int len;
/*
 * Apply this many floating-point bad values.
 */
{
	if (old != new)
	{
		for (; len > 0; data++, len--)
			if (*data == old)
				*data = new;
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
		fids[*nfield] = tag->zt_Fids[fld];
		(*nfield)++;
	}
	return (*nfield);
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
	zn_GetBlock (tag, offset, fb, sizeof (zn_Free));
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
	long before = 0, after = 0, free;
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
		else if ((offset + len) == free)
			after = free;
	}
# ifdef notdef
ui_printf ("Free %d at %ld, before %ld, after %ld\n", len, offset, before, after);
zn_DumpFL (tag, hdr);
# endif
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
	zn_PutBlock (tag, offset, &fb, sizeof (fb));
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
		else if (after == fb.znf_Next)
			fb.znf_Next = afterfb.znf_Next;
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
		hdr->znh_NFree--;
		zn_PutBlock (tag, offset, &fb, sizeof (fb));
	}
/*
 * All done.  Clean things up and quit.
 */
	tag->zt_Sync |= SF_HEADER;
	hdr->znh_NFreeB += len;
# ifdef notdef
zn_DumpFL (tag, hdr);
# endif
}




zn_DumpFL (tag, hdr)
znTag *tag;
zn_Header *hdr;
/*
 * Dup out the free list.
 */
{
	long block;
	zn_Free fb;
	int i = 0;

	ui_printf ("ffree %ld, nfree %d, nfreeb %d len %ld\n", hdr->znh_Free,
		hdr->znh_NFree, hdr->znh_NFreeB, hdr->znh_Len);
	for (block = hdr->znh_Free; block > 0; block = fb.znf_Next)
	{
		zn_GetFreeBlock (tag, block, &fb);
		ui_printf ("  %2d at %7ld size %5d next %7ld\n", i++,
			block, fb.znf_Size, fb.znf_Next);
	}
	ui_printf ("\n");
}





static int
zn_SASize (hdr, nsample)
zn_Header *hdr;
int nsample;
/*
 * Compute the size of the sample array.
 */
{
	return ((hdr->znh_Org == OrgFixedScalar) ?
		nsample*sizeof (zn_Sample) :
		nsample*hdr->znh_NField*sizeof (zn_Sample));
}




static zn_Sample *
zn_FindSampStr (tag, sample)
znTag *tag;
int sample;
/*
 * Find the appropriate sample offset structure.
 */
{
	return (tag->zt_Sample + ((tag->zt_Hdr.znh_Org == OrgFixedScalar) ?
			sample : sample*tag->zt_Hdr.znh_NField));
}
