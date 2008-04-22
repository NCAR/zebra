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

# include <stdio.h>
# include <sys/types.h>
# include <fcntl.h>
# include <unistd.h>
# include <errno.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
#ifdef SYS4
#  include <string.h>
#endif

RCSID ("$Id: DFA_Zebra.c,v 1.37 2002-09-17 18:28:43 granger Exp $")

/*
 * There is a conflict with the symbol DataFormat between DFA and the
 * znf header file.  So include the DFA DataFormat declaration and
 * rename the DataFormat type in znfile.h, since we don't actually
 * use it here.
 */
# include "DataFormat.h"

# define DataFormat ZNF_DataFormat
# include "znfile.h"
# undef DataFormat

/*
 * The open file tag.
 */
typedef struct _znTag
{
	zn_Header	zt_Hdr;		/* The file header		*/
	int		zt_Fd;		/* Descriptor of open file	*/
	int		zt_Sync;	/* Which pieces need synching	*/
	int		zt_Write;	/* File open for write access	*/
	int		zt_Append;	/* Append all new blocks to end */
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


typedef struct _ZebraOpenFile 
{
	OpenFile	open_file;
	znTag		zn_tag;
}
ZebraOpenFile;

#define TAGP(ofp) (&((ZebraOpenFile *)ofp)->zn_tag)

/*
 * The class/organization compatibility table.  If the desired class
 * and the given file organization appear together here, we can do it.
 */
static CO_Compat COCTable[] =
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


/*
 * Zebra Native Format methods.
 */
P_CreateFile (zn_CreateFile);
P_QueryTime (zn_QueryTime);
P_SyncFile (zn_Sync);
P_CloseFile (zn_Close);
P_OpenFile (zn_Open);
P_GetData (zn_GetData);
P_GetObsSamples (zn_GetObsSamples);
P_GetFields (zn_Fields);
P_PutBlock (zn_PutSampleBlock);
P_Setup (zn_Setup);
P_GetTimes (zn_GetTimes);

/*
 * Zeb Native Format structure.
 */
static DataFormat zebraFormatRec =
{
	"Zebra",
	FTZebra,
	".znf",
	COCTable,       		/* org/class compatibility table*/
	N_COC(COCTable),
	sizeof(ZebraOpenFile),
	FALSE,				/* read-only 			*/

	FORMAT_INIT,

	zn_QueryTime,			/* Query times			*/
	fmt_MakeFileName,

	zn_Setup,			/* setup			*/
	zn_Open,			/* Open				*/
	zn_Close,			/* Close			*/
	zn_Sync,			/* Synchronize			*/
	zn_GetData,			/* Get the data			*/
	___,				/* Get altitude info		*/
	fmt_DataTimes,			/* Get data times		*/
	___,				/* Get forecast times		*/
	zn_CreateFile,			/* Create a new file		*/
	___ /*zn_PutSample*/,		/* Write to file		*/
	zn_PutSampleBlock,		/* Write block to a file	*/
	zn_GetObsSamples,		/* Get observation samples	*/
	zn_Fields,			/* Get fields			*/
	___,				/* Get Attributes		*/
	zn_GetTimes,			/* Return array of ZebTimes	*/
	___                             /* Get the associated files     */
};

DataFormat *zebraFormat = (DataFormat *) &zebraFormatRec;



/*
 * Forwards.
 */
static void	zn_CFMakeFields FP ((znTag *, DataChunk *));
static void	zn_WriteSync FP ((znTag *));
static void	zn_CFMakeStations FP ((znTag *, DataChunk *));
static int	zn_FindDest FP ((OpenFile *, ZebTime *, int nsample, 
				 WriteCode));
static void	zn_ExpandTOC FP ((znTag *, int increase));
static void	zn_ReallocTOC FP ((znTag *tag, int newns));
static void	zn_OpenSlot FP ((znTag *, int));
static int	zn_WrBoundary FP ((znTag *, DataChunk *, int, zn_Sample *,
			 WriteCode, int));
static int	zn_WrGrid FP ((znTag *, DataChunk *, int, int, zn_Sample *, 
			WriteCode, int *, FieldId *, int));
static int	zn_WrIRGrid FP ((znTag *, DataChunk *, int, int, zn_Sample *, 
			WriteCode, int *, FieldId *, int));
static void	zn_WrScalar FP ((znTag *, DataChunk *, int, int, zn_Sample *, 
			WriteCode, int *, FieldId *, int));
static int	zn_WrFixScalar FP ((znTag *, DataChunk *, int, int,
			zn_Sample *, WriteCode, int *, FieldId *, int));
static void 	zn_WrLocInfo FP ((znTag *, int, Location *, RGrid *));
static void	zn_WrLocations FP ((znTag *tag, DataChunk *dc, int fsample,
				    int dcsample, int nsample));
static int	zn_WrTrans FP ((znTag *, DataChunk *, int, int, zn_Sample *, 
			WriteCode));
static void	zn_GetFieldIndex FP ((znTag *, FieldId *, int, int *, int));
static void	zn_DataWrite FP ((znTag *, void *, int, zn_Sample *,
			WriteCode));
static void	zn_OFLoadStations FP ((znTag *));
static void	zn_PutAttrs FP ((znTag *, int, void *, int));
#ifdef maybe
static void	zn_PutAttrsBlock FP ((znTag *tag, DataChunk *dc,
				      int fsample, int sample, int nsample));
#endif
static void	zn_ReadBoundary FP ((znTag *, DataChunk *, int, int));
static void	zn_ReadGrid FP ((znTag *, DataChunk *, int, int, int,
				 dsDetail *, int));
static void	zn_ReadIRG FP ((znTag *, DataChunk *, int, int, int));
static void	zn_ReadScalar FP ((znTag *, DataChunk *, int, int, int));
static void	zn_ReadTrans FP ((znTag *, DataChunk *, int, int, int));
static void	zn_ReadLocation FP ((znTag *, DataChunk *, int, int));
static void	zn_RdRGridOffset FP ((RGrid *, Location *, int *, int *,
			dsDetail *, int));
static void	zn_DoBadval FP ((float *, int, double, double));
static void	zn_SetBad FP ((float *, int, double));

static int	zn_WriteBlock FP((znTag *tag, DataChunk *dc, int fsample, 
				    int sample, int nsample, WriteCode wc,
				    unsigned int *size));
static void	zn_LoopBlock FP((znTag *tag, DataChunk *dc, int fsample, 
				 int sample, int nsample, WriteCode wc,
				 int *index, FieldId *fids, int nfield));
static int	zn_DetectDataBlock FP((znTag *tag, DataChunk *dc, int fsample,
				       int nsample, int *index, int nfield,
				       int *roffset, int *rsize));
static void*	zn_WrScalarBlock FP((znTag *tag, DataChunk *dc, int fsample,
				     int dcsample, int nsample, WriteCode wc,
				     int *index, FieldId *fids, int nfield,
				     unsigned int *size));
static void*	zn_WrFixScalarBlock FP((znTag *tag, DataChunk *dc, 
					int fsample, int dcsample, int nsample,
					WriteCode wc, int *index, 
					FieldId *fids, int nfield, 
					unsigned int *size));
static void*	zn_WrTransBlock FP((znTag *tag, DataChunk *dc, int fsample, 
				    int dcsample, int nsample, WriteCode wc,
				    unsigned int *size));
static void*	zn_WrBoundaryBlock FP((znTag *tag, DataChunk *dc,
				       int fsample, int dcsample, int nsample,
				       WriteCode wc, int index, 
				       unsigned int *size));
static void*	zn_WrGridBlock FP((znTag *tag, DataChunk *dc, int fsample, 
				   int dcsample, int nsample, WriteCode wc,
				   int *index, FieldId *fids, int nfield,
				   unsigned int *size));
static void*	zn_WrIRGridBlock FP((znTag *tag, DataChunk *dc, int fsample,
				     int dcsample, int nsample, WriteCode wc,
				     int *index, FieldId *fids, int nfield,
				     unsigned int *size));

static int	zn_GetFromFree FP ((znTag *, int, int, zn_Free *, int));
static int	zn_GetSpace FP ((znTag *, int));
static void	zn_GetFreeBlock FP ((znTag *, int, zn_Free *));
static void	zn_PutFreeBlock FP ((znTag *tag, int offset, zn_Free *fb));
static void	zn_GetBlock FP ((znTag *, int, void *, int));
static void	zn_PutBlock FP ((znTag *, int, void *, int));
static void	zn_FreeSpace FP ((znTag *, int, int));
static void	zn_TruncateFreeBlock FP ((znTag *, int offset, zn_Free *fb));

static int 	zn_SASize FP ((zn_Header *, int));
static zn_Sample *zn_FindSampStr FP ((znTag *, int));
static int	zn_AltUnits FP ((AltUnitType));
static AltUnitType zn_CvtAltUnits FP ((int zau));
static void	zn_RdFieldInfo FP ((znTag *));
static void	zn_WrFieldInfo FP ((znTag *));




inline
static int
zn_SASize (hdr, nsample)
zn_Header *hdr;
int nsample;
/*
 * Compute the size of the sample array.  For those organizations and
 * classes with no fields, and for FixedScalar, there is only one zn_Sample
 * structure per sample time.
 */
{
	return (((hdr->znh_Org == OrgFixedScalar) ||
		 (hdr->znh_NField == 0)) ? 
		nsample*sizeof (zn_Sample) :
		nsample*hdr->znh_NField*sizeof (zn_Sample));
}




inline
static zn_Sample *
zn_FindSampStr (tag, sample)
znTag *tag;
int sample;
/*
 * Find the appropriate sample offset structure.
 */
{
	return (tag->zt_Sample + (((tag->zt_Hdr.znh_Org == OrgFixedScalar) 
				  || (tag->zt_Hdr.znh_NField == 0)) ?
		sample : sample*tag->zt_Hdr.znh_NField));
}




static int
zn_CreateFile (ofp, dc, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
dsDetail *details;
int ndetail;
/*
 * Create a new zebra native file.
 */
{
	znTag *tag = TAGP (ofp);
	zn_Header *hdr = &tag->zt_Hdr;
	int ssize, asize;
	char *fname = ofp->of_df.df_fullname;
	zbool grid;
	zbool hint = FALSE;
	void *ablock;
	SValue svalue;
	int res_size = 0, reserved = -1;
	int grain = ZN_GRAIN;
/*
 * Create the file itself before we go anywhere.
 */
	if ((tag->zt_Fd = open (fname, O_RDWR|O_TRUNC|O_CREAT, 0666)) <0)
	{
		msg_ELog (EF_PROBLEM, "Can't create file %s (%d)",
			fname, errno);
		return (FALSE);
	}
/*
 * Initialize the file tag.
 */
	tag->zt_Sync = SF_HEADER;
	tag->zt_Sample = 0; tag->zt_Fids = 0;
	tag->zt_Fields = 0; tag->zt_Ids = 0; tag->zt_Locs = 0;
	tag->zt_Write = TRUE;
	tag->zt_Append = FALSE;
	tag->zt_Attr = 0;
	tag->zt_GlAttr = 0;
	tag->zt_Rg = 0;
/*
 * Header initialization.  Once this is done we can start space allocation.
 */
	hdr->znh_Magic = ZN_MAGIC;
	hdr->znh_Free = -1;
	hdr->znh_Len = hdr->znh_NFree = hdr->znh_NFreeB = 0;
	hdr->znh_NSample = hdr->znh_NField = 0;
	hdr->znh_Org = (DataOrganization) 
	    ds_PlatformDataOrg (ofp->of_df.df_pid);
	hdr->znh_OffLoc = -1;
	hdr->znh_OffStation = -1;
	hdr->znh_NStation = 0;
	hdr->znh_OffGlAttr = -1;
	hdr->znh_OffAttr = -1;
	hdr->znh_GlAttrLen = 0;
	hdr->znh_OffRg = -1;
	hdr->znh_Version = ZN_VERSION;
	hdr->znh_AltUnits = zn_AltUnits (dc_GetLocAltUnits (dc));
	
	if (hdr->znh_AltUnits == ZAU_BAD)
	{
		msg_ELog (EF_PROBLEM, "znf: Can't handle altitude units '%s'",
			  au_UnitsName (dc_GetLocAltUnits (dc)));
		close (tag->zt_Fd);
		return (FALSE);
	}
/*
 * Allocate the space for the header itself and sync it out.
 */
	(void) zn_GetSpace (tag, sizeof (zn_Header)); /* Know it's at 0 */
	zn_WriteSync (tag);
/*
 * Check for any space reservation requests
 */
	if (ds_GetDetail (DD_ZN_RESERVE_BLOCK, details, ndetail, &svalue) &&
	    (svalue.us_v_int > 0))
	{
	/*
	 * Get a block and hold it while other stuff is allocated.  This
	 * prevents the block from being immediately truncated since it
	 * lies at the end of the file.  We free the block at the end of
	 * this function.
	 */
		reserved = zn_GetSpace (tag, svalue.us_v_int);
		res_size = svalue.us_v_int;
		msg_ELog (EF_DEBUG, "znf createfile: reserving %d bytes",
			  svalue.us_v_int);
	}
/*
 * Check for hints about the number of samples for which we should prepare, but
 * don't let them go below a reasonable default (protects against < 0 also)
 */
	if (ds_GetDetail (DD_ZN_HINT_NSAMPLES, details, ndetail, &svalue) &&
	    (svalue.us_v_int > grain))
	{
		hint = TRUE;
		grain = svalue.us_v_int;
		msg_ELog (EF_DEBUG, "znf: creating file for %d samples",grain);
	}
/*
 * Allocate the time array.
 */
	tag->zt_Time = (ZebTime *) malloc (grain * sizeof (ZebTime));
	hdr->znh_OffTime = zn_GetSpace (tag, grain * sizeof (ZebTime));
	hdr->znh_NSampAlloc = grain;
	tag->zt_Sync |= SF_HEADER | SF_TIME;
/*
 * Allocate and fill in the field info array.
 */
 	zn_CFMakeFields (tag, dc);
/*
 * Allocate the sample offset array.
 */
	ssize = zn_SASize (hdr, grain);
	tag->zt_Sample = (zn_Sample *) malloc (ssize);
	hdr->znh_OffSample = zn_GetSpace (tag, ssize);
	tag->zt_Sync |= SF_HEADER | SF_SAMPLE;
/*
 * If we're being hinted about the number of samples, then allocate the
 * sample attribute offset array as well, just in case they eventually
 * want attributes.  Some day this could be a hint, if so desired.
 */
	if (hint)
	{
		int size = grain * sizeof (zn_Sample);
		hdr->znh_OffAttr = zn_GetSpace (tag, size);
		tag->zt_Attr = (zn_Sample *) malloc (size);
		memset (tag->zt_Attr, 0, size);
		tag->zt_Sync |= SF_HEADER | SF_ATTR;
	}
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
	else if (grid || ds_IsMobile (dc->dc_Platform)) /* Locations	*/
	{
		tag->zt_Locs = (Location *) malloc (grain * sizeof(Location));
		hdr->znh_OffLoc = zn_GetSpace (tag, grain * sizeof(Location));
		tag->zt_Sync |= SF_LOCATION;
	}
	else
		dc_GetLoc (dc, 0, &hdr->znh_Loc);
/*
 * Grid dimensions.
 */
	if (grid)
	{
		tag->zt_Rg = (RGrid *) malloc (grain * sizeof (RGrid));
		hdr->znh_OffRg = zn_GetSpace (tag, grain * sizeof (RGrid));
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
 * Release any space we reserved above.
 */
	if (reserved > 0)
		zn_FreeSpace (tag, reserved, res_size);
/*
 * Sync up and we are done.
 */
	zn_WriteSync (tag);
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
 * Make a special case for boundaries. (XXX)
 */
	if (dc->dc_Class == DCC_Boundary)
	{
		bfid = F_Lookup ("boundary");
		fids = &bfid;
		nf = 1;
	}
	else if (dc->dc_Class == DCC_Transparent)
	{
	/*
	 * Transparent class has no fields.
	 */
		tag->zt_Fids = NULL;
		tag->zt_Hdr.znh_OffField = -1;
		tag->zt_Hdr.znh_NField = 0;
		tag->zt_Fields = NULL;
		return;
	}
	else
	{
		fids = dc_GetFields (dc, &nf);
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
		strcpy (tag->zt_Fields[i].zf_Name, F_GetFullName (fids[i]));
		tag->zt_Fields[i].zf_Format = DF_Float;
		if (dc_IsSubClass (dc->dc_ClassP, DCP_MetData))
			tag->zt_Fields[i].zf_Badval = 
				dc_FindFloatBadval (dc, fids[i]);
		else
			tag->zt_Fields[i].zf_Badval = dc_GetBadval (dc);
		tag->zt_Fields[i].zf_AttrLen = 0;
		tag->zt_Fields[i].zf_OffAttr = -1;
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
	int hdrsize;
/*
 * Dump the header, using the smaller size if it's a Version 1 file
 */
	hdrsize = sizeof (zn_Header);
	if (hdr->znh_Version == 1)
		hdrsize = ZN_V1_HDRLEN;
	
	if (tag->zt_Sync & SF_HEADER)
		zn_PutBlock (tag, 0, &tag->zt_Hdr, hdrsize);
/*
 * Dump out the various tables.
 */
	if (tag->zt_Sync & SF_TIME)
		zn_PutBlock (tag, hdr->znh_OffTime, tag->zt_Time,
			hdr->znh_NSampAlloc*sizeof (ZebTime));
	if (tag->zt_Sync & SF_SAMPLE)
		zn_PutBlock (tag, hdr->znh_OffSample, tag->zt_Sample,
		       zn_SASize (hdr, hdr->znh_NSampAlloc));
	if (tag->zt_Sync & SF_LOCATION)
		zn_PutBlock (tag, hdr->znh_OffLoc, tag->zt_Locs,
			hdr->znh_NSampAlloc*sizeof (Location));
	if ((tag->zt_Sync & SF_FIELD) && hdr->znh_NField)
		zn_WrFieldInfo (tag);
	if (tag->zt_Sync & SF_ATTR)
		zn_PutBlock (tag, hdr->znh_OffAttr, tag->zt_Attr,
			hdr->znh_NSampAlloc*sizeof (zn_Sample));
	if (tag->zt_Sync & SF_RGRID)
		zn_PutBlock (tag, hdr->znh_OffRg, tag->zt_Rg,
			hdr->znh_NSampAlloc*sizeof (RGrid));
	tag->zt_Sync = 0;
#ifdef maybe /* might this be necessary for concurrent reads and writes? */
	fsync (tag->zt_Fd);
#endif
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




static int
zn_QueryTime (const char *file, ZebraTime *begin, ZebraTime *end, int *nsample)
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





static int
zn_PutSampleBlock (ofp, dc, sample, nsample, wc, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int sample, nsample;
WriteCode wc;
dsDetail *details;
int ndetail;
/*
 * Dump a block of samples into the file.
 *
 * The idea is this: the datachunk contains a series of 'nsample' samples,
 * chronological and continuous, beginning at 'sample'.  Construct a block
 * of data holding all of the samples from all of the fields, while
 * creating zn_Sample entries for each sample and field in the block.  Only
 * the size and relative offset will be known for each zn_Sample structure:
 * the actual offsets of each will not be known until the block is placed
 * with zn_GetSpace().  (Kind of like linking object files.)  Then put the
 * block, times, and the new array of zn_Sample's into the file.
 *
 * Sample attributes could be done similarly in parallel with the sample
 * data, but for now they're done one-by-one.
 *
 * If appending or inserting, we are allocating new space so we do not need
 * to release any old space.  If overwriting, we first try to free all of
 * the space pointed to by the zn_Sample structures being overwritten.  If
 * the space cannot be freed without introducing lots of fragmentation in
 * the file, the samples are overwritten in place using the same method as
 * zn_PutSample().
 *
 * To avoid too much fragmentation, and to make block overwrites faster in
 * the long run, look for a dsDetail which tells us how many samples we can
 * expect in this file.  At some point we might want to provide the option
 * of wiring this to the platform's maxsamples limit.
 */
{
	int fsample, alen;
	int i;
	unsigned int block_size;
	znTag *tag = TAGP (ofp);
	ZebTime t;
	zn_Sample *samp;
	zn_Header *hdr = &tag->zt_Hdr;
	void *ablock;
	char atime[30];
	SValue svalue;
/*
 * Make sure we have altitude unit consistency
 */
	if (zn_AltUnits (dc_GetLocAltUnits (dc)) != hdr->znh_AltUnits)
	{
		msg_ELog (EF_PROBLEM, 
			  "zn_PutSampleBlock failed: Alt units mismatch");
		return (0);
	}
/*
 * Check for hints about the number of samples we should have alloc'ed
 */
	if (ds_GetDetail (DD_ZN_HINT_NSAMPLES, details, ndetail, &svalue) &&
	    (svalue.us_v_int > hdr->znh_NSampAlloc))
	{
		msg_ELog (EF_DEBUG, "znf: realloc'ing file for %d samples", 
			  svalue.us_v_int);
		zn_ReallocTOC (tag, svalue.us_v_int);
	}
/*
 * Figure out where the first sample is to be written. zn_FindDest()
 * will automatically adjust the sizes of our sample arrays if needed.
 */
	dc_GetTime (dc, sample, &t);
	fsample = zn_FindDest (ofp, &t, nsample, wc);
	TC_EncodeTime (&t, TC_TimeOnly, atime);
	msg_ELog (EF_DEVELOP, 
		  "znf PutBlock, wc %d, %d samples at fsample %d, %s", wc,
		  nsample, fsample, atime);
/*
 * Check whether this sample is supposed to be appended
 */
	if (ds_GetDetail (DD_ZN_APPEND_SAMPLES, details, ndetail, NULL))
		tag->zt_Append = TRUE;
	(void) zn_WriteBlock (tag, dc, fsample, sample, nsample, wc,
			      &block_size);
	tag->zt_Append = FALSE;
/*
 * We calculate and flush the affected times out here rather than
 * dirty up and sync the entire array, but only if the times are new.
 */
	if (wc != wc_Overwrite)
	{
		for (i = 0; i < nsample; ++i)
		{
			dc_GetTime (dc, sample + i, &t);
			tag->zt_Time[fsample + i] = t;
		}
		zn_PutBlock (tag, hdr->znh_OffTime + fsample*sizeof (ZebTime),
			     tag->zt_Time + fsample, nsample*sizeof (ZebTime));
	}
/*
 * Save out the sample array.
 */
	samp = zn_FindSampStr (tag, fsample);
	zn_PutBlock (tag, hdr->znh_OffSample + zn_SASize (hdr, fsample),
		     samp, zn_SASize (hdr, nsample));
/*
 * If there are sample attributes, save them; otherwise erase any existing
 * ones (ablock == NULL).  For now, write sample attributes one-at-a-time
 * rather than as a block.  This appears to perform as well or better than
 * zn_PutAttrsBlock(), though I never gprof'ed it or anything...
 */
	for (i = 0; i < nsample; ++i)
	{
		ablock = dc_GetSaAttrBlock (dc, sample+i, &alen);
		zn_PutAttrs (tag, fsample+i, ablock, alen);
	}
#ifdef maybe
	zn_PutAttrsBlock (tag, dc, fsample, sample, nsample);
#endif
/*
 * And we're finally done.
 */
	zn_WriteSync (tag);
	return (1);
}



static int
zn_WriteBlock (tag, dc, fsample, sample, nsample, wc, size)
znTag *tag;
DataChunk *dc;
int fsample, sample, nsample;
WriteCode wc;
unsigned int *size;
/*
 * It is up to each organization to construct a transparent block of bytes
 * of data and edit the sizes and offsets in the Sample array according to
 * the size of each field within each sample in the block, and also to
 * modify and write out the locations array.  The offsets should be set
 * relative to the beginning of the block of data.  NO data (except
 * locations) should be written to the file inside the routine.  The
 * organization-specific routine returns a pointer to the block and the
 * size of the block.
 *
 * When overwriting, if we cannot free the target samples efficiently, as
 * determined by zn_DetectDataBlock(), we bail and loop over each sample
 * and overwrite it in place using the per-sample functions.
 */
{
	void *block;
	int *index;
	unsigned int block_size;
	FieldId *fids = NULL;
	zn_Sample *samp;
	int nfield, i, fld;
	zn_Header *hdr = &tag->zt_Hdr;
	int offset;
	int freed, freed_size;

	/*
	 * Get our field array into order.  The field index maps a field's
	 * index in the datachunk to its index in the znf header's Fields[]
	 * array.  If new fields are being added to the file, the
	 * zt_Sample[] array will be expanded to make room for the new
	 * fields.
	 */
	index = NULL;
	if (dc->dc_Class != DCC_Boundary && dc->dc_Class != DCC_Transparent)
	{
		fids = dc_GetFields (dc, &nfield);
		index = (int *) malloc (nfield*sizeof (int));
		zn_GetFieldIndex (tag, fids, nfield, index, TRUE);
	}
	/*
	 * If overwriting, see if we can free up the block as a whole.
	 * Note that no space is actually freed in this call.  We need the
	 * data to be valid for the FixedScalar organization, and we also
	 * may be able to directly overwrite the disk space without going
	 * through the zn_FreeSpace() overhead.
	 */
	freed = FALSE;
	if (wc == wc_Overwrite)
		freed = zn_DetectDataBlock (tag, dc, fsample, nsample, index, 
					    nfield, &offset, &freed_size);
	block = NULL;
	block_size = 0;
	/*
	 * If the samples being overwritten we're not free-able above, then
	 * we'll have to overwrite in place by looping over the per-sample
	 * calls.
	 */
	if ((wc == wc_Overwrite) && (!freed))
	{
		msg_ELog (EF_DEBUG, "znf overwriting block by samples");
		zn_LoopBlock (tag, dc, fsample, sample, nsample, wc, 
			      index, fids, nfield);
	}
	else
	{
		/*
		 * Otherwise we build a block and write it out all at once.
		 */
		switch (hdr->znh_Org)
		{
		   case OrgOutline:
			block = zn_WrBoundaryBlock (tag, dc, fsample, sample,
					    nsample, wc, 0, &block_size);
			break;
			
		   case Org1dGrid:
		   case Org2dGrid:
		   case Org3dGrid:
			block = zn_WrGridBlock (tag, dc, fsample, sample, 
				nsample, wc, index, fids, nfield, &block_size);
			break;
			
		   case OrgIRGrid:
			block = zn_WrIRGridBlock (tag, dc, fsample, sample, 
			    nsample, wc, index, fids, nfield, &block_size);
			break;
			
		   case OrgScalar:
			block = zn_WrScalarBlock (tag, dc, fsample, sample, 
			    nsample, wc, index, fids, nfield, &block_size);
			break;
			
		   case OrgFixedScalar:
			/*
			 * The block we are overwriting will be freed or
			 * re-used after this call, but for this call the
			 * data in the file must be valid and unchanged
			 * since this routine needs the original data to
			 * fill the block.
			 */
			block = zn_WrFixScalarBlock (tag, dc, fsample, sample, 
			      nsample, wc, index, fids, nfield, &block_size);
			break;
			
		   case OrgTransparent:
			block = zn_WrTransBlock (tag, dc, fsample, sample, 
						 nsample, wc, &block_size);
			break;
		   default:
			/* no other orgs handled */
			break;
		}
		if (!block)
		{
			if (index) 
				free (index);
			return (0);
		}
		/*
		 * Find the space for the block, then don't forget to
		 * actually write it and free it.  If the space freed above
		 * fits, use it.  Otherwise free it and get a different block.
		 */
		if (freed && (freed_size != block_size))
		{
			msg_ELog (EF_DEBUG, 
				  "%s: %i bytes at %li",
				  "znf freeing overwrite block",
				  freed_size, offset);
			zn_FreeSpace (tag, offset, freed_size);
			offset = zn_GetSpace (tag, block_size);
		}
		else if (!freed)
			/*
			 * Must be we're appending or inserting new stuff
			 */
			offset = zn_GetSpace (tag, block_size);
		else
			msg_ELog (EF_DEBUG, "znf re-using block space");
		zn_PutBlock (tag, offset, block, block_size);
		free (block);
		/*
		 * Now we have nsample samples of data, stored in
		 * block_size contiguous bytes beginning at block.  The
		 * zt_Sample array has correct sizes for each sample, but
		 * offsets are relative to block.  Add the location of the
		 * block to the offset in each of the zt_Sample entries.
		 */
		for (i = 0; i < nsample; ++i)
		{
			samp = zn_FindSampStr (tag, fsample + i);
			if (dc->dc_Class != DCC_Boundary && 
			    dc->dc_Class != DCC_Transparent && 
			    tag->zt_Hdr.znh_Org != OrgFixedScalar)
			{
				for (fld = 0; fld < nfield; ++fld)
				   samp[ index[fld] ].znf_Offset += offset;
			}
			else
				samp->znf_Offset += offset;
		}
	}

	if (index)
		free (index);
	*size = block_size;
	return (offset);
}




static void
zn_LoopBlock (tag, dc, fsample, sample, nsample, wc, index, fids, nfield)
znTag *tag;
DataChunk *dc;
int fsample, sample, nsample;
WriteCode wc;
int *index;
FieldId *fids;
int nfield;
/*
 * Writes a block of 'nsample' samples by looping over each sample
 * individually.  The block begins at sample 'fsample' in the file and 
 * sample number 'sample' in the DataChunk.  Begin a block implies that
 * sample time (fsample + i) in the file corresponds to sample time
 * (sample + i) in the DataChunk, for 0 <= i < nsample.
 */
{
	zn_Sample *samp;
	zn_Header *hdr = &tag->zt_Hdr;
	int i;

	for (i = 0; i < nsample; ++i)
	{
		samp = zn_FindSampStr (tag, fsample + i);
		switch (hdr->znh_Org)
		{
		   case OrgOutline:
			zn_WrBoundary (tag, dc, sample+i, samp, wc, 0);
			break;
			
		   case Org1dGrid:
		   case Org2dGrid:
		   case Org3dGrid:
			zn_WrGrid (tag, dc, fsample+i, sample+i, samp, wc, 
				   index, fids, nfield);
			break;
			
		   case OrgIRGrid:
			zn_WrIRGrid (tag, dc, fsample+i, sample+i, samp, wc, 
				     index, fids, nfield);
			break;
			
		   case OrgScalar:
			zn_WrScalar (tag, dc, fsample+i, sample+i, samp, wc, 
				     index, fids, nfield);
			break;
			
		   case OrgFixedScalar:
			zn_WrFixScalar (tag, dc, fsample+i, sample+i, samp, 
					wc, index, fids, nfield);
			break;
			
		   case OrgTransparent:
			zn_WrTrans (tag, dc, fsample+i, sample+i, samp, wc);
			break;
		   default:
			/* no other orgs handled */
			break;
		}
	}
}




#ifdef maybe	/* 
		 * Decommissioned until such time, if any, as someone thinks
		 * it may provide a real time savings for their purposes.
		 * Maybe use of this function could be a dsDetail. 
		 */
static void
zn_PutAttrsBlock (tag, dc, fsample, sample, nsample)
znTag *tag;
DataChunk *dc;
int fsample, sample, nsample;
/*
 * Loop through the sample-atts in the block to get the total space
 * required, then malloc the space and actually copy each sample att
 * into the block of memory, setting the size and offset of SAttr list
 * accordingly
 */
{
	zn_Header *hdr = &tag->zt_Hdr;
	zn_Sample *zs;
	int i;
	int alen;
	int aspace;
	char *ablock, *satt, *bptr;
	int offset;

	aspace = 0;
	for (i = 0; i < nsample; ++i)
	{
		satt = dc_GetSaAttrBlock (dc, sample+i, &alen);
		if (satt)
			aspace += alen;
	}

	/*
	 * If no attribute block and no attribute array, we're outta here
	 */
	if ((hdr->znh_OffAttr < 0) && (aspace == 0))
		return;

	/*
	 * If no attribute array yet, we need to make one.
	 */
	if (hdr->znh_OffAttr < 0)
	{
		int size = hdr->znh_NSampAlloc*sizeof (zn_Sample);
		hdr->znh_OffAttr = zn_GetSpace (tag, size);
		tag->zt_Attr = (zn_Sample *) malloc (size);
		memset (tag->zt_Attr, 0, size);
		tag->zt_Sync |= SF_HEADER | SF_ATTR;
	}
	/*
	 * Otherwise we need to free the attributes which will be
	 * overwritten by this block.
	 */
	else
	{
		zs = tag->zt_Attr + fsample;
		for (i = 0; i < nsample; ++i)
		{
			if (zs->znf_Size > 0)
				zn_FreeSpace (tag, zs->znf_Offset, 
					      zs->znf_Size);
			++zs;
		}
	}

	ablock = (char *) malloc (aspace);
	offset = zn_GetSpace (tag, aspace);
	/*
	 * Now we have an offset for our block, so we can calculate the
	 * offsets for the zn_Sample structures as we copy each sample's
	 * attributes.
	 */
	zs = tag->zt_Attr + fsample;
	bptr = ablock;
	for (i = 0; i < nsample; ++i)
	{
		satt = dc_GetSaAttrBlock (dc, sample+i, &alen);
		if (satt)
		{
			memcpy (bptr, satt, alen);
			zs->znf_Size = alen;
			zs->znf_Offset = offset + (bptr - ablock);
			bptr += alen;
		}
		else
		{
			zs->znf_Size = 0;
			zs->znf_Offset = 0;
		}
		++zs;
	}
	/*
	 * Sync out the attribute block and the array of sample entries.
	 */
	zn_PutBlock (tag, offset, ablock, aspace);
	zn_PutBlock (tag, hdr->znh_OffAttr + fsample*sizeof (zn_Sample),
		tag->zt_Attr + fsample, nsample*sizeof (zn_Sample));

	free (ablock);
}	
#endif /* zn_PutAttrsBlock */




static int
zn_DetectDataBlock (tag, dc, fsample, nsample, index, nfield, roffset, rsize)
znTag *tag;
DataChunk *dc;
int fsample, nsample;
int *index;
int nfield;
int *roffset;
int *rsize;
/*
 * Verify that the desired fields of the desired samples all occur
 * contiguously in the file as one block.  Essentially this means that the
 * sample data occurs in the same order in the file as the zn_Sample
 * structures.  So for fixed-scalar, field order does not matter; only
 * sample order in the file matters.  For scalar orgs, the samples must
 * be in order, and within each sample the field data must be in the order
 * of the zn_Sample entries for each field, which is the order in the
 * file headers field list.
 *
 * If a block is detected, the offset of the block and its size are returned
 * in *roffset and *rsize, and then function returns TRUE.  Otherwise
 * *roffset and *rsize are zero, and the function returns FALSE.
 *
 * This function is used to minimize writes when overwriting existing data,
 * and to reduce reads to a single zn_GetBlock whenever possible.
 */
{
	zn_Sample *samp;
	int i, fld;
	int offset;
	int size;

	*roffset = 0;
	*rsize = 0;
	offset = -1;
	size = 0;
	samp = zn_FindSampStr (tag, fsample);

	if (dc->dc_Class != DCC_Boundary && dc->dc_Class != DCC_Transparent &&
	    tag->zt_Hdr.znh_Org != OrgFixedScalar)
	{
		for (i = 0; i < nsample; ++i)
		{
			for (fld = 0; fld < nfield; ++fld)
			{
				if ((index[fld] < 0) || 
				    (samp[ index[fld] ].znf_Size == 0))
					continue;
				if ((offset >= 0) && (offset + size == 
					      samp[ index[fld] ].znf_Offset))
				{
					size += samp[index[fld]].znf_Size;
				}
				else
				{
					/*
					 * There is space to free, but we
					 * didn't make it through all the
					 * samples, so fail immediately.
					 */
					if (size)
						return (FALSE);
					offset = samp[index[fld]].znf_Offset;
					size = samp[index[fld]].znf_Size;
				}
			}
			samp += tag->zt_Hdr.znh_NField;
		}
	}
	else
	{
	/*
	 * Don't need to worry about fields for Boundary and Transparent
	 * classes, nor for FixedScalar organizations.
	 */
		for (i = 0; i < nsample; ++i)
		{
			if (samp->znf_Size == 0)
				continue;
			if ((offset >= 0) && 
			    (offset + size == samp->znf_Offset))
			{
				size += samp->znf_Size;
			}
			else
			{
				if (size)
					return (FALSE);
				offset = samp->znf_Offset;
				size = samp->znf_Size;
			}
			++samp;
		}
	}
	*roffset = offset;
	*rsize = size;
	return (TRUE);
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
		tag->zt_Sync |= SF_HEADER | SF_ATTR;
	}
/*
 * If there is already an attribute block there, free it up.  If there was
 * nothing there before, and we're not adding anything, then we don't
 * have to sync anything, so we just return.
 */
	zs = tag->zt_Attr + sample;
	if ((zs->znf_Size == 0) && (ablock == NULL))
		return;
	else if ((ablock != NULL) && (zs->znf_Size == alen))
	{
		zn_PutBlock (tag, zs->znf_Offset, ablock, alen);
		return;
	}
	else if (zs->znf_Size > 0)
		zn_FreeSpace (tag, zs->znf_Offset, zs->znf_Size);
/*
 * Allocate new space and fill it in, or reset the entry to "empty"
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
 * Figure out file field which corresponds to each dc field.  This routine
 * will add the fields to the file if necessary.
 */
{
	int nnew = 0, dcfield, ffield, newsize, sample, field, zfldsize;
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
			np->znf_Offset = np->znf_Size = 0;
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
/*
 * Deal with space in the file, too.  For version 1 files, we allocate
 * less space, since we only write a part of the whole field structure.
 */
	zfldsize = (hdr->znh_Version == 1) ? 
		sizeof (zn_FieldV1) : sizeof (zn_Field);
	
	zn_FreeSpace (tag, hdr->znh_OffField, hdr->znh_NField * zfldsize);
	hdr->znh_OffField = zn_GetSpace (tag, 
					 (hdr->znh_NField + nnew) * zfldsize);
/*
 * Now we can map the remaining fields.
 */
	for (dcfield = 0; dcfield < nfield; dcfield++)
		if (index[dcfield] < 0)
		{
			strcpy (tag->zt_Fields[hdr->znh_NField].zf_Name,
					F_GetFullName (fids[dcfield]));
			tag->zt_Fields[hdr->znh_NField].zf_Format = DF_Float;
			tag->zt_Fields[hdr->znh_NField].zf_AttrLen = 0;
			tag->zt_Fields[hdr->znh_NField].zf_OffAttr = -1;
			index[dcfield] = hdr->znh_NField++;
		}
/*
 * Write out the new field name table (sync won't do that) and we are 
 * done.
 */
	zn_WrFieldInfo (tag);
}





static int
zn_FindDest (ofp, t, nsample, wc)
OpenFile *ofp;
ZebTime *t;
int nsample;
WriteCode wc;
/*
 * Figure out just where we will be writing this piece of data, and make
 * sure there is enough room for 'nsample' samples at that location.
 */
{
	znTag *tag = TAGP (ofp);
	int sample;
	switch (wc)
	{
	/*
	 * The append case is relatively simple.  Expand our space for
	 * more samples and set the dest to the 1st sample of the new space.
	 */
	   case wc_Append:
	   case wc_NewFile:
	   	zn_ExpandTOC (tag, nsample);
		return (tag->zt_Hdr.znh_NSample - nsample);
	/*
	 * For the overwrite case, we just find the unlucky sample (and
	 * the nsample-1 unlucky ones after it) and return that.
	 */
	   case wc_Overwrite:
		sample = dfa_TimeIndex (ofp, t, 0);
		return (sample);
	/*
	 * For the insert case, we find the place to do the insertion,
	 * and open up the space.  At present, inserts are only supported
	 * for a single sample, which is ok since block inserts don't
	 * exist yet.
	 */
	   case wc_Insert:
		sample = dfa_TimeIndex (ofp, t, 0) + 1;
		zn_OpenSlot (tag, sample);
		return (sample);
	    case wc_SkipIt:
		return (-99999);
		break;
	}
	return (-99999);	/* Should never get here */
}





static void
zn_ExpandTOC (tag, n)
znTag *tag;
int n;
/*
 * Make the contents information in this file 'n' entries bigger.  We just
 * take care of the growth algorithm for used sample entries, the actual
 * expansion of the alloc'ed TOC space is handled in a separate function.
 */
{
	zn_Header *hdr = &tag->zt_Hdr;
	int newns;
/*
 * The easy case is when the tables are already big enough.
 */
	if ((hdr->znh_NSample + n) <= hdr->znh_NSampAlloc)
	{
		hdr->znh_NSample += n;
		tag->zt_Sync |= SF_HEADER;
		return;
	}
/*
 * Otherwise we have to make everything bigger.  Start with the time array.
 *
 * (12/92 jc) try doubling the TOC size instead of just slowly growing it,
 * so as to reduce fragmentation problems.
 *
 * (8/93 gg) try doubling up to 1024, and from there increase by 512,
 * otherwise platforms with just over 1024 samples and tens of fields
 * (such as NEXUS soundings) waste alot of space.
 */
/*	newns = hdr->znh_NSampAlloc + ZN_GRAIN; */
	newns = hdr->znh_NSampAlloc;
	do {
		newns += (newns >= 1024) ? 512 : newns;
	} 
	while ((hdr->znh_NSample + n) > newns);
/*
 * Now pass the actual realloc process to somewhere else
 */
	zn_ReallocTOC (tag, newns);
	hdr->znh_NSample += n;
}




static void
zn_ReallocTOC (tag, newns)
znTag *tag;
int newns;
/*
 * Expand the TOC arrays to at least 'newns' entries.  Try to do it
 * strategically to cause as little fragmentation as possible.  Free all of
 * the old space before allocating new space.
 */
{
	zn_Header *hdr = &tag->zt_Hdr;
	int oldns = hdr->znh_NSampAlloc;
/*
 * The easy case is when the tables are already big enough.
 */
	if (newns <= hdr->znh_NSampAlloc)
		return;

	hdr->znh_NSampAlloc = newns;
/*
 * Release all of the old space first, in the hopes of merging some free
 * space into larger and more easily parcelled blocks 
 */
	zn_FreeSpace (tag, hdr->znh_OffTime, oldns*sizeof (ZebTime));
	zn_FreeSpace (tag, hdr->znh_OffSample, zn_SASize (hdr, oldns));
	if (hdr->znh_OffLoc >= 0)
		zn_FreeSpace (tag, hdr->znh_OffLoc, oldns*sizeof (Location));
	if (hdr->znh_OffRg >= 0)
		zn_FreeSpace (tag, hdr->znh_OffRg, oldns*sizeof (RGrid));
	if (hdr->znh_OffAttr >= 0)
		zn_FreeSpace (tag, hdr->znh_OffAttr, oldns*sizeof (zn_Sample));
/*
 * Now actually reallocate the tag arrays, get new offsets, and write to disk.
 */
	tag->zt_Time = (ZebTime *) realloc (tag->zt_Time, 
					    newns*sizeof (ZebTime));
	hdr->znh_OffTime = zn_GetSpace (tag, newns*sizeof (ZebTime));
	tag->zt_Sync |= SF_HEADER | SF_TIME;
/*
 * Now the sample information array.
 */
	tag->zt_Sample = (zn_Sample *) realloc (tag->zt_Sample,
						zn_SASize (hdr, newns));
	hdr->znh_OffSample = zn_GetSpace (tag, zn_SASize (hdr, newns));
	tag->zt_Sync |= SF_HEADER | SF_SAMPLE;
/*
 * The location array if need be.
 */
	if (hdr->znh_OffLoc >= 0)
	{
		tag->zt_Locs = (Location *) realloc (tag->zt_Locs,
						     newns*sizeof (Location));
		hdr->znh_OffLoc = zn_GetSpace (tag, newns*sizeof (Location));
		tag->zt_Sync |= SF_HEADER | SF_LOCATION;
	}
/*
 * RGRid arrays, if need be.
 */
	if (hdr->znh_OffRg >= 0)
	{
		tag->zt_Rg = (RGrid *) realloc (tag->zt_Rg,
						newns*sizeof (RGrid));
		hdr->znh_OffRg = zn_GetSpace (tag, newns*sizeof (RGrid));
		tag->zt_Sync |= SF_HEADER | SF_RGRID;
	}
/*
 * Attributes, if need be.
 */
	if (hdr->znh_OffAttr >= 0)
	{
		tag->zt_Attr = (zn_Sample *) realloc (tag->zt_Attr,
					newns*sizeof (zn_Sample));
		hdr->znh_OffAttr = zn_GetSpace (tag, newns*sizeof (zn_Sample));
		memset (tag->zt_Attr + oldns, 0,
				(newns - oldns)*sizeof (zn_Sample));
		zn_PutBlock (tag, hdr->znh_OffAttr, tag->zt_Attr,
				newns*sizeof (zn_Sample));
	}
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
	zn_ExpandTOC (tag, 1);
/*
 * Shift the various tables around.
 */
#ifdef SVR4
	memcpy  (tag->zt_Time + sample + 1, tag->zt_Time + sample,
			nmove*sizeof (ZebTime));
#else
	bcopy (tag->zt_Time + sample, tag->zt_Time + sample + 1,
			nmove*sizeof (ZebTime));
#endif
#ifdef SVR4
	memcpy (zn_FindSampStr (tag, sample + 1),
		zn_FindSampStr (tag, sample),
		zn_SASize (hdr, nmove));
#else
	bcopy (zn_FindSampStr (tag, sample),
		zn_FindSampStr (tag, sample + 1),
		zn_SASize (hdr, nmove));
#endif
	tag->zt_Sync |= SF_HEADER | SF_TIME | SF_SAMPLE;
/*
 * If there are locations do them too.
 */
	if (hdr->znh_OffLoc >= 0)
	{
#ifdef SVR4
		memcpy (tag->zt_Locs + sample + 1, tag->zt_Locs + sample,
			nmove*sizeof (Location));
#else
		bcopy (tag->zt_Locs + sample, tag->zt_Locs + sample + 1,
			nmove*sizeof (Location));
#endif
		tag->zt_Sync |= SF_LOCATION;
	}
/*
 * Same with rgrids.
 */
	if (hdr->znh_OffRg >= 0)
	{
#ifdef SVR4
		memcpy (tag->zt_Rg + sample + 1, tag->zt_Rg + sample,
			nmove*sizeof (RGrid));
#else
		bcopy (tag->zt_Rg + sample, tag->zt_Rg + sample + 1,
			nmove*sizeof (RGrid));
#endif
		tag->zt_Sync |= SF_RGRID;
	}
/*
 * If attributes, also them.
 */
	if (hdr->znh_OffAttr >= 0)
	{
#ifdef SVR4
		memcpy (tag->zt_Attr + sample + 1, tag->zt_Attr + sample,
			nmove*sizeof (zn_Sample));
#else
		bcopy (tag->zt_Attr + sample, tag->zt_Attr + sample + 1,
			nmove*sizeof (zn_Sample));
#endif
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
	return (1);
}





/*ARGSUSED*/
static void *
zn_WrGridBlock (tag, dc, fsample, dcsample, nsample, wc, index, fids, nfield,
		size)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, nsample;
WriteCode wc;
int *index; 
FieldId *fids;
int nfield;
unsigned int *size;
/*
 * Write out grid samples into a block of memory.
 */
{
	zn_Header *hdr = (zn_Header *) &tag->zt_Hdr;
	float *block, *bptr;
	int fld, len, i;
	float *data;
	RGrid rg;
	Location loc;
	zn_Sample *samp;

	*size = 0;
	for (i = 0; i < nsample; ++i)
	{
		for (fld = 0; fld < nfield; fld++)
		{
			data = dc_RGGetGrid (dc, dcsample + i, fids[fld], 
					     &loc, &rg, &len);
			if (data != NULL)
				*size += len;
		}
	}

	if (*size == 0)
		return (NULL);

	block = (float *) malloc ( *size );
	bptr = block;
	for (i = 0; i < nsample; ++i)
	{
		samp = zn_FindSampStr (tag, fsample + i);
		for (fld = 0; fld < nfield; fld++)
		{
			data = dc_RGGetGrid (dc, dcsample + i, fids[fld], 
					     &loc, &rg, &len);
			if (data != NULL)
			{
				memcpy (bptr, data, len);
				samp[ index[fld] ].znf_Offset = 
					(bptr - block) * sizeof(float);
				samp[ index[fld] ].znf_Size = len;
				bptr += len / sizeof(float);
			}
			else
			{
				samp[ index[fld] ].znf_Offset = 0;
				samp[ index[fld] ].znf_Size = 0;
			}
		}
		tag->zt_Locs[fsample + i] = loc;
		tag->zt_Rg[fsample + i] = rg;
	}

	zn_PutBlock (tag, hdr->znh_OffLoc + fsample * sizeof (Location), 
			tag->zt_Locs + fsample, nsample * sizeof (Location));
	zn_PutBlock (tag, hdr->znh_OffRg + fsample * sizeof (RGrid), 
			tag->zt_Rg + fsample, nsample * sizeof (RGrid));
	return ((void *)block);
}





/*ARGSUSED*/
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
	int fld;
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
	return (1);
}






/*ARGSUSED*/
static void *
zn_WrIRGridBlock (tag, dc, fsample, dcsample, nsample, wc, index, fids, 
		  nfield, size)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, nsample;
WriteCode wc;
int *index; 
FieldId *fids;
int nfield;
unsigned int *size;
/*
 * Write out irregular grid samples into a block of memory.
 */
{
	int fld, i;
	float *data;
	float *block, *bptr;
	int flen;
	zn_Header *hdr = (zn_Header *) &tag->zt_Hdr;
	zn_Sample *samp;
	
	flen = hdr->znh_NStation * sizeof(float);
	*size = nsample * nfield * flen;
	block = (float *) malloc ( *size );
	bptr = block;
	samp = zn_FindSampStr (tag, fsample);
	for (i = 0; i < nsample; ++i)
	{
		for (fld = 0; fld < nfield; fld++)
		{
			data = dc_IRGetGrid (dc, dcsample + i, fids[fld]);
			if (data != NULL)
			{
				memcpy (bptr, data, flen);
				samp[ index[fld] ].znf_Size = flen;
				samp[ index[fld] ].znf_Offset = 
					(bptr - block) * sizeof(float);
				bptr += hdr->znh_NStation;
			}
			else
			{
				samp[ index[fld] ].znf_Size = 0;
				samp[ index[fld] ].znf_Offset = 0;
			}
		}
		samp += hdr->znh_NField;
	}
	return ((void *)block);
}





static void
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
	int fld;
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



/*ARGSUSED*/
static void *
zn_WrScalarBlock (tag, dc, fsample, dcsample, nsample, wc, 
		  index, fids, nfield, size)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, nsample;
WriteCode wc;
int *index;
FieldId *fids;
int nfield;
unsigned int *size;
/*
 * Store some scalar data into a block of memory.
 */
{
	zn_Sample *samp;
	float *data, *di;
	int i, fld;

	*size = nsample * nfield * sizeof(float);
	data = (float *) malloc (*size);
	samp = zn_FindSampStr (tag, fsample);
/*
 * Write out the data for each field.
 */
	for (i = 0; i < nsample; ++i)
	{
		di = data + (i * nfield);
		for (fld = 0; fld < nfield; fld++)
		{
			di[fld] = dc_GetScalar (dc, dcsample + i, fids[fld]);
			(samp + index[fld])->znf_Size = sizeof(float);
			(samp + index[fld])->znf_Offset = 
				((i * nfield) + fld) * sizeof(float);
		}
		samp += tag->zt_Hdr.znh_NField;
	}
	zn_WrLocations (tag, dc, fsample, dcsample, nsample);
	return ((void *)data);
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
	int fld;
	float *fdata, bad = dc_GetBadval (dc);
	zn_Header *hdr = &tag->zt_Hdr;
/*
 * Get an array to hold all the data for this sample.
 */
	fdata = (float *) malloc (hdr->znh_NField * sizeof (float));
/*
 * For new samples, initialize all of the fields in the file to bad values,
 * since not all of the file's fields may be in the DataChunk.
 */
	if ((wc == wc_Append) || (wc == wc_Insert))
		for (fld = 0; fld < hdr->znh_NField; fld++)
			fdata[fld] = bad;
	else
		zn_GetBlock (tag, samp->znf_Offset, fdata, samp->znf_Size);
/*
 * Fill in the data for each field.  Use index[] to make sure the fields are
 * stored in the same places as in the zn_Field array, and to make sure the
 * DC field is found in the file.
 */
	for (fld = 0; fld < nfield; fld++)
		if (index[fld] >= 0)
		{
			fdata[ index[fld] ] = 
				dc_GetScalar (dc, dcsample, fids[fld]);
		}
	zn_DataWrite (tag, fdata, hdr->znh_NField*sizeof (float), samp, wc);
/*
 * Free the data.  Way to go, TestCenter!
 */
	free (fdata);
/*
 * Put out the location if necessary.
 */
	zn_WrLocations (tag, dc, fsample, dcsample, 1);
	return (1);
}






static void *
zn_WrFixScalarBlock (tag, dc, fsample, dcsample, nsample, 
		     wc, index, fids, nfield, size)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, nsample;
WriteCode wc;
int *index;
FieldId *fids;
int nfield;
unsigned int *size;
/*
 * Write out some fixed-field scalar data into a block of memory.
 */
{
	zn_Sample *samp;
	float *data, *di;
	int i, fld;
	zbool no_init;
	float bad = dc_GetBadval (dc);
	zn_Header *hdr = &tag->zt_Hdr;

	*size = nsample * hdr->znh_NField * sizeof(float);
	data = (float *) malloc (*size);
	samp = zn_FindSampStr (tag, fsample);
/*
 * Do a quick check to see if we're writing every field.  If so, we can
 * avoid some overhead inside the loop, especially when overwriting.
 */
	no_init = (nfield == hdr->znh_NField);
	for (i = 0; (i < nfield) && no_init; ++i)
		if (index[i] < 0)
			no_init = FALSE;
/*
 * Write out the data for each field.
 */
	for (i = 0; i < nsample; ++i)
	{
		di = data + (i * hdr->znh_NField);

		/*
		 * For new samples, initialize all of the fields in the
		 * file to bad values, since not all of the file's fields
		 * may be in the DataChunk.
		 */
		if (no_init)
		{
			/* fall through */
		}
		else if ((wc == wc_Append) || (wc == wc_Insert))
		{
			for (fld = 0; fld < hdr->znh_NField; fld++)
				*(di + fld) = bad;
		}
		else
		{
			/*
			 * Grab the original data, but do not free the sample.
			 * We assume this space will be freed later.
			 */
			zn_GetBlock(tag, samp->znf_Offset, di, samp->znf_Size);
		}

		/*
		 * Finally insert our new data for this sample, and reset our
		 * sample pointers.
		 */
		for (fld = 0; fld < nfield; fld++)
		{
			if (index[fld] >= 0)
				di[ index[fld] ] = dc_GetScalar (dc, 
						dcsample + i, fids[fld]);
		}
		samp->znf_Offset = i * hdr->znh_NField * sizeof(float);
		samp->znf_Size = hdr->znh_NField * sizeof(float);
		++samp;
	}
	zn_WrLocations (tag, dc, fsample, dcsample, nsample);
	return ((void *)data);
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




/*ARGSUSED*/
static void *
zn_WrBoundaryBlock (tag, dc, fsample, dcsample, nsample, wc, index, size)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, nsample;
WriteCode wc;
int index;
unsigned int *size;
/*
 * Construct a boundary block out of 'nsample' samples.
 */
{
	Location *locs;
	int nloc;
	zn_Sample *samp;
	Location *block, *bptr;
	int i;

	/*
	 * Similar strategy to transparent blocks: loop through samples to
	 * find total size required, allocate the space, then actually
	 * fill in the block and the zn_Sample array.
	 */
	*size = 0;
	for (i = 0; i < nsample; ++i)
	{
		locs = dc_BndGet (dc, dcsample + i, &nloc);
		if (locs != NULL)
			*size += nloc * sizeof(Location);
	}
	if (*size == 0)
		return (NULL);
	block = (Location *) malloc (*size);
	bptr = block;
	for (i = 0; i < nsample; ++i)
	{
		samp = zn_FindSampStr (tag, fsample + i);
		locs = dc_BndGet (dc, dcsample + i, &nloc);
		if (locs != NULL)
		{
			memcpy (bptr, locs, nloc*sizeof(Location));
			(samp + index)->znf_Size = nloc*sizeof(Location);
			(samp + index)->znf_Offset = 
				(bptr - block) * sizeof(Location);
			bptr += nloc;
		}
		else
		{
			(samp + index)->znf_Size = 0;
			(samp + index)->znf_Offset = 0;
		}
	}
	return ((void *)block);
}





static int
zn_WrTrans (tag, dc, fsample, dcsample, samp, wc)
znTag *tag;
DataChunk *dc;
int fsample, dcsample;
zn_Sample *samp;
WriteCode wc;
/*
 * Write out a transparent sample.
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
	zn_WrLocations (tag, dc, fsample, dcsample, 1);
	return (1);
}



/*ARGSUSED*/
static void *
zn_WrTransBlock (tag, dc, fsample, dcsample, nsample, wc, size)
znTag *tag;
DataChunk *dc;
int fsample, dcsample, nsample;
WriteCode wc;
unsigned int *size;
/*
 * Write out a block of transparent samples to memory.
 */
{
	char *block, *bptr;
	DataPtr data;
	int len;
	int i;
	zn_Sample *samp;

	/*
	 * Loop through the data to find the lengths, and total the space
	 * required.
	 */
	*size = 0;
	for (i = 0; i < nsample; ++i)
	{
		data = dc_GetSample (dc, dcsample + i, &len);
		if (data != NULL)
			*size += len;
	}

	if (*size == 0)
		return (NULL);

	/*
	 * Allocate space for all of the samples
	 */
	block = (char *) malloc ( *size );

	/*
	 * Now pull out the data itself and copy it into the block.
	 */
	samp = zn_FindSampStr (tag, fsample);
	bptr = block;
	for (i = 0; i < nsample; ++i)
	{
		data = dc_GetSample (dc, dcsample + i, &len);
		if (data != NULL)
		{
			memcpy (bptr, data, len);
			samp->znf_Size = len;
			samp->znf_Offset = bptr - block;
			bptr += len;
		}
		else
		{
			samp->znf_Size = 0;
			samp->znf_Offset = 0;
		}
		++samp;
	}

	/*
	 * Put out the locations if necessary.
	 */
	zn_WrLocations (tag, dc, fsample, dcsample, nsample);
	return ((void *)block);
}




static void
zn_WrLocations (tag, dc, fsample, dcsample, nsample)
znTag *tag;
DataChunk *dc;
int fsample;
int dcsample;
int nsample;
/*
 * Put out locations if necessary.
 */
{
	int i;

	if (!ds_IsMobile (dc->dc_Platform))
	{
		return;
	}

	for (i = 0; i < nsample; ++i)
	{
		dc_GetLoc (dc, dcsample + i, tag->zt_Locs + fsample + i);
	}
	zn_PutBlock (tag, tag->zt_Hdr.znh_OffLoc + (fsample*sizeof(Location)),
		     tag->zt_Locs + fsample, nsample * sizeof (Location));
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




static void
zn_Close (ofp)
OpenFile *ofp;
/*
 * Close this file.
 */
{
	znTag *tag = TAGP (ofp);
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
}




static int
zn_Open (ofp, write)
OpenFile *ofp;
zbool write;
/*
 * Open an existing data file.
 */
{
	znTag *tag = TAGP (ofp);
	zn_Header *hdr = &tag->zt_Hdr;
	int nsa, field, magic;
	zbool grid;
	char *fname = ofp->of_df.df_fullname;
/*
 * Open the file.
 */
	memset (tag, 0, sizeof (znTag));
	if ((tag->zt_Fd = open (fname, write ? O_RDWR : O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Can't open %s (%d)", fname, errno);
		return (FALSE);
	}
/*
 * Check the magic number and grab the header
 */
	zn_GetBlock (tag, 0, &magic, sizeof (int));

	if (magic == ZN_MAGIC)
		zn_GetBlock (tag, 0, &tag->zt_Hdr, sizeof (zn_Header));
	else if (magic == ZN_OLDMAGIC)
	{
	/*
	 * Read the version 1 header and fill in the pieces added with 
	 * version 2
	 */
		zn_GetBlock (tag, 0, &tag->zt_Hdr, ZN_V1_HDRLEN); 
		hdr->znh_Version = 1;
		hdr->znh_AltUnits = ZAU_KMMSL; /* Always km MSL in Version 1 */
	}
	else
	{
		msg_ELog (EF_PROBLEM, "znf: Bad magic number %d in %s!",
			magic, fname);
		close (tag->zt_Fd);
		return (FALSE);
	}
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
 * Pull in the fields and convert them to fids, if this is an organization
 * which even has fields.
 */
	if (hdr->znh_NField)
	{
		tag->zt_Fields = 
			(zn_Field *) malloc(hdr->znh_NField*sizeof(zn_Field));

		zn_RdFieldInfo (tag);

		tag->zt_Fids = 
			(FieldId *) malloc (hdr->znh_NField*sizeof (FieldId));
		for (field = 0; field < hdr->znh_NField; field++)
			tag->zt_Fids[field] = 
				F_Lookup (tag->zt_Fields[field].zf_Name);
	}
	else
		tag->zt_Fields = NULL;
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
	if (grid || ds_IsMobile (ofp->of_df.df_pid))
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




static int
zn_Sync (ofp)
OpenFile *ofp;
/*
 * Synchronize this file with what is on disk.
 */
{
	znTag *tag = TAGP (ofp);
	zn_Header *hdr = &tag->zt_Hdr;
	int oldnsa = hdr->znh_NSampAlloc, oldnf = hdr->znh_NField, nsa;
	int hdrsize;
/*
 * Pull in the new header.  If the sizes of tables have changed, we need
 * to reallocate them.
 *
 * In retrospect, the use of "realloc" below is probably not the right
 * way to do this, since all of the arrays get read in completely 
 * anyway.  Probably we are doing a lot of copying for nothing.
 */
	hdrsize = sizeof (zn_Header);
	if (hdr->znh_Version == 1)
		hdrsize = ZN_V1_HDRLEN;
	
	zn_GetBlock (tag, 0, hdr, hdrsize);

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

		zn_RdFieldInfo (tag);

		for (fld = 0; fld < hdr->znh_NField; fld++)
			tag->zt_Fids[fld] = 
				F_Lookup (tag->zt_Fields[fld].zf_Name);
	}
	return (TRUE);
}





static DataChunk *
zn_Setup (ofp, fields, nfield, class)
OpenFile *ofp;
FieldId *fields;
int nfield;
DataClass class;
/*
 * Get set up to pull out some data.
 */
{
	znTag *tag = TAGP (ofp);
	zn_Header *hdr = &tag->zt_Hdr;
	DataChunk *dc;
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
	dc_SetLocAltUnits (dc, zn_CvtAltUnits (hdr->znh_AltUnits));
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
	   default:
		/* all other classes ok */
		break;
	}
#ifdef notyet
	/*
	 * For metdata, add field bad values
	 */
	if (dc_IsSubclass (DataClass, DCP_MetData))
	{
	}
#endif
/*
 * Done.
 */
	return (dc);
}




static int
zn_GetData (ofp, dc, tbegin, nsample, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int tbegin;
int nsample;
dsDetail *details;
int ndetail;
/*
 * Extract some data from the file.
 */
{
	znTag *tag = TAGP (ofp);
	int tend;
	int samp;
	int dcsamp = dc_GetNSample (dc);
/*
 * Met data stuff
 */
	tend = tbegin + nsample - 1;
	msg_ELog (EF_DEBUG, "znf GetData tbegin=%d to tend=%d",
		  tbegin, tend);
/*
 * For all but Boundary class, we create space in our datachunk here.  The
 * Boundary class does it itself because it knows the size of its boundaries.
 */
	if (dc->dc_Class != DCC_Boundary)
		dc_AddMoreSamples (dc, nsample, 0);
/*
 * Now things get organization-specific.
 */
	switch (dc->dc_Class)
	{
	   case DCC_Boundary:
	   	zn_ReadBoundary (tag, dc, tbegin, tend);
		break;

	   case DCC_RGrid:
	   	zn_ReadGrid (tag, dc, dcsamp, tbegin, tend, details, ndetail);
		break;

	   case DCC_IRGrid:
	   	zn_ReadIRG (tag, dc, dcsamp, tbegin, tend);
		break;

	   case DCC_Scalar:
	   	zn_ReadScalar (tag, dc, dcsamp, tbegin, tend);
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
	int nsamp = tend - tbegin + 1;
/*
 * Find the biggest boundary we need to pull in, and allocate sufficient
 * memory for that.
 */
	for (samp = tbegin; samp <= tend; samp++)
		if (tag->zt_Sample[samp].znf_Size > max)
			max = tag->zt_Sample[samp].znf_Size;
	locs = (Location *) malloc (max);
/*
 * Use the max boundary size to allocate space in the DataChunk
 */
	dc_AddMoreSamples (dc, nsamp, max);
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
zn_ReadScalar (tag, dc, dcsamp, tbegin, tend)
znTag *tag;
DataChunk *dc;
int dcsamp, tbegin, tend;
{
	int sample, *index, nfield, fld, offset = 0, plat;
	int boffset;
	int bsize;
	FieldId *fids;
	zn_Sample *zs;
	zn_Header *hdr = &tag->zt_Hdr;
	int nsamp = tend - tbegin + 1;
	float badval = CFG_DC_DEFAULT_BADVAL;
	float *data = (float *) malloc (nsamp * sizeof (float));
	float *block = NULL;
	float *dp, *bp;
	zbool fixed = (hdr->znh_Org == OrgFixedScalar);
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
 * For now, only scalar attempts this: check for a block,
 * and if we get one, read it and collect data from there instead of separate
 * zn_GetBlock calls for each one.
 */
	if (zn_DetectDataBlock (tag, dc, tbegin, nsamp, index, nfield,
				&boffset, &bsize))
	{
		/*
		 * Allocate and then read the block
		 */
		block = (float *) malloc (bsize);
		zn_GetBlock (tag, boffset, (void *)block, bsize);
		msg_ELog (EF_DEBUG, "znf readscalar: data block detected");
	}
	else
		msg_ELog (EF_DEBUG, "znf readscalar: no data block detected");
/*
 * Go through and get the entire set of data for the given field.
 */
	for (fld = 0; fld < nfield; fld++)
	{
	/*
	 * Now we get each sample.
	 */
		dp = data;
		bp = block;
	 	for (sample = tbegin; sample <= tend; sample++)
		{
			zs = zn_FindSampStr (tag, sample) + 
				(fixed ? 0 : index[fld]);
			if (index[fld] < 0 || zs->znf_Size <= 0)
				*dp = badval;
			else if (block && fixed)
			{
				*dp = bp[ index[fld] ];
				bp += hdr->znh_NField;
			}
			else if (block)
			{
				*dp = *(float *)((char *)block + 
					 zs->znf_Offset + offset - boffset);
			}
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
				  nsamp, fids[fld], data);
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
		dc_SetMLoc (dc, dcsamp, nsamp, tag->zt_Locs + tbegin);
/*
 * Clean up and we are done.
 */
	if (block)
		free (block);
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
	/* zn_Header *hdr = &tag->zt_Hdr; */
	DataPtr data = 0;
/*
 * Pull it in one sample at a time.
 */
	for (sample = tbegin; sample <= tend; sample++)
	{
		zs = tag->zt_Sample + sample;

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
		dc_SetMLoc (dc, dcsamp, tend-tbegin+1, tag->zt_Locs+tbegin);
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
zn_ReadIRG (tag, dc, dcsamp, tbegin, tend)
znTag *tag;
DataChunk *dc;
int dcsamp, tbegin, tend;
/*
 * Pull in some irregular grid data.
 */
{
	int sample, *index, nfield, fld;
	FieldId *fids;
	float badval = dc_GetBadval (dc);
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
zn_ReadGrid (tag, dc, dcsamp, tbegin, tend, details, ndetail)
znTag *tag;
DataChunk *dc;
int dcsamp, tbegin, tend, ndetail;
dsDetail *details;
/*
 * Pull some grids in.
 */
{
	int offset;
	int size, sample, *index, nfield, fld;
	FieldId *fids;
	RGrid rg;
	float badval = dc_GetBadval (dc);
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
			data = (float *)dc_GetMData (dc, dcsamp, fids[fld], 0);
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
int *offset;
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
	if (! ds_GetDetail (DD_FETCH_ALTITUDE, details, ndetail, &v))
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





static int
zn_GetObsSamples (ofp, times, locs, max)
OpenFile *ofp;
ZebTime *times;
Location *locs;
int max;
/*
 * Return sample info.
 */
{
	znTag *tag = TAGP (ofp);
	int i;
/*
 * Now we blast through and copy out as many as we can.
 */
	for (i = 0; (i < tag->zt_Hdr.znh_NSample) && (i < max); i++)
	{
		if (times)
			*times++ = tag->zt_Time[i];
		if (locs)
			*locs++ = tag->zt_Locs ? tag->zt_Locs[i] : 
				tag->zt_Hdr.znh_Loc;
	}
	return (i);
}





static int
zn_Fields (ofp, sample, nfield, fids)
OpenFile *ofp;
int sample;
int *nfield;
FieldId *fids;
/*
 * Return the fields available at this time.
 */
{
	znTag *tag = TAGP (ofp);
	zn_Header *hdr = &tag->zt_Hdr;
	int fld;
	int max = *nfield;
/*
 * Question: should this routine return (1) the list of all known fields, 
 * 	     or (2) just the fields which are present at the given time?
 *	     For now (1) is implemented.
 *
 * Copy out the fields, but only as many as asked for.
 */
	*nfield = 0;
	for (fld = 0; fld < hdr->znh_NField && (*nfield < max); fld++)
	{
		fids[*nfield] = tag->zt_Fids[fld];
		(*nfield)++;
	}
	return (*nfield);
}




static ZebTime *
zn_GetTimes (ofp, ntime)
OpenFile *ofp;
int *ntime;
{
	znTag *tag = TAGP (ofp);
	zn_Header *hdr = &tag->zt_Hdr;

	*ntime = hdr->znh_NSample;
	return (tag->zt_Time);
}






/*
 * Low-level space allocation.
 */
static int
zn_GetSpace (tag, size)
znTag *tag;
int size;
{
	zn_Free fb;
	int prev = -1, prevlast = -1, free, last;
	zn_Header *hdr = &tag->zt_Hdr;
/*
 * Scan through the free list to see if there is anything usable.  First try
 * for any existing blocks in which the space will fit, next try for a free
 * block at the end of the file, and finally just add space to the end of the
 * file.
 *
 * If the tag's Append flag is set, just return a block at the end of the
 * file.
 */
	last = -1;
	for (free = hdr->znh_Free; free > 0 && !tag->zt_Append;
	     free = fb.znf_Next)
	{
	/*
	 * Pull up this block and see if it is big enough or at the end of
	 * the file.
	 */
		zn_GetFreeBlock (tag, free, &fb);
		if (free + fb.znf_Size == hdr->znh_Len)
		{
			last = free;
			prevlast = prev;
		}
		if (fb.znf_Size >= size)
			break;
		prev = free;
	}
			
	if (free > 0 && !tag->zt_Append)	/* normal case */
	{
		return (zn_GetFromFree (tag, size, free, &fb, prev));
	}
	else if (last > 0 && !tag->zt_Append)	/* take free space at end */
	{
	/*
	 * Increase file length and remove free block at the end of the file
	 */
		zn_GetFreeBlock (tag, last, &fb);
		hdr->znh_Len += (size - fb.znf_Size);
		(void) zn_GetFromFree (tag, fb.znf_Size, last, &fb, prevlast);
		free = last;
	}
	else				/* append to end of file */
	{
	/*
	 * None of that worked, or we're forced to append, 
	 * so we'll just allocate the space at the end.
	 */
		free = hdr->znh_Len;
		hdr->znh_Len += size;
	}

	tag->zt_Sync |= SF_HEADER;
	return (free);
}





static int
zn_GetFromFree (tag, size, offset, fb, prev)
znTag *tag;
int size;
int offset, prev;
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
 * chunk at the end and tweak the size.  All we need is at least space
 * for a long to store a condensed free block node.  If that won't fit,
 * go on to handing out the entire block.
 */
	if ((size + sizeof (int)) <= fb->znf_Size)
	{
		int ret = offset + fb->znf_Size - size;
		fb->znf_Size -= size;
		zn_PutFreeBlock (tag, offset, fb);
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
		zn_PutFreeBlock (tag, prev, &prevfb);
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
int offset;
zn_Free *fb;
/*
 * Pull in a free space block.
 */
{
	int magic;

	zn_GetBlock (tag, offset, &magic, sizeof (int));
	if (magic != ZN_FREE_MAGIC)
	{
	/*
	 * This means, assuming things haven't been corrupted, that this
	 * is a short block containing only the offset of the next block.
	 */
		if (magic == 0)
			magic = ZN_FREE_MAGIC;
		fb->znf_FMagic = ZN_FREE_MAGIC;
		fb->znf_Next = magic;
		fb->znf_Size = sizeof(int);
	/*
	 * Look for adjacent short blocks and string them together
	 */
		while (fb->znf_Next == offset + fb->znf_Size)
		{
			zn_GetBlock(tag,fb->znf_Next,&magic,sizeof(int));
			if (magic == 0)
				magic = ZN_FREE_MAGIC;
			fb->znf_Next = magic;
			fb->znf_Size += sizeof(int);
		}
	}
	else
	/*
	 * Otherwise things are as they once always were
	 */
	{
		zn_GetBlock (tag, offset, fb, sizeof (zn_Free));
	}
}





static void
zn_PutFreeBlock (tag, offset, fb)
znTag *tag;
int offset;
zn_Free *fb;
/*
 * Write out a free space block, accounting for blocks which aren't large
 * enough to hold a full zn_Free structure.
 */
{
	if (fb->znf_Size < sizeof(zn_Free))
	{
		int next;
		int zero = 0;
		int size_left = fb->znf_Size;
	/*
	 * Store a series of offsets pointing to each other in 
	 * sequence, as many as will fit in the block.  Store ZN_FREE_MAGIC
	 * as zero to avoid confusion with other freed space.  This assumes
	 * we'll never try to free the file header.
	 */
		next = offset + sizeof(int);
		while (size_left >= 2*sizeof(int))
		{
			zn_PutBlock (tag, offset, (next == ZN_FREE_MAGIC) ?
				     (&zero) : (&next), sizeof(int));
			next += sizeof(int);
			offset += sizeof(int);
			size_left -= sizeof(int);
		}
		next = fb->znf_Next;
		zn_PutBlock (tag, offset,
			     (next == ZN_FREE_MAGIC) ? (&zero) : (&next), 
			     sizeof(int));
	}
	else
	{
	/*
	 * Otherwise we use the normal method and store the whole zn_Free
	 */
		zn_PutBlock (tag, offset, fb, sizeof (zn_Free));
	}
}




static void
zn_GetBlock (tag, offset, dest, len)
znTag *tag;
int offset;
void *dest;
int len;
/*
 * Pull in a chunk of data from this file.
 */
{
	int n;

	if (len == 0)
		return;
	lseek (tag->zt_Fd, offset, SEEK_SET);
	if ((n = read (tag->zt_Fd, dest, len)) < 0)
		msg_ELog (EF_PROBLEM, "ZN read: error %d", errno);
	else if (n < len)
	{

		msg_ELog (EF_PROBLEM, "ZN read: %s: got %d bytes out of %d",
			  "unexpected eof", n, len);
#ifdef DEBUGGER
		{
			char dbg[128];
			sprintf (dbg, "%s aline %d &", DEBUGGER, getpid());
			system (dbg);
			sleep (15);
		}
#endif
	}
}




static void
zn_PutBlock (tag, offset, data, len)
znTag *tag;
int offset;
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
int offset;
int len;
/*
 * Free up a chunk of space in the file.  Put new blocks at the END of the
 * free list, since more than likely the block is larger and occurs later
 * in the file than previously released blocks, due to the behavior of the
 * TOC arrays.
 */
{
	int before = 0, after = 0, free, last = 0;
	zn_Header *hdr = &tag->zt_Hdr;
	zn_Free fb, afterfb, pfb;
/*
 * Just in case, let someone know we're handling a case which might
 * indicate other problems elsewhere...
 */
	if (offset + len > hdr->znh_Len)
		msg_ELog (EF_PROBLEM, 
		  "znf WaRnInG: trying to free past end of file");
/*
 * It might happen that this block does not contain enough space for
 * a zn_Free node.  For now, we have to forget the extremely small ones.
 * Those larger than an int but smaller than zn_Free are specially handled
 * in zn_PutFreeBlock().
 */
	if (len < sizeof(int))
	{
		msg_ELog (EF_PROBLEM,
			  "znf: can't free block size %d, too small", len);
		return;
	}
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
		last = free;
	}
# ifdef DEBUG_FREE_LIST
	ui_printf ("Free %d at %ld, before %ld, after %ld\n", 
		   len, offset, before, after);
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
 * Otherwise we make a new free block and put it at the back.
 */
	else
	{
		if (hdr->znh_Free < 0)
			hdr->znh_Free = offset;
		else
		{
			fb.znf_Next = offset;
			zn_PutFreeBlock (tag, last, &fb);
		}
		fb.znf_FMagic = ZN_FREE_MAGIC;
		fb.znf_Size = len;
		fb.znf_Next = -1;
		/* fb.znf_Next = hdr->znh_Free; */
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
		else if (after == fb.znf_Next)
			fb.znf_Next = afterfb.znf_Next;
		else
		{
		 	for (free = hdr->znh_Free; free >= 0;
					free = pfb.znf_Next)
			{
			/*
			 * If looking for the node we currently have in memory,
			 * use it rather than read from the disk.  We already
			 * know it's not the node we're looking for.
			 */
				if (free == offset)
					pfb.znf_Next = fb.znf_Next;
				else
				{
					zn_GetFreeBlock (tag, free, &pfb);
					if (pfb.znf_Next == after)
						break;
				}
			}
			pfb.znf_Next = afterfb.znf_Next;
			zn_PutFreeBlock (tag, free, &pfb);
		}
		hdr->znh_NFree--;
	}
/*
 * Update the free node on the disk
 */
	zn_PutFreeBlock (tag, offset, &fb);
	hdr->znh_NFreeB += len;
	tag->zt_Sync |= SF_HEADER;
/*
 * Truncate this free space from the file if the free space is at the
 * end of the file, and only if it is worth our while.
 */
	if ((offset + fb.znf_Size >= hdr->znh_Len) && (fb.znf_Size >= 8192))
		zn_TruncateFreeBlock (tag, offset, &fb);
# ifdef DEBUG_FREE_LIST
	zn_DumpFL (tag, hdr);
# endif
}




static void
zn_TruncateFreeBlock (tag, offset, fb)
znTag *tag;
int offset;
zn_Free *fb;
{
	int prev;
	zn_Free prevfb;
	zn_Header *hdr = &tag->zt_Hdr;
/*
 * Release this free block node.
 */
	if (hdr->znh_Free == offset) 
		hdr->znh_Free = fb->znf_Next;
	else
	{
		for (prev = hdr->znh_Free; prev >= 0; prev = prevfb.znf_Next)
		{
			zn_GetFreeBlock (tag, prev, &prevfb);
			if (prevfb.znf_Next == offset)
			{
				prevfb.znf_Next = fb->znf_Next;
				zn_PutFreeBlock (tag, prev, &prevfb);
				break;
			}
		}
	}
/*
 * Adjust stats.  Try to account for (offset + fb.znf_Size > hdr->znh_Len)
 * by using the beginning of the free block, offset, as the point to
 * truncate to, instead of (hdr->znh_Len - fb.znf_Size).
 */
 	hdr->znh_NFree--;
	hdr->znh_NFreeB -= (hdr->znh_Len - offset);
/*
 * Truncate the file to the reduced length
 */
	msg_ELog (EF_DEBUG, "znf: truncating %li bytes from file to len %li",
		  (hdr->znh_Len - offset), offset);
	/* hdr->znh_Len -= fb->znf_Size; */
	hdr->znh_Len = offset;
	ftruncate (tag->zt_Fd, hdr->znh_Len);
	tag->zt_Sync |= SF_HEADER;
}




static int
zn_AltUnits (atype)
AltUnitType atype;
/*
 * Convert an AltUnitType to our internal integer altitude unit identifier.
 * Return ZAU_BAD if it's a type we don't understand.
 */
{
	switch (atype)
	{
	    case AU_kmMSL:
		return (ZAU_KMMSL);
	    case AU_mMSL:
		return (ZAU_MMSL);
	    case AU_mb:
		return (ZAU_MB);
	    default:
		return (ZAU_BAD);
	}
}

	    


static AltUnitType
zn_CvtAltUnits (zau)
int zau;
/*
 * Convert ZNF internal altitude to an AltUnitType.
 * Return CFG default if it's a type we don't understand.
 */
{
	switch (zau)
	{
	   case ZAU_KMMSL:
		return (AU_kmMSL);
	   case ZAU_MMSL:
		return (AU_mMSL);
	   case ZAU_MB:
		return (AU_mb);
	   default:
		return (CFG_ALTITUDE_UNITS);
	}
}

	    


static void
zn_RdFieldInfo (tag)
znTag	*tag;
/*
 * Read the zn_Field structures from the file
 */
{		
	int	f;
	zn_Header	*hdr = &tag->zt_Hdr;

	if (hdr->znh_Version == 1)
	{
	/*
	 * For version 1 files, we need to read each field structure
	 * individually, since the file only has the version 1 subset of
	 * each one.
	 */
		for (f = 0; f < hdr->znh_NField; f++)
		{
			zn_GetBlock (tag, 
				     hdr->znh_OffField + f*sizeof (zn_FieldV1),
				     tag->zt_Fields + f, sizeof (zn_FieldV1));
		/*
		 * Fill in the pieces not in version 1 files
		 */
			tag->zt_Fields[f].zf_AttrLen = 0;
			tag->zt_Fields[f].zf_OffAttr = -1;
		}
	}
	else
		zn_GetBlock (tag, hdr->znh_OffField, tag->zt_Fields,
			     hdr->znh_NField * sizeof (zn_Field));
}




static void
zn_WrFieldInfo (tag)
znTag	*tag;
/*
 * Write the field info to the file
 */
{
	int	f;
	zn_Header	*hdr = &tag->zt_Hdr;

	if (hdr->znh_Version == 1)
	{
	/*
	 * For version 1 files, we need to write each field structure
	 * individually, since we only write the version 1 subset of
	 * each one.
	 */
		for (f = 0; f < hdr->znh_NField; f++)
			zn_PutBlock (tag, 
				     hdr->znh_OffField + f*sizeof (zn_FieldV1),
				     tag->zt_Fields + f, sizeof (zn_FieldV1));
	}
	else
		zn_PutBlock (tag, hdr->znh_OffField, tag->zt_Fields,
			     hdr->znh_NField*sizeof (zn_Field));
}



