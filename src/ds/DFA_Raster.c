/*
 * Access to the FCC rasterfile format.
 */
# include <stdio.h>
# include <sys/types.h>
# include <errno.h>
# include <fcntl.h>
# include <unistd.h>
# include <string.h>

# include <defs.h>
# include <message.h>
# include <byteorder.h>

# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "RasterFile.h"
# include "DataFormat.h"

RCSID ("$Id: DFA_Raster.c,v 3.28 2002-09-17 18:28:43 granger Exp $")

/*
 * This is the tag for an open raster file.
 */
typedef struct _RFTag
{
	RFHeader	rt_hdr;		/* The file header		*/
	RFToc		*rt_toc;	/* The table of contents	*/
	int		rt_fd;		/* The associated file descr	*/
	FieldId		*rt_fids;	/* Translated fields		*/
	ZebTime		*rt_times;	/* Handy stash of ZebTimes	*/
} RFTag;


typedef struct _RasterOpenFile
{
	OpenFile	open_file;
	RFTag		rf_tag;
} 
RasterOpenFile;

#define RFTAGP(ofp) (&((RasterOpenFile *)ofp)->rf_tag)

static CO_Compat COCTable [] =
{
	{ OrgImage,		DCC_Image	},
	{ OrgImage,		DCC_Location	},	/* Fetch only */
};


/* 
 * Raster file format methods
 */
P_OpenFile (drf_OpenFile);
P_CloseFile (drf_CloseFile);
P_QueryTime (drf_QueryTime);
P_PutSample (drf_PutSample);
P_CreateFile (drf_CreateFile);
P_SyncFile (drf_Sync);
P_GetData (drf_GetData);
P_GetObsSamples (drf_GetObsSamples);
P_GetFields (drf_GetFields);
P_Setup (drf_Setup);
P_GetAttrs (drf_GetAttrs);
P_GetTimes (drf_GetTimes);

static DataFormat rasterFormatRec =
{
    	"Raster",
	FTRaster,
	".rf",

	COCTable,       		/* org/class compatibility table*/
	N_COC(COCTable),
	sizeof(RasterOpenFile),
	FALSE,				/* read-only 			*/

	FORMAT_INIT,			/* dynamic data			*/

	drf_QueryTime,			/* Query times			*/
	fmt_MakeFileName,		/* Make file name		*/

	drf_Setup,			/* setup			*/
	drf_OpenFile,			/* Open				*/
	drf_CloseFile,			/* Close			*/
	drf_Sync,			/* Synchronize			*/
	drf_GetData,			/* Get the data			*/
	___,				/* Get altitude info		*/
	fmt_DataTimes,			/* Get data times		*/
	___,				/* Get forecast times		*/
	drf_CreateFile,			/* Create a new file		*/
	drf_PutSample,			/* Write to file		*/
	___,				/* Write block to a file	*/
	drf_GetObsSamples,		/* Get observation samples	*/
	drf_GetFields,			/* Get fields			*/
	drf_GetAttrs,			/* Get Attributes		*/
	drf_GetTimes,			/* Get times 			*/
	___                             /* Get the associated files     */
};

DataFormat *rasterFormat = (DataFormat *) &rasterFormatRec;

/*
 * Raster files in compressed mode.
 */
static DataFormat cmpRasterFormatRec =
{
    	"CmpRaster",
	FTCmpRaster,
	".rf",

	COCTable,       		/* org/class compatibility table*/
	N_COC(COCTable),
	sizeof(RasterOpenFile),
	FALSE,				/* read-only			*/

	FORMAT_INIT,			/* dynamic class data		*/

	drf_QueryTime,			/* Query times			*/
	fmt_MakeFileName,		/* Make file name		*/

	drf_Setup,			/* setup			*/
	drf_OpenFile,			/* Open				*/
	drf_CloseFile,			/* Close			*/
	drf_Sync,			/* Synchronize			*/
	drf_GetData,			/* Get the data			*/
	___,				/* Get altitude info		*/
	fmt_DataTimes, /* drf_DataTimes */ /* Get data times		*/
	___,				/* Get forecast times		*/
	drf_CreateFile,			/* Create a new file		*/
	drf_PutSample,			/* Write to file		*/
	___,				/* Write block to a file	*/
	drf_GetObsSamples,		/* Get observation samples	*/
	drf_GetFields,			/* Get fields			*/
	drf_GetAttrs,			/* Get Attributes		*/
	drf_GetTimes			/* Get array of times		*/
};

DataFormat *cmpRasterFormat = (DataFormat *) &cmpRasterFormatRec;

/*
 * Buffer used for attribute encoding/decoding.
 */
static char AttrBuf[2048];
static int AttrLen;


/*
 * Local routines.
 */
static void	drf_WSync FP ((RFTag *));
static int	drf_WriteImage FP ((RFTag *, DataChunk *, int, RFToc *, int));
static int	drf_FldOffset FP ((RFTag *, FieldId));
#ifdef notdef
static int	drf_GetField FP ((const RFTag * const, const RFToc *const,
				  const int));
static void	drf_ReadOldToc FP ((RFTag *));
# endif
static void	drf_ReadAttrs FP ((RFTag *, RFToc *, int, DataChunk *));
#ifdef notdef
static void	drf_ClearToc FP ((RFHeader *, RFToc *));
static void	drf_FindSpace FP ((RFTag *, RFToc *, int, int, int));
#endif
static int	drf_ProcAttr FP ((char *, char *));
static void	drf_WriteAttrs FP ((RFTag *, RFToc *, DataChunk *, int, int));
static void	drf_SyncTimes FP ((RFTag *tag));





static int
drf_OpenFile (ofp, write)
OpenFile *ofp;
zbool write;
/*
 * Open up a raster file.
 */
{
	RFTag *tag = RFTAGP(ofp);
	int fld;
	char *fname = ofp->of_df.df_fullname;
/*
 * Open up the actual disk file and read the header.
 */
	if ((tag->rt_fd = open (fname, write ? O_RDWR : O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, fname);
		return (FALSE);
	}
	if (drf_ReadHeader (tag->rt_fd, &tag->rt_hdr) < 0)
	{
		msg_ELog (EF_PROBLEM, "Read header failed on raster file '%s'",
			  fname);
                close (tag->rt_fd);
                return (FALSE);
	}
/*
 * Refuse to write to older files.
 */
	if (write && tag->rt_hdr.rf_Magic != RF_MAGIC)
	{
		msg_ELog (EF_PROBLEM, "%s: old raster files can only be read.",
			  fname);
		close (tag->rt_fd);
		return (FALSE);
	}
/*
 * Now get the table of contents.
 */
	tag->rt_toc = drf_ReadTOC (&tag->rt_hdr, tag->rt_fd);
/*
 * Make a translated set of fields.
 */
	tag->rt_fids = (FieldId *)
			malloc (tag->rt_hdr.rf_NField * sizeof (FieldId));
	for (fld = 0; fld < tag->rt_hdr.rf_NField; fld++)
		tag->rt_fids[fld] =
				F_Lookup (tag->rt_hdr.rf_Fields[fld].rff_Name);
/*
 * Setup our contiguous copy of ZebTimes
 */
	tag->rt_times = (ZebTime *) 
		malloc(tag->rt_hdr.rf_MaxSample*sizeof(ZebTime));
	drf_SyncTimes (tag);
/*
 * Good enough.
 */
	return (TRUE);
}





static void
drf_CloseFile (ofp)
OpenFile *ofp;
/*
 * Close an open file.
 */
{
	RFTag *tag = RFTAGP (ofp);

	close (tag->rt_fd);
	free (tag->rt_toc);
	free (tag->rt_fids);
	free (tag->rt_times);
}





static int
drf_QueryTime (const char *file, ZebraTime *begin, ZebraTime *end, 
	       int *nsample)
/*
 * Query the times available in this file.
 */
{
	int fd;
	RFHeader hdr;
	RFToc *toc;
/*
 * Open up the actual disk file.
 */
	if ((fd = open (file, O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s", errno, file);
		return (FALSE);
	}
/*
 * Read in the header.
 */
	if (drf_ReadHeader (fd, &hdr) < 0)
	{
                msg_ELog (EF_PROBLEM, "Error %d reading RF hdr on %s", errno,
			  file);
                close (fd);
                return (FALSE);
        }
/*
 * Now get the table of contents.
 */
	toc = drf_ReadTOC (&hdr, fd);
/*
 * Return the info, clean up, and we're done.
 */
	*nsample = hdr.rf_NSample;
	TC_UIToZt (&toc[0].rft_Time, begin);
	TC_UIToZt (&toc[*nsample - 1].rft_Time, end);
	free (toc);
	close (fd);
	return (TRUE);
}








static int
drf_CreateFile (ofp, dc, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
dsDetail *details;
int ndetail;
/*
 * Create a new raster file to contain this data chunk.
 */
{
	RFTag *tag = RFTAGP(ofp);
	ScaleInfo scale;
	int fld, nfld;
	PlatformId id = dc->dc_Platform;
	FieldId *fids;
	char *fname = ofp->of_df.df_fullname;
/*
 * We gotta create a file before doing much of anything.
 */
	if ((tag->rt_fd = open (fname, O_RDWR | O_CREAT, 0664)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening '%s'", errno,
				fname);
		return (FALSE);
	}
/*
 * Start to fill in the header.
 */
	tag->rt_hdr.rf_Magic = RF_MAGIC;
	strcpy (tag->rt_hdr.rf_Platform, ds_PlatformName (id));
	tag->rt_hdr.rf_MaxSample = ds_MaxSamples (id);
	tag->rt_hdr.rf_NSample = 0;
	tag->rt_hdr.rf_Flags = (ofp->of_df.df_core.dfc_ftype == FTCmpRaster) ? 
	    RFF_COMPRESS :0;
/*
 * Fill in the fields info.
 */
	fids = dc_GetFields (dc, &nfld);
	tag->rt_fids = (FieldId *) malloc (nfld * sizeof (FieldId));
	for (fld = 0; fld < nfld; fld++)
	{
		strcpy (tag->rt_hdr.rf_Fields[fld].rff_Name,
				F_GetName (fids[fld]));
		(void) dc_ImgGetImage (dc, 0, fids[fld], 0, 0, 0, &scale);
		tag->rt_hdr.rf_Fields[fld].rff_Scale = scale;
		tag->rt_fids[fld] = fids[fld];
	}
	tag->rt_hdr.rf_NField = nfld;
/*
 * Get a table of contents.
 */
	tag->rt_toc = (RFToc *) malloc(tag->rt_hdr.rf_MaxSample*sizeof(RFToc));
	tag->rt_times = (ZebTime *) 
		malloc(tag->rt_hdr.rf_MaxSample*sizeof(ZebTime));
/*
 * Sync everything out to disk, and we are done.
 */
	drf_WSync (tag);
	return (TRUE);
}





static void
drf_WSync (tag)
RFTag *tag;
/*
 * Write out changes to the file header.
 */
{
	drf_WriteHeader (tag->rt_fd, &tag->rt_hdr, tag->rt_toc);
#ifdef notdef
	lseek (tag->rt_fd, 0, SEEK_SET);
/*
 * Put out the header.
 */
	if (LittleEndian())
	    drf_SwapHeader (&tag->rt_hdr);

	write (tag->rt_fd, &tag->rt_hdr, sizeof (RFHeader));

	if (LittleEndian())
	    drf_SwapHeader (&tag->rt_hdr);
/*
 * Now the TOC.
 */
	if (LittleEndian())
	    drf_SwapTOC (tag->rt_toc, tag->rt_hdr.rf_MaxSample);

	write (tag->rt_fd, tag->rt_toc,tag->rt_hdr.rf_MaxSample*sizeof(RFToc));

	if (LittleEndian())
	    drf_SwapTOC (tag->rt_toc, tag->rt_hdr.rf_MaxSample);
#endif
/*
 * Update internal array of times with new table of contents
 */
	drf_SyncTimes (tag);
}




static void
drf_SyncTimes (tag)
RFTag *tag;
/*
 * Sync the ZebTimes in the tag with the TOC entries
 */
{
	int i;

	for (i = 0; i < tag->rt_hdr.rf_NSample; ++i)
		TC_UIToZt (&tag->rt_toc[i].rft_Time, tag->rt_times+i);
}




static int
drf_PutSample (ofp, dc, sample, wc, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int sample;
WriteCode wc;
dsDetail *details;
int ndetail;
/*
 * Write some data to the file.
 */
{
	RFTag *tag = RFTAGP(ofp);
	RFToc *toc = NULL;
	RFHeader *hdr = &tag->rt_hdr;
	int soffset, i, ret;
	ZebTime t;
/*
 * No writing old format files.
 */
	if (tag->rt_hdr.rf_Magic == RF_OLDMAGIC)
	{
		msg_ELog (EF_PROBLEM, "I won't write old file");
		return (0);
	}
/*
 * Make sure they are not trying to overwrite us.
 */
	if (hdr->rf_NSample >= hdr->rf_MaxSample && wc != wc_Overwrite)
	{
		msg_ELog (EF_PROBLEM, "drf_PutSample: File %s overfull", 
			  ofp->of_df.df_fullname);
		return (0);
	}
/*
 * Figure out what we're going to do with this sample.
 */
	switch (wc)
	{
	/*
	 * For the append case, we need a new TOC entry and some
	 * new space.  NewFile case just satisfies compiler checks.
	 */
	   case wc_Append:
	   case wc_NewFile:
		toc = tag->rt_toc + hdr->rf_NSample++;	
		drf_ClearToc (hdr, toc);
		break;
	/*
	 * For overwrites, find the sample to zap.
	 */
	   case wc_Overwrite:
		dc_GetTime (dc, sample, &t);
	   	soffset = dfa_TimeIndex (ofp, &t, 0);
		toc = tag->rt_toc + soffset;
		break;
	/*
	 * For inserts, we need to open up a hole.
	 */
	   case wc_Insert:
	   	dc_GetTime (dc, sample, &t);
	   	soffset = dfa_TimeIndex (ofp, &t, 0);
		for (i = hdr->rf_NSample - 1; i > soffset; i--)
			tag->rt_toc[i + 1] = tag->rt_toc[i];
		toc = tag->rt_toc + soffset;
		drf_ClearToc (hdr, toc);
		break;
	    case wc_SkipIt:
		return 1;
		break;
	}
/*
 * Now actually output the data.
 */
	ret = drf_WriteImage (tag, dc, sample, toc, wc == wc_Overwrite);
	drf_WSync (tag);
	return (ret);
}




static int
drf_WriteImage (tag, dc, sample, toc, reuse)
RFTag *tag;
DataChunk *dc;
int sample, reuse;
RFToc *toc;
/*
 * Write an image to the file.
 * Entry:
 *	TAG	is the file tag.
 *	DC	is the data chunk containing the image.
 *	SAMPLE	is the index of the sample to be written.
 *	TOC	is the TOC entry to describe this sample.
 *	REUSE	is TRUE iff we should attempt to reuse the space already
 *		allocated by this TOC entry.
 */
{
	RFHeader *hdr = &tag->rt_hdr;
	int nb, fld, dfield, nfield;
	unsigned char *data;
	FieldId *fids;
	ScaleInfo scale;
/*
 * Start to fill in the TOC entry.
 */
	if (! reuse)
	{
		ZebTime zt;
		dc_GetTime (dc, sample, &zt);
		TC_ZtToUI (&zt, &toc->rft_Time);
	}
/*
 * Now write each field.
 */
	fids = dc_GetFields (dc, &nfield);
	for (fld = 0; fld < nfield; fld++)
	{
	/*
	 * See if we know about this field.
	 */
	 	if ((dfield = drf_FldOffset (tag, fids[fld])) < 0)
		{
			continue;
		}
	/*
	 * Get the data.
	 */
		data = dc_ImgGetImage (dc, sample, fids[fld], &toc->rft_Origin,
				       &toc->rft_Rg, &nb, &scale);
		if (! drf_WriteData (tag->rt_fd, hdr, toc, 
				     dfield, data, nb, reuse))
			return 0;
	}
/*
 * Write out the attribute table if there is one.
 */
	drf_WriteAttrs (tag, toc, dc, sample, reuse);
	return (1);
}





static void
drf_WriteAttrs (tag, toc, dc, sample, reuse)
RFTag *tag;
RFToc *toc;
DataChunk *dc;
int sample, reuse;
/*
 * Write the attributes into a file.
 */
{
/*
 * Pull the attributes out of the dc.  XXX  Note that we're pulling global
 * attributes from a datachunk and inserting them as sample attributes!
 *
 * As a quick hack, look for sample attributes.  If not found, revert to
 * global.
 */
	AttrLen = 0;
	dc_ProcSampleAttrArrays (dc, sample, NULL, drf_ProcAttr, NULL);
	if (AttrLen == 0)
		dc_ProcessAttrs (dc, NULL, drf_ProcAttr);
	if (AttrLen == 0)
		return;
/*
 * Figure out where to put them.  If we are reusing, try to reuse the
 * old attribute space; otherwise make a new spot.
 */
	if (reuse && AttrLen <= toc->rft_AttrLen)
		lseek (tag->rt_fd, toc->rft_AttrOffset, SEEK_SET);
	else
		toc->rft_AttrOffset = lseek (tag->rt_fd, 0, SEEK_END);
/*
 * Just do the write and we are finished.
 */
	if (write (tag->rt_fd, AttrBuf, AttrLen) < AttrLen)
		msg_ELog (EF_PROBLEM, "Error writing raster attrs");
	toc->rft_AttrLen = AttrLen;
}




static int
drf_ProcAttr (name, value)
char *name, *value;
/*
 * Add this attribute to the list.
 */
{
	strcpy (AttrBuf + AttrLen, name);
	strcpy (AttrBuf + AttrLen + strlen (name) + 1, value);
	AttrLen += strlen (name) + strlen (value) + 2;
	return (0);
}





static int
drf_FldOffset (tag, fid)
RFTag *tag;
FieldId fid;
/*
 * Find the offset in the file for this field.  Return -1 if not found.
 */
{
	int nf;
	RFHeader *hdr = &tag->rt_hdr;
/*
 * See if we know about this field.
 */
	for (nf = 0; nf < hdr->rf_NField; nf++)
		if (tag->rt_fids[nf] == fid)
			return (nf);
/*
 * For the moment, gripe.  What we really need to do is to add the
 * field to the list of those we know.
 */
	msg_ELog (EF_PROBLEM, "No RF slot for field %d", fid);
	return (-1);
}





static int
drf_Sync (ofp)
OpenFile *ofp;
/*
 * Catch up with changes in this file.
 */
{
	RFTag *tag = RFTAGP(ofp);
/*
 * Simply reread the header and table of contents.
 */
	lseek (tag->rt_fd, 0, SEEK_SET);
	read (tag->rt_fd, &tag->rt_hdr, sizeof (RFHeader));

	if (LittleEndian())
	    drf_SwapHeader (&tag->rt_hdr);

	read (tag->rt_fd, tag->rt_toc, tag->rt_hdr.rf_NSample*sizeof (RFToc));

	if (LittleEndian())
	    drf_SwapTOC (tag->rt_toc, tag->rt_hdr.rf_NSample);
/*
 * Update internal copy of times with new TOC entries
 */
	drf_SyncTimes (tag);
	return (1);
}




static DataChunk *
drf_Setup (ofp, fields, nfield, class)
OpenFile *ofp;
FieldId *fields;
int nfield;
DataClass class;
/*
 * Initialize for a data snarf.
 */
{
	RFTag *tag = RFTAGP(ofp);
	RFHeader *hdr;
	DataChunk *dc;
	ScaleInfo *sc;
	int ufield, ffield;

	dc = dc_CreateDC (class);
	if (class == DCC_Location)
		return (dc);
	hdr = &tag->rt_hdr;
	sc = (ScaleInfo *) malloc (nfield * sizeof (ScaleInfo));
/*
 * Pass through each user field and find the equivalent file field.
 */
	for (ufield = 0; ufield < nfield; ufield++)
	{
	/*
	 * Find this field and grab the scale info.
	 */
	 	for (ffield = 0; ffield < hdr->rf_NField; ffield++)
			if (fields[ufield] == tag->rt_fids[ffield])
				break;
		if (ffield >= hdr->rf_NField)
			msg_ELog (EF_PROBLEM, "Field %s not in rfile",
					F_GetName (fields[ufield]));
		else
		 	sc[ufield] = hdr->rf_Fields[ffield].rff_Scale;
	}
/*
 * Now we init the data chunk and we're done.
 */
	dc_ImgSetup (dc, nfield, fields, sc);
	free (sc);
	return (dc);
}





static int 
drf_GetData (ofp, dc, begin, nsample, details, ndetail)
OpenFile *ofp;
DataChunk *dc;
int begin;
int nsample;
dsDetail *details;
int ndetail;
/*
 * Retrieve the data called for here.
 */
{
	RFTag *tag = RFTAGP(ofp);
	RFToc *toc;
	RFHeader *hdr;
	int sample, fld, rfld;
	int fieldmap[MAXFIELD], nfield;
	int dcsamp = dc_GetNSample (dc);
	FieldId *fids = NULL;
	ZebTime t_hack;

	hdr = &tag->rt_hdr;
	dc_AddMoreSamples (dc, nsample, 0);
	if (dc_IsSubClassOf (dc_ClassId(dc), DCC_MetData))
		fids = dc_GetFields (dc, &nfield);
	else
		nfield = 0;
/*
 * Go through and map the fields.  Ugly.  If this proves to take a lot
 * of time, we may have to go to an stbl or something, but I don't think
 * it matters.
 */
	for (fld = 0; fld < nfield; fld++)
	{
	/*
	 * Search for this field in the list of those available.
	 */
		for (rfld = 0; rfld < hdr->rf_NField; rfld++)
			if (tag->rt_fids[rfld] == fids[fld])
				break;
	/*
	 * If it's there, fill in the info; otherwise complain and mark
	 * it absent.
	 * (Don't complain any more, since drf_Setup will have already
	 *  done a sufficient amount of griping).
	 */
		if (rfld < hdr->rf_NField)
			fieldmap[fld] = rfld;
		else
			fieldmap[fld] = -1;
	}
/*
 * Now we plow through and pull in the data.
 */
	for (sample = begin; sample < begin + nsample; sample++)
	{
		toc = tag->rt_toc + sample;
		TC_UIToZt (&toc->rft_Time, &t_hack);
	/*
	 * Go through and pull in each field.
	 */
		for (fld = 0; fld < nfield; fld++)
		{
			unsigned char *img = 0;
		/*
		 * Get the data from the file and dump it into the data chunk.
		 */
			if (fieldmap[fld] >= 0 &&
			    (img = drf_GetField (tag->rt_fd, &tag->rt_hdr, 
						 toc, fieldmap[fld])))
			{
				dc_ImgAddImage (dc, dcsamp, fids[fld],
						&toc->rft_Origin, &toc->rft_Rg,
						&t_hack, img, 0);
			}
			else
			{
				dc_ImgAddMissing (dc, dcsamp, fids[fld],
						  &toc->rft_Origin,
						  &toc->rft_Rg, &t_hack, 0);
			}
		}
	/*
	 * If dealing with a 0-field case (i.e. DCC_Location), 
	 * just add the location and time for this sample.
	 */
		if (nfield == 0)
			dc_LocAdd (dc, &t_hack, &toc->rft_Origin);
	/*
	 * Deal with attributes.
	 */
	 	drf_ReadAttrs (tag, toc, dcsamp, dc);
		dcsamp++;
	}
	return (TRUE);
}





static void
drf_ReadAttrs (tag, toc, sample, dc)
RFTag *tag;
RFToc *toc;
int sample;
DataChunk *dc;
/*
 * Pull in the attributes for this sample, converting the format
 * if need be.
 */
{
	char *adata, *aname, *avalue;
	int len;
/*
 * If no attributes, don't bother.
 */
	if ((len = toc->rft_AttrLen) <= 0)
		return;
	if (len >= 512)
	{
	    msg_ELog (EF_INFO, "sample %d attributes look bogus: "
		      "len %i, offset %li", sample,
		      toc->rft_AttrLen, toc->rft_AttrOffset);
	    return;
	}
/*
 * Get some space and pull in the attributes.
 */
	adata = malloc (len+1);
	lseek (tag->rt_fd, toc->rft_AttrOffset, SEEK_SET);
	if (read (tag->rt_fd, adata, len) < len)
	{
		msg_ELog (EF_PROBLEM, "Error %d reading attributes", errno);
		return;
	}
	adata[len] = '\0';
/*
 * If we have attributes in the old format, convert them here.  This 
 * recognizes occurences of "radar,<scan>" and "newfile,radar,<scan>".
 */
	if ((aname = strstr (adata, "radar,")))
	{
		dc_SetGlobalAttr (dc, "radar_space", "true");
		dc_SetGlobalAttr (dc, "scan_type", aname + 6);
		free (adata);
		return;
	}
/*
 * Go through and store all the attributes.
 */
	for (aname = adata; aname < adata + len;)
	{
		avalue = aname + strlen (aname) + 1;
		dc_SetGlobalAttr (dc, aname, avalue);
		dc_SetSampleAttr (dc, sample, aname, avalue);
		aname = avalue + strlen (avalue) + 1;
	}
	free (adata);
}





#ifdef notdef
static int
drf_GetField (tag, toc, field)
const RFTag * const tag;
const RFToc * const toc;
const int field;
/*
 * Pull in this field.
 */
{
	int fnb = toc->rft_Rg.rg_nX * toc->rft_Rg.rg_nY;
	int nb = toc->rft_Size[field];

	if (nb == 0 || toc->rft_Offset[field] == 0)
		return (0);
/*
 * Move to the right place in the file.
 */
	lseek (tag->rt_fd, toc->rft_Offset[field], SEEK_SET);
/*
 * Without compression, we read straight into the destination array.
 */
	if ((tag->rt_hdr.rf_Flags & RFF_COMPRESS) == 0)
	{
		GetScratch (nb);
		if (read (tag->rt_fd, Sbuf, nb) != nb)
			msg_ELog (EF_PROBLEM, "Read error %d on rast file",
					errno);
	}
/*
 * Otherwise we read into Sbuf and decompress into our destination.
 * Now that we always go into sbuf, things are a bit different.  To try
 * minimize memory use, we read into the far end of Sbuf, and decompress
 * in place.
 */
	else
	{
		int rpos = fnb - nb + 100;
		GetScratch (fnb + 100);
		if (read (tag->rt_fd, Sbuf + rpos, nb) != nb)
			msg_ELog (EF_PROBLEM, "Read error %d on rast file",
					errno);
		RL_Decode (Sbuf, Sbuf + rpos, nb);
	}
	return (1);
}
#endif




static int
drf_GetObsSamples (ofp, times, locs, max)
OpenFile *ofp;
ZebTime *times;
Location *locs;
int max;
/*
 * Return sample info.
 */
{
	RFTag *tag = RFTAGP(ofp);
	RFToc *toc;
	int i;
/*
 * Now we blast through and copy out as many as we can.
 */
	toc = tag->rt_toc;
	for (i = 0; i < tag->rt_hdr.rf_NSample && i < max; i++)
	{
		/* *times++ = toc[i].rft_Time; */
		TC_UIToZt (&toc[i].rft_Time, times++);
		*locs++ = toc[i].rft_Origin;
	}
	return (i);
}




static int
drf_GetFields (ofp, sample, nfld, flist)
OpenFile *ofp;
int sample;
int *nfld;
FieldId *flist;
/*
 * Return a list of fields available from this file at this time.
 */
{
	RFTag *tag = RFTAGP(ofp);
	RFHeader *hdr = &tag->rt_hdr;
	int f;
/*
 * Now copy.
 */
	for (f = 0; f < hdr->rf_NField && f < *nfld; f++)
		flist[f] = F_Lookup (hdr->rf_Fields[f].rff_Name);
	*nfld = f;
	return (*nfld);
}




#ifndef NO_GETATTR
static char *
drf_GetAttrs (of, sample, len)
OpenFile *of;
int sample;
int *len;
/*
 * Pull out the attributes, if any.
 */
{
	RFTag *tag = RFTAGP(of);
	char *ret;
/*
 * Just pull out the attribute table if there is one.
 */
	if ((*len = tag->rt_toc[sample].rft_AttrLen) <= 0)
		return (0);
	ret = malloc (*len);
	lseek (tag->rt_fd, tag->rt_toc[sample].rft_AttrOffset, SEEK_SET);
	if (read (tag->rt_fd, ret, *len) < *len)
		msg_ELog (EF_PROBLEM, "Error %d reading attributes", errno);
	return (ret);
}
#endif


#ifdef NO_GETATTR
static void
drf_GetAttrs (tag, ix, dc, sample)
RFTag *tag;
int ix;		/* sample index into file */
DataChunk *dc;
int sample;	/* sample being added in datachunk */
/*
 * Pull out the attributes for this sample and add them to the datachunk.
 */
{
	char *ret, **pattr;
	int len;

	if ((len = tag->rt_toc[sample].rft_AttrLen) <= 0)
		return;
	ret = (char *) malloc (len);
	pattr = (char **) malloc (len);
	lseek (tag->rt_fd, tag->rt_toc[ix].rft_AttrOffset, SEEK_SET);
	if (read (tag->rt_fd, ret, len) < len)
	{
		msg_ELog (EF_PROBLEM, "Error %d reading attributes", errno);
	}
	else
	{
		/* Add attributes one by one */
		len = CommaParse (ret, pattr);
		for (i = 0; i < len; i++)
			dc_SetSampleAttr (dc, sample, pattr[i], pattr[i]);
	}
	free (pattr);
	free (ret);
}
#endif



static ZebTime *
drf_GetTimes (ofp, ntime)
OpenFile *ofp;
int *ntime;
{
	RFTag *tag = RFTAGP(ofp);

	*ntime = tag->rt_hdr.rf_NSample;
	return (tag->rt_times);
}


