/*
 * Access to the FCC rasterfile format.
 */
# include <sys/types.h>
# include <errno.h>
# include <fcntl.h>
# include <unistd.h>

# include "defs.h"
# include "message.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "dfa.h"
# include "RasterFile.h"
MAKE_RCSID ("$Id: DFA_Raster.c,v 3.8 1994-04-27 08:23:48 granger Exp $")





/*
 * This is the tag for an open raster file.
 */
typedef struct _RFTag
{
	RFHeader	rt_hdr;		/* The file header		*/
	RFToc		*rt_toc;	/* The table of contents	*/
	int		rt_fd;		/* The associated file descr	*/
	FieldId		*rt_fids;	/* Translated fields		*/
} RFTag;


/*
 * Scratch buffer used for compression/decompression.
 */
static unsigned char *Sbuf = 0;
static int NSbuf = 0;

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
static int	drf_TimeIndex FP ((const RFTag *, const ZebTime *));
static void	drf_GetField FP ((const RFTag *, const RFToc *,int));
static void	drf_ReadOldToc FP ((RFTag *));
static void	drf_ReadAttrs FP ((RFTag *, RFToc *, int, DataChunk *));
static void	drf_ClearToc FP ((RFHeader *, RFToc *));
static void	drf_FindSpace FP ((RFTag *, RFToc *, int, int, int));
static int	drf_ProcAttr FP ((char *, char *));
static void	drf_WriteAttrs FP ((RFTag *, RFToc *, DataChunk *, int, int));




static void
GetScratch (size)
int size;
/*
 * Make sure our scratch space is at least this big.
 */
{
	if (NSbuf >= size)
		return;
	if (NSbuf > 0)
		free (Sbuf);
/*
 * Minimum size imposed here.  Experience shows we reach this size anyway,
 * this way we avoid fragmenting the memory pool in the process.
 */
	if (size < 700000)
		size = 700000;
	Sbuf = (unsigned char *) malloc (size);
	NSbuf = size;
}






int
drf_OpenFile (fname, dp, write, rtag)
char *fname;
DataFile *dp;
bool write;
void **rtag;
/*
 * Open up a raster file.
 */
{
	RFTag *tag = ALLOC (RFTag);
	int fld;
/*
 * Open up the actual disk file.
 */
	if ((tag->rt_fd = open (fname, write ? O_RDWR : O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s",errno, fname);
		free (tag);
		return (FALSE);
	}
/*
 * Read in the header.
 */
	if (read (tag->rt_fd, &tag->rt_hdr, sizeof (RFHeader)) !=
			sizeof (RFHeader))
        {
                msg_ELog (EF_PROBLEM, "Error %d reading RF hdr on %s", errno,
                        fname);
                close (tag->rt_fd);
                free (tag);
                return (FALSE);
        }
/*
 * Check it.
 */
	if (tag->rt_hdr.rf_Magic != RF_MAGIC &&
			tag->rt_hdr.rf_Magic != RF_OLDMAGIC)
	{
		msg_ELog (EF_PROBLEM, "Bad ID in file %s", fname);
		close (tag->rt_fd);
		free (tag);
		return (FALSE);
	}
/*
 * Now get the table of contents.
 */
	tag->rt_toc = (RFToc *) malloc(tag->rt_hdr.rf_MaxSample*sizeof(RFToc));
	if (tag->rt_hdr.rf_Magic == RF_OLDMAGIC)
	{
		if (write)
			msg_ELog (EF_PROBLEM,
				"WRITE on old format raster file");
		drf_ReadOldToc (tag);
	}
	else
		read (tag->rt_fd, tag->rt_toc,
				tag->rt_hdr.rf_MaxSample*sizeof(RFToc));
/*
 * Make a translated set of fields.
 */
	tag->rt_fids = (FieldId *)
			malloc (tag->rt_hdr.rf_NField * sizeof (FieldId));
	for (fld = 0; fld < tag->rt_hdr.rf_NField; fld++)
		tag->rt_fids[fld] =
				F_Lookup (tag->rt_hdr.rf_Fields[fld].rff_Name);
/*
 * Good enough.  Return the tag info and we're done.
 */
	*rtag = (void *) tag;
	return (TRUE);
}





static void
drf_ReadOldToc (tag)
RFTag *tag;
/*
 * Read in and convert an old format TOC into a new one.
 */
{
	int i;
	RFHeader *hdr = &tag->rt_hdr;
	OldRFToc *old = (OldRFToc *)malloc(hdr->rf_MaxSample*sizeof(OldRFToc));
/*
 * Read in the old TOC.
 */
	read (tag->rt_fd, old, hdr->rf_MaxSample * sizeof (OldRFToc));
/*
 * Now pass through and process it.
 */
	for (i = 0; i < hdr->rf_MaxSample; i++)
	{
		*(OldRFToc *)(tag->rt_toc + i) = old[i];	/* XXX */
		tag->rt_toc[i].rft_AttrLen = tag->rt_toc[i].rft_AttrOffset = 0;
	}
}






void
drf_CloseFile (tag)
RFTag *tag;
/*
 * Close an open file.
 */
{
	close (tag->rt_fd);
	free (tag->rt_toc);
	free (tag->rt_fids);
	free (tag);
}





int
drf_QueryTime (file, begin, end, nsample)
char *file;
ZebTime *begin, *end;
int *nsample;
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
	if (read (fd, &hdr, sizeof (RFHeader)) != sizeof (RFHeader))
        {
                msg_ELog (EF_PROBLEM, "Error %d reading RF hdr on %s", errno,
                        file);
                close (fd);
                return (FALSE);
        }
/*
 * Now get the table of contents.
 */
	toc = (RFToc *) malloc (hdr.rf_MaxSample*sizeof(RFToc));
	read (fd, toc, hdr.rf_MaxSample*sizeof(RFToc));
/*
 * Return the info, clean up, and we're done.
 */
	*nsample = hdr.rf_NSample;
# ifdef notdef
	*begin = toc[0].rft_Time;
	*end = toc[*nsample - 1].rft_Time;
# endif
	TC_UIToZt (&toc[0].rft_Time, begin);
	TC_UIToZt (&toc[*nsample - 1].rft_Time, end);
	free (toc);
	close (fd);
	return (TRUE);
}





void
drf_MakeFileName (dir, name, t, dest)
char *dir, *name, *dest;
ZebTime *t;
/*
 * Generate a file name.
 */
{
	UItime ut;
	
	TC_ZtToUI (t, &ut);
	sprintf (dest, "%s.%06d.%06d.rf", name, ut.ds_yymmdd, ut.ds_hhmmss);
}




int
drf_CreateFile (fname, dp, dc, rtag)
char *fname;
DataFile *dp;
DataChunk *dc;
void **rtag;
/*
 * Create a new raster file to contain this data chunk.
 */
{
	RFTag *tag = ALLOC (RFTag);
	ScaleInfo scale;
	int fld, nfld;
	PlatformId id = dc->dc_Platform;
	FieldId *fids;
	ClientPlatform p;
/*
 * We gotta create a file before doing much of anything.
 */
	if ((tag->rt_fd = open (fname, O_RDWR | O_CREAT, 0664)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening '%s'", errno,
				fname);
		free (tag);
		return (FALSE);
	}
/*
 * Start to fill in the header.
 */
	ds_GetPlatStruct (id, &p, FALSE);
	tag->rt_hdr.rf_Magic = RF_MAGIC;
	strcpy (tag->rt_hdr.rf_Platform, p.cp_name);
	tag->rt_hdr.rf_MaxSample = p.cp_maxsamp;
	tag->rt_hdr.rf_NSample = 0;
	tag->rt_hdr.rf_Flags = (dp->df_ftype == FTCmpRaster) ? RFF_COMPRESS :0;
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
/*
 * Sync everything out to disk, and we are done.
 */
	drf_WSync (tag);
	*rtag = (void *) tag;
	return (TRUE);
}





static void
drf_WSync (tag)
RFTag *tag;
/*
 * Write out changes to the file header.
 */
{
	lseek (tag->rt_fd, 0, SEEK_SET);
	write (tag->rt_fd, &tag->rt_hdr, sizeof (RFHeader));
	write (tag->rt_fd, tag->rt_toc,tag->rt_hdr.rf_MaxSample*sizeof(RFToc));
}





int
drf_PutSample (dfile, dc, sample, wc)
int dfile, sample;
DataChunk *dc;
WriteCode wc;
/*
 * Write some data to the file.
 */
{
	RFTag *tag;
	RFToc *toc;
	RFHeader *hdr;
	int soffset, i, ret;
	ZebTime t;
/*
 * Open the file for write access.
 */
	if (! dfa_OpenFile (dfile, TRUE, (void *) &tag))
		return (FALSE);
	hdr = &tag->rt_hdr;
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
		msg_ELog (EF_PROBLEM, "File %d overfull", dfile);
		return (0);
	}
/*
 * Figure out what we're going to do with this sample.
 */
	switch (wc)
	{
	/*
	 * For the append case, we need a new TOC   entry and some
	 * new space.
	 */
	   case wc_Append:
		toc = tag->rt_toc + hdr->rf_NSample++;	
		drf_ClearToc (hdr, toc);
		break;
	/*
	 * For overwrites, find the sample to zap.
	 */
	   case wc_Overwrite:
		dc_GetTime (dc, sample, &t);
	   	soffset = drf_TimeIndex (tag, &t);
		toc = tag->rt_toc + soffset;
		break;
	/*
	 * For inserts, we need to open up a hole.
	 */
	   case wc_Insert:
	   	dc_GetTime (dc, sample, &t);
	   	soffset = drf_TimeIndex (tag, &t);
		for (i = hdr->rf_NSample - 1; i > soffset; i--)
			tag->rt_toc[i + 1] = tag->rt_toc[i];
		toc = tag->rt_toc + soffset;
		drf_ClearToc (hdr, toc);
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
		toc->rft_Rg = *rg;
		toc->rft_Origin = dobj->do_aloc[begin];
*/
/*
 * Now write each field.
 */
	fids = dc_GetFields (dc, &nfield);
	for (fld = 0; fld < nfield; fld++)
	{
	/*
	 * Get the data.
	 */
		data = dc_ImgGetImage (dc, sample, fids[fld], &toc->rft_Origin,
				&toc->rft_Rg, &nb, &scale);
	/*
	 * Compress the data if called for.
	 */
		if (hdr->rf_Flags & RFF_COMPRESS)
		{
			int junk;

			GetScratch (nb);
			RL_Encode (data, Sbuf, nb, NSbuf, &junk, &nb);
			data = Sbuf;
		}
	/*
	 * Find the offset for this field, move there, and write it.
	 */
	 	dfield = drf_FldOffset (tag, fids[fld]);
		drf_FindSpace (tag, toc, dfield, nb, reuse);
		if (write (tag->rt_fd, data, nb) < nb)
		{
			msg_ELog (EF_PROBLEM, "Error %d writing image", errno);
			return (0);
		}
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
 * Pull the attributes out of the dc.
 */
	AttrLen = 0;
	dc_ProcessAttrs (dc, 0, drf_ProcAttr);
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







static void
drf_FindSpace (tag, toc, fld, nb, reuse)
RFTag *tag;
RFToc *toc;
int fld, nb, reuse;
/*
 * Try to find a place to put this data.  Fills in the TOC entry and
 * positions the file at the beginning of the space.
 */
{
/*
 * If we are trying to reuse space, see if the existing allocation is
 * sufficient.  If so, just return it.
 */
	if (reuse && toc->rft_Size[fld] >= nb)
		lseek (tag->rt_fd, toc->rft_Offset[fld], SEEK_SET);
/*
 * Otherwise we (for now) just grab something at the end.  If we were
 * trying to reuse, this causes a fair amount of space to be wasted; 
 * later we need some sort of way to keep track of free space.
 */
	else
		toc->rft_Offset[fld] = lseek (tag->rt_fd, 0, SEEK_END);
	toc->rft_Size[fld] = nb;
}







static void
drf_ClearToc (hdr, toc)
RFHeader *hdr;
RFToc *toc;
/*
 * Initialize this TOC entry to clear.
 */
{
	memset (toc, 0, sizeof (RFToc));
}





static int
drf_FldOffset (tag, fid)
RFTag *tag;
FieldId fid;
/*
 * Find the offset in the file for this field.
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
	return (0);	/* XXX */
}





void
drf_Sync (tag)
RFTag *tag;
/*
 * Catch up with changes in this file.
 */
{
/*
 * Simply reread the header and table of contents.
 */
	lseek (tag->rt_fd, 0, SEEK_SET);
	read (tag->rt_fd, &tag->rt_hdr, sizeof (RFHeader));
	read (tag->rt_fd, tag->rt_toc, tag->rt_hdr.rf_NSample*sizeof (RFToc));
}




DataChunk *
drf_Setup (gp, fields, nfield, class)
GetList *gp;
FieldId *fields;
int nfield;
DataClass class;
/*
 * Initialize for a data snarf.
 */
{
	RFTag *tag;
	RFHeader *hdr;
	DataChunk *dc;
	ScaleInfo *sc;
	int ufield, ffield;
/*
 * Verify that the class makes sense,
 * and create a nice, empty data chunk for it.
 */
	if (class != DCC_Image)
	{
		msg_ELog (EF_PROBLEM, "Non-image fetch from raster fmt");
		return (NULL);
	}
	dc = dc_CreateDC (DCC_Image);
/*
 * Now we need to find the first file so we can pull out the scale info.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
	{
		dc_DestroyDC (dc);
		return (FALSE);
	}
	hdr = &tag->rt_hdr;
	sc = (ScaleInfo *) malloc (nfield * sizeof (ScaleInfo));
/*
 * Pass through each user field and field the equivalent file field.
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
drf_TimeIndex (tag, zbegin)
const RFTag * const tag;
const ZebTime * const zbegin;
/*
 * Find the index of this sample.
 */
{
	int offset;
	RFToc *toc = tag->rt_toc;
	UItime begin;
/*
 * Yet another dumb sequential search loop.
 */
	TC_ZtToUI (zbegin, &begin);
	for (offset = tag->rt_hdr.rf_NSample - 1; offset >= 0; offset--)
		if (DLE (toc[offset].rft_Time, begin))
			return (offset);
	return (-1);
}





int 
drf_GetData (dc, gp)
DataChunk *dc;
const GetList *gp;
/*
 * Retrieve the data called for here.
 */
{
	RFToc *toc;
	RFTag *tag;
	RFHeader *hdr;
	/* RastImg *rip = &dobj->do_desc.d_img; */
	int tbegin, tend, sample, fld, rfld;
	int fieldmap[MAXFIELD], offset = 0, nfield;
	int dcsamp = dc_GetNSample (dc);
	FieldId *fids = dc_GetFields (dc, &nfield);
	ZebTime t_hack;
/*
 * Open the file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (FALSE);
	hdr = &tag->rt_hdr;
/*
 * Sample offsets, again.
 */
	tbegin = drf_TimeIndex (tag, &gp->gl_begin);
	tend = drf_TimeIndex (tag, &gp->gl_end);
	dc_AddMoreSamples (dc, tend - tbegin + 1, 0);
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
	for (sample = tbegin; sample <= tend; sample++)
	{
		toc = tag->rt_toc + sample;
		TC_UIToZt (&toc->rft_Time, &t_hack);
	/*
	 * Go through and pull in each field.
	 */
		for (fld = 0; fld < nfield; fld++)
		{
		/*
		 * Get the data from the file.
		 */
			if (fieldmap[fld] >= 0)
				drf_GetField (tag, toc, fieldmap[fld]);
			else
				continue;
		/*
		 * Dump it into the data chunk.
		 */
		 	dc_ImgAddImage (dc, dcsamp, fids[fld],&toc->rft_Origin,
				&toc->rft_Rg, &t_hack, Sbuf, 0);
		}
	/*
	 * Deal with attributes.
	 */
	 	drf_ReadAttrs (tag, toc, sample, dc);
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
/*
 * Get some space and pull in the attributes.
 */
	adata = malloc (len);
	lseek (tag->rt_fd, toc->rft_AttrOffset, SEEK_SET);
	if (read (tag->rt_fd, adata, len) < len)
	{
		msg_ELog (EF_PROBLEM, "Error %d reading attributes", errno);
		return;
	}
/*
 * If we have attributes in the old format, convert them here.
 */
	if (! strncmp (adata, "radar,", 6))
	{
		dc_SetGlobalAttr (dc, "radar_space", "true");
		dc_SetGlobalAttr (dc, "scan_type", adata + 6);
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
		aname = avalue + strlen (avalue) + 1;
	}
}






static void
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
}





int 
drf_DataTimes (dfindex, t, which, ntime, dest)
int dfindex, ntime;
ZebTime *t, *dest;
TimeSpec which;
/*
 * Return a list of available times.
 */
{
	int begin, i;
	RFTag *tag;
/*
 * Open the file, as always.
 */
	if (! dfa_OpenFile (dfindex, FALSE, (void *) &tag))
		return (0);
/*
 * Now find the offset to the desired time, and copy out the info.
 */
	begin = drf_TimeIndex (tag, t);
	if (which == DsBefore)
		for (i = 0; begin >= 0 && i < ntime; i++)
			TC_UIToZt (&tag->rt_toc[begin--].rft_Time, dest++);
	else if (which == DsAfter)
	{
		begin++;
		for (i = 0; begin < tag->rt_hdr.rf_NSample && i < ntime; i++)
			TC_UIToZt (&tag->rt_toc[begin++].rft_Time, dest++);
	}
	return (i);
}





int
drf_GetObsSamples (dfile, times, locs, max)
int dfile, max;
ZebTime *times;
Location *locs;
/*
 * Return sample info.
 */
{
	RFTag *tag;
	RFToc *toc;
	int i;
/*
 * Open the file.
 */
	if (! dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);
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




int
drf_GetFields (dfile, t, nfld, flist)
int dfile, *nfld;
ZebTime *t;
FieldId *flist;
/*
 * Return a list of fields available from this file at this time.
 */
{
	RFTag *tag;
	RFHeader *hdr;
	int f;
/*
 * Open up the file first of all.
 */
	if (! dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);
	hdr = &tag->rt_hdr;
/*
 * Now copy.
 */
	*nfld = hdr->rf_NField;
	for (f = 0; f < hdr->rf_NField; f++)
		flist[f] = F_Lookup (hdr->rf_Fields[f].rff_Name);
	return (*nfld);
}





char *
drf_GetAttrs (dfile, t, len)
int dfile, *len;
ZebTime *t;
/*
 * Pull out the attributes, if any.
 */
{
	RFTag *tag;
	int tindex;
	char *ret;
/*
 * Open the file and find our time.
 */
	if (! dfa_OpenFile (dfile, FALSE, (void *) &tag))
		return (0);
	tindex = drf_TimeIndex (tag, t);
/*
 * Just pull out the attribute table if there is one.
 */
	if ((*len = tag->rt_toc[tindex].rft_AttrLen) <= 0)
		return (0);
	ret = malloc (*len);
	lseek (tag->rt_fd, tag->rt_toc[tindex].rft_AttrOffset, SEEK_SET);
	if (read (tag->rt_fd, ret, *len) < *len)
		msg_ELog (EF_PROBLEM, "Error %d reading attributes", errno);
	return (ret);
}
