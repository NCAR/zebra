/*
 * Access to the FCC rasterfile format.
 */
# include <sys/types.h>
# include <errno.h>
# include <fcntl.h>
# include <unistd.h>
# include <memory.h>

# include "../include/defs.h"
# include "../include/message.h"
# include "dfa.h"
# include "DataStore.h"
# include "dsPrivate.h"
# include "dslib.h"
# include "RasterFile.h"





/*
 * This is the tag for an open raster file.
 */
typedef struct _RFTag
{
	RFHeader	rt_hdr;		/* The file header		*/
	RFToc		*rt_toc;	/* The table of contents	*/
	int		rt_fd;		/* The associated file descr	*/
} RFTag;


/*
 * Scratch buffer used for compression/decompression.
 */
static unsigned char *Sbuf = 0;
static int NSbuf = 0;



/*
 * Local routines.
 */
# ifdef __STDC__
	static void	drf_WSync (RFTag *);
	static int	drf_FindOffset (DataObject *, int);
	static int	drf_WriteImage (RFTag *, DataObject *, int, int);
	static int	drf_FldOffset (RFHeader *, char *);
	static int	drf_TimeIndex (const RFTag *, const time *);
	static void	drf_GetField (const RFTag *, const RFToc *,int,char *);
	static void	drf_ReadOldToc (RFTag *);
# else
	static void	drf_WSync ();
	static int	drf_FindOffset ();
	static int	drf_WriteImage ();
	static int	drf_FldOffset ();
	static int	drf_TimeIndex ();
	static void	drf_GetField ();
	static void	drf_ReadOldToc ();
# endif




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
drf_OpenFile (dp, write, rtag)
DataFile *dp;
bool write;
void **rtag;
/*
 * Open up a raster file.
 */
{
	RFTag *tag = ALLOC (RFTag);
/*
 * Open up the actual disk file.
 */
	if ((tag->rt_fd = open (dp->df_name, write ? O_RDWR : O_RDONLY)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening %s",errno,dp->df_name);
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
                        dp->df_name);
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
		msg_ELog (EF_PROBLEM, "Bad ID in file %s", dp->df_name);
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
	free (tag);
}





int
drf_QueryTime (file, begin, end, nsample)
char *file;
time *begin, *end;
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
	*begin = toc[0].rft_Time;
	*end = toc[*nsample - 1].rft_Time;
	free (toc);
	close (fd);
	return (TRUE);
}





void
drf_MakeFileName (dir, name, t, dest)
char *dir, *name, *dest;
time *t;
/*
 * Generate a file name.
 */
{
	sprintf (dest, "%s/%s.%06d.%04d.rf", dir, name, t->ds_yymmdd,
		t->ds_hhmmss/100);
}




int
drf_CreateFile (dp, dobj, rtag)
DataFile *dp;
DataObject *dobj;
void **rtag;
/*
 * Create a new raster file to contain this data object.
 */
{
	RFTag *tag = ALLOC (RFTag);
	ScaleInfo *scale = dobj->do_desc.d_img.ri_scale;
	int fld;
/*
 * We gotta create a file before doing much of anything.
 */
	if ((tag->rt_fd = open (dp->df_name, O_RDWR | O_CREAT, 0664)) < 0)
	{
		msg_ELog (EF_PROBLEM, "Error %d opening '%s'", errno,
				dp->df_name);
		free (tag);
		return (FALSE);
	}
/*
 * Start to fill in the header.
 */
	tag->rt_hdr.rf_Magic = RF_MAGIC;
	strcpy (tag->rt_hdr.rf_Platform, PTable[dobj->do_id].dp_name);
	tag->rt_hdr.rf_MaxSample = PTable[dobj->do_id].dp_maxsamp;
	tag->rt_hdr.rf_NSample = 0;
	tag->rt_hdr.rf_Flags = (dp->df_ftype == FTCmpRaster) ? RFF_COMPRESS :0;
/*
 * Fill in the fields info.
 */
	for (fld = 0; fld < dobj->do_nfield; fld++)
	{
		strcpy (tag->rt_hdr.rf_Fields[fld].rff_Name,
				dobj->do_fields[fld]);
		tag->rt_hdr.rf_Fields[fld].rff_Scale = scale[fld];
	}
	tag->rt_hdr.rf_NField = dobj->do_nfield;
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
drf_PutData (dfile, dobj, begin, end)
int dfile;
DataObject *dobj;
int begin, end;
/*
 * Write some data to the file.
 */
{
	RFTag *tag;
	RFHeader *hdr;
	int soffset;
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
		return (FALSE);
	}
/*
 * Make sure they are not trying to overwrite us.
 */
	if ((hdr->rf_NSample + end - begin + 1) > hdr->rf_MaxSample)
	{
		msg_ELog (EF_PROBLEM, "File %s overfull",
					DFTable[dfile].df_name);
		end = begin + (hdr->rf_MaxSample - hdr->rf_NSample) - 1;
	}
/*
 * Find the offset to the first sample that we will be writing.
 */
	soffset = drf_FindOffset (dobj, begin);
/*
 * Now actually output the data.
 */
	for (; begin <= end; begin++)
		if ((soffset = drf_WriteImage (tag, dobj, begin, soffset)) < 0)
			break;	/* bail */
	drf_WSync (tag);
}





static int
drf_FindOffset (dobj, begin)
DataObject *dobj;
int begin;
/*
 * Find the byte offset into the data fields for the beginning sample.
 */
{
	int offset = 0, sample;
	RGrid *rg = dobj->do_desc.d_img.ri_rg;

	for (sample = 0; sample < begin; sample++)
		offset += rg[sample].rg_nX * rg[sample].rg_nY;
	return (offset);
}





static int
drf_WriteImage (tag, dobj, begin, offset)
RFTag *tag;
DataObject *dobj;
int begin, offset;
/*
 * Write an image to the file.
 */
{
	RFHeader *hdr = &tag->rt_hdr;
	RFToc *toc;
	RGrid *rg = dobj->do_desc.d_img.ri_rg + begin;
	int nb, fld, dfield;
	unsigned char *data;
/*
 * Allocate a new TOC entry.
 */
	toc = tag->rt_toc + hdr->rf_NSample++;
	toc->rft_Time = dobj->do_times[begin];
	toc->rft_Rg = *rg;
	toc->rft_Origin = dobj->do_aloc[begin];
/*
 * Now write each field.
 */
	for (fld = 0; fld < dobj->do_nfield; fld++)
	{
		data = (unsigned char *) (dobj->do_data[fld] + offset);
	/*
	 * Compress the data if called for.
	 */
		nb = rg->rg_nX * rg->rg_nY;
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
	 	dfield = drf_FldOffset (hdr, dobj->do_fields[fld]);
		toc->rft_Offset[dfield] = lseek (tag->rt_fd, 0, SEEK_END);
		toc->rft_Size[dfield] = nb;
		if (write (tag->rt_fd, data, nb) < nb)
			msg_ELog (EF_PROBLEM, "Error %d writing image", errno);
	}
/*
 * Write out the attribute table if there is one.
 */
	if (dobj->do_attr)
	{
		toc->rft_AttrLen = strlen (dobj->do_attr) + 1;
		toc->rft_AttrOffset = lseek (tag->rt_fd, 0, SEEK_END);
		write (tag->rt_fd, dobj->do_attr, toc->rft_AttrLen);
	}
	return (offset + nb);
}






static int
drf_FldOffset (hdr, fname)
RFHeader *hdr;
char *fname;
/*
 * Find the offset in the file for this field.
 */
{
	int nf;
/*
 * Ugly.
 */
	for (nf = 0; nf < hdr->rf_NField; nf++)
		if (! strcmp (fname, hdr->rf_Fields[nf].rff_Name))
			return (nf);
	msg_ELog (EF_PROBLEM, "No RF slot for field %s", fname);
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




int
drf_Setup (gp)
GetList *gp;
/*
 * Get set up for this piece of a data grab.
 */
{
	RFTag *tag;
	int tbegin, tend, sample;
/*
 * Gotta open the file.
 */
	if (! dfa_OpenFile (gp->gl_dfindex, FALSE, (void *) &tag))
		return (FALSE);
/*
 * Find the TOC offsets.
 */
	tbegin = drf_TimeIndex (tag, &gp->gl_begin);
	tend = drf_TimeIndex (tag, &gp->gl_end);
/*
 * Now fill in the getlist and we're done.
 */
	gp->gl_npoint = 0;
	for (sample = tbegin; sample <= tend; sample++)
	{
		RGrid *rg = &tag->rt_toc[sample].rft_Rg;
		gp->gl_npoint += rg->rg_nX*rg->rg_nY;
	}
	gp->gl_nsample = tend - tbegin + 1;
	return (TRUE);
}




static int
drf_TimeIndex (tag, begin)
const RFTag * const tag;
const time * const begin;
/*
 * Find the index of this sample.
 */
{
	int offset;
	RFToc *toc = tag->rt_toc;
/*
 * Yet another dumb sequential search loop.
 */
	for (offset = tag->rt_hdr.rf_NSample - 1; offset >= 0; offset--)
		if (DLE (toc[offset].rft_Time, *begin))
			return (offset);
	return (0);
}





int 
drf_GetData (gp)
const GetList *gp;
/*
 * Retrieve the data called for here.
 */
{
	DataObject *dobj = gp->gl_dobj;
	RFToc *toc;
	RFTag *tag;
	RFHeader *hdr;
	RastImg *rip = &dobj->do_desc.d_img;
	int tbegin, tend, sample, fld, rfld;
	int fieldmap[MAXFIELD], offset = 0;
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
/*
 * Go through and map the fields.  Ugly.  If this proves to take a lot
 * of time, we may have to go to an stbl or something, but I don't think
 * it matters.
 */
	for (fld = 0; fld < dobj->do_nfield; fld++)
	{
	/*
	 * Search for this field in the list of those available.
	 */
		for (rfld = 0; rfld < hdr->rf_NField; rfld++)
			if (! strcmp (dobj->do_fields[fld],
					hdr->rf_Fields[rfld].rff_Name))
				break;
	/*
	 * If it's there, fill in the info; otherwise complain and mark
	 * it absent.
	 */
		if (rfld < hdr->rf_NField)
		{
			fieldmap[fld] = rfld;
			rip->ri_scale[fld] = hdr->rf_Fields[rfld].rff_Scale;
		}
		else
		{
			msg_ELog (EF_PROBLEM, "Unknown RF field: %s", 
					dobj->do_fields[fld]);
			fieldmap[fld] = -1;
		}
	}
/*
 * Now we plow through and pull in the data.
 */
	for (sample = tbegin; sample <= tend; sample++)
	{
		toc = tag->rt_toc + sample;
	/*
	 * Go through and pull in each field.
	 */
		for (fld = 0; fld < dobj->do_nfield; fld++)
		{
			char *dp = offset + (char *) gp->gl_data[fld];
			if (fieldmap[fld] >= 0)
				drf_GetField (tag, toc, fieldmap[fld], dp);
			else
				memset (dp, 0,
					toc->rft_Rg.rg_nX*toc->rft_Rg.rg_nY);
		}
	/*
	 * Fill in the location info.
	 */
		gp->gl_time[sample - tbegin] = toc->rft_Time;
		gp->gl_locs[sample - tbegin] = toc->rft_Origin;
		rip->ri_rg[gp->gl_sindex + sample - tbegin] = toc->rft_Rg;
	/*
	 * Bump our offset for the next field.
	 */
		offset += toc->rft_Rg.rg_nX*toc->rft_Rg.rg_nY;
	}
	return (TRUE);
}




static void
drf_GetField (tag, toc, field, dest)
const RFTag * const tag;
const RFToc * const toc;
const int field;
char * const dest;
/*
 * Pull in this field.
 */
{
/*	int nb = toc->rft_Rg.rg_nX * toc->rft_Rg.rg_nY; */
	int nb = toc->rft_Size[field];

/*	lseek (tag->rt_fd, toc->rft_Offset + field*nb, SEEK_SET); */
	lseek (tag->rt_fd, toc->rft_Offset[field], SEEK_SET);
/*
 * Without compression, we read straight into the destination array.
 */
	if ((tag->rt_hdr.rf_Flags & RFF_COMPRESS) == 0)
	{
		if (read (tag->rt_fd, dest, nb) != nb)
			msg_ELog (EF_PROBLEM, "Read error %d on rast file",
					errno);
	}
/*
 * Otherwise we read into Sbuf and decompress into our destination.
 */
	else
	{
		GetScratch (nb);
		if (read (tag->rt_fd, Sbuf, nb) != nb)
			msg_ELog (EF_PROBLEM, "Read error %d on rast file",
					errno);
		RL_Decode ((unsigned char *) dest, Sbuf, nb);
	}
}





int 
drf_DataTimes (dfindex, t, which, ntime, dest)
int dfindex, ntime;
time *t, *dest;
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
			*dest++ = tag->rt_toc[begin--].rft_Time;
	else if (which == DsAfter)
	{
		begin++;
		for (i = 0; begin < tag->rt_hdr.rf_NSample; i++)
			*dest++ = tag->rt_toc[begin++].rft_Time;
	}
	return (i);
}





int
drf_GetObsSamples (dfile, times, locs, max)
int dfile, max;
time *times;
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
		*times++ = toc[i].rft_Time;
		*locs++ = toc[i].rft_Origin;
	}
	return (i);
}




int
drf_GetFields (dfile, t, nfld, flist)
int dfile, *nfld;
time *t;
char **flist;
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
		flist[f] = hdr->rf_Fields[f].rff_Name;
	return (*nfld);
}





char *
drf_GetAttrs (dfile, t, len)
int dfile, *len;
time *t;
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
