/*
 * Lower level access to the rasterfile format which can be shared between
 * DFA_Raster.c and rfdump.c.
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

RCSID ("$Id: Raster.c,v 3.4 2000-11-20 18:07:29 granger Exp $")


/*
 * Private prototypes.
 */
static void	drf_FixOldHeader FP ((RFHeader *));
static void	drf_SwapRG FP ((RGrid *));
static void	drf_SwapLoc FP ((Location *));
static void	drf_SwapLArray FP ((long *, int));



/*
 * Scratch buffer used for compression/decompression.
 */
static unsigned char *Sbuf = 0;
static int NSbuf = 0;


unsigned char *
drf_GetScratch (int size, int *nalloc)
/*
 * Make sure our scratch space is at least this big, returning a pointer
 * to the space.  If passed zero, return the buffer unchanged.  If alloc
 * non-zero, return the number of bytes allocated in the buffer.
 */
{
	if (NSbuf < size)
	{
		if (NSbuf > 0)
			free (Sbuf);
		/* 
		 * Minimum size imposed here.  Experience shows we reach
		 * this size anyway, this way we avoid fragmenting the
		 * memory pool in the process.
		 */
		if (size < 700000)
			size = 700000;
		Sbuf = (unsigned char *) malloc (size);
		NSbuf = size;
	}
	if (nalloc)
		*nalloc = NSbuf;
	return Sbuf;
}



void
drf_ClearToc (RFHeader *hdr, RFToc *toc)
/*
 * Initialize this TOC entry to clear.
 */
{
	memset (toc, 0, sizeof (RFToc));
	toc->rft_Rg.rg_nX = toc->rft_Rg.rg_nY = toc->rft_Rg.rg_nZ = 1;
}



void
drf_FindSpace (int fd, RFToc *toc, int fld, int nb, int reuse)
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
		lseek (fd, toc->rft_Offset[fld], SEEK_SET);
/*
 * Otherwise we (for now) just grab something at the end.  If we were
 * trying to reuse, this causes a fair amount of space to be wasted; 
 * later we need some sort of way to keep track of free space.
 */
	else
		toc->rft_Offset[fld] = lseek (fd, 0, SEEK_END);
	toc->rft_Size[fld] = nb;
}




int
drf_ReadHeader (int fd, RFHeader *hdr)
/*
 * Read in the header.  Return less than zero if there's an error, zero
 * otherwise.  Takes care of fixing old headers and swapping the header
 * on little-endian machines.  Upon successful exit the file is positioned
 * just past the end of the header, whether new or old.
 */
{
	if (read (fd, hdr, sizeof (RFHeader)) != sizeof (RFHeader))
        {
                msg_ELog (EF_PROBLEM, "Error %d reading RF hdr", errno);
                return (-1);
        }
/*
 * Swap if need be.
 */
	if (LittleEndian())
		drf_SwapHeader (hdr);
/*
 * Look for really ancient files, which should not ought to exist any more.
 */
	if (hdr->rf_Magic == RF_ANCIENTMAGIC)
	{
		msg_ELog (EF_PROBLEM, "Ancient raster file support removed");
		msg_ELog (EF_PROBLEM, "talk to rdp-support@atd.ucar.edu");
		return (-1);
	}
/*
 * If it's an old header, fix up the fields.  Also reposition within the
 * file to where the TOC lives.
 */
	if (hdr->rf_Magic == RF_OLDMAGIC)
	{
		drf_FixOldHeader (hdr);
		if (lseek (fd, sizeof (OldRFHeader), SEEK_SET) < 0)
		{
			msg_ELog (EF_PROBLEM, "Old RF toc seek err %d", errno);
			return (-1);
		}
	}
/*
 * Otherwise it should identify itself as a current header.
 */
	else if (hdr->rf_Magic != RF_MAGIC)
	{
		msg_ELog (EF_PROBLEM, "Bad or corrupt magic number");
		return (-1);
	}
	return (0);
}




unsigned char *
drf_GetField (int fd, RFHeader *hdr, const RFToc *const toc, const int field)
/*
 * Pull in this field and return a pointer to the image, else 0.
 */
{
	int fnb = toc->rft_Rg.rg_nX * toc->rft_Rg.rg_nY;
	int nb = toc->rft_Size[field];

	if (nb == 0 || toc->rft_Offset[field] == 0)
		return (0);
/*
 * Move to the right place in the file.
 */
	lseek (fd, toc->rft_Offset[field], SEEK_SET);
/*
 * Without compression, we read straight into the destination array.
 */
	if ((hdr->rf_Flags & RFF_COMPRESS) == 0)
	{
		if (read (fd, drf_GetScratch (nb, 0), nb) != nb)
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
		drf_GetScratch (fnb + 100, 0);
		if (read (fd, Sbuf + rpos, nb) != nb)
		{
			msg_ELog (EF_PROBLEM, "Read error %d on rast file",
				  errno);
		}
		RL_Decode (Sbuf, Sbuf + rpos, nb);
	}
	return Sbuf;
}



unsigned char *
drf_Compress (unsigned char *data, int nb, int *newnb)
{
	int junk;
	int nsbuf;
	unsigned char *sbuf = drf_GetScratch (nb, &nsbuf);
	RL_Encode (data, sbuf, nb, nsbuf, &junk, newnb);
	return sbuf;
}



void
drf_WriteHeader (int fd, RFHeader *hdr, RFToc *toc)
{
    lseek (fd, 0, SEEK_SET);
/*
 * Put out the header.
 */
    if (LittleEndian())
	drf_SwapHeader (hdr);

    write (fd, hdr, sizeof (RFHeader));

    if (LittleEndian())
	drf_SwapHeader (hdr);
/*
 * Now the TOC.
 */
    if (LittleEndian())
	drf_SwapTOC (toc, hdr->rf_MaxSample);

    write (fd, toc, hdr->rf_MaxSample*sizeof(RFToc));

    if (LittleEndian())
	drf_SwapTOC (toc, hdr->rf_MaxSample);
}



int
drf_WriteData (int fd, RFHeader *hdr, RFToc *toc, int dfield, 
	       unsigned char *data, int nb, int reuse)
/*
 * Return zero on error.
 */
{
	/*
	 * Compress the data if called for.
	 */
	if (data && (hdr->rf_Flags & RFF_COMPRESS))
	{
		data = drf_Compress (data, nb, &nb);
	}
	/* 
	 * If no data, mark the slot empty unless it already holds an image.
	 */
	if (! data)
	{
		if (! reuse)
		{
			toc->rft_Size[dfield] = 0;
			toc->rft_Offset[dfield] = 0;
		}
	}
	else
	{
		drf_FindSpace (fd, toc, dfield, nb, reuse);
		if (write (fd, data, nb) < nb)
		{
			msg_ELog (EF_PROBLEM, "Error %d writing image", errno);
			return (0);
		}
	}
	return 1;
}



static void
drf_FixOldHeader (hdr)
RFHeader *hdr;
/*
 * Fix up an old version header.
 */
{
	OldRFHeader *old = (OldRFHeader *) hdr;
/*
 * All we really have to do is to fetch the flags from the right place.
 */
	msg_ELog (EF_INFO, "Fixing old raster file header");
	hdr->rf_Flags = old->rf_Flags;
	if (LittleEndian())
	    swap4 (&hdr->rf_Flags);
}



void
drf_SwapHeader (hdr)
RFHeader *hdr;
/*
 * Swap a header to/from native order.
 */
{
	int field, nf = hdr->rf_NField;
	swap4 (&hdr->rf_Magic);
	swap4 (&hdr->rf_MaxSample);
	swap4 (&hdr->rf_NSample);
	swap4 (&hdr->rf_NField);
	swap4 (&hdr->rf_Flags);
/*
 * Would you believe this loop used to go to rf_NField, without regard to
 * whether said quantity was swapped or not?  It worked for years.  Until
 * it broke, of course.  At this point we have no idea of which version of
 * rf_NField is right, so we take the most reasonable one.
 */
	if (nf < 0 || nf > MaxRFField)
		nf = hdr->rf_NField;	/* Assume we want swapped. */
	for (field = 0; field < nf; field++)
	{
		swap4 (&hdr->rf_Fields[field].rff_Scale.s_Scale);
		swap4 (&hdr->rf_Fields[field].rff_Scale.s_Offset);
	}
}





RFToc *
drf_ReadTOC (hdr, fd)
RFHeader *hdr;
int fd;
/*
 * Read in the table of contents.
 */
{
	RFToc *toc;
	int t;
/*
 * Allocate memory for the TOC.
 */
	toc = (RFToc *) malloc (hdr->rf_MaxSample*sizeof(RFToc));
/*
 * Now read it in.  How we do that depends on the age of this file.
 */
	if (hdr->rf_Magic == RF_OLDMAGIC)
	{
		int i, fld;
		OldRFToc *old = (OldRFToc *)
			malloc (hdr->rf_MaxSample*sizeof (OldRFToc));
		if (read (fd, old, hdr->rf_MaxSample*sizeof (OldRFToc)) <
				hdr->rf_MaxSample*sizeof (OldRFToc))
			msg_ELog (EF_PROBLEM, "Old raster TOC read error");
		for (i = 0; i < hdr->rf_NSample; i++)
		{
		        int attrlen = old[i].rft_AttrLen;
			long attroffset = old[i].rft_AttrOffset;
			toc[i].rft_Time = old[i].rft_Time;
			toc[i].rft_Rg = old[i].rft_Rg;
			toc[i].rft_Origin = old[i].rft_Origin;
			/* 
			 * Make sure the attributes make sense.  There
			 * are CAPE raster files out there with an
			 * intermediate format between AncientMagic and
			 * OldMagic which does not seem to have attributes
			 * and for which the attr fields hold garbage.
			 */
			if (LittleEndian())
			{
			    swap4 (&attrlen);
			    swap4 (&attroffset);
			}
			if (attrlen < 512) 
			{
			    toc[i].rft_AttrLen = old[i].rft_AttrLen;
			    toc[i].rft_AttrOffset = old[i].rft_AttrOffset;
			}
			else
			{
			    long attroffset = old[i].rft_AttrOffset;
			    msg_ELog (EF_INFO, 
				      "sample %d attributes look bogus: "
				      "len %i, offset %li", 
				      i, attrlen, attroffset);
			    toc[i].rft_AttrLen = 0;
			    toc[i].rft_AttrOffset = 0;
			}
			for (fld = 0; fld < hdr->rf_NField; fld++)
			{
				toc[i].rft_Offset[fld] =old[i].rft_Offset[fld];
				toc[i].rft_Size[fld] = old[i].rft_Size[fld];
			}
		}
		free (old);
	}
/*
 * How nice, this is a new file.  All we have to do is read it.
 */
	else
		read (fd, toc, hdr->rf_MaxSample*sizeof(RFToc));
/*
 * Do we need to swap this thing too??
 */
	if (LittleEndian())
	    drf_SwapTOC (toc, hdr->rf_MaxSample);
/*
 * Once things are swapped, rationalize the dates.
 */
	for (t = 0; t < hdr->rf_MaxSample; t++)
		TC_y2k (&toc[t].rft_Time);
	
	return (toc);
}




/*
 * The following is swapping code needed for little-endian machines only.
 */

void
drf_SwapTOC (toc, ntoc)
RFToc *toc;
int ntoc;
/*
 * Fix up a table of contents.
 */
{
	for (; ntoc > 0; ntoc--)
	{
		swap4 (&toc->rft_Time.ds_yymmdd);
		swap4 (&toc->rft_Time.ds_hhmmss);
		drf_SwapLArray (toc->rft_Offset, MaxRFField);
		drf_SwapLArray (toc->rft_Size, MaxRFField);
		drf_SwapRG (&toc->rft_Rg);
		drf_SwapLoc (&toc->rft_Origin);
		swap4 (&toc->rft_AttrLen);
		swap4 (&toc->rft_AttrOffset);
		toc++;
	}
}



static void
drf_SwapLArray (array, n)
long *array;
int n;
/*
 * Swap an array of longs.
 */
{
	for (; n > 0; n--)
		swap4 (array++);
}




static void
drf_SwapRG (rg)
RGrid *rg;
/*
 * Fix up an rgrid structure.
 */
{
	swap4 (&rg->rg_Xspacing);
	swap4 (&rg->rg_Yspacing);
	swap4 (&rg->rg_Zspacing);
	swap4 (&rg->rg_nX);
	swap4 (&rg->rg_nY);
	swap4 (&rg->rg_nZ);
}



static void
drf_SwapLoc (loc)
Location *loc;
{
	swap4 (&loc->l_lat);
	swap4 (&loc->l_lon);
	swap4 (&loc->l_alt);
}




# ifdef notdef		/* Ancient header support.  Not sure it ever worked */

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

# endif



