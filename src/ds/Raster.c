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

RCSID ("$Id: Raster.c,v 3.1 1997-06-19 20:19:29 granger Exp $")


/*
 * Private prototypes.
 */
static void	drf_FixOldHeader FP ((RFHeader *));
static void	drf_SwapRG FP ((RGrid *));
static void	drf_SwapLoc FP ((Location *));
static void	drf_SwapLArray FP ((long *, int));



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
		for (i = 0; i < hdr->rf_MaxSample; i++)
		{
			toc[i].rft_Time = old[i].rft_Time;
			toc[i].rft_Rg = old[i].rft_Rg;
			toc[i].rft_Origin = old[i].rft_Origin;
			toc[i].rft_AttrLen = old[i].rft_AttrLen;
			toc[i].rft_AttrOffset = old[i].rft_AttrOffset;
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



