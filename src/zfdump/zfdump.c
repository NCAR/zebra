/*
 * Dump out a zeb file
 */

# include <stdio.h>
# include <stdlib.h>
# include <sys/types.h>
# include <string.h>
# include <errno.h>
# include <unistd.h>
# include <fcntl.h>

# include <defs.h>
# include "DataStore.h"
# include "znfile.h"

RCSID ("$Id: zfdump.c,v 1.17 1999-03-01 02:03:46 burghart Exp $")

extern int optind;

static void DumpHeader FP((zn_Header *hdr));
static int DumpFile FP((char *filename, int, int, int));
static void DumpSample FP((int fd, zn_Header *hdr, zn_Sample *sample,
			   RGrid *rg));
static zn_Field *DumpFields FP((int fd, zn_Header *hdr));
static void DumpGlobalAttrs FP((int fd, zn_Header *hdr));
static void ReadFreeNode FP ((int fd, long offset, zn_Free *zf));
static int PrintAttr FP((char *key, void *value, int nval, DC_ElemType type,
			 void *arg));

void
Usage(prog)
char *prog;
{
	printf ("Usage: %s [-h] [-f] [-a] file ...\n", prog);
	printf ("Prints header, sample indices, and attributes by default.\n");
	printf ("   -h\tDump header only\n");
	printf ("   -f\tDump free list\n");
	printf ("   -a\tDump everything else, ");
	printf ("including (scalar and fixed-scalar) data\n");
}
	

int
main (argc, argv)
int argc;
char **argv;
{
	int c, i;
	int dump_free = 0, dump_header = 0, dump_all = 0;
	int failure = 0;
/*
 * Checking.
 */
	if (argc < 2)
	{
		Usage (argv[0]);
		exit (1);
	}
/*
 * Arg processing
 */
	while ((c = getopt (argc, argv, "fha")) != -1)
	{
		switch (c)
		{
		   case 'f':
		   	dump_free = 1;
			break;
		   case 'h':
			dump_header = 1;
			break;
		   case 'a':
			dump_all = 1;
			break;
		}
	}
	if (optind >= argc)
	{
		Usage (argv[0]);
		exit (1);
	}
/*
 * Loop through remaining filename args
 */
	F_Init ();
	for (i = optind; i < argc; ++i)
	{
		failure += DumpFile (argv[i], 
				     dump_free, dump_header, dump_all);
	}
	exit (failure);
	return (0);	/* compiler tranquility */
}




static int
DumpFile (filename, dump_free, dump_header, dump_all)
char *filename;
int dump_free, dump_header, dump_all;
/*
 * Returns non-zero on FAILURE!
 */
{
	int fd, t, ssps = 0, hdrlen, magic;
	char atime[30];
	zn_Header hdr;
	ZebTime *zt = NULL;
	zn_Sample *sample, *samples = NULL;
	zn_Field *zflds;
	zn_Sample *satts = 0;
	Location *locns = 0;
	RGrid *rg = 0;
/*
 * Open the file.
 */
	if ((fd = open (filename, 0)) < 0)
	{
		perror (filename);
		return 1;
	}
/*
 * Check the magic number and set the header length
 */
	if ((read (fd, &magic, sizeof (int))) != sizeof (int))
	{
		printf ("Error %d reading file '%s'\n", errno, filename);
		return 2;
	}

	if (magic == ZN_MAGIC)
		hdrlen = sizeof (hdr);
	else if (magic == ZN_OLDMAGIC)
		hdrlen = ZN_V1_HDRLEN;	/* old version 1 header */
	else
	{
		printf ("%s: not a znf file. Bad magic number %x.\n",
			filename, magic);
		return (2);
	}

	lseek (fd, 0, SEEK_SET);	/* rewind */
/*
 * Get the header.
 */
	if ((read (fd, &hdr, hdrlen) != hdrlen))
	{
		printf ("%s: short header!\n", filename);
		return 2;
	}
/*
 * Fill in the missing pieces if we read a version 1 header
 */
	if (magic == ZN_OLDMAGIC)
	{
		hdr.znh_Version = 1;
		hdr.znh_AltUnits = ZAU_KMMSL;
	}
/*
 * Start printing.  In every case we print at least the header and the fields.
 */
	printf ("\n///// ZNF: %s\n", filename);
	DumpHeader (&hdr);
	DumpGlobalAttrs (fd, &hdr);
	zflds = DumpFields (fd, &hdr);
/*
 * Pull in tables.
 */
	if (hdr.znh_NSample > 0)
	{
		zt = (ZebTime *) malloc (hdr.znh_NSample * sizeof (ZebTime));
		lseek (fd, hdr.znh_OffTime, 0);
		read (fd, zt, hdr.znh_NSample*sizeof (ZebTime));

		ssps = ((hdr.znh_Org == OrgFixedScalar) ||
			(hdr.znh_NField == 0)) ? 1 : hdr.znh_NField;
		samples = (zn_Sample *) malloc (hdr.znh_NSample * ssps *
						sizeof (zn_Sample));
		lseek (fd, hdr.znh_OffSample, 0);
		read(fd, samples, hdr.znh_NSample * ssps * sizeof (zn_Sample));
	}
/*
 * Retrieve rgrid info
 */
	if (hdr.znh_NSample > 0 && hdr.znh_OffRg >= 0)
	{
		lseek (fd, hdr.znh_OffRg, 0);
		rg = (RGrid *) malloc (hdr.znh_NSample*sizeof (RGrid));
		read (fd, rg, hdr.znh_NSample*sizeof (RGrid));
	}
/*
 * Retrieve dynamic locns
 */
	if (hdr.znh_NSample > 0 && hdr.znh_OffLoc > 0)
	{
		lseek (fd, hdr.znh_OffLoc, 0);
		locns = (Location *)malloc(hdr.znh_NSample*sizeof(Location));
		read (fd, locns, hdr.znh_NSample*sizeof(Location));
	}
/*
 * Get any per sample atts
 */
	if (hdr.znh_NSample > 0 && hdr.znh_OffAttr > 0)
	{
		long slen = hdr.znh_NSample * sizeof(zn_Sample);

		lseek (fd, hdr.znh_OffAttr, 0);
		satts = (zn_Sample *)malloc(slen);
		read (fd, satts, slen);
	}
/*
 * Maybe a free list dump.
 */
	if (dump_free)
	{
		zn_Free zf;
		long foff = hdr.znh_Free;
		printf ("\nFree list:\n");
		while (foff > 0)
		{
			ReadFreeNode (fd, foff, &zf);
			printf ("\tBlk at %ld, size %d\n", foff, zf.znf_Size);
			foff = zf.znf_Next;
		}
		printf ("\n");
	}
/*
 * If header-only option chosen, skip sample dumps
 */
	if (!dump_header && samples)
	{
	/*
	 * Sample dump.
	 */
		sample = samples;
		for (t = 0; t < hdr.znh_NSample; t++)
		{
			TC_EncodeTime (zt + t, TC_Full, atime);
			printf ("Samp %3d, %s, %d at %ld", t, atime,
				sample->znf_Size, sample->znf_Offset);
			if (rg)
				printf (", RG %dx%dx%d", rg[t].rg_nX, 
					rg[t].rg_nY, rg[t].rg_nZ);
			if (locns)
			{
				printf (" @ ");
				printf ("%6.2f, %6.2f, ",
					locns[t].l_lat, locns[t].l_lon);
				printf ("%6.2f",
					locns[t].l_alt);
			}
			printf ("\n");
			if (satts && (satts[t].znf_Size > 0))
			{
				char *ablock;
				DataChunk *dc;

				ablock = (char *)malloc(satts[t].znf_Size);
				lseek (fd, satts[t].znf_Offset, 0);
				read (fd, ablock, satts[t].znf_Size);
				dc = dc_CreateDC (DCC_Raw);
				dc_SetGlAttrBlock (dc, (void *)ablock, 
						   satts[t].znf_Size);
				dc_ProcessAttrArrays (dc,NULL,PrintAttr,NULL);
				dc_DestroyDC (dc);
				free (ablock);
			}
			if (dump_all)
				DumpSample (fd, &hdr, sample, rg+t);
			sample += ssps;
		}
	}
	if (zt)
		free (zt);
	if (samples)
		free (samples);
	if (zflds)
		free (zflds);
	if (locns)
		free (locns);
	if (rg) 
		free (rg);
	if (satts)
		free (satts);
	return (0);
}



static void
DumpGlobalAttrs (fd, hdr)
int fd;
zn_Header *hdr;
/*
 * Dump the global attributes of the given znf file
 */
{
	long blen;
	char *ablock;
	DataChunk *dc;

	printf ("Global Attributes:\n");
	blen = hdr->znh_GlAttrLen;
	if (blen == 0)
		return;
	ablock = (char *)malloc(blen);
	lseek (fd, hdr->znh_OffGlAttr, SEEK_SET);
	read (fd, ablock, blen);
/*
 * Now have our block of attributes.  Process it.  To do this with the new
 * complicated typed attribute arrays, we'll add the block to a DataChunk
 * and dump the datachunk.
 */
	dc = dc_CreateDC (DCC_Raw);
	dc_SetGlAttrBlock (dc, (void *)ablock, blen);
	dc_ProcessAttrArrays (dc, NULL, PrintAttr, NULL);
	dc_DestroyDC (dc);
	free (ablock);
}



static zn_Field *
DumpFields (fd, hdr)
int fd;
zn_Header *hdr;
/*
 * Retrieve fields block, if present, and print info on each field.
 * Return the list of fields.  Free this list when done with it.
 * If no fields, returns NULL.
 */
{
	zn_Field *zflds;
	long flen;
	int i;

	if (hdr->znh_NField == 0)
		return NULL;

	printf ("Fields:\n");
	flen = (hdr->znh_Version == 1) ? 
		sizeof (zn_FieldV1) : sizeof (zn_Field);
/*
 * Use the current zn_Field size for the internal field array, filling
 * it in with defaults when the file only has version 1 information.
 * Read only as much of the structure as exists, according to the version.
 */
	zflds = (zn_Field *) malloc (hdr->znh_NField * sizeof (zn_Field));
	lseek (fd, hdr->znh_OffField, SEEK_SET);

	printf ("   %-15s %10s %5s %10s %10s\n",
		"Name","Badval","Fmt","Scale","Offset");
	for (i = 0; i < hdr->znh_NField; ++i)
	{
		read (fd, zflds + i, flen);
	/*
	 * For version 1 files, fill in the missing pieces of the zn_Field
	 * structure
	 */
		if (hdr->znh_Version == 1)
		{
			zflds[i].zf_AttrLen = 0;
			zflds[i].zf_OffAttr = -1;
		}
		
		printf ("   %-15s %10.4f ",
			zflds[i].zf_Name, zflds[i].zf_Badval);
		switch (zflds[i].zf_Format)
		{
		   case DF_Float:
			printf ("%5s ", "float");
			break;
		   case DF_Byte:
			printf ("%5s %10.4f %10.4f",
				"byte", zflds[i].zf_Scale.s_Scale,
				zflds[i].zf_Scale.s_Offset);
			break;
		   default:
			printf ("%-5s ", "hosed");
		}
		printf ("\n");
	}
	return (zflds);
}


static void *
ReadSample (fd, hdr, sample)
int fd;
zn_Header *hdr;
zn_Sample *sample;
{
	static char *buf = NULL;
	static long buf_size = 0;

	if (sample->znf_Offset <= 0 || sample->znf_Size <= 0)
		return (NULL);
	if (!buf)
	{
		buf_size = sample->znf_Size;
		buf = (char *)malloc(buf_size);
	}
	else if (sample->znf_Size > buf_size)
	{
		buf_size = sample->znf_Size;
		buf = (char *)realloc(buf, buf_size);
	}
	lseek (fd, sample->znf_Offset, SEEK_SET);
	read (fd, buf, sample->znf_Size);
	return (buf);
}



static void
PrintFloat (f)
float f;
{
	char buf[30];

	/*
	 * If more than one, just take 6 significant digits,
	 * otherwise, round out to the 6th decimal place.
	 * If negative, we reduce to 5 significant digits to
	 * make room for the minus sign.
	 */
	if (f < 1.0 && f >= 0.0)
		f = ((int)(f * 1e+6)) * 1e-6;
	else if (f < 0.0 && f > -1.0)
		f = ((int)(f * 1e+5)) * 1e-5;
	if (f > -1.0)
		sprintf (buf, "%.7g", f);
	else
		sprintf (buf, "%.6g", f);
	if (!strchr (buf, '.') && (strlen(buf) <= (unsigned)6))
		strcat (buf, ".0");
	printf ("%9s", buf);
}		



static void
DumpSample (fd, hdr, sample, rg)
int fd;
zn_Header *hdr;
zn_Sample *sample;
RGrid *rg;
/*
 * Given first zn_Sample of a sample, read the field data and print it
 */
{
	int fld;
	void *buf = NULL;
	char *blank_fmt = " - - - - ";

	/*
	 * For FixedScalar, all of the fields are read at once
	 */
	if (hdr->znh_Org == OrgFixedScalar)
		buf = ReadSample (fd, hdr, sample);
	else if (hdr->znh_Org != OrgScalar)
		return;				/* no sense in continuing */
	for (fld = 0; fld < hdr->znh_NField; ++fld)
	{
		if (fld % 8 == 0)
			printf ("   ");
		switch (hdr->znh_Org)
		{
		   case OrgFixedScalar:
			PrintFloat ( ((float *)buf)[fld] );
			break;
		   case OrgScalar:
			buf = ReadSample (fd, hdr, sample+fld);
			if (buf)
				PrintFloat ( *(float *)buf );
			else
				printf (blank_fmt);
		   default:
			/* can't handle anything else */
			break;
		}
		if ((fld + 1) % 8 == 0)
			printf ("\n");
	}
	if (fld % 8 != 0)
		printf ("\n");
}




static void
DumpHeader (hdr)
zn_Header *hdr;
{
	printf ("ZNF version: %d\n", hdr->znh_Version);
	printf ("Free space: %d bytes in %d chunks; file len = %ld\n",
		hdr->znh_NFreeB, hdr->znh_NFree, hdr->znh_Len);
	printf ("%d samples used of %d alloc.  %d fields.  Org = ",
		hdr->znh_NSample, hdr->znh_NSampAlloc, hdr->znh_NField);
	switch (hdr->znh_Org)
	{
	   case OrgOutline:	printf ("OUTLINE\n"); break;
	   case OrgImage:	printf ("IMAGE\n"); break;
	   case OrgIRGrid:	printf ("IRGRID\n"); break;
	   case OrgScalar:	printf ("SCALAR\n"); break;
	   case OrgFixedScalar:	printf ("FIXED-FIELD SCALAR\n"); break;
	   case Org3dGrid:	printf ("3D GRID\n"); break;
	   case Org2dGrid:	printf ("2D GRID\n"); break;
	   case Org1dGrid:	printf ("1D GRID\n"); break;
	   case OrgTransparent: printf ("TRANSPARENT\n"); break;
	   default:		printf ("** HOSED **\n"); break;
	}
	if (hdr->znh_Org != OrgOutline && hdr->znh_Org != OrgTransparent)
	{
		if (hdr->znh_OffLoc < 1)	/* Static location */
		{
			Location *locn = &(hdr->znh_Loc);
			
			printf ("Static location: ");
			printf ("%8.3f lat, %8.3f lon, %8.2f alt\n",
				locn->l_lat, locn->l_lon, locn->l_alt);
		}
		else
			printf ("Dynamic locations at %ld\n", hdr->znh_OffLoc);
	}

	printf ("Altitude units: ");
	switch (hdr->znh_AltUnits)
	{
	    case ZAU_KMMSL:	printf ("km MSL\n"); break;
	    case ZAU_MMSL:	printf ("m MSL\n"); break;
	    case ZAU_MB:	printf ("mb\n"); break;
	    default:		printf ("(unknown)\n"); break;
	}
/*
 * Print organization-specific stuff
 */
	if (hdr->znh_Org == OrgIRGrid)
		printf ("%d stations at %ld\n", hdr->znh_NStation,
				hdr->znh_OffStation);

	printf ("Times at %ld, samples at %ld, fields at %ld\n",
		hdr->znh_OffTime, hdr->znh_OffSample, hdr->znh_OffField);
/*
 * Attribute offsets
 */
	if (hdr->znh_GlAttrLen > 0)
		printf ("Global attrs, %d bytes at %ld", hdr->znh_GlAttrLen,
			hdr->znh_OffGlAttr);
	if (hdr->znh_OffAttr > 0)
		printf (" Sample attrs at %ld", hdr->znh_OffAttr);
	printf ("\n");
}



static void
ReadFreeNode (fd, offset, fb)
int fd;
long offset;
zn_Free *fb;
{
	long magic;

	lseek (fd, offset, SEEK_SET);
	read (fd, &magic, sizeof(long));
	if (magic != ZN_FREE_MAGIC)
	{
		if (magic == 0)
			magic = ZN_FREE_MAGIC;
		fb->znf_FMagic = ZN_FREE_MAGIC;
		fb->znf_Next = magic;
		fb->znf_Size = sizeof(long);

		while (fb->znf_Next == offset + fb->znf_Size)
		{
			lseek (fd, fb->znf_Next, SEEK_SET);
			read (fd, &magic, sizeof(long));
			if (magic == 0)
				magic = ZN_FREE_MAGIC;
			fb->znf_Next = magic;
			fb->znf_Size += sizeof(long);
		}
	}
	else
	{
		lseek (fd, offset, SEEK_SET);
		read (fd, fb, sizeof(zn_Free));
	}
}




static int
PrintAttr (key, value, nval, type, arg)
char *key;
void *value;
int nval;
DC_ElemType type;
void *arg;
/*
 * Print out an attribute value.
 */
{
	int i;

	if (nval && (type == DCT_String))
	{
		printf ("   %s --> '%s'\n", key, (char *)value);
		return (0);
	}
	printf ("   %s --> ", key);
	for (i = 0; i < nval; ++i)
	{
		printf ("%s%s", dc_PrintValue (value, type),
			(i == nval - 1) ? "\n" : ", ");
		value = (char *)value + dc_SizeOfType (type);
	}
	if (nval == 0)
		printf ("\n");
	return (0);
}



/*
 * To avoid linking in the whole library, since none of our use of DataChunks
 * will require it.
 */
const char *
ds_PlatformName (PlatformId id)
{
	return ("unknown");
}
