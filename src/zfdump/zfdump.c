/*
 * Dump out a zeb file
 */

# include <stdio.h>
# include <sys/types.h>
# include <unistd.h>
# include <defs.h>
# include "DataStore.h"
# include "znfile.h"

MAKE_RCSID ("$Id: zfdump.c,v 1.5 1993-07-07 19:56:40 granger Exp $")

extern int optind;

static void DumpHeader FP((zn_Header *hdr));
static int DumpFile FP((char *filename, int, int, int));
static void DumpSample FP((int fd, zn_Header *hdr, zn_Sample *sample,
			   RGrid *rg));
static zn_Field *DumpFields FP((int fd, zn_Header *hdr));
static void DumpGlobalAttrs FP((int fd, zn_Header *hdr));

void
Usage(prog)
char *prog;
{
	printf ("Usage: %s [-f] [-a] file ...\n", prog);
	printf ("   -f\tDump free list\n");
	printf ("   -h\tDump header only\n");
	printf ("   -a\tDump everything else, including data\n");
}
	

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
	for (i = optind; i < argc; ++i)
	{
		failure += DumpFile (argv[i], 
				     dump_free, dump_header, dump_all);
	}
	exit (failure);
}




static int
DumpFile (filename, dump_free, dump_header, dump_all)
char *filename;
int dump_free, dump_header, dump_all;
/*
 * Returns non-zero on FAILURE!
 */
{
	int fd, t, c, ssps;
	char atime[30];
	zn_Header hdr;
	ZebTime *zt;
	zn_Sample *sample, *samples;
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
 * Get the header.
 */
	if ((read (fd, &hdr, sizeof (hdr)) != sizeof (hdr)) ||
	    hdr.znh_Magic != ZN_MAGIC)
	{
		printf ("%s: not a znf file!\n", filename);
		return 2;
	}
/*
 * Start printing.  In every case we print at least the header and the fields.
 */
	printf ("\n----- ZNF file: %s -----\n", filename);
	DumpHeader (&hdr);
	DumpGlobalAttrs (fd, &hdr);
	zflds = DumpFields (fd, &hdr);
/*
 * Pull in tables.
 */
	zt = (ZebTime *) malloc (hdr.znh_NSample * sizeof (ZebTime));
	lseek (fd, hdr.znh_OffTime, 0);
	read (fd, zt, hdr.znh_NSample*sizeof (ZebTime));

	ssps = (hdr.znh_Org == OrgFixedScalar) ? 1 : hdr.znh_NField;
	samples = (zn_Sample *) malloc (hdr.znh_NSample * ssps *
				sizeof (zn_Sample));
	lseek (fd, hdr.znh_OffSample, 0);
	read (fd, samples, hdr.znh_NSample * ssps * sizeof (zn_Sample));
	if (hdr.znh_OffRg >= 0)		/* retrieve rgrid info 	*/
	{
		lseek (fd, hdr.znh_OffRg, 0);
		rg = (RGrid *) malloc (hdr.znh_NSample*sizeof (RGrid));
		read (fd, rg, hdr.znh_NSample*sizeof (RGrid));
	}
	if (hdr.znh_OffLoc > 0)		/* retrieve dynamic locns */
	{
		lseek (fd, hdr.znh_OffLoc, 0);
		locns = (Location *)malloc(hdr.znh_NSample*sizeof(Location));
		read (fd, locns, hdr.znh_NSample*sizeof(Location));
	}
	if (hdr.znh_OffAttr > 0)	/* get any per sample atts */
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
			lseek (fd, foff, 0);
			read (fd, &zf, sizeof (zf));
			printf ("\tBlk at %ld, size %d\n", foff, zf.znf_Size);
			foff = zf.znf_Next;
		}
		printf ("\n");
	}
/*
 * If header-only option chosen, skip sample dumps
 */
	if (!dump_header)
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
				char *ablock, *att;

				ablock = (char *)malloc(satts[t].znf_Size);
				lseek (fd, satts[t].znf_Offset, 0);
				read (fd, ablock, satts[t].znf_Size);
				for (att = ablock; 
				     (*att) && 
				     (att - ablock < satts[t].znf_Size); 
				     att += strlen(att) + 1)
					printf ("\t%s\n", att);
				free (ablock);
			}
			if (dump_all)
				DumpSample (fd, &hdr, sample, rg+t);
			sample += ssps;
		}
	}
	free (zt);
	free (samples);
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
	char *ablock, *att;

	printf ("Global Attributes:\n");
	blen = hdr->znh_GlAttrLen;
	if (blen == 0)
		return;
	ablock = (char *)malloc(blen);
	lseek (fd, hdr->znh_OffGlAttr, SEEK_SET);
	read (fd, ablock, blen);
/*
 * Now have our block of attributes.  Process it.
 */
	for (att = ablock; (*att) && (att - ablock < blen); 
	     att += strlen(att) + 1)
		printf ("\t%s\n", att);

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

	if ((hdr->znh_NField == 0) 
	    /*|| (hdr->znh_Org == OrgTransparent)*/ )
		return NULL;

	printf ("Fields:\n");
	flen = hdr->znh_NField * sizeof(zn_Field);
	zflds = (zn_Field *)malloc(flen);
	lseek (fd, hdr->znh_OffField, SEEK_SET);
	read (fd, zflds, flen);

	printf ("   %-15s %10s %5s %10s %10s\n",
		"--Name--------",
		"--Badval--","-Fmt-","--Scale--","--Offset--");
	for (i = 0; i < hdr->znh_NField; ++i)
	{
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




static void
DumpSample (fd, hdr, sample, rg)
int fd;
zn_Header *hdr;
zn_Sample *sample;
RGrid *rg;
/*
 * Given sample size and offset, read sample and dump its data
 */
{
	static char *buf = NULL;
	static long buf_size = 0;

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
/*
 * Now have our data in buffer, dump it out based on organization
 * and format
 */
	
}


static void
DumpHeader (hdr)
zn_Header *hdr;
{
	printf ("Free space: %d bytes in %d chunks; file len = %d\n",
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
