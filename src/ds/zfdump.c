/*
 * Dump out a zeb file
 */

# include <defs.h>
# include "DataStore.h"
# include "znfile.h"

MAKE_RCSID ("$Id: zfdump.c,v 1.2 1993-05-11 02:29:20 granger Exp $")

extern int optind;

main (argc, argv)
int argc;
char **argv;
{
	int fd, t, c, dump_free = 0, ssps;
	char atime[30];
	zn_Header hdr;
	ZebTime *zt;
	zn_Sample *sample;
	RGrid *rg = 0;
/*
 * Checking.
 */
	if (argc < 2)
	{
		printf ("Usage: %s [-f] file\n", argv[0]);
		exit (1);
	}
/*
 * Arg processing
 */
	while ((c = getopt (argc, argv, "f")) != -1)
	{
		switch (c)
		{
		   case 'f':
		   	dump_free = 1;
			break;
		}
	}
	if (optind >= argc)
	{
		printf ("Usage: %s [-f] file\n", argv[0]);
		exit (1);
	}
/*
 * Open the file.
 */
	if ((fd = open (argv[optind], 0)) < 0)
	{
		perror (argv[optind]);
		exit (optind);
	}
/*
 * Get the header.
 */
	read (fd, &hdr, sizeof (hdr));
	if (hdr.znh_Magic != ZN_MAGIC)
	{
		printf ("This is not a znf file!\n");
		exit (1);
	}
/*
 * Start printing.
 */
	printf ("Free space: %d bytes in %d chunks; file len = %d\n",
		hdr.znh_NFreeB, hdr.znh_NFree, hdr.znh_Len);
	printf ("%d samples used of %d alloc.  %d fields.  Org = ",
		hdr.znh_NSample, hdr.znh_NSampAlloc, hdr.znh_NField);
	switch (hdr.znh_Org)
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
/*
 * Continue printing.
 */
	if (hdr.znh_Org == OrgIRGrid)
		printf ("%d stations at %ld\n", hdr.znh_NStation,
				hdr.znh_OffStation);
	printf ("Times at %ld, sample at %ld, field at %ld\n",
		hdr.znh_OffTime, hdr.znh_OffSample, hdr.znh_OffField);
	if (hdr.znh_GlAttrLen > 0)
		printf ("Global attrs, %d bytes at %ld", hdr.znh_GlAttrLen,
			hdr.znh_OffGlAttr);
	if (hdr.znh_OffAttr > 0)
		printf (" Sample attrs at %ld", hdr.znh_OffAttr);
	printf ("\n");
/*
 * Pull in tables.
 */
	zt = (ZebTime *) malloc (hdr.znh_NSample * sizeof (ZebTime));
	lseek (fd, hdr.znh_OffTime, 0);
	read (fd, zt, hdr.znh_NSample*sizeof (ZebTime));

	ssps = (hdr.znh_Org == OrgFixedScalar) ? 1 : hdr.znh_NField;
	sample = (zn_Sample *) malloc (hdr.znh_NSample*ssps*
				sizeof (zn_Sample));
	lseek (fd, hdr.znh_OffSample, 0);
	read (fd, sample, hdr.znh_NSample*ssps*sizeof (zn_Sample));
	if (hdr.znh_OffRg >= 0)
	{
		lseek (fd, hdr.znh_OffRg, 0);
		rg = (RGrid *) malloc (hdr.znh_NSample*sizeof (RGrid));
		read (fd, rg, hdr.znh_NSample*sizeof (RGrid));
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
 * Sample dump.
 */
	for (t = 0; t < hdr.znh_NSample; t++)
	{
		TC_EncodeTime (zt + t, TC_Full, atime);
		printf ("Samp %d, %s, %d at %ld", t, atime,
			sample->znf_Size, sample->znf_Offset);
		if (rg)
			printf (" RG %dx%dx%d", rg[t].rg_nX, rg[t].rg_nY,
					rg[t].rg_nZ);
		printf ("\n");
		sample += ssps;
	}
}
