/*
 * Dump out a raster file.
 */
static char *rcsid = "$Id: rfdump.c,v 1.1 1991-06-14 22:17:36 corbet Exp $";


# include <defs.h>
# include "DataStore.h"
# include "RasterFile.h"





main (argc, argv)
int argc;
char **argv;
{
	int fd, fld, ntocb, i;
	RFHeader hdr;
	RFToc *toc;

	if (argc != 2)
	{
		printf ("Usage: %s rasterfile\n", argv[0]);
		exit (1);
	}
/*
 * Open the file.
 */
	if ((fd = open (argv[1], 0)) < 0)
	{
		perror (argv[1]);
		exit (1);
	}
/*
 * Get the header.
 */
	if (read (fd, &hdr, sizeof (hdr)) != sizeof (hdr))
	{
		perror ("Header read error");
		exit (1);
	}
/*
 * Print it.
 */
	printf ("Header magic = 0x%x, platform '%s'\n", hdr.rf_Magic,
		hdr.rf_Platform);
	printf ("Currently %d of max %d samples, with %d fields\n", 
		hdr.rf_NSample, hdr.rf_MaxSample, hdr.rf_NField);
	for (fld = 0; fld < hdr.rf_NField; fld++)
		printf ("\tField %d, '%s'\n", fld,hdr.rf_Fields[fld].rff_Name);
/*
 * Get and read the table of contents.
 */
	ntocb = hdr.rf_NSample*sizeof (RFToc);
	toc = (RFToc *) malloc (ntocb);
	if (read (fd, toc, ntocb) != ntocb)
	{
		perror ("TOC read error");
		exit (1);
	}
/*
 * Dump it out.
 */
	for (i = 0; i < hdr.rf_NSample; i++)
		printf ("%2d: %d %06d at %8d, (%dx%d) space %.2f, L %.2f %.2f %.2f\n", i,
			toc[i].rft_Time.ds_yymmdd, toc[i].rft_Time.ds_hhmmss,
			toc[i].rft_Offset, toc[i].rft_Rg.rg_nX,
			toc[i].rft_Rg.rg_nY, toc[i].rft_Rg.rg_Xspacing,
			toc[i].rft_Origin.l_lat, toc[i].rft_Origin.l_lon,
			toc[i].rft_Origin.l_alt);
}
