# include "../include/defs.h"
# include "BoundaryFile.h"
# include <fcntl.h>




main (argc, argv)
int argc;
char **argv;
{
	int fd, i;
	struct BFHeader hdr;
	struct BFBTable *btable;
/*
 * Open.
 */
	if ((fd = open (argv[1], O_RDONLY)) < 0)
	{
		perror (argv[1]);
		exit (1);
	}
/*
 * Pull in the header.
 */
	read (fd, &hdr, sizeof (hdr));
	printf ("Magic: 0x%x, platform '%s'\n", hdr.bh_Magic, hdr.bh_Platform);
	printf ("Max boundary %d, current %d, from %d %06d to %d %06d\n",
		hdr.bh_MaxBoundary, hdr.bh_NBoundary, hdr.bh_Begin.ds_yymmdd,
		hdr.bh_Begin.ds_hhmmss, hdr.bh_End.ds_yymmdd,
		hdr.bh_End.ds_hhmmss);
/*
 * Boundary table.
 */
	btable = (struct BFBTable *) malloc
			(hdr.bh_NBoundary * sizeof (struct BFBTable));
	read (fd, btable, hdr.bh_NBoundary * sizeof (struct BFBTable));
	for (i = 0; i < hdr.bh_NBoundary; i++)
		printf ("Bnd %2d, np %d, time %d %06d, offset %d\n", i,
			btable[i].bt_NPoint, btable[i].bt_Time.ds_yymmdd,
			btable[i].bt_Time.ds_hhmmss, btable[i].bt_Offset);
}

