/*
 * Read a genpro tape to disk.
 *
 * readgp device file
 */
# include <sys/fcntl.h>
# include <stdio.h>

char Buf[60000];

main (argc, argv)
int argc;
char **argv;
{
	int tape, file, nrec, nb;
/*
 * Make sure they have their act together.
 */
	if (argc != 3)
	{
		printf ("Usage: %s input-tape output-file\n", argv[0]);
		exit (1);
	}
/*
 * Open things.
 */
	if ((tape = open (argv[1], O_RDONLY)) < 0)
	{
		perror (argv[1]);
		exit (1);
	}
	if ((file = open (argv[2], O_WRONLY|O_CREAT|O_TRUNC, 0666)) < 0)
	{
		perror (argv[2]);
		exit (1);
	}
/*
 * Time to copy the header.
 */
	printf ("Reading GENPRO header..."); fflush (stdout);
	nrec = 0;
	while (GetHdrRec (tape, Buf) > 0)
	{
		if (nrec == 0 && strncmp (Buf, " BEGINHD", 8))
			printf (
		       "\nWARNING: This doesn't look like a GENPRO header!\n");
		nrec++;
		write (file, Buf, 80);
	}
	printf ("%d records read.\n", nrec);
/*
 * Now the data.
 */
	printf ("Reading data "); fflush (stdout);
	nrec = 0;
	while ((nb = read (tape, Buf, 60000)) > 0)
	{
		write (file, Buf, nb);
		if ((++nrec % 100) == 0)
		{
			printf (".");
			fflush (stdout);
		}
	}
	printf ("%d records read.\n", nrec);
}




int
GetHdrRec (tape, dest)
int tape;
char *dest;
/*
 * Pull in a header line.  Try to cope with blocked headers.
 */
{
	static char hbuf[8000];
	static int hlen = -1, hpos = 0;
/*
 * If we have something in the buffer still just return it.
 */
	if (hpos < hlen)
	{
		memcpy (dest, hbuf + hpos, 80);
		hpos += 80;
		return (80);
	}
/*
 * Nope, gotta read another.
 */
	if ((hlen = read (tape, hbuf, 8000)) <= 0)
		return (0);	/* All done. */
	memcpy (dest, hbuf, 80);
	hpos = 80;
	return (80);
}
