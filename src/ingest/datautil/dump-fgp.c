/*
 * Dump out a fixed GP file.
 */

# include <fcntl.h>
# include <sys/file.h>
# include "defs.h"
# include "fixed-gp.h"


struct gp_header Hdr;
struct gp_field Fields[50];
struct gp_toc Toc[500];

char Dbuf[500];

main (argc, argv)
int argc;
char **argv;
{
	int fd, i, len, when, f;
	struct data_rec *dr = (struct data_rec *) Dbuf;
/*
 * You always gotta check after these folks.....
 */
	if (argc != 3)
	{
		printf ("Usage: %s file time\n", argv[0]);
		exit (1);
	}
	when = atoi (argv[2]);
/*
 * Open the file.
 */
	if ((fd = open (argv[1], 0)) < 0)
		bummer (argv[1]);
/*
 * Read and print the header.
 */
	if (read (fd, &Hdr, sizeof (Hdr)) < sizeof (Hdr))
		bummer ("Header read");
	printf ("Header magic: 0x%x, %d fields, %d toc entries at %d\n",
		Hdr.gph_magic, Hdr.gph_nfield, Hdr.gph_ntoc, Hdr.gph_tocoff);
	printf ("\tRecord len is %d, data from %d %d to %d %d\n",
		Hdr.gph_reclen, Hdr.gph_begin.ds_yymmdd,
		Hdr.gph_begin.ds_hhmmss, Hdr.gph_end.ds_yymmdd, 
		Hdr.gph_end.ds_hhmmss);
/*
 * Field list.
 */
	if (read (fd, Fields, Hdr.gph_nfield*sizeof (struct gp_field)) <= 0)
		bummer ("Field read");
	for (i = 0; i < Hdr.gph_nfield; i++)
		printf ("Field %s: '%s' '%s' offset %d\n", Fields[i].gpf_name,
			Fields[i].gpf_desc, Fields[i].gpf_unit,
			Fields[i].gpf_offset);
/*
 * Pull in the TOC too.
 */
	lseek (fd, Hdr.gph_tocoff, L_SET);
	len = Hdr.gph_ntoc * sizeof (struct gp_toc);
	if (read (fd, Toc, len) < len)
		bummer ("TOC read");
	for (i = 0; i < 10; i++)
		printf ("%2d: %d at %d\n", i, Toc[i].gpt_when,
			Toc[i].gpt_offset);
/*
 * Find the time.
 */
	for (i = 0; i < Hdr.gph_ntoc; i++)
		if (Toc[i].gpt_when > when)
			break;
	if (i >= Hdr.gph_ntoc)
		bummer ("Time not found");
	if (i > 0)
		i--;
/*
 * Dump some data.
 */
	lseek (fd, Toc[i].gpt_offset, L_SET);
	for (i = 0; i < 10; i++)
	{
		if (read (fd, Dbuf, Hdr.gph_reclen) < Hdr.gph_reclen)
			bummer ("Data read");
		printf ("%d: ", dr->d_time);
		for (f = 0; f < 4; f++)
			printf ("%s = %.2f ", Fields[f].gpf_name, 
				dr->d_data[Fields[f].gpf_offset]);
		printf ("\n");
	}
}




bummer (s)
char *s;
{
	perror (s);
	exit (1);
}
