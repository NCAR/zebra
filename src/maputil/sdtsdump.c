# include <stdio.h>
# include <stdarg.h>
# include <errno.h>
# include "fips123/stc123.h"


void	DDRecDump (FILE *infile);
void	DataDump (FILE *infile);
void	die (char *format, ...);


main (int argc, char *argv[])
{
    char	fname[128], ice[4], ccs[4];
    long	level;
    FILE	*infile;

    /*
     * Get the input file name from the command line or prompt for it
     */
    if (argc > 2)
	die ("Usage: %s <sdts_file>\n", argv[0]);
    else if (argc == 1)
    {
	printf ("SDTS file: ");
	gets (fname);
    }
    else
	strcpy (fname, argv[1]);

    /*
     * Open the file
     */
    if (! beg123file (fname, 'r', &level, ice, ccs, &infile))
	die ("Error %d opening file '%s'\n", errno, fname);
# ifdef notdef
    printf ("Interchange level: %d\n", level);
    printf ("Inline code extension: %c\n", ice[0]);
    printf ("Code character set: '%3s'\n", ccs);
# endif
    if (ice[0] != ' ' || strncmp (ccs, "   ", 3))
	die ("Unexpected ice (%d) or ccs ('%3s')!\n", ice[0], ccs);


    /*
     * dump the data descriptive record
     */
    DDRecDump (infile);
    DataDump (infile);
    
    exit (0);
}



void
DDRecDump (FILE *infile)
/*
 * Dump the data descriptive record
 */
{
    char	tag[32], data[1024];
    int		status;

    if (! beg123ddrec (infile))
	die ("beg123ddrec failure!\n");

    printf ("\n");
    printf ("Data Descriptive Record\n");
    printf ("=========\n");
    
    while (rd123ddsfld (infile, tag, data, &status))
    {
	/*
	 * Don't print info for start-of-record (2) or start-of-field (6)
	 * subfields
	 */
	if (status == 2 || status == 6)
	    continue;
	
	printf ("\t%s - %s\n", tag, data);
	/*
	 * Quit on end-of-record (3) or end-of-file (4)
	 */
	if (status == 3 || status == 4)
	    break;
    }
    
    if (! (status == 3 || status == 4))
	die ("Ended DD record on weird status %d!\n", status);
}



void
DataDump (FILE *infile)
/*
 * Dump the data records
 */
{
    char	tag[128], lead_id[4], data[32*1024];
    long	datalen;
    int		status, bigEndian;

    g123order (&bigEndian);

    while (1)
    {
	printf ("\n");
	printf ("Data Record\n");
	printf ("===========\n");
    
	while (rd123sfld (infile, tag, lead_id, data, &datalen, &status))
	{
	    if (! strcmp (tag, "SADR"))
	    {
		long	*pos;

		if (! bigEndian)
		{
		    char c;
		    c = data[0]; data[0] = data[3]; data[3] = c;
		    c = data[1]; data[1] = data[2]; data[2] = c;
		}
		    
		pos = (long *)data;
		printf ("\t%s - %.6f\n", tag, 1.0e-6 * (*pos));
	    }
	    else
		printf ("\t%s - %s\n", tag, data);

	    /*
	     * Quit on end-of-record (3) or end-of-file (4)
	     */
	    if (status == 3 || status == 4)
		break;
	}
	/*
	 * Keep going to end-of-file (status == 4)
	 */
	if (status == 4)
	    break;
    }
}



void
die (char *format, ...)
{
    va_list	args;

    va_start (args, format);
    vfprintf (stderr, format, args);
    va_end (args);

    exit (1);
}

    
