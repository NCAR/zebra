/*
 * The GENPRO processing program.  Here we go through and boil GENPRO files
 * down into something reasonable.
 */

# include <fcntl.h>
# include <sys/file.h>
# include <ctype.h>
# include <stdio.h>
# include "defs.h"
# include "fixed-gp.h"

/*
 * The mandatory fields.
 */
char *M_fields[] =
{
	"alat",
	"alon",
};
# define N_M_FIELD (sizeof(M_fields)/sizeof (char *))


/*
 * Data structures to be dumped into the file.
 */
# define NTOC 2000
struct gp_header Hdr;
struct gp_field *Fields;
struct gp_toc Toc[NTOC];
int Out_fd;		/* The output file descriptor		*/
# define TOC_FREQ	30	/* One TOC entry every this many recs */
/*
 * Local Genpro stuff.
 */
int *Foffsets;		/* Offsets into the GP record		*/
int Gp_fd;		/* The genpro file FD			*/
int Gp_lrlen;		/* Logical Record length;		*/
int Gp_prlen;		/* Physical Record length;		*/
int Gp_lperp;		/* Logical per physical record		*/

int Time_off = 6;	/* The time offset to use		*/
/*
 * The data record buffer.
 */
char D_buf[30000];
int Logical = 99999;	/* Current logical record.		*/

int Nrec = 0;		/* Number of comment records		*/
# define HDR_LEN 80

/*
 * Quick white-space skipping.
 */
# define SKIP_WHITE(cp) while (*cp && (*cp == ' ' || *cp == '\t')) cp++;


main (argc, argv)
int argc; 
char **argv;
{
/*
 * Basic sanity checking.
 */
	if (argc < 4)
	{
		printf ("Usage: %s infile outfile fields...\n", argv[0]);
		exit (1);
	}
/*
 * Make our local field list.
 */
	make_fields (argv + 3, argc - 3);
/*
 * Figure out time offsets.
 */
	if (getenv ("TIMEOFF"))
		Time_off = atoi (getenv ("TIMEOFF"));
	printf ("Using time offset of %d hours\n", Time_off);
/*
 * Open the input file.
 */
	GpOpen (argv[1]);
/* 
 * Create the output file.
 */
	if ((Out_fd = open (argv[2], O_WRONLY | O_CREAT | O_TRUNC, 0666)) < 0)
	{
		perror (argv[1]);
		exit (1);
	}
	sync ();
/*
 * Plow through the data.
 */
	Nrec = 0;
	Plow ();
	sync ();
}






make_fields (fields, count)
char **fields;
int count;
/*
 * Put together our initial field list.
 */
{
	int i, j;
/*
 * Initialize the header.
 */
	Hdr.gph_magic = GPH_MAGIC;
	Hdr.gph_nfield = count + N_M_FIELD;
	Hdr.gph_reclen = Hdr.gph_nfield*sizeof (float) + sizeof (int);
	Hdr.gph_ntoc = 0;
	Fields = (struct gp_field *)
			malloc (Hdr.gph_nfield*sizeof (struct gp_field));
	Foffsets = (int *) malloc (Hdr.gph_nfield * sizeof (int));
/*
 * Put in the user-supplied fields.
 */
	for (i = 0; i < count; i++)
	{
		strcpy (Fields[i].gpf_name, fields[i]);
		Fields[i].gpf_offset = i;
	}
/*
 * And the mandatory ones.
 */
	for (j = 0; j < N_M_FIELD; j++)
	{
		strcpy (Fields[i].gpf_name, M_fields[j]);
		Fields[i].gpf_offset = i;
		i++;
	}
}




sync ()
/*
 * Output the stuff to the file.
 */
{
/*
 * If there is a TOC, write it first.
 */
	if (Hdr.gph_ntoc)
	{
		Hdr.gph_tocoff = lseek (Out_fd, 0l, L_XTND);
		write (Out_fd, Toc, Hdr.gph_ntoc * sizeof (struct gp_toc));
		printf ("TOC at %d\n", Hdr.gph_tocoff);
	}
/*
 * Now put out the headers.
 */
	lseek (Out_fd, 0l, L_SET);
	if (write (Out_fd, &Hdr, sizeof (Hdr)) != sizeof (Hdr))
		GiveUp ("Header write error");
	write (Out_fd, Fields, Hdr.gph_nfield * sizeof (struct gp_field));
}




GpOpen (file)
char *file;
/*
 * Open up this genpro file.
 */
{
	char hbuf[HDR_LEN], *cp;
	int i;
/*
 * Actually open it.
 */
	if ((Gp_fd = open (file, 0)) < 0)
	{
		perror (file);
		exit (1);
	}
/*
 * Get and check the first header line.
 */
	get_rec (hbuf, HDR_LEN);
	if (strncmp (hbuf, " BEGINHD", 8))
	{
		printf ("Bad header line: '%s'\n", hbuf);
		exit (1);
	}
/*
 * Get the dates.
 */
	get_dates ();
/*
 * Find the logical record size.
 */
 	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("No 'LOGBIT' line in the file!");
		if (! strncmp (hbuf, " LOGBIT", 7))
			break;
	}
	for (cp = hbuf + 8; ! isdigit (*cp); cp++)
		;
	sscanf (cp, "%d", &Gp_lrlen);
	Gp_lrlen /= 8;
/*
 * Log per phys.
 */
	if (! get_rec (hbuf, HDR_LEN))
		GiveUp ("EOF looking for DATLOG");
	for (cp = hbuf + 8; ! isdigit (*cp); cp++)
		;
	sscanf (cp, "%d", &Gp_lperp);
/*
 * Phys rec len.
 */
	if (! get_rec (hbuf, HDR_LEN))
		GiveUp ("EOF looking for DATSIZ");
	for (cp = hbuf + 8; ! isdigit (*cp); cp++)
		;
	sscanf (cp, "%d", &Gp_prlen);
	Gp_prlen /= 8;
	printf ("Records: logical %d, phys %d, %d log/phys\n", Gp_lrlen,
		Gp_prlen, Gp_lperp);
/*
 * Now scan forward to the variable definitions.
 */
 	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("No 'ORDVAR=TITLE' line in the file!");
		if (! strncmp (hbuf, " ORDVAR = TITLE", 15))
			break;
	}
/*
 * Process the variable lines.
 */
	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("EOF encountered in the header");
		if (strncmp (hbuf, " LETVAR", 7))
			break;
		ProcTitle (hbuf);
	}
/*
 * Do the units lines.
 */
	if (strncmp (hbuf, " ORDVAR = UNITS", 15))
		GiveUp ("No ORDVAR = UNITS");
	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("EOF encountered in the header");
		if (strncmp (hbuf, " LETVAR", 7))
			break;
		ProcUnit (hbuf);
	}
/*
 * Skip to the ENDHD.  Since all the scales still are, as far as I can
 * tell, 1000, I'm not going to bother with them here.
 */
	for (;;)
	{
		if (! strncmp (hbuf, " ENDHD", 6))
			break;
		get_rec (hbuf, HDR_LEN);
	}
	while (Nrec % 10)
		get_rec (hbuf, HDR_LEN);
}





get_dates ()
/*
 * Pull the begin/end dates out of the file.
 */
{
	char hbuf[80], *cp, *strchr (), *begq, *endq;
	int d, mon, year;
	static char *months[] = { "junk", "JAN", "FEB", "MAR", "APR", "MAY",
		"JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" };
/*
 * Find the PRDATE line.
 */
	for (;;)
	{
		if (! get_rec (hbuf, HDR_LEN))
			GiveUp ("(PRDATE) EOF encountered in the header");
		if (! strncmp (hbuf, "/PRDATE", 7))
			break;
	}
/*
 * Interpret it.
 */
	begq = strchr (hbuf, '\"');
	endq = strchr (begq + 1, '\"');
	*endq = '\0';
	sscanf (begq + 1, "%d", &d);	/* Got day */
	begq = strchr (endq + 1, '\"');
	endq = strchr (begq + 1, '\"');
	*endq = '\0';
	for (mon = 1; mon <= 12; mon++)
		if (! strcmp (begq + 1, months[mon]))
			break;
	d += mon*100;
	begq = strchr (endq + 1, '\"');
	endq = strchr (begq + 1, '\"');
	*endq = '\0';
	sscanf (begq + 1, "%d", &year);
	d += year*10000;
/*
 * Now we remember the date portion.
 */
	Hdr.gph_begin.ds_yymmdd = Hdr.gph_end.ds_yymmdd = d;
	printf ("Flight date %d\n", d);
/*
 * Get the next line, and hope to hell it's the PRTIME.  Skip it, in favor
 * of the BEGSNP line that follows.
 */
	get_rec (hbuf, HDR_LEN);
	if (! get_rec (hbuf, HDR_LEN))
		GiveUp ("(BEGSNP) EOF encountered in the header");
/*
 * Pick it apart.
 */
	Hdr.gph_begin.ds_hhmmss = interpSNP (hbuf);
	pmu_dadd (&Hdr.gph_begin.ds_yymmdd, &Hdr.gph_begin.ds_hhmmss, 
		Time_off*10000);
/*
 * Same for the ending.
 */
	if (! get_rec (hbuf, HDR_LEN))
		GiveUp ("(BEGSNP) EOF encountered in the header");
	Hdr.gph_end.ds_hhmmss = interpSNP (hbuf);
	pmu_dadd (&Hdr.gph_end.ds_yymmdd, &Hdr.gph_end.ds_hhmmss, 
		Time_off*10000);
}




interpSNP (line)
char *line;
/*
 * Interpret this SNP line.
 */
{
	char *strchr (), *cp;
	int hour, min, sec;

	cp = strchr (line, '(') + 1;
	while (! isdigit (*cp))
		cp++;
	sscanf (cp, "%d", &hour);
	
	cp = strchr (cp, ',') + 1;
	while (! isdigit (*cp))
		cp++;
	sscanf (cp, "%d", &min);

	cp = strchr (cp, ',') + 1;
	while (! isdigit (*cp))
		cp++;
	sscanf (cp, "%d", &sec);
	return (hour*10000 + min*100 + sec);
}





ProcTitle (line)
char *line;
/*
 * Process this title LETVAR line.
 */
{
	char *begq, *endq, *field, *fend, *strchr ();
	int i;
/*
 * Isolate the quotes.
 */
	if (! (begq = strchr (line, '"')))
		GiveUp ("Funky LETVAR: '%s'", line);
	begq++;
	if (! (endq = strchr (begq, '"')))
		GiveUp ("Funky LETVAR: '%s'", line);
	*endq = 0;
/*
 * Remove the (substantial) trailing blanks from this field.
 */
	field = endq + 2;
 	while (endq[-1] == ' ')
		*--endq = '\0';
/*
 * Now try to find the field name.
 */
 	if (! (field = strchr (field, ',')))
		GiveUp ("Funky LETVAR: '%s'", field);
	field++;
	SKIP_WHITE (field);
	for (fend = field; *fend && isalnum (*fend); fend++)
		*fend = tolower (*fend);
	*fend = '\0';
/*
 * Now see if we want this field.
 */
	for (i = 0; i < Hdr.gph_nfield; i++)
		if (! strcmp (Fields[i].gpf_name, field))
			break;
	if (i >= Hdr.gph_nfield)
		return;
	printf ("Found field %s: %s\n", field, begq);
	strcpy (Fields[i].gpf_desc, begq);
}




ProcUnit (line)
char *line;
/*
 * Deal with this UNIT line.
 */
{
	char *strchr (), *unit, *uend, *offset, *field, *fend;
	int i;
/*
 * Find the beginning of the units info.
 */
 	if (! (unit = strchr (line, '"')))
		GiveUp ("Funky UNIT line: '%s'", line);
	unit++;
	SKIP_WHITE (unit);
	if (*unit == '"')	/* Empty units */
		unit -= 2;
/*
 * Now find the end.
 */
	for (uend = unit + 1; *uend && *uend != ' ' && *uend != '"'; uend++)
		;
	*uend = '\0';
/*
 * Skip fields up to the offset field.
 */
 	offset = uend + 1;
	for (i = 0; i < 4; i++)
	 	if (! (offset = strchr (offset + 1, ',')))
			GiveUp ("Funky UNIT line: '%s'", line);
	offset++;
	SKIP_WHITE (offset);
/*
 * Now find the field name.
 */
	field = offset;
	for (i = 0; i < 3; i++)
	 	if (! (field = strchr (field + 1, ',')))
			GiveUp ("Funky UNIT line: '%s'", line);
	field++;
	SKIP_WHITE (field);
	for (fend = field; *fend && isalnum (*fend); fend++)
		*fend = tolower (*fend);
	*fend = '\0';
/*
 * Now see if we want this field.
 */
	for (i = 0; i < Hdr.gph_nfield; i++)
		if (! strcmp (Fields[i].gpf_name, field))
			break;
	if (i >= Hdr.gph_nfield)
		return;
/*
 * Grab the stuff.
 */
	strcpy (Fields[i].gpf_unit, unit);
	sscanf (offset, "%d", Foffsets + i);
	Foffsets[i] /= 32;
	printf ("Field %s, unit '%s', offset %d\n", Fields[i].gpf_name,
		Fields[i].gpf_unit, Foffsets[i]);
}


get_rec (buf, len)
char *buf;
int len;
/*
 * Read a record.
 */
{
	Nrec++;
	return (read (Gp_fd, buf, len) == len);
}



char *
DataRec ()
/*
 * Get the next data record.
 */
{
	if (++Logical >= Gp_lperp)
	{
		if (! get_rec (D_buf, Gp_prlen))
			return (0);
		Logical = 0;
	}
	return (D_buf + (Logical * Gp_lrlen));
}


GiveUp (line)
char *line;
/*
 * Print this line and quit.
 */
{
	printf ("%s\n", line);
	exit (1);
}





Plow ()
/*
 * Push through the data.
 */
{
	int *ip, i, hrs, nr = 0;
	struct data_rec *dr;

/*
 * Allocate space for our output data.
 */
	printf ("Output rec len is %d\n", Hdr.gph_reclen);
	dr = (struct data_rec *) malloc (Hdr.gph_reclen);
	fflush (stdout);
	getchar ();
/*
 * Now work through the file.
 */
	for (;;)
	{
	/*
	 * Get a record.
	 */
	 	if (! (ip = (int *) DataRec ()))
			break;
	/*
	 * Pull out the time.
	 */
		hrs = ((ip[0]/1000 - 1000) + Time_off)*10000 + 	/*XXXX*/
		      (ip[1]/1000 - 1000)*100 + (ip[2]/1000) - 1000;
	/*
	 * Maybe put out a TOC entry.
	 */
		if ((nr++ % TOC_FREQ) == 0)
			put_toc (hrs);
	/*
	 * Package up the data.
	 */
		dr->d_time = hrs;
	 	for (i = 0; i < Hdr.gph_nfield; i++)
			dr->d_data[i] = ip[Foffsets[i]]/1000.0-1000.0;
	/*
	 * Put it out.
	 */
		if (write (Out_fd, dr, Hdr.gph_reclen) != Hdr.gph_reclen)
		{
			perror ("Data write");
			exit (1);
		}
	}
}





put_toc (hrs)
int hrs;
/*
 * Put out a TOC entry.
 */
{
	Toc[Hdr.gph_ntoc].gpt_when = hrs;
	Toc[Hdr.gph_ntoc++].gpt_offset = lseek (Out_fd, 0L, L_INCR);
	printf ("TOC: %d %d\n", hrs, Toc[Hdr.gph_ntoc-1].gpt_offset);
}
