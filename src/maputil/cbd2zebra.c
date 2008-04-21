/*
 * Read and convert CBD binary map files into Zebra-compatible map files.
 *
 * Usage: cbd2zebra <cbd_file> rank [rank ...]
 *
 * Where <cbd_file> is the map file to read and the rank(s) are the desired
 * segment ranks to be extracted (see below for more information on segment
 * ranks).  The resulting Zebra-compatible map is written to the standard
 * output.
 *
 * CBD Database information
 * ------------------------
 * The CBD map files encode features from the "CIA World Data Bank
 * II map database".
 * 
 * The CIA World Bank II master information is a series of COBOL records,
 * specifying 5,719,617 individual vectors, which occupies about 130
 * megabytes of disk space. The "cbd" files here are compressed binary
 * encodings of it, collectively occupying about 15 megabytes of disk
 * space.  These "cbd" files were produced from the original ASCII
 * database by Brian Reid at Decwrl.
 * 
 * The original database is available from the U.S. Government and is in
 * the public domain. The map database is divided into continents (North
 * America, South America, Europe, Africa, and Asia), and within
 * continents it is divided into "Coastlines, Islands, and Lakes" (cil
 * files), "Rivers" (riv files), "International political boundaries"
 * (bdy files) and "National political boundaries" (pby files). Each file
 * is divided into several thousand "segments", and each segment is
 * divided into some number of "strokes". The .cbd encoding of the
 * database encodes each continent into a directory and each division
 * into a file.
 * 
 * The "rank" of segments within a file is an indication of what the
 * segment depicts:
 * 
 * In "Boundary" files (bdy):
 * 	01	Demarcated or delimited boundary
 * 	02	Indefinite or in Dispute
 * 	03	Other line of separation of sovreignty on land
 * 
 * In "Coast, Islands and Lakes" files (cil):
 * 	01	Coast, islands and lakes that appear on all maps
 * 	02	Additional major islands and lakes
 * 	03	Intermediate islands and lakes
 * 	04	Minor islands and lakes
 * 	06	Intermittent major lakes
 * 	07	Intermittent minor lakes
 * 	08	Reefs
 * 	09	Salt pans -- major
 * 	10	Salt pans -- minor
 * 	13	Ice Shelves -- major
 * 	14	Ice Shelves -- minor
 * 	15	Glaciers
 *
 * In "Political Boundary" (pby) file, North America only:
 *	01	State or province boundary
 *
 * In "Rivers" files (riv):
 * 	01	Permanent major rivers
 * 	02	Additional major rivers
 * 	03	Additional rivers
 * 	04	Minor rivers
 * 	05	Double lined rivers
 * 	06	Intermittent rivers -- major
 * 	07	Intermittent rivers -- additional
 * 	08	Intermittent rivers -- minor
 * 	10	Major canals
 * 	11	Canals of lesser importance
 * 	12	Canals -- irrigation type
 */
# include <stdio.h>
# include <stdlib.h>
# include <errno.h>
# include <math.h>
# include <sys/file.h>
# ifdef AIXV3
# include <fcntl.h>
# else
# include <sys/fcntl.h>
# endif

# define SHORTFLAG 0x4000
# define CBD_MAGIC 0x20770002
# define CBD_MAGIC_INV 0x02007720	/* Inverted version of the magic # */

# define TRUE	1
# define FALSE	0

int DoSwap;	/* is byte swapping necessary? */

/*
 * Compressed binary data file header
 */
typedef struct _CBDhead
{
	long	magic;		/* Magic number */
	long	dictaddr;	/* Offset of segment dictionary in file */
	long	segcount;	/* Number of segments in file */
	long	dictbytes;	/* Length of segment dictionary */
	long	segmax;		/* Size of largest segment's strokes, /2 */
	long	fill[5];	/* Filler */
} CBDhead;

/*
 * Segment dictionary
 */
typedef struct _DictionaryEntry
{
	long	segid;		/* segment ID */
	long	maxlat, minlat, maxlong, minlong; /* segment bounds */
	long	absaddr;	/* starting position of segment in file */
	short	nbytes;		/* size of the data portion of the segment */
	short	rank;		/* "rank" of the segment (see CBD.doc) */
} DictionaryEntry, *Dictionary;

/*
 * Structure for a map segment.  Lat and long positions are stored in
 * integer seconds (= degrees * 3600.0)
 */
typedef struct _SegHeader
{
	long	lonorg, latorg;	/* origin (first point) of this segment */
	long	id;		/* segment ID */
	short	nstrokes;	/* number of strokes in the segment */
	short	dummy;		/* filler */
} SegHeader;

typedef struct _Segment
{
	SegHeader	hdr;
	unsigned short	data[1];	/* this is expanded as necessary */
} Segment;

/*
 * The highest segment "rank" we need to handle
 */
# define MAXRANK	15

/*
 * Protos
 */
void	Usage (char *ourname);
void	get_CBDheader (int ifile, CBDhead *header);
Segment	*get_segment (int ifile, Dictionary dict, int which);
Dictionary	get_dictionary (int ifile, CBDhead *header);
void	swap4 (long *addr, int count);
void	swap2 (short *addr, int count);




main (argc, argv)
int	argc;
char	**argv;
{
	int	i, rank, ifile, idx, idy, olt, oln;
	int	dpos, stroke, iseg, nstrokes;
	int	good_rank[MAXRANK + 1];
	char	*filename;
	unsigned short	s;
	float	*lat, *lon;
	CBDhead	header;
	Dictionary	dict;
	Segment		*seg;

	if (argc < 3)
	    Usage (argv[0]);
/*
 * Build the list of ranks we'll accept
 */
	for (i = 0; i <= MAXRANK; i++)
		good_rank[i] = FALSE;

	for (i = 2; i < argc; i++)
	{
		rank = atoi (argv[i]);
		if (rank <= 0 || rank > MAXRANK)
		{
			fprintf (stderr, "Invalid rank '%s' ignored\n", 
				 argv[i]);
			continue;
		}
		good_rank[rank] = TRUE;
	}
/*
 * Open the map file
 */	
	if ((ifile = open (argv[1], O_RDONLY, 0)) < 0)
	{
		fprintf (stderr, "Error %d opening '%s'\n", errno, argv[0]);
		exit (1);
	}
/*
 * Read in the file header, check it for the correct magic number,
 * and learn the address of the segment dictionary
 */
	get_CBDheader (ifile, &header);
/* 
 * Go read in the segment dictionary
 */
	if (!(dict = get_dictionary (ifile, &header)))
		exit (1);
/*
 * Allocate lat and lon arrays
 */
	lat = (float *) malloc (header.segmax * sizeof (float));
	lon = (float *) malloc (header.segmax * sizeof (float));
/*
 * Now loop through the segments
 */
	for (iseg = 0; iseg < header.segcount; iseg++)
	{
		if (! good_rank[dict[iseg].rank])
			continue;

		seg = get_segment (ifile, dict, iseg);

		oln = seg->hdr.lonorg;
		olt = seg->hdr.latorg;

		lat[0] = (float) olt / 3600.;
		lon[0] = (float) oln / 3600.;

		nstrokes = seg->hdr.nstrokes + 1;

		dpos = 0;

		for (stroke = 1; stroke < nstrokes; stroke++)
		{
			if (seg->data[dpos] & SHORTFLAG)
			{
			/* 
			 * Flag bit on: unpack a 16-bit field 
			 * into dx and dy
			 */
				s = seg->data[dpos++];

				idy = s & 0xff;
				if (idy & 0x80)
					idy -= 0x100; /* extend sign */

				idx = s >> 8;
				if (idx & 0x80)
					idx -= 0x100; /* extend sign */
				else
					idx -= 0x40;
			}
			else
			{
			/* 
			 * Flag bit off: take dx and dy from 32-bit 
			 * fields.
			 */
				idx = seg->data[dpos++];
				
				if (idx & 0x8000)
					idx |= SHORTFLAG;
				idx = (idx << 16) | 
					seg->data[dpos++];

				idy = seg->data[dpos++];
				if (idy < 0)
					idy |= SHORTFLAG;

				idy = (idy << 16) | seg->data[dpos++];
			}

			olt = olt + idy;
			oln = oln + idx;

			lat[stroke] = (float) olt / 3600.;
			lon[stroke] = (float) oln / 3600.;
		}
	/*
	 * Write out this segment in Zebra format
	 */
		printf ("%4d %9.3f %9.3f %9.3f %9.3f", 2 * nstrokes, 
			(float) dict[iseg].maxlat / 3600., 
			(float) dict[iseg].minlat / 3600., 
			(float) dict[iseg].maxlong / 3600., 
			(float) dict[iseg].minlong / 3600.);

		for (stroke = 0; stroke < nstrokes; stroke++)
		{
			if (! (stroke % 4))
				printf ("\n");
			printf (" %9.3f %9.3f", lat[stroke], lon[stroke]);
		}
		printf ("\n");
	}

	close (ifile);
}




void
Usage (char *ourname)
{
    fprintf (stderr, "Usage: %s <filename> <rank> [<rank> ...]\n",
	     ourname);
    fprintf (stderr,
"	The \"rank\" of segments within a file is an indication of what the \n"
"	segment depicts: \n"
"	\n"
"	In \"Boundary\" files (bdy): \n"
"		01	Demarcated or delimited boundary (country border)\n"
"		02	Indefinite or in Dispute \n"
"		03	Other line of separation of sovreignty on land \n"
"	\n"
"	In \"Coast, Islands and Lakes\" files (cil) \n"
"		01	Coast, islands and lakes that appear on all maps \n"
"		02	Additional major islands and lakes \n"
"		03	Intermediate islands and lakes \n"
"		04	Minor islands and lakes \n"
"		06	Intermittent major lakes \n"
"		07	Intermittent minor lakes \n"
"		08	Reefs \n"
"		09	Salt pans -- major \n"
"		10	Salt pans -- minor \n"
"		13	Ice Shelves -- major \n"
"		14	Ice Shelves -- minor \n"
"		15	Glaciers \n"
"	\n"
"	In \"Political Boundary\" (pby) file, North America only:\n"
"		01	State or province boundary \n"
"	\n"
"	In \"Rivers\" files (riv) \n"
"		01	Permanent major rivers \n"
"		02	Additional major rivers \n"
"		03	Additional rivers \n"
"		04	Minor rivers \n"
"		05	Double lined rivers \n"
"		06	Intermittent rivers -- major \n"
"		07	Intermittent rivers -- additional \n"
"		08	Intermittent rivers -- minor \n"
"		10	Major canals \n"
"		11	Canals of lesser importance \n"
" 		12	Canals -- irrigation type\n");
    exit (1);
}




void
get_CBDheader (int ifile, CBDhead *header)
{
/*
 * Read the CBD header, setting a flag for byte swapping if necessary
 */
	read (ifile, header, sizeof (CBDhead));

	if (header->magic == CBD_MAGIC)
		DoSwap = 0;
	else if (header->magic == CBD_MAGIC_INV)
		DoSwap = 1;
	else
	{
		fprintf (stderr, "File has bad magic number %x (!= %x)\n",
			 header->magic, CBD_MAGIC);
		exit (2);
	}


	if (DoSwap)
		swap4 ((long *) header, sizeof (CBDhead) / 4);

	return;
}



Dictionary
get_dictionary (int ifile, CBDhead *header)
/*
 * Read the segment dictionary from the file
 */
{
	int	i, dlen = header->dictbytes;
	Dictionary dict = (Dictionary) malloc (dlen);

	if (dlen != (sizeof (DictionaryEntry) * header->segcount))
	{
		fprintf (stderr, "Dictionary size mismatch\n");
		return (NULL);
	}
	
	lseek (ifile, header->dictaddr, L_SET);
	i = read (ifile, dict, dlen);

	if (i < dlen)
	{
		fprintf (stderr, "Segment dictionary expecting %d got %d\n",
			 dlen, i);
		free (dict);
		return (NULL);
	}

	if (DoSwap)
	{
		for (i = 0; i < header->segcount; i++)
		{
			swap4 (&(dict[i].segid), 6);
			swap2 (&(dict[i].nbytes), 2);
		}
	}

	return (dict);
}


Segment*
get_segment (int ifile, Dictionary dict, int which)
/*
 * Get the which'th segment from ifile, based on the dictionary given
 */
{
	static Segment	*seg = NULL;
	static int	seg_datalen = 0;
	int		i, databytes = dict[which].nbytes;
/*
 * Make sure we have our static segment allocated, and make sure it has enough
 * space to hold the data for the segment we're grabbing.
 */
	if (! seg)
	{
		seg = (Segment *) malloc (sizeof (Segment) + 
					  databytes);
		seg_datalen = databytes;
	}

/*
 * Get more data space in the segment if necessary
 */
	if (seg_datalen < databytes)
	{
		seg = (Segment *) realloc (seg, sizeof (Segment) + 
					   databytes);
		seg_datalen = databytes;
	}
/*
 * Position in the file and read the non-data portion
 */
	lseek (ifile, dict[which].absaddr, L_SET);
	read (ifile, seg, sizeof (SegHeader));

	if (DoSwap)
	{
		swap4 (&(seg->hdr.lonorg), 3);
		swap2 (&(seg->hdr.nstrokes), 1);
	}
/*
 * Now read the data portion
 */
	read (ifile, seg->data, databytes);
	if (DoSwap)
		swap2 ((short *)seg->data, databytes / 2);
			
	return (seg);
}



void
swap4 (long *addr, int count)
{
	int	i;
	unsigned char	*bytes;
	unsigned long	temp;

	for (i = 0; i < count; i++)
	{
		bytes = (unsigned char *) addr;
		temp = (bytes[3] << 24) | (bytes[2] << 16) | 
			(bytes[1] << 8) | bytes[0];
		*addr++ = temp;
	}
}



void 
swap2 (short *addr, int count)
{
	int	i;
	unsigned char	*bytes;
	unsigned short	temp;

	for (i = 0; i < count; i++)
	{
		bytes = (unsigned char *) addr;
		temp = (bytes[1] << 8) | bytes[0];
		*addr++ = temp;
	}
}
