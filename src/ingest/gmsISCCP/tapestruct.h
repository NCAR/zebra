/*
 * Various structures for the NOAA ISCCP satellite tape format
 */

/* 
 * File header record
 */
typedef struct _fileheader
{
	char	sat_id[8];
	char	skip[54];
	char	start_line[5];		/* starting line # of sampled image */
	char	end_line[5];		/* final line # of sampled image */
	char	start_element[5];	/* first pixel */
	char	stop_element[5];	/* last pixel */
	char	line_sample[5];		/* line sampling rate */
	char	element_sample[5];	/* element sampling rate */
	char	nefl[5];		/* number of effective lines */
	char	nefp[5];		/* number of effective pixels */
	char	ntop[5];		/* north top of earth line */
	char	istop[5];		/* south top of earth line */
	char	what1[286 - 112];	/* ? */
	char	longitude[25];		/* satellite subpoint location in */
	char	latitude[25];		/* decimal degrees */
	char	what2[216];		/* ? */
	char	irtable[256*7];		/* table of counts vs. temp. for IR */
	char	what3[3841];		/* ? */
	char	vistable[256*7];	/* table of counts vs. ? for vis */
	char	what4[210];		/* ? */
	char	line_el[121][14];	/* line and element in 2(f7.2) fmt */
	char	empty[119];		/* unused (makes size 10000 bytes) */
} fileheader;

/*
 * Line control word
 */
typedef struct _lcword
{
	char	type;
	char	un2[32];
	char	year[4];
	char	month[2];
	char	day[2];
	char	gmtime[4];
	char	unused[7];
} lcword;

/*
 * Data block structure
 */
typedef struct _dblock
{
	lcword		lcw;
	unsigned char	pixels[1100];
	char		tail[8]; 
} dblock;

/* 
 * 24 byte header on each record
 */
typedef struct _rechdr {
	char	recnum[5];	/* record number (?) */
	char	reclen[6];	/* record length */
	char	blockcount[3];	/* number of data blocks contained */
	char	blocklen[5];	/* sizeof(dblock) */
	char	lcwsize[3];	/* sizeof(lcword) */
	char	what3[2];	/* ? */
} rechdr;

/*
 * The data record
 */
# define BF		28	/* max # of data blocks per record */

typedef struct _taperec
{
	rechdr	rheader;
	dblock	block[BF];
} taperec;
