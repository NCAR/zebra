
/*
This program reads in an 1100 by 1100 pixel image
from a NOAA isccp tape format
and places it on the masscomp screen
*/
#define NROWS	1100
#define NCOLS	1100
#include <stdio.h>
#include <fcntl.h>
unsigned char IMAGE[NROWS][NCOLS];

#include "filehead.h"

/* define the physical record structure on tape */
#define RECSIZE	32504	/* maximum size of data records */
#define BF	28	/* max # of data blocks per record */
struct hd { 	/* 24 byte header on each record */
	char
		Unused[6],
		Blocksize[5],
		Un1[13];
/*		Type; */
		} head;
struct
{
struct data
	{ 	/* 1160 byte data blocks. */
	struct lcw {
		char
			Type,
			Un2[32],
			Year[4],
			Month[2],
			Day[2],
			GMTime[4],
			Unused[7];
		} LCW;	/* Line Control Word */
	unsigned char
		Pixels[1100];
	char
		Tail[8]; 
	} block[BF];
} RECORD;
unsigned char BUFFER[RECSIZE];	/* tape record buffer */

main()
	{
	struct lcw *L;
	char junk[90];
	int 
		deof = 0,	/* double end of file marker */
		count = 0,
		filecount = 0,
		reccount = 0,
		limit = 10000,
		i, 
		file,
		line = 0,
		nbytes;

	StartGraphics();
#ifdef old
	printf("Size of RECORD is %d\n", sizeof(RECORD));
	printf("Size of head is %d\n", sizeof(struct hd));
	printf("Size of data block is %d\n", sizeof(struct data));
	printf("Size of LCW is %d\n", sizeof(struct lcw));
#endif
	file = open("/dev/rmt0", O_RDONLY);
	if( file < 1)
		{
		perror("File not open");
		exit(1);
		}

	/* read in the header record */
while(limit--)
	{
	line = 0;
	deof = 0;
	reread:
	nbytes = read(file, &HEADER, sizeof(HEADER));
	reccount++;

	if( nbytes == 0)
		{
		if( deof >= 3) goto finished;
		deof += 1;
		filecount++;
		goto reread;
		}
	deof = 0;
	if( nbytes == 80 ) goto reread;
	if( nbytes != 10000)
		{
		printf("Read only %d bytes filecount %d reccount %d\n", 
			nbytes,filecount,reccount);
		exit(1);
		}
	ebctoascii(&HEADER, sizeof(HEADER));

/* Now start reading in data blocks */
do {
	nbytes = read(file, BUFFER, sizeof(BUFFER));
	reccount++;
/*
copy data from the buffer into the data structure.  I must do this
because the struct hd has an odd number of bytes in it, and the compiler
will not aligned struct data on an odd byte
*/
	memcpy(&head, BUFFER, sizeof(struct hd));
	memcpy(&RECORD, &BUFFER[24], sizeof(RECORD));
	ebctoascii(&head, 24);

	for(i = 0; i < BF; i++)
		{

		ebctoascii(&RECORD.block[i].LCW, sizeof(struct lcw));
		ebctoascii(RECORD.block[i].Tail, 8);
		L = &RECORD.block[i].LCW;
/*		printf("%.4s %.2s %.2s %.4s \n", L->Year, L->Month, L->Day, L->GMTime); */
		}
/* copy pixel data to image array */
	for( i = 0; i < BF; i++)
		{
		if( line >= NROWS)
			break;
		if( RECORD.block[i].LCW.Type != 'I') continue;
		wordcopy(&IMAGE[line][0], RECORD.block[i].Pixels, 1100);
		line++;
		}
	}
	while(nbytes > 0);

#ifdef test
	/* read in the trailing 80 char record */
	nbytes = read(file, junk, 80);
	reccount++;
	if( nbytes != 80)
		{
		printf("Did not find ending record\n");
		quit();	
		}
#endif

	DumpImage(&RECORD.block[0].LCW);
/*	Geography(); */
	/* image is read in, now plot it */

/*	MakeImage('I',RECORD.block[0].LCW.Year, 
			RECORD.block[0].LCW.Month, 
			RECORD.block[0].LCW.Day, 
			RECORD.block[0].LCW.GMTime );
*/
	count++;
	}
finished:
	printf("Read in %d images\n", count);
	exit(0);
	}
/* use 4 byte transfer of character data */
int wordcopy(out, in, n)
register int n;
register unsigned int *in;
register unsigned int *out;
	{
	register int nwords;
	nwords = n/ 4;
	while(nwords--)
		*out++ = *in++;
	return(0);
	}
