/* generate an average image of satellite data */
#include <stdio.h>
#include <fcntl.h>
#define NROWS	512
#define NCOLS	512
#include "lcw.h"

long int delay = 0;
unsigned char tailend[100000];
main(argc, argv)
int argc;
char *argv[];
	{
	int ntail;
	float average;
	static char label[50];
	int file;
	static char filename[1024];
	int nbytes;
	int ntimes = 0;
	int count = 1;
	unsigned char output[NROWS][NCOLS], small[NROWS][NCOLS];
	long int sum[NROWS][NCOLS];
	char *c;	
	int i,j;
	struct lcw dtg;


	bzero(output, sizeof(output));
	for( i = 0; i < NCOLS; i++)
		for( j = 0; j < NROWS; j++)
			sum[i][j] = 0.;
while(1)
	{
	c = gets(filename);
	if( ! c) break;
	file = open(filename, O_RDONLY);

	if( file < 1)
		{
		perror("IRdata not open");
		break;
		}
	nbytes = read(file, &dtg, sizeof( struct lcw));
	if( nbytes < sizeof( struct lcw))
		{
		puts("End of data");
		break;
		}
	nbytes = read(file, small, sizeof(small));
	if( nbytes <= 0)
		break;
	if( nbytes != sizeof(small))
		{
		printf("Read only %d bytes\n", nbytes);
		}
	ntail = read(file, tailend, sizeof(tailend));
	close(file);
	sprintf(label, "%c %.2s/%.2s/%.4s, %.4s UTC", dtg.Type, dtg.Month,
		dtg.Day, dtg.Year, dtg.GMTime);
	puts(label);
	/* sum the pixel values */
	sumit(sum, small);
	ntimes++;
	}
	Average(output, sum, ntimes);
	/* now write out the average */
	if( argv[1])
		file = open(argv[1], O_WRONLY | O_CREAT, 0777);
	else
		file = open("avepixel", O_WRONLY | O_CREAT, 0777);
	if( file < 1)
		{
		perror("avepixel not open");
		}
	else
		{
		write(file, &dtg, sizeof( struct lcw));
		write( file, output, sizeof(output));
		if( ntail > 0)
			write(file, tailend, ntail);
		close(file);
		}	
	return(0);
	}

sumit(sum, data)
int *sum;
unsigned char *data;
	{
	int nwords = NROWS * NCOLS;

	while( nwords--)
		*sum++ += *data++;
	return(0);

	}
Average(output, sum, ntimes)
unsigned char *output;
long int *sum;
int ntimes;
	{
	int i, 
	nwords = NROWS * NCOLS;
	register float average;
	register float scale;
	/* now determine the average pixel value */
	scale = 1. / (float) ntimes;
	while(nwords--)
			{
			average = (float) *sum++ * scale + 0.5;
			*output++ = (int) average;
			}
	return(0);
	}
