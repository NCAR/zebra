/* copygeo copies the geography from one file and
appends to a second file
*/
#include <stdio.h>
#include <fcntl.h>
#include "lcw.h"

main(argc, argv)
int argc;
char *argv[];
	{

#define NX1	512
#define NY1	512


	int i;
	int TopLine, LeftEl;
	int left = 167, bottom = 151, top;
	static char label[50];
	char *b;
	int file, outfile;
	int nbytes, npoints, Line[10000], Element[10000];
	int image;
	int dy = 0;	/* empirical offset for iamges */
	char message[80];
	int count = 0;
	char small[NY1][NX1];
	struct lcw dtg;

	file = open(argv[1], O_RDONLY);
	if( file < 1)
		{
		sprintf(message,"File %s not open", argv[1]);
		perror(message);
		exit(1);
		}
	nbytes = read(file, &dtg, sizeof( struct lcw));
	if( nbytes < sizeof( struct lcw))
		{
		puts("Could not read header");
		exit(1);
		}
	nbytes = read(file, small, sizeof(small));
	if( nbytes <= 0)
		exit(1);
	if( nbytes != sizeof(small))
		{
		printf("Read only %d bytes\n", nbytes);
		}
	sprintf(label, "%.2s/%.2s/%.4s, %.4s UTC", dtg.Month,
		dtg.Day, dtg.Year, dtg.GMTime);

	/* try reading in geography data, if there is any */
	nbytes = read(file, &TopLine, sizeof(int));
	if( nbytes == sizeof(int)) /* geography is present */
		{
		read(file, &LeftEl, sizeof(int));
		left = LeftEl;
		top = 1100 - TopLine;
		bottom = top - 511 - dy;
		}
	else
		{
		puts("No geography is in the input file");
		exit(0);
		}
	outfile = open(argv[2], O_WRONLY | O_APPEND);
	if( outfile < 1)
		{
		puts("Can not open output file");
		exit(0);
		}
	while(1)
		{
		nbytes = read(file, &npoints, sizeof(int));
		if( nbytes < sizeof(int)) 
			break;
		read(file, Line, npoints * sizeof(int));
		read(file, Element, npoints * sizeof(int));
		nbytes = write(outfile, &npoints, sizeof(int));
		if( nbytes < sizeof(int)) 
			break;
		write(outfile, Line, npoints * sizeof(int));
		write(outfile, Element, npoints * sizeof(int));

		} 

	close(file);
	close(outfile);
	exit(0);
	}
