#include "filehead.h"

FileHeader(file)
int file;	/* file to read from */
	{
	int 
		i,j,
		nbytes;
	float Lat, Lon;

	/* read in the header record */
	nbytes = read(file, &HEADER, HEADSIZE);
	if( nbytes != HEADSIZE)
		{
		printf("Read only %d bytes in header record\n", nbytes);
		return(1);
		}
	ebctoascii(&HEADER, sizeof(HEADER));
/*	MakeGrid(); */
	return(0);
	}
