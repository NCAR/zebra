#include <stdio.h>
#include <fcntl.h>
#define NROWS	1100
#define NCOLS	1100
#define NPTS	5
#define NX1	NCOLS / NPTS
#define NY1	NROWS / NPTS
extern unsigned int IMAGE[1536][1536];
unsigned int *p;
unsigned char small[NY1][NX1];
int file;
#include "lcw.h"

DumpImage(dtg)
struct lcw *dtg;	/* Line Control Word */
	{
	int i, j;
	int k, m;


	/* average the image down to 1/5 resolution */

#define HALF	2
	long int sum;
	int jlow, jhi, ilow, ihi;
	int file;
	int value;

/*	mgiimage(IMAGE, 0, 0, NCOLS - 1, NROWS - 1);  */
	/* for each box in the small array, get the average */
	for( m = 0; m < NY1; m++)
		{
		jlow = m * NPTS;
		jhi = (m + 1) * NPTS;

		ilow = 0;
		ihi = NPTS;
		for( k = 0; k < NX1; k++)
			{
			sum = 0;

			for( j = jlow; j < jhi; j++)			
				{
				p = &IMAGE[j][ilow];
				for( i = ilow; i < ihi; i++)
					sum +=  *p++;
				}
			value = (int) ( (float) sum / (float) (NPTS * NPTS) + 0.5);

			small[m][k] = value & 0xff;
			ilow += NPTS;
			ihi += NPTS;
			}
		}
	mgiimage(small, 0, 0, NX1 - 1, NY1 - 1);  
	DumpArray(dtg);
	return(0);
	}
