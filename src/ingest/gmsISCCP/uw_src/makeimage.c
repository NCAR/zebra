#include <stdio.h>
#include <fcntl.h>
#define NROWS	1100
#define NCOLS	1100
#define NPTS	5
#define NX1	NCOLS / NPTS
#define NY1	NROWS / NPTS
extern unsigned char IMAGE[NROWS][NCOLS];
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
	float scale;
	int file;
	int value;

/*	mgiimage(IMAGE, 0, 0, NCOLS - 1, NROWS - 1);  */
	/* for each box in the small array, get the average */
	scale = 1. / (float) (NPTS * NPTS);
	for( m = 0; m < NY1; m++)
		{
		jlow = m * NPTS;
		jhi = (m + 1) * NPTS;

		ilow = 0;
		ihi = NPTS;
		for( k = 0; k < NX1; k++)
			{
			sum = 0;
#ifdef old
			ilow = k * NPTS;
			ihi = (k +1) * NPTS;
#endif
			for( j = jlow; j < jhi; j++)			
				for( i = ilow; i < ihi; i++)
					sum +=  IMAGE[j][i];
			value = (int) ( (float) sum * scale + 0.5);

			small[m][k] = value & 0xff;
			ilow += NPTS;
			ihi += NPTS;
			}
		}
/*	mgiimage(small, 0, 0, NX1 - 1, NY1 - 1);   */
	DumpArray(dtg);
	return(0);
	}
int MakeImage(IRorVIS, Year, Month, Day, GMTime)
char IRorVIS;	/* either a "V" or "I" */
char *Year, *Month, *Day, *GMTime;	/* (input) NOT null terminated strings! */
	{
	char
		*Type;
	static char 
		*IR  = "INFRARED", 
		*VIS = " VISIBLE";
/* IR data needs to be modified */
	int i, j;
	char string[40];


/*	transfer(); */
/*	filter(); */
/*	mgiimage(SCREEN, 0, 0, NXPIX -1, NYPIX -1); */
#ifdef flipir
	/* reverse the IR values, to make a positive image */
	if( IRorVIS == 'I') 
		flipir(IMAGE, sizeof(IMAGE)); 
#endif

/*	mgiimage(IMAGE, 0, 0, NCOLS - 1, NROWS - 1); */
/* do a zoomed copy from buffer 2 to 1 */

/*	mgifbfbzm(1, 2, 0,0, 2,2, 256 + 64,256,512 + 64,512,4); */
#ifdef graphics
	mgifb(1,1);	/* show and modify buffer 1*/ 
	mgipln(255);
	mgisyncrb(1);
	mgihue(0);
	/* black out a box for time and date */
	mgibox(450, 850, 800, 890);
	if( IRorVIS == 'I')
		{
		Type = IR;

		}
	else
		Type = VIS;
	sprintf(string, "%s %.2s/%.2s/%.4s %.4s UTC", Type, Month, Day, Year, GMTime);
	mgihue(255);
	mgigfs(460, 860, 0, string);
#endif
	return(0);
	}
#ifdef old
transfer()
	{
	register int i, j;
	int maxx, maxy;
	maxx = 1100;	/* mininum of screen width and image width */
	maxy = 1100 / 2;	/* minimum of screen height and image height */
	for(i = 0; i < maxy; i++)
		for(j = 0; j < maxx; j++)
			SCREEN[i][j] = IMAGE[2 * i + 1][j];
	return(0);
	}
#endif
flipir(data, n)
register int n;
char *data;
	{
	register char *c;
	register int temp;
	c = data;
	while(n--)
		{
		temp = abs(*c - 255);
		*c++ = temp;
		}
	return(0);
	}

#ifdef old
filter()

	{
	int i,j;
	/* flip values of every other row */
	for( i = 0; i < NYPIX; i += 2)
		for(j = 0; j < NXPIX; j++)
		
		{
		SCREEN[i][j] = abs(SCREEN[i][j] - 255);
		}
	return(0);
	}
#endif
StartGraphics()
	{
	int i, value;
	mgiasngp(0,0);
	mgiclearpln(0,-1,0);
	mgifetchgf(1,"7x9_bold");
	mgigf(1);
	/* load a straight black and white gray scale */
	for( i = 0; i < 255; i++)
		{
		value =( i << 16 ) | (i << 8) | i;
		
		mgicm(255 - i, value);
		}
	value = 0xffffff;
	mgicm(255, value);
	}
quit()
	{
	mgideagp();
	close(file);
	exit(0);
	}
int startup = 1;
DumpArray(dtg)
struct lcw *dtg;
	{

	if( startup)
		{
		/* write out the smaller array */
		file = open("IRdata", O_WRONLY);
		if( file < 1)
			{
			perror("output file not open");
			return(1);
			}
			startup = 0;
		}
	write(file, dtg, sizeof(struct lcw));
	write(file, small, sizeof(small));

	return(0);
	}
