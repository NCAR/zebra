#include <stdio.h>
#include <fcntl.h>
/* plotmap reads in the geography database and plots it
in the image array.
Only the continental outlines get plotted.
*/
#define MAXLAT	10.
#define MINLAT	-50.
#define MINLON	90.
#define MAXLON	160.
Geography()
	{
#define NPOINTS		1000
	int input;
	int limit;
	int nread, i;

	float 
		Lat, Lon,
		x[NPOINTS], 
		y[NPOINTS];	/* lat & lon */
	int
		code,
		n,
		line, element,
		Line[NPOINTS], 
		Element[NPOINTS];
	struct head {
		long int npts;
		long int grid;
		float flim[4];
		long int type;
		} H;

	MakeGrid();	/* initialize grid data structure */

	input = open ("/u2/dean/DARWIN/ezmapc.dat",  O_RDONLY  );
	if( ! input)
		{
		puts("Input file not open");
		exit(1);
		}
	while(1)
		{
		nread = read ( input, &H, sizeof(H));
		if( nread < sizeof(H))
			{
			printf("Read only %d items\n", nread);
			break;
			}
		limit = H.npts / 2;
		if( limit >= NPOINTS)
			{
			puts("Too many data points\n");
			break;
			}
		if ( limit >  1  )
			{
			nread = read(input, x, 4 * limit);
			nread = read (input,y, 4 * limit);
			/* grid 1 is course continental boundaries */
			/* grid 2 is finer US grid */
			/* grid 3 is just international boundaries */

			if( H.grid > 1)  
				break;	 /* plot only continental lines */
			n = 0; 	/* number of data points in window */
			for( i = 0; i < limit; i++)
				{
				/* window down the geography to my subwindow over Australia */
				if( y[i] < MINLON || y[i] > MAXLON) continue;
				if( x[i] < MINLAT || x[i] > MAXLAT) continue;
				code = Navigate(x[i], y[i], &element, &line);
				if( ! code )
					{
					Line[n] = 1100 - line;
					Element[n] = element;
					n++;

					}
				}
/*			printf("Found %d points\n", n); */
/*			memline(n, Line, Element);*/
			}
		}
	close(input);
	/* Draw in the equator */

for(Lat = MAXLAT; Lat >= MINLAT; Lat -= 10.)
	{
	n = 0;
	for( Lon = MINLON; Lon <= MAXLON; Lon += 1.)
		{
		code = Navigate(Lat, Lon, &element, &line);
		if( ! code)
			{
			Line[n] = 1100 - line;
			Element[n] = element;
			n++;
			}
		}
	memline(n, Line, Element);
	}


	for( Lon = MINLON ; Lon <= MAXLON; Lon += 10.)
		{
		n = 0;
		for(Lat = MAXLAT; Lat >= MINLAT; Lat -= 1.)
			{
			code = Navigate(Lat, Lon, &element, &line);
			if( ! code)
				{
				Line[n] = 1100 - line;
				Element[n] = element;
				n++;
				}
			}
		memline(n, Line, Element);
		}

	return(0);
	}
