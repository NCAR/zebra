#include <math.h>
#include "tapestruct.h"
#define NGRID	11	/* defines the size of the nav grid array */
#define GRIDINC	10.	/* distance in degrees  between grid points */
#define YES	1
#define NO	0

struct grid {
	float Lat, Lon, Element, Line;
	} GRID[NGRID][NGRID];

NavInit(header)
fileheader	*header;
	{
	double atof();
	float

		Lat, Lon,
		Line, Element,
		StartElement,
		StartLine;
	int
		index = 0,
		i, j;	
	char s[20];
	strncpy(s, header->start_element, 5);
	s[5] = '\0';
	StartElement = atof(s);
	strncpy(s, header->start_line, 5);
	StartLine = atof(s);

/* decode the grid point information */

	for(j = 0, Lon = 90; j < NGRID; j++, Lon += GRIDINC)
		for( i = 0, Lat = 50.; i < NGRID; i++, Lat -= GRIDINC)
			{
			GRID[i][j].Lat = Lat;
			GRID[i][j].Lon = Lon;
			strncpy(s,&header->line_el[index][0], 7);
			s[7] = '\0';
			Line = atof(s); 
			strncpy(s,&header->line_el[index][7], 7);
			Element = atof(s);
			GRID[i][j].Line = Line / 2. - StartLine / 2.;
			GRID[i][j].Element = Element / 6. - StartElement /6.;
			
			index++;
			}
	return(0);
	}
/* navigate determines the line and element in the image associated with
the user specified latitude and longitude.  The grid points in the header
have  10 x 10 degree resolution, covering 100 x 100 degrees of the image.
Navigate uses a bilinear interpolation to determine the line and element.
*/
Navigate(Lat, Lon, Line, Element)
float
	Lat, Lon;	/* input requested lat/lon point */
int
	*Element, *Line;	/* output the image line & element */
	{
	float
		value,
		Xrat = 0.,	
		Yrat = 0.,
		yval1, yval2, 
		xval1, xval2;

	int
		RowFinished = NO,
		ColFinished = NO,
		FoundCol = NO,
		FoundRow = NO,
		Row = 0,	/* line-coordinate in GRID */
		Col = 0,	/* element-coordinate in GRID */
		i, j;
/* search for the point in the GRID array that surrounds the desired
latitude and longitude
*/
	if( (int) Lat == -50)
		{
		Row = 10;
		FoundRow = YES;
		}
	else
		{
		for( i = 0; i < NGRID -1; i++)
			{
			if( fabs( Lat - GRID[i][0].Lat ) < .01)
				{
				Row = i;
				RowFinished = YES;
				break;
				}

			if( Lat <= GRID[i][0].Lat && Lat >= GRID[i +1][0].Lat)
				{
				Row = i;
				Yrat = -1. * (Lat - GRID[i][0].Lat) / 10.;

				FoundRow = YES;
				break;
				}
			}
		}
	/* search for longitude */
	if( (int) Lon == 190)
		{
		FoundCol = YES;
		Col = 10;
		}
	else
		{
		for( i = 0; i < NGRID -1; i++)
			{
			if( fabs( Lon - GRID[Row][i].Lon ) < .01)
				{
				Col = i;
				ColFinished = YES;
				break;
				}

			if( Lon >= GRID[Row][i].Lon && 
				Lon <= GRID[Row][i+1].Lon)
				{
				Col = i;
				Xrat =  (Lon - GRID[Row][Col].Lon) / 10.;
				FoundCol = YES;
				break;
				}
			}
		}

	if( ColFinished && RowFinished)
		{
		*Element = Col;
		*Line = Row;
		return(0);
		}

	if( FoundCol == NO || FoundRow == NO)
		{
		*Element = 0;
		*Line = 0;
		return(1);
		}

	/* Now do the bilinear interpolation */
	yval1 = (GRID[Row +1][Col].Line - GRID[Row][Col].Line) * Yrat 
		+ GRID[Row][Col].Line;
	yval2 = (GRID[Row+1][Col+1].Line - GRID[Row][Col +1].Line) * Yrat 
		+ GRID[Row][Col +1].Line;
	value = (yval1 * (1.0 - Xrat)) + (yval2 * Xrat);
	*Line = (int)( value + 0.5);
 
	xval1 = (GRID[Row][Col +1].Element - GRID[Row][Col].Element) * Xrat 
		+ GRID[Row][Col].Element;
	xval2 = (GRID[Row+1][Col+1].Element - GRID[Row +1 ][Col].Element) 
		* Xrat 	+ GRID[Row +1 ][Col].Element;
	value = (xval1 * (1.0 - Yrat)) + (xval2 * Yrat);
	*Element = (int)( value + 0.5);
/*	printf("line = %d ele = %d\n", *Line, *Element);	 */
	return(0);
	}





