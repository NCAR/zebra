/* windbarb - plot a wind barb.  
Input speed (in m/s, knots, whatever), direction in degrees from
north, a size scaling factor, and x,y location of barb,
and reference window in which to plot it. The color used is whatever
the current color is.
Output is a plotted wind barb relative to specified reference window.
Function returns:
	0	- okay
	2	- bad size
	3	- out of memory in array X or Y
	4	- wind speed unrealistically high.
*/
#define MAXPTS		100	/* max # of data plotted at once */
#define DegToRad	3.14159 / 180.
#include <math.h>

int windbarb( speed, direction, fudge, lon, lat)

float
	lon, lat;	/* latitude and longitude (degs) to plot barb */
float
	speed,		/* wind speed (any units) */
	direction,	/* degrees from north */
	fudge;		/* fudge scaling factor */
	{
	int


		nflags,	/* number of 50 kt wind flags */
		nbarbs,	/* number of 10 kt wind barbs */
		ntics,	/* number of 5 kt half barbs */
		zoom;
	int 
		n = 0,		/* current index in arrays x, y, and connect */
		i;
	float
		length;	/* length of barb post */

	float
		X[MAXPTS], Y[MAXPTS],
		x1, y11,	/* coordinate on post where next tic, barb, or flag is drawn */
		sine,
		cosine,
		Hem,	/* == 1 for northern hemispher, -1 for south */
		ds = 10.;	/* aesthetic  pixel scaling factor */

#ifdef debug
	printf("Spd = %f dir = %f lat = %f lon = %f\n",
		speed, direction, lat, lon);
#endif
	if( speed < 0.)
		return(0);
	if( speed > 400. || direction > 361.)
		return(0);	/* bad data */
	/* set the hemisphere flag, so that barbs point in correct direction */
	if( lat > 0.)
		Hem = 1.;
	else
		Hem = -1.;

	/* set up the scale factor */
	ds *= fudge;
	/* determine the number of flags & barbs */
	nflags = speed / 50.;
	speed -= nflags * 50;
	if( speed >= 47.5)
		{
		nflags++;
		speed = 0.;
		}
	nbarbs = speed / 10.;
	speed -= nbarbs * 10;
	if( speed >= 7.5)
		{
		nbarbs++;
		speed = 0;
		}
	else if( speed >= 2.5 )
		ntics = 1;
	else
		ntics = 0;
#ifdef old
	length = nflags + nbarbs + ntics + 1;
	/* make sure the post is at least 4 units long. */
	if( length < 4)
		length = 4;
#else
	length = 4;
#endif
	/* the algebra below rotates the angle to degrees from north */
	/* scale the sine & cosine by the magnification factor */
	
	cosine = ds * cos( direction * DegToRad);
	sine = ds * sin(direction * DegToRad) ;
	/* draw the post */

	
	X[0] = lon;
	Y[0] = lat;
	/*  Plot a marker at x0, y0 if there are no barbs, tics or flags */

#ifdef old
	if(ntics < 1 && nflags  < 1 && nbarbs < 1)
		gpolymarker(1, p);
#endif
	x1 = X[1] = X[0] + length * sine;
	y11 = Y[1]= Y[0] + length * cosine;
	mgrls(2, X, Y);





	/* draw a triangle for each wind flag */

	n = 0;
	for(i = 0; i < nflags; i++)
		{
		X[n] = x1 +  2. * cosine * Hem;
		Y[n] = y11 -  2. * sine  * Hem;
		++n;
		X[n] = x1  - sine * Hem;
		Y[n] = y11  - cosine * Hem;
		++n;
		x1 -= sine;
		y11 -= cosine;

		}
	
	if( n > MAXPTS)
		return(3);
	if( n > 0)
		mgrls(n, X, Y);

	X[0] = x1;
	Y[0] = y11;
	for( i = 0; i < nbarbs; i++)
		{
		if( i != 0 )
			{
			/* move down the post a step */
			x1 = X[0] = x1 - sine;
			y11 = Y[0] = y11  -  cosine;
			}
		X[1]= x1 + 2 * cosine * Hem;
		Y[1] = y11 - 2 * sine * Hem;
		mgrls(2, X, Y);
		}
	if( ntics)
		{
		/* move down the post */
		x1 -= sine;
		y11 -= cosine;
		X[0] = x1;
		Y[0] = y11;
		/* draw the tic mark */
		X[1] = X[0] + cosine * Hem / 1.4;
		Y[1] = Y[0] - sine  * Hem / 1.4;
		mgrls(2, X, Y);
		}

	return(0);
	}
