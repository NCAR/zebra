/*
	Compute equivalent potential temperature 
	using Bolton's (1980) method. 
	Monthly Weather Review, pg 1046.

	Written by Dean Churchill, Nov. 88.

*/
#include <math.h>

float bolton(press, temp, dewpoint)
float

	press,	/* input pressure in millibars */
	temp,	/* input temperature in C */
	dewpoint;	/* input dewpoint temperature in C */

	{
	static float es();
	double pow(), exp();
	float 
		r,		/* mixing ratio */
		e,		/* vapor pressure */
		tlcl,		/* temperature at lifting condensation level */
		thetae,		/* equiv. potential temperature */
		theta;		/* dry potential temperature */

	temp += 273.16;  /* convert to Kelvin */

	/* If the mixing ratio is small, just return the
	potential temperature 
	*/
	if( dewpoint < -100)
		{
		theta = temp * pow(1000. / press, .2854);
		return(theta);
		}

	e = es(dewpoint, press);
	r = 622. * e / ( press - e);
	/* compute lifting condensation level temperature */


	tlcl = 2840. / (3.5 * log( temp) - log(e) - 4.805) + 55.;


	thetae = temp * pow(1000. / press, 0.2854 *(1. - .28*.001 * r)) *
		exp((3.376 / tlcl - 0.00254) 
		* r * (1. + 0.81 * 0.001 * r));

	return(thetae);
	}
	
static float es(T)	/* saturation vapor pressure in mb */
float T;	/* temperature in deg C */
	{
	float value;
	double exp();
	value = 6.11 * exp(17.67 * T / (T + 243.5));
	return(value);
	}

