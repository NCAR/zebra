/*
 * Handle weather derivation.
 */

# include <Python.h>
# include <math.h>
# include <stdio.h>


/*
 * XXX This is the bad value flag that sounding.py uses.
 */
# define BADFLAG -99999.0

/*
 * Some constants (from Herzegh memo of 10/10/85)
 */
# define _A_	5.0065		/* The underscores are in these names so we  */
# define _B_	19.83923	/* don't have problems with real A's and B's */
# define _R_	287.05
# define E3	6.1078
# define T3	273.15



static PyObject *wder_TempHumToDp (PyObject *, PyObject *);
static PyObject *wder_ThetaE (PyObject *, PyObject *);
static float bolton (float, float, float);

static PyMethodDef WDerMethods[] = {
	{ "TempHumToDp", wder_TempHumToDp, 1},
	{ "ThetaE", wder_ThetaE, 1},
	{ NULL, NULL },
};



void initwder ()
/*
 * Initialize the module.
 */
{
	(void) Py_InitModule ("wder", WDerMethods);
}




static PyObject *
wder_TempHumToDp (self, args)
PyObject *self, *args;
/*
 * Turn temperature and humidity into dewpoint.
 */
{
	float temp, hum, esw, e, dp;
/*
 * Get the args.
 */
	if (! PyArg_ParseTuple (args, "ff", &temp, &hum))
		return NULL;
/*
 * Do the derivation.
 */
	if (temp == BADFLAG || hum == BADFLAG || hum <= 0.0)
		dp = BADFLAG;
	else
	{
	/*
	 * Derive the dewpoint per note 5 from the Herzegh memo
	 * of 10/10/85, using e = (hum / 100) * esw.
	 */
		esw = E3 * exp (_A_ * log (T3 / (temp + T3))) *
				exp ((_A_ + _B_) * (1.0 - T3 / (temp + T3)));
		e = hum / 100.0 * esw;
		dp = (237.3 * log (e / E3)) / (17.2694 - log (e / E3));
	}
/*
 * Build and return the result.
 */
	return (Py_BuildValue ("f", dp));
}




static PyObject *
wder_ThetaE (self, args)
PyObject *self, *args;
/*
 * ThetaE (temp, dp, pres)
 * Calculate theta-e.
 */
{
	float temp, dp, pres, thetae;
/*
 * Get the args.
 */
	if (! PyArg_ParseTuple (args, "fff", &temp, &dp, &pres))
		return NULL;
/*
 * Do the derivation.
 */
	if (temp == BADFLAG || dp == BADFLAG || pres == BADFLAG)
		thetae = BADFLAG;
	else
		thetae = bolton (pres, temp, dp);
/*
 * Return the result.
 */
	return (Py_BuildValue ("f", thetae));
}



/*
	Compute equivalent potential temperature 
	using Bolton's (1980) method. 
	Monthly Weather Review, pg 1046.

	Written by Dean Churchill, Nov. 88.

*/
static float bolton(press, temp, dewpoint)
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

