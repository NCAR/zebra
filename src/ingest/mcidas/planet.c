/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 PLANET.C 27-Feb-96,13:15:54,`ROBERTM' initial checkin of DMSP nav       */
/* 2 PLANET.C 17-Apr-96,14:53:32,`USER' Released                             */
/* 3 PLANET.C 27-Sep-96,16:29:50,`ROBERTM' include mcidasp.h; make M0PLplot  */
/* 4 PLANET.C 22-Oct-96,19:43:02,`USER' Released                             */
/**** McIDAS Revision History *** */

/* planet.c -- routines with dependencies on planet geometry (such
 *	       things as nadir and horizon calculations, geodetic
 *	       latitudes).
 */


/* This is the header file for the 'planet' module. 
 * The planet module provides physical characteristics
 * for the 'current' planet set by planetInit.
 * The data structures for 'planet' are all contained
 * in planet.c -- these routines provide the only access
 *
 * The planet module also works closely with the vector
 * coordinate transforms (vector.c), specificially in
 * the area of geodetic latitude conversions, as these
 * are ambiguous without a specified planetary geometry.
 * Members involving geodetic latitudes have been moved
 * to 'planet.c' as they must be subject to dynamic
 * linking. The remaining vector routines in 'vector.c'
 * are purely algorithmic.
 */

/*
 * This module encapsulates physical constants and performs planet-
 * specific coordinate transforms.  A call to M0PLinit() sets
 * the "current planet" to that specified. Currently only
 * "earth" and a hightly oblate planet "test" are available.
 *
 * The vernal equinox routine is adapted from the McIDAS
 * (FORTRAN) version and optimized. It is, of course, only
 * valid for Earth.
 *
 * Prerelease Changes:
 * 95.10.25 rtm Initial 'code complete' for core. Appears
 *		to pass unit test.
 * 95.06.02 rtm Make changes suggested by BethA.
 * 95.02.22 rtm Rename static data and functions to begin
 *		with PLNX for dynamic linking emulation
 * 95.02.09 rtm planetVerneq documented; assertion added
 *		to trap planets other than earth.
 * 95.01.30 rtm Reworked planetVecDown to compute local vertical
 *		directly from direction cosines rather than
 *		by differencing. It's faster and will work
 *		even for surface points.
 * 95.01.19 rtm	Change log added. Unit test done.
 * 
 */

/********************************************
 * INCLUDES
 *******************************************/

#include <ctype.h>
#include <float.h>
#include <math.h>
#include <string.h>
#include "mcidas.h"
#include "mcidasp.h"
#include "m0frame.h"
#include "m0gpnav.h"


/********************************************
 * TYPEDEF for planet information storage
 *******************************************/

typedef struct {
	char	*szName;	/* Planet name 			*/
	double	re;		/* Equatorial radius (km)	*/
	double	rp;		/* Polar radius (km)		*/
}
M0PLinfo;


/*********************************
 * SUPPORT FOR MULTIPLE INSTANCES 
 ********************************/

static M0ND
*hNDpl_data = NULL;


/********************************************
 * FUNCTION DEFINITIONS
 *******************************************/


/*
*| Name:
*|      M0PLinit - Sets current planet.
*|
*| Interface:
*|      #include "m0gpnav.h"
*|
*|      int
*|      M0PLinit(int handle, char *szName)
*|
*| Input:
*|      handle   - Unique identifier of navigation instance.
*|      szName   - Name of planet to make current.
*|
*| Input and Output:
*|      none
*|
*| Output:
*|	none
*|
*| Return values:
*|       0       - Success.
*|      -1       - Could not initialize planet data collection.
*|      -2       - Could not allocate copy of planet name.
*|      -3       - No known planet with that name.
*|      -4       - Could not allocate space to save planet
*|                 information.
*|
*| Remarks:
*|      This routine allocates memory, in the form of
*|	a lower-case copy of the planet name. If a given
*|	handle is re-used, the old planet information,
*|	including the name, is freed first.
*|
*|      The only currently accepted planets are "earth"
*|	and "test" and the longitude of vernal equinox is only
*|	available for the former.  Physical constants are from
*|	Bate, R. R. Mueller, D. D. and J. E. White 1971:
*|	Fundamentals of astrodynamics. Dover Publications,
*|	Inc., New York. 455 pp. (page 94). They may not be
*|	equal to the 'official' ones used elsewhere in McIDAS
*|
*| Categories: 
*|	navigation  
*/

int
M0PLinit( int handle, char *szName )
{
	char		*szName_lc;	/* local copy of name	*
					 * to make lower case	*/

	int		rc;		/* function return code	*/

	M0PLinfo	PLinfo;		/* Physical constants	*
					 * for this instance's	*
					 * planet		*/

	/* If this is the first call, initialize the planet	*
	 * data collection. If there is already a planet set	*
	 * for this instance, free its dynamically allocated	*
	 * name before proceeding				*/

	if( hNDpl_data == NULL ) {
		rc = M0NDnew( sizeof(M0PLinfo), &hNDpl_data);
		if( rc < 0 ) {
			return -1;
		}
	}

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);
	if( rc == 0 ) {
		M0freeMemory( (void **)&PLinfo.szName);
	}

	/* Allocate a copy (lower case) of the current planet	*
	 * and save it and the physical constants. Then add	*
	 * it to the collection.				*/

	szName_lc = M0strdup(szName);
	if( szName_lc == NULL ) {
		return -2;
	}	
	Mclocase( szName_lc );

	if( strcmp( szName_lc, "earth" ) == 0 ) {
		PLinfo.szName = szName_lc;
		PLinfo.re	= 6378.145;
		PLinfo.rp	= 6356.785;
	}
	else if( strcmp( szName_lc, "test" ) == 0 ) {
		PLinfo.szName	= szName_lc;
		PLinfo.re	= 6000.;
		PLinfo.rp	= 3000.;
	}
	else {
		return -3;
	}

	rc = M0NDadd( hNDpl_data, handle, &PLinfo, sizeof(PLinfo));
	if( rc < 0 ) {
		return -4;
	}
	return 0;
}



/*
*| Name:
*|      M0PLsize - Returns size and shape of current planet.
*|
*| Interface:
*|
*|	#include "m0planet.h"
*|
*|	void
*|	M0PLsize(int handle, double *pRe, double *pRp,
*|	  double *pEcc, double *pFlat)
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|      *pRe    - Equatorial radius.
*|      *pRp    - Polar radius.
*|      *pEcc   - Eccentricity.
*|      *pFlat  - Flattening.
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      Radii are in km. Eccentricity and flattening are unitless.
*|      Routine M0PLinit() must be called first to initialize the
*|      module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/


void
M0PLsize(int handle, double *pRe, double *pRp,
  double *pEcc, double *pFlat)
{
	int		rc;		/* return code		*/

	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rc==0);

	/* Extract equatorial and polar radii; compute		*
	 * eccentricity and flattening				*/

	*pRe = PLinfo.re;
	*pRp = PLinfo.rp;

	*pFlat	= (*pRe - *pRp) / *pRe;
	*pEcc	= sqrt (*pFlat*(2.-*pFlat)); 

	return;
}



/*
*| Name:
*|      M0PLintersect - Computes intersection of vector and planet.
*|
*| Interface:
*|	#include "mcidas.h"
*|	#include "m0planet.h"
*|
*|	M0flag
*|	M0PLintersect(int handle, M0PT PTsat, M0VC VCptg, M0PT *pPTsfc )
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|      PTsat   - Position of satellite (Cartesian).
*|      VCptg   - Pointing vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|      pPTsfc  - Intersection of vector with planet surface.
*|
*| Return values:
*|      M0TRUE  - Vector intersects planet.
*|      M0FALSE - Vector misses planet, *pPsfc is undefined.
*|
*| Remarks:
*|	    Routine M0PLinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*|	ROUGH TERRAIN: This algorithm NEGLECTS height above
*|	the reference ellipsoid used to approximate the planet.
*|	It may therefore require modification to compute
*|	accurate surface postions when viewing angles are
*|	large and terrain is rough (large deviations from
*|	reference).
*|
*|	NATURE OF SOLUTIONS: The intersection of vector and
*|	planet yields a quadratic equation in distance.
*|	- If there are no real roots, the vector does not
*|        intersect the planet. M0PLintersect returns nonzero.
*|	- If the root(s) are both negative, the planet is
*|	  'behind' the satellite and there is no intersection;
*|	  routine returns nonzero.
*|	- If one root is positive and the other negative, the
*|	  satellite is INSIDE the planet. Since this probably
*|	  represents a bug in the caller, it fires an assertion
*|	  if compiled with M0DEBUG defined. Otherwise it 
*|	  returns nonzero.
*|	- If both roots are positive, the routine returns zero
*|	  and the nearer of the two solutions (visible point)
*|	  is computed and placed in pPsfc. The other solution
*|	  corresponds to the exit point of the vector on the
*|        far side of the ellipsoid.
*|
*|	QUADRATIC ROOT FINDER:  The analytic solution for the
*|	quadratic equation representing this problem for a geoid
*|      with no height perturbations is patterned after that in
*|	Numerical Recipes (FORTRAN) p. 145.
*|
*| Categories: 
*|	navigation  
*/

M0flag
M0PLintersect(int handle, M0PT PTsat, M0VC VCptg, M0PT *pPTsfc)
{
	double	ecc;		/* eccentricity of planet	*/
	double	flat;		/* flattening of planet		*/
	double	re;		/* Equatorial radius of planet	*/
	double	re2;		/* Equatorial radius squared	*/
	double	rerp2;		/* re/rp squared		*/
	double	rp;		/* Polar radius of planet	*/

	double	A, B, C;	/* Quadratic coefficients */
	double	D, Q;		/* quadratic solver terms */
	double	F, F1, F2;	/* solutions to quadratic */

	double	u, v, w;	/* Pointing vector
				 * components			*/
	double	xs, ys, zs;	/* Components of satellite
				 * position			*/
	int		rc;	/* return code		*/

	M0PLinfo	PLinfo;	/* physical constants	*
				 * for current planet	*/



	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rc==0);

	/* Validate input coordinate types	*/

	M0ASSERT ( M0VCvalid_type(VCptg.type) ); /* corrupt VC	*
						 * type		*/
	M0ASSERT ( VCptg.type == PTsat.type );  /* non-Cartesian*
						 * satellite	*
						 * position 	*/


	/* For M0DEBUG compilation, prefill surface position
	 * with garbage to trigger a loud 'bang' if caller
	 * improperly handles return code and tries to use
	 * the position */

#ifdef M0DEBUG 
	memset ( (void *)pPTsfc, M0GARBAGE, sizeof(M0PT) );
#endif


	/* Get geometry of current planet. Precompute
	 * necessary secondary constants re2 and rerp2 */

	M0PLsize( handle, &re, &rp, &ecc, &flat);
	re2	= re*re;
	rerp2	= re2/rp/rp;

	/* Capture components of satellite position and pointing
	 * vector in temporary variables to make it easier to read */

	xs	= PTsat.c[0];
	ys	= PTsat.c[1];
	zs	= PTsat.c[2];

	u	= VCptg.c[0];
	v	= VCptg.c[1];
	w	= VCptg.c[2];


	/* compute quadratic coefficients	*/

	A =     u * u + v * v + rerp2* w* w;
	B = 2.*(xs* u + ys* v + rerp2*zs* w);
	C =     xs*xs + ys*ys + rerp2*zs*zs - re2;


	/* compute discrimminant and return M0FALSE if
	 * there are no real solutions (Pointing vector
	 * never intersects the geoid) */

	D = B*B - 4.*A*C;
	if ( D < 0. ) {
		return M0FALSE;
	}


	/* solve for roots               	*/

	Q = - ( B + B/fabs(B)*sqrt(D) ) / 2.;
	F1 = Q/A;
	F2 = C/Q;


	/* Reject the solution if both roots are negative. This
	 * means that the geoid is 'behind' the pointing vector */

	if ( F1 < 0 && F2 < 0 ) {
		return M0FALSE;	
	}


	/* If both roots are positive, choose the smaller one.
	 * One positive and one negative root should happen
	 * only when the satellite is inside the geoid.
	 * Assert on this under M0DEBUG compilation as it
	 * probably indicates an error in the caller. */

	if ( (F1*F2) < 0. ) {
		M0ASSERT(M0FALSE);	/* Satellite inside geoid? */
		return M0FALSE;
	}
	else {
		F = F1<F2 ? F1 : F2;
		VCptg 	= M0VCscale  ( F,    VCptg );
		*pPTsfc	= M0PToffset ( PTsat, VCptg );
		return M0TRUE;
	}
}

/*
*| Name:
*|      M0PLdown - Computes local vertical through a point.
*|
*| Interface:
*|	#include "mcidas.h"
*|	#include "m0planet.h"
*|
*|	M0VC
*|	M0PLdown(int handle, M0PT PT)
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|      PT      - Position above spheriod surface.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	Unit vector through PT and normal to the spheroid
*|	surface, directed towards the interior. The vector
*|	coordinate system type (celestial or terrestrial)
*|	is consistent with that of the input position.
*|
*| Remarks:
*|      Routine M0PLinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|      are undefined.  When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| 	The geodetic latitude is defined as the angle between
*|	local vertical (normal to plane tangent to spheroid)
*|	and the equatorial plane. Once the position is known
*|	in geodetic coordinates, the direction cosines of local
*|	vertical can be easily computed.
*|
*| Categories: 
*|	navigation  
*/

M0VC
M0PLdown(int handle, M0PT PT)
{
	int		rc;		/* return code		*/

	double		adr[3]; 	/* Right ascension /	*
					 * declination / radius	*/
	double		coslat; 	/* cosine of latitude	*/
	double		llh[3];		/* longitude-latitude 	*
					 * (geodetic) and height*/

	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/

	M0VC		VCdown;		/* vector pointing down	*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rc==0);

	M0ASSERT(M0PTvalid_type(PT.type)); /* pos type corrupted */
	
	/* Get position into longitude/geodetic latitude/
	 * height representation */

	if ( PT.type == CC || PT.type == CT ) {
		M0xyz_adr( PT.c, adr );
		M0PLadr_llh( handle, adr, llh);
	}
	else if ( PT.type == ADC || PT.type == ADT ) {
		M0PLadr_llh( handle, PT.c, llh );
	}
	else {				/* type == LLT */
		llh[0] = PT.c[0];
		llh[1] = PT.c[1];
		llh[2] = PT.c[2];
	}

	/* Now  determine output vector type */

	if ( PT.type == CC || PT.type == ADC ) {
		VCdown.type = CC;
	}
	else {
		VCdown.type = CT;
	}

	/* and compute direction cosines of local vertical */

	coslat		=  cos(llh[1]);

	VCdown.c[2]	= -sin(llh[1]);
	VCdown.c[1]	= -sin(llh[0])*coslat;
	VCdown.c[0]	= -cos(llh[0])*coslat;

	return VCdown;
}

/*
*| Name:
*|      M0PLhorizon - Computes angle from local vertical to horizon.
*|
*| Interface:
*|
*|	double
*|	M0PLhorizon(int handle, M0PT PTsat)
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|      PTsat   - Position above spheroid surface.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|      The angle between local vertical (through point) and horizon.
*|
*| Remarks:
*|      Routine M0PLinit() must be called first to initialize the
*|      module before this routine can be used.  "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|      are undefined.  When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*|      The angle is in radians.  If Psat is inside the spheroid,
*|      an angle of pi/2 is returned.  Under M0DEBUG compilation,
*|	this condition asserts as it probably indicates a bug
*|	in the calling routine.
*|	Effects of elevated terrain are neglected.
*|
*| Categories: 
*|	navigation  
*/

double
M0PLhorizon(int handle, M0PT PTsat)
{
	double		horizon;	/* angle to horizon	*/
	double		local_rad;	/* local earth radius	*/
	double		re;		/* equatorial radius	*/
	double		rp;		/* polar radius		*/
	double		ecc;		/* eccentricity		*/
	double		flat;		/* flattening		*/
	double		height;		/* height above geoid	*/

	int		rc;		/* return code		*/

	M0DT		DTcurrent;	/* dummy date for	*
					 * coordinate conversion*
					 * call 		*/

	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/

	M0PT		PTsubpt;	/* subpoint position	*/


	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rc==0);
	DTcurrent = M0DTcurrent();

	M0PLsize(handle, &re, &rp, &ecc, &flat);

        PTsubpt = M0PLpt_convert( handle, PTsat, DTcurrent, LLT);
	height	= PTsubpt.c[2];
	M0ASSERT ( height >= 0. );
	horizon	= 90.*M0D2R;
	if ( height >= 0. ) {
		PTsubpt.c[2]	= 0.;
		PTsubpt	= M0PLpt_convert
		    ( handle, PTsubpt, DTcurrent, ADT );
		local_rad	= PTsubpt.c[2];
		horizon		-= 
		    acos ( local_rad / (local_rad+height) );
	}
	return horizon;
}

/*
*| Name:
*|      M0PLverneq - Returns longitude of vernal equinox.
*|
*| Interface:
*|	#include "mcidas.h"
*|	#include "m0gnav.h"
*|
*|	double
*|	M0PLverneq(M0DT DTepoch)
*|
*| Input:
*|      Handle  - Unique identifier of navigation instance.
*|      DTepoch - Absolute time.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return value:
*|      Longitude of vernal equinox in radians, east positive,
*|      in range pi/2 >= lon_ve > -pi/2.
*|
*| Remarks:
*|      Routine M0PLinit() must be called first to initialize the
*|      module before this routine can be used.  "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|      are undefined.  When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*|      The origin of the algorithm is not known.  This
*|	implementation is adapted from McIDAS module 'verneq.for'
*|	The value returned will be that for the earth regardless
*|      of the current planet,  but use with a planet other than
*|	earth will assert if this module was compiled with M0DEBUG
*|	defined.
*|
*| Categories: 
*|	navigation  
*/

double
M0PLverneq(int handle, M0DT DTepoch)
{
	double		dc;		/* fraction of circum-	*
					 * ference from 0 lon	*/
	double		dc_int;		/* integer part of dc	*/

	int		rc;		/* return code		*/

	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rc==0);

	/* this assertion fires for planets other than Earth
	 * and 'test' */

	M0ASSERT( (strcmp("earth",PLinfo.szName) == 0) || 
	          (strcmp("test", PLinfo.szName) == 0) );

	dc = -0.598514 - 1.002737909*(M0dabtim(DTepoch)-2442348.);
	dc = fmod ( dc,365. ) + 365;
	dc = modf ( dc, &dc_int );
	dc = dc > 0.5 ? dc-1. :  dc;
	return M0TWOPI*dc;
}


/*
*| Name:
*|      M0PLvc_convert - Returns vector components as specified by a
*|                       transformation to a new coordinate system.
*|
*| Interface:
*|
*|	M0VC
*|	M0PLvc_convert(int handle, M0VC VCold, M0DT DT, M0CT new_crd)
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|      VCold   - Vector in any valid coordinate type.
*|      DT      - Date and time (needed for conversion from
*|                celestial to terrestrial or vice versa).
*|      new_crd - Coordinate system of transformed vector.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	Vector in new coordinate system.
*|
*| Remarks:
*|      Routine M0PLinit() must be called first to initialize the
*|      module before this routine can be used.  "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|      are undefined.  When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*|	Conversions are valid only for those planets supported
*|	by vernal equinox computation M0PLverneq.
*|
*| Categories: 
*|	navigation  
*/

M0VC
M0PLvc_convert(int handle, M0VC VCold, M0DT DT, M0CT new_crd)
{
	double		lon_ve; 	/* longitude of vernal	*
					 * equinox		*/

	int		rc;		/* return code		*/

	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/

	M0VC		VCnew;		/* vector after con-	*
					 * version		*/


	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rc==0);
	M0ASSERT ( M0VCvalid_type(VCold.type) ); /* corrupt input vector
						 * type */
	M0ASSERT ( M0VCvalid_type(new_crd   ) ); /* not a vector type */

	/* If types are the same, just do a straight copy.
	 * Otherwise, do the conversion by rotating about
	 * the Z axis relative to the longitude of vernal
	 * equinox and the prime meridian */

	if ( VCold.type == new_crd ) {
		return VCold;
	}
	else {

		/* Determine the longitude of vernal
		 * equinox. Then do direct rotation
		 * of the x and y coordinates. This
		 * gives the most accurate solution
		 * even near the poles */

		lon_ve = M0PLverneq( handle, DT);

		/* rotation is positive by the longitude
		 * of the vernal equinox for a conversion 
		 * from celestial to terrestrial (CC->CT). */

		if ( new_crd == CC ) {
			lon_ve = -lon_ve;
		}
		
		M0zrot(VCold.c, lon_ve, VCnew.c );
	}
	VCnew.type = new_crd;

	return VCnew;
}


/*
*| Name:
*|      M0PLpt_convert - Transforms a point from one coordinate system
*|                       to another.
*|
*| Interface:
*|
*|	M0PT
*|	M0PLpt_convert(int handle, M0PT PTold, M0DT DT, M0CT new_crd)
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|      PTold   - Position in any valid coordinate type.
*|	DT	- date and time (needed for conversion from
*|		  celestial to terrestrial or vice versa)
*|      new_crd - Coordinate system of transformed position.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	Position in new coordinate system.
*|
*| Remarks:
*|      Routine M0PLinit() must be called first to initialize the
*|      module before this routine can be used.  "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|      are undefined.  When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*|	Conversions are valid only for those planets supported
*|	by vernal equinox computation M0PLverneq.
*|
*| Categories: 
*|	navigation  
*/

M0PT
M0PLpt_convert(int handle, M0PT PTold, M0DT DT, M0CT new_crd)
{
	double		lon_ve;		/* longitude of vernal
					 * equinox 		*/

	int		rc;		/* return code		*/

	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/

	M0PT		PTnew;		/* position in new type	*/
	M0PT		PTtmp;		/* temporary position	*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rc==0);
	M0ASSERT ( M0PTvalid_type(PTold.type ) );
	M0ASSERT ( M0PTvalid_type(  new_crd ) );


	/* Get trivial case of 'no change' out
	 * of the way first */

	if ( PTold.type == new_crd ) {
		return PTold;
	}



	/* Now begins huge if block. Essentially this
	 * is a 5x5 matrix of possible conversions
	 * 'from' and 'to', minus the diagonal elements.
	 * In the interest of clarity, the complete set of
	 * steps is executed for each transform, even though
	 * this means some duplicated code. The coordinate
	 * system at the end of each step is shown as the 
	 * end-line comment. */

	/* The longitude of vernal equinox is needed in
	 * every conversion from celestial to terrestrial */

	lon_ve  = M0PLverneq(handle, DT);

	if( PTold.type == LLT ) {

		if      ( new_crd == CT  ) {

			M0PLllh_adr(handle,
			  PTold.c, PTtmp.c);	   	/* ADT  */
			M0adr_xyz (PTtmp.c, PTnew.c);   /* CT   */
		}
		else if ( new_crd == ADT ) {

			M0PLllh_adr(handle,
			  PTold.c, PTnew.c);		/* ADT  */
		}
		else if ( new_crd == ADC ) {

			M0PLllh_adr(handle,
			  PTold.c, PTnew.c);		/* ADT  */
			PTnew.c[0] -= lon_ve;		/* ADC  */
		}
		else /*   new_crd == CC */ {

			M0PLllh_adr( handle,
			  PTold.c, PTtmp.c);	   	/* ADT  */
			PTtmp.c[0] -= lon_ve;		/* ADC  */
			M0adr_xyz  (PTtmp.c, PTnew.c);	/* CC   */
		}
	}
	else if ( PTold.type == CT ) {

		if      ( new_crd == LLT ) {

			M0xyz_adr (PTold.c, PTtmp.c);	/* ADT  */
			M0PLadr_llh( handle,
			  PTtmp.c, PTnew.c);		/* LLT  */
		}
		else if ( new_crd == ADT ) {

			M0xyz_adr(PTold.c, PTnew.c);	/* ADT */
		}
		else if ( new_crd == ADC ) {

			M0xyz_adr(PTold.c, PTnew.c);	/* ADT */
			PTnew.c[0] -= lon_ve;          	/* ADC */
		}
		else /*   new_crd == CC */ {

			M0zrot(PTold.c,
			  -lon_ve, PTnew.c);		/* CC  */
		}
	}
	else if ( PTold.type == ADT ) {

		if      ( new_crd == LLT ) {

			M0PLadr_llh(handle, 
			  PTold.c, PTnew.c);		/* LLT */
		}
		else if ( new_crd == CT  ) {
			
			M0adr_xyz (PTold.c, PTnew.c);	/* CT  */
		}
		else if ( new_crd == ADC ) {

			PTnew = PTold;
			PTnew.c[0] -= lon_ve;		/* ADC */
		}
		else /*   new_crd == CC */ {

			PTtmp = PTold;
			PTtmp.c[0] -= lon_ve;		/* ADC */
			M0adr_xyz(PTtmp.c, PTnew.c);	/* CC  */
		}
	}
	else if ( PTold.type == ADC ) {

		if      ( new_crd == LLT ) {

			PTtmp = PTold;
			PTtmp.c[0] += lon_ve;		/* ADT */
			M0PLadr_llh( handle, 
			  PTtmp.c, PTnew.c);		/* LLT */
		}
		else if ( new_crd == CT  ) {

			PTtmp = PTold;
			PTtmp.c[0] += lon_ve;		/* ADT */
			M0adr_xyz(PTtmp.c, PTnew.c);	/* CT  */
		}
		else if ( new_crd == ADT ) {

			PTnew = PTold;
			PTnew.c[0] += lon_ve;		/* ADT  */
		}
		else /*   new_crd == CC */ {

			M0adr_xyz(PTold.c, PTnew.c);	/* CC   */
		}
	}
	else    /* PTold.type == CC ) */ {

		if      ( new_crd == LLT ) {
			
			M0xyz_adr (PTold.c, PTtmp.c);	/* ADC  */
			PTtmp.c[0] += lon_ve;		/* ADT  */
			M0PLadr_llh( handle,
			  PTtmp.c, PTnew.c);		/* LLT  */
		}
		else if ( new_crd == CT  ) {

			M0zrot(PTold.c,
			  lon_ve, PTnew.c);		/* CT   */
		}
		else if ( new_crd == ADT ) {

			M0xyz_adr(PTold.c, PTnew.c);	/* ADC  */
			PTnew.c[0] += lon_ve;		/* ADT  */
		}
		else /*   new_crd == ADC*/ {

			M0xyz_adr(PTold.c, PTnew.c);	/* ADC  */
		}
	}

	/* Clean up the new position */

	PTnew.type = new_crd;
	M0PTrange( &PTnew);
	return PTnew;
}


/*
*| Name:
*|      M0PLpt_extract - Extracts 3 position components of a point
*|                       which has been converted from one coordinate
*|                       system to another.
*|
*| Interface:
*|
*|	void
*|	M0PLpt_extract(int handle, M0PT PTin, M0DT DT, M0CT crd_type,
*|	  double *pc0, double *pc1, double *pc2)
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|      PTin    - Position in any valid coordinate type.
*|      DT      - Date and time (needed for conversion from
*|                celestial to terrestrial or vice versa).
*|      crd_type- Coordinate system for output position components.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|      pc0     - First position component.
*|      pc1     - Second position component.
*|      pc2     - Third position component.
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    Routine M0PLinit() must be called first to initialize the
*|      module before this routine can be used.  "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*|	The contents of pc0...pc2 depend upon 'crd_type.' For
*|	types CC and CT, they are x, y, and z in km. For
*|	ADT and ADC they are right ascension and declination.
*|	For LLT they are longitude, geodetic latitude, and height
*|	above the approximating spheroid surface.
*|
*| Categories: 
*|	navigation  
*/

void
M0PLpt_extract(int handle, M0PT PTin, M0DT DT, M0CT crd_type,
  double *pc0, double *pc1, double *pc2)
{
	int		rc;		/* return code		*/

	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/

	M0PT		PTout;		/* position in 		*
					 * 'crd_type' coord-	*
					 * inates		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rc==0);
	PTout = M0PLpt_convert( handle, PTin, DT, crd_type);

	*pc0 = PTout.c[0];
	*pc1 = PTout.c[1];
	*pc2 = PTout.c[2];

	return;
}



/*
*| Name:
*|      M0PLadr_llh - Performs a geocentric to geodetic conversion.
*|
*| Interface:
*|
*|	void
*|	M0PLadr_llh (int handle, double adr[3], double llh[3])
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|	adr	- Array containing right ascension, declination,
*|		  and radius (radians, east positive, radians,
*|		  north positive, and km).
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	llh	- Array containing longitude, latitude, and
*|		  height above surface of spheroid, with units
*|                as above for 'adr'.
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      Routine M0PLinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*|	The algorithm is adapted from 'Transformation 3' in
*|	Escobal's "Methods of Orbit Determination"
*|
*| Categories: 
*|	navigation  
*/

void
M0PLadr_llh(int handle, double adr[3], double llh[3])

{
	double		cos_ld_sp;	/* cosine of subpoint	*
					 * declination minus	*
					 * latitude (geodetic)	*/
	double		cos_sp_decl;	/* cosine of subpoint	*
					 * declination		*/	
	double		ecc;		/* eccentricty of	*
					 * planet		*/
	double		ecc2;		/* cosine of declination*
					 * of subpoint, square	*
					 * of eccentricity 	*/	
	double		flat;		/* flattening of 	*
					 * planet 		*/
	double		rc;		/* distance from origin	*
					 * to subpoint		*/
	double		re;		/* equatorial radius	*/
	double		rp;		/* polar radius		*/
	double		rrc;		/* radius scaled by 'rc'*/
	double		sin_ld_sp;	/* sine of subpoint	*
					 * declination minus	*
					 * latitude (geodetic)	*/
	double		sin_sp_decl;	/* sine of subpoint	*
					 * declination		*/
	double		sp_decl;	/* declination of	*
					 * subpoint		*/
	double 		sp_decl_old;	/* previous estimate of	*
					 * above		*/
	double 		tol;		/* numerical tolerance	*/

	int		rcode;		/* return code		*/

	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/


	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rcode = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rcode==0);

	/* set tolerance to the smallest 'double'
	 * increment normalized by the largest latitude */

	tol 	= DBL_EPSILON*acos(0.);

	M0PLsize( handle, &re, &rp, &ecc, &flat );
	ecc2	= ecc*ecc;

	/* The conversion from geocentric to geodetic latitude
	 * for a point on the surface of the spheroid is
	 * analytic. The complication here is that the point
	 * is not necessarily on the surface. It is therefore
	 * necessary to solve iteratively for the subpoint
	 * declination. */

	sp_decl_old = 2.*acos(0.);/* initialize previous declin-
				   * ation with an 'impossible'
				   * value */
	sp_decl	= adr[1];	  /* initialize declination of
				   * subpoint with declination
				   * of input point */	

	do {
		cos_sp_decl = cos ( sp_decl );
		sin_sp_decl = sin ( sp_decl );

		rc	= re * sqrt ( (1.-ecc2)
		        / (1.-ecc2*cos_sp_decl*cos_sp_decl ) );
		rrc	= adr[2] / rc;

		/* Compute geodetic latitude based on
		 * current subpoint declination estimate
		 * Use of atan2, rather than atan, is intended
		 * to avoid difficulties at the pole where
		 * tan(sp_decl) -> infinity. */

		llh[1] = atan2 ( sin_sp_decl,
		         (1.-flat)*(1.-flat)*cos_sp_decl );

		/* Compute sine and cosine of difference betwee
		 * geodetic latitude and declination of subpoint
		 * and then compute the height llh[2] */

		cos_ld_sp   = cos ( llh[1]-sp_decl );
		sin_ld_sp   = sin ( llh[1]-sp_decl );

		/* This is the old form of H computation. It	*
		 * gives results that are slightly too large	*
		 * llh[2] = sqrt ( adr[2]*adr[2]		*
		 *      + rc2*sin_ld_sp*sin_sp_ld )		*
		 *      - rc*cos_ld_sp; 			*
		 * Factoring 'rc' out solves the problem        */

		llh[2]	= rc*(sqrt(rrc*rrc-sin_ld_sp*sin_ld_sp)
			- cos_ld_sp);
		
		/* Then recompute the subpoint declination. Note
		 * that if the height llh[2] is zero there will
		 * be no change from the input declination and
		 * the do { } while() will exit immediately.
		 * Otherwise it will be necessary to recompute
		 * the geodetic latitude and height again with
		 * a better estimate of the declination of the
		 * subpoint */

		sp_decl_old = sp_decl;
		sp_decl     = adr[1] 
			    - asin ( llh[2]*sin_ld_sp/adr[2] );

	}  
	while ( fabs(sp_decl-sp_decl_old) > tol );

	/* Grab the longitude directly from the right ascension.
	 * They're the same. */

	llh[0] = adr[0];

	return;
}

/*
*| Name:
*|      M0PLllh_adr - Performs a geodetic to geocentric conversion.
*|
*| Interface:
*|
*|	void
*|	M0PLllh_adr(int handle, double llh[3], double adr[3])
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|	llh	- Array containing longitude, latitude, and
*|		  height above surface of spheroid, with units
*|                as below, for 'adr'.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	adr	- Array containing right ascension, declination,
*|		  and radius (radians, east positive, radians,
*|		  north positive, and km).
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      Routine M0PLinit() must be called first to initialize the
*|      module before this routine can be used.  "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|      are undefined.  When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*|	The algorithm is adapted from 'Transformation 4' in
*|      Escobal's "Methods of Orbit Determination".
*|
*| Categories: 
*|	navigation  
*/


void
M0PLllh_adr(int handle, double llh[3], double adr[3])

{
	double		cos_sp_dcl;	/* cosine of declination*
					 * of subpoint		*/
	double		ecc;		/* eccentricity of	*
					 * planet		*/
	double		ecc2;		/* eccentricty squared	*/
	double		flat;		/* flattening of planet	*/
	double		re;		/* Equatorial radius	*/
	double		rp;		/* Polar radius		*/
	double		rc;		/* distance from Z axis	*
					 * to subpoint		*/
	double		sp_decl;	/* declination of	*
					 * subpoint		*/

	int		rcode;		/* return code		*/

	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/



	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rcode = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rcode==0);
	M0PLsize( handle, &re, &rp, &ecc, &flat);
	ecc2	= ecc*ecc;

	/* Compute the declination of the subpoint and 
	 * its cosine. Use of atan2 rather than atan here is aimed at
	 * avoiding a singularity near the poles where
	 * tan(llh[1]..the geodetic latitude) becomes very large */

	sp_decl = atan2 ( (1.-flat)*(1.-flat)*sin(llh[1]),
			  cos(llh[1]) );
	cos_sp_dcl= cos ( sp_decl );

	/* Compute the distance from the planet axis to the subpoint.
	 * Make sure that rc is computed in a manner consistent with
	 * M0PLadr_llh version. These algorithms are touchy
	 * regarding rc and adr[2]. Then use that to compute the
	 * radius from Earth center and the declination of the
	 * target point. Note that the latter is a bit different
	 * from the declination of the subpoint if the height
	 * llh[2] is nonzero. */

	rc	= re * sqrt ( (1.-ecc2)
	        / (1.-ecc2*cos_sp_dcl*cos_sp_dcl) );

	adr[2]	= sqrt (
		  rc*rc
		+ llh[2]*llh[2]
		+ 2.*rc*llh[2]*cos(llh[1]-sp_decl) );
	adr[1]	= sp_decl
		+ asin ( llh[2]*sin(llh[1]-sp_decl)/adr[2] );

	/* The right ascension is the same as the longitude */

	adr[0]	= llh[0];

	return;
}


#ifdef M0NVLOG
/*
*| Name:
*|      M0PLplot - Writes lat-lon mesh to plot file.
*|
*| Interface:
*|
*|	void
*|	M0PLplot(int handle, M0DT DTepoch, M0CT crd_type,
*|	  double mnLat, double mxLat, double incLat,
*|	  double mnLon, double mxLon, double incLon,
*|	  double res,   int color,    int special )
*|
*| Input:
*|      handle  - Unique identifier of navigation instance.
*|      DTepoch - Date and time of plot; used to locate meridians
*|                in celestial coordinates.
*|      crd_type- Coordinate type (celestial or terrestrial).
*|      mnLat   - Minimum latitude to plot.
*|      mxLat   - Maximum latitude to plot.
*|      incLat  - Increment of latitude parallels to plot.
*|      mnLon   - Minimum (westernmost) longitude to plot.
*|      mxLon   - Maximum (easternmost) longitude to plot.
*|      incLon  - Increment of longitude meridians to plot.
*|	res	- Line segment length to approximate parallels
*|                and meridians.
*|      color   - Color of parallels and meridians (McIDAS graphics
*|                level).
*|      special - Color of Equator and Prime Meridian (McIDAS
*|                graphics level).
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	none
*|
*| Remarks:
*|      Routine M0PLinit() must be called first to initialize the
*|      module before this routine can be used.  "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|      are undefined.  When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*|	Unlike other routines in this module, inputs are
*|	in DEGREES, east and north positive. This routine is
*|	intended as a debugging aid. Output is written to the
*|	currently opened navigation log file (see module
*|	nvlog.c).
*|
*| Categories: 
*|	navigation  
*/

void
M0PLplot(int handle, M0DT DTepoch, M0CT crd_type,
  double mnLat, double mxLat,
  double incLat, double mnLon, double mxLon, double incLon,
  double res, int color, int special)
{
	M0PLinfo	PLinfo;		/* physical constants	*
					 * for current planet	*/
	int		rc;		/* return code		*/

	M0PT		PTplot;		/* Point to plot next	*/
	double		lat;		/* current latitude	*/
	double		lon;		/* current longitude	*/
	int		cInt;		/* count of intervals	*/
	double		plt_res;	/* actual plot reso-	*
					 * lution (degrees)	*/
	int		i;		/* loop index		*/
	double		plt_lon;	/* actual plotting lon	*
				 	 * -180 to 180		*/
	double		dlon;		/* longitude increment	*/
	double		dmLon;		/* longitude range	*/
	double		dmLat;		/* latitude range	*/

	int		plt_color;	/* current plot color	*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);

	M0ASSERT(rc==0);
	M0ASSERT(crd_type==CT || crd_type==CC);	/* invalid plot type */

	/* Compute resolution for plotting longitudes.
	 * First compute the range of longitudes, then
	 * the number of intervals and the actual
	 * increment in longitude between plot points */

	dmLon = mxLon - mnLon;
	if ( dmLon < 0. ) {
		dmLon += 360.;
		mxLon += 360.;
	}
	M0ASSERT ( dmLon >= 0. );	/* longitude range negative;
					 * funny mnLon and mxLon from
					 * caller ? */
	cInt	= dmLon/res + 1.;
	plt_res	= dmLon / (double)cInt;


	/* This loop plots parallels of latitude */

	for(lat = -90.; lat <= 90.; lat += incLat) {

		/* Skip those parallels that are outside the
		 * plot range */

		if ( lat < mnLat || lat > mxLat ) 
			continue;

		/* for those inside the plot range, plot the
		 * portion of the parallel in segments of length
		 * plt_res degrees */

		for ( i=0, lon=mnLon; i <= cInt; i++, lon += plt_res ) {
			if ( lon > mxLon )  {
				lon = mxLon;
			}

			/* On first point, set color to 0 to move
			 * pen without plotting. Otherwise set
			 * to 'color' or 'special' color if Equator */

			if ( lon == mnLon ) {
				plt_color = 0;
			}
			else {
				if ( fabs(lat) < DBL_EPSILON )
					plt_color = special;
				else
					plt_color = color;
			}
			plt_lon  = ( lon > 180. ) ? lon-360. : lon;

			/* Now make a M0PT position in lat/lon
			 * coordinates and convert it to Celestial
			 * or Terrestrial, as requested */

			PTplot = M0PTmake(M0D2R*plt_lon, M0D2R*lat,
			  0., LLT );
			PTplot = M0PLpt_convert( handle, PTplot,
			  DTepoch, crd_type );
			M0NLpt(handle, plt_color, PTplot);
		}
	}

	/* Compute the range and increment in latitude for
	 * drawing longitude meridian segments */

	dmLat = mxLat - mnLat;
	M0ASSERT ( dmLat >= 0. );
	cInt	= dmLat / res + 1.;
	plt_res	= dmLat / (double)cInt;

	/* this loop plots meridians of longitude */

	for ( lon = -180; lon <= 180.; lon += incLon ) {

		/* compute current longitude; if it is
		 * outside the requested range, skip it */

		dlon = lon-mnLon;
		dlon = ( dlon < 0. ) ? dlon+360. : dlon;

		if ( dlon < 0 || dlon > dmLon )  {
			continue;
		}

		/* now plot the portion of the meridian between
		 * the latitude range */

		for (i=0,lat=mnLat; i<=cInt; i++,lat += plt_res) {

			/* if the first point on this meridian,
			 * set the color level to zero to move
			 * the pen without drawing. Otherwise
			 * use 'color' except for the Prime
			 * Meridian, then use 'special.' */


			if ( lat == mnLat ) {
				plt_color = 0;
			}
			else {
				if ( fabs(lon) < DBL_EPSILON ) 
					plt_color = special;
				else
					plt_color = color;
			}

			/* Now create a M0PT position at the
			 * plot location in lat/lon coordinates
			 * and convert it to Cartesian */

			PTplot = M0PTmake( M0D2R*lon, M0D2R*lat,
			  0., LLT);
			PTplot = M0PLpt_convert( handle, PTplot,
			  DTepoch, crd_type);
			M0NLpt(handle, plt_color, PTplot);
		}
	}

	return;
}
#endif


#ifdef M0DEBUG

void
M0PLmemchk(int handle)
{
	int		rc;	/* return code			*/
	M0PLinfo	PLinfo;	/* this instance's planet data	*/

	/* Validate the planet data collection, then the planet
	 * name string for this instance */

	M0NDmemchk( hNDpl_data );
	rc = M0NDget( hNDpl_data, handle, sizeof(M0PLinfo), &PLinfo);
	M0ASSERT(rc == 0);
	M0noteMemRef( PLinfo.szName);

	return;
}

#endif



/* UNIT TEST    UNIT TEST    UNIT TEST    UNIT TEST */

/* The following code produces a stand-alone unit test when
 * this module is compiled with UNIT_TEST defined. The unit test
 * is not guaranteed to have been kept up to date but is a
 * convenient starting point for testing particular routines
 * in the module */

#ifdef UNIT_TEST 

#include <stdio.h>


int main(int argc, char* argv[])
{
	double	Re;		/* Equatorial radius		*/
	double	Rp;		/* Polar radius			*/
	double	ecc;		/* Eccentricity			*/
	double	flat;		/* Flattening			*/

	double	u;		/* Pointing vector x component	*/
	double	v;		/* Pointing vector y component	*/
	double	w;		/* Pointing vector z component	*/

	M0PT	PTview;		/* View point ('satellite')	*/
	M0PT	PTsfc;		/* Surface point		*/

	M0VC	VCview;		/* View vector			*/
	M0VC	VCdiff;		/* View vector (computed by
				 * differencing)		*/

	M0DT	DTnow;		/* current time			*/

	M0CT	crd;		/* coordinate type		*/

	int	color;		/* parallel and meridian color	*/
	int	handle;		/* identifier for this instance	*/
	int	special;	/* Equator and Prime Meridian
				 * color	*/
	int	vcolor;		/* pointing vector color	*/
	double	mnLon;		/* minimum longitude to draw	*/
	double	mxLon;		/* maximum longitude to draw	*/
	double	incLon;		/* longitude increment		*/
	double	mnLat;		/* minimum latitude to draw	*/
	double	mxLat;		/* maximum latitude to draw	*/
	double	incLat;		/* latitude increment		*/
	double	res;		/* resolution to draw segments	*/

	char	szBuf[BUFSIZ];	/* text input buffer		*/

	if ( argc < 3 ) {
		fprintf( stderr, "usage: %s <planet> <handle>\n",
		  argv[0] );
		fprintf( stderr,"    <planet> = planet name\n");
		fprintf( stderr,"    <handle> = slot number (>=1)\n");
		return -1;
	}
	sscanf( argv[2], "%d", &handle );
	if ( M0PLinit( handle, argv[1] ) != 0 ) {
		printf ( "cannot recognize planet %s\n", argv[1] );
		return -2;
	}

	M0NLopen(handle, "plantest.log", "planet.c unit test");

	DTnow = M0DTcurrent();

	M0PLsize(handle, &Re, &Rp, &ecc, &flat );
	printf("Physical properties of %s\n", argv[1] );
	printf("Equatorial radius      %10.2lf km\n", Re);
	printf("Polar      radius      %10.2lf km\n", Rp);
	printf("Eccentricity           %10.8lf  \n", ecc);
	printf("Flattening             %10.8lf  \n", flat);
	printf("1/F                    %10.2lf  \n", 1./flat);

	printf ( "Enter parameters for surface plot\n"
	  "mnLat mxLat incLat: " );
	gets(szBuf);
	sscanf ( szBuf, "%lf %lf %lf", &mnLat, &mxLat, &incLat );
	printf ( "mnLon mxLon incLon: ");
	gets(szBuf);
	sscanf ( szBuf, "%lf %lf %lf", &mnLon, &mxLon, &incLon );
	printf ( "resolution (deg.), color, and 0 lat/lon color: ");
	gets(szBuf);
	sscanf ( szBuf, "%lf %d %d", &res, &color, &special );
	printf("pointing vector color: ");
	gets(szBuf);
	sscanf(szBuf, "%d", &vcolor);
	printf ( "coordinate system (CT or CC): " );
	gets(szBuf);
	if ( strstr ( szBuf, "CC" ) != NULL )  {
		crd = CC;
	}
	else {
		crd = CT;
	}

	M0PLplot(handle, DTnow, crd, mnLat, mxLat, incLat,
	  mnLon, mxLon, incLon, res, color, special);

	PTview = M0PTget("Location to view from");
	PTview = M0PLpt_convert(handle, PTview, DTnow, crd);

	printf("nadir angle to horizon %10.2lf \n",
	  M0R2D*M0PLhorizon(handle, PTview) );

	VCview = M0VCget("Pointing vector");
	VCview = M0PLvc_convert(handle, VCview, DTnow, crd);

	if( M0PLintersect( handle, PTview, VCview, &PTsfc) ) {
		M0PTput( "Intersection at", PTsfc );
		VCdiff = M0PTdiff(PTview, PTsfc);
		PTsfc  = M0PLpt_convert( handle, PTsfc, DTnow, LLT );
		M0PTput("LLT form       ", PTsfc);
		printf("Verifying: angle from specified"
		  " to diagnosed pointing vector is %lf\n",
		  M0D2R*M0VCangle(VCview, VCdiff) );
	}
	else {
		printf ( "Pointing vector misses planet\n");
	}

	for ( u = -1.; u <= 1.; u += 0.5 ) {
	for ( v = -1.; v <= 1.; v += 0.5 ) {
	for ( w = -1.; w <= 1.; w += 0.5 ) {

		VCview = M0PTvc(M0PTmake( u, v, w, crd) );
		if( M0VCzero(VCview) )  {
			continue;
		}
		if( M0PLintersect( handle, PTview, VCview, &PTsfc) ) {
			VCview = M0PTdiff(PTview, PTsfc);
			M0NLvc(handle, vcolor, VCview, PTview);
		    printf ( "u v w %lf %lf %lf"
		      " intersect %lf %lf %lf\n",
		      u, v, w, 
		      PTsfc.c[0], PTsfc.c[1], PTsfc.c[2] );

		    /* Add 'sea urchin' spikes based on local
		     * vertical */

		    M0NLvc ( handle, color,
		      M0VCscale(-500.,M0PLdown( handle, PTsfc)),
		      PTsfc);

		}
	}
	}
	}
	M0NLmessage( handle, "done!\n");

	fprintf( stderr, "End of \'planet.c\' unit test\n" );

	return 0;
}

#endif
