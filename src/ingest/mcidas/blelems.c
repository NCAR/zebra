/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 BLELEMS.C 27-Feb-96,13:12:36,`ROBERTM' initial checkin of DMSP nav      */
/* 2 BLELEMS.C 17-Apr-96,14:47:22,`USER' Released                            */
/* 3 BLELEMS.C 27-Sep-96,16:29:30,`ROBERTM' Include mcidasp.h (7018)         */
/* 4 BLELEMS.C 22-Oct-96,19:40:54,`USER' Released                            */
/**** McIDAS Revision History *** */

/* blelems.c
 *
 *     This module provides re-entrant support for the Brouwer-Lyddane
 * orbit prediction model by providing a place for the storage and
 * recovery of multiple sets of orbital elements.
 * C-interface routine M0BLsetels() is used to set the elements,
 * and FORTRAN-callable routine m0blgetels() is used to recover them
 */

/***********************************
 * INCLUDES
 **********************************/

#include <string.h>
#include "mcidas.h"
#include "mcidasp.h"
#include "m0frame.h"
#include "m0gpnav.h"


/**********************************************************
 * TYPEDEF for orbital element storage
 *********************************************************/

typedef struct {
	Fdouble	epoch;	/* Epoch time, Julian Date and fraction	*/
	Fdouble	smaxis;	/* Semi-major axis (km)			*/
	Fdouble	ecc;	/* Eccentricity (unitless)		*/
	Fdouble	incl;	/* Inclination 				*/
	Fdouble	asnod;	/* Right ascension of ascending node	*/
	Fdouble	argper;	/* Argument of periapsis		*/
	Fdouble	manom;	/* Mean anomaly				*/
}
BLelems;

/*********************************
 * SUPPORT FOR MULTIPLE INSTANCES 
 ********************************/

static M0ND
*hNDbl_data;


/**********************************************************
 * DEFINITIONS of Brouwer orbital element stacker functions
 *********************************************************/

/*
*| Name:
*|	M0BLsetels - store Brouwer-Lyddane elements for
*|		     this instance
*|
*| Interface:
*|
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0blsetels(int handle, double epoch,
*|	  precision smaxis, precision    ecc,
*|	  precision   incl, precision  asnod,
*|	  precision argper, precision  manom);
*|
*| Input:
*|	handle	- current navigation instance
*|	epoch	- Epoch time, Julian Date and fraction
*|	smaxis	- Semi-major axis (km)
*|	ecc	- Eccentricity (unitless)
*|	incl	- Inclination (degrees)
*|	asnod	- Right ascension of ascending node (degrees)
*|	argper	- Argument of periapsis (degrees)
*|	manom	- Mean anomaly (degrees)
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	 0	- success
*|	-1	- could not allocate space for this element set
*|	-2	- could not allocate space for element collection
*|
*| Remarks:
*|	    This routine allocates memory via a call to M0NDnew().
*|	'Handle' should be a (small) positive integer; m0nchdl()
*|	will assign a handle for you.
*|
*| Categories: 
*|	navigation  
*/

int
M0BLsetels(int handle, double epoch, double smaxis,
  double ecc, double incl, double asnod, double argper,
  double manom) 
{
	int	rc;		/* return code			*/
	BLelems	ble;		/* Current Brouwer elements	*/


	/* On first call, initialize the collection */

	if( hNDbl_data == NULL ) {
		rc = M0NDnew( sizeof(BLelems), &hNDbl_data);
		if( rc < 0 ) {
			return -2;
		}
	}


	/* populate an instance of orbital elements ... */

	ble.epoch	= epoch;
	ble.smaxis	= smaxis;
	ble.ecc		= ecc;
	ble.incl	= incl  * M0D2R;
	ble.asnod	= asnod * M0D2R;
	ble.argper	= argper* M0D2R;
	ble.manom	= manom * M0D2R;


	/* ... and add it to the collection */

	rc = M0NDadd( hNDbl_data, handle, &ble, sizeof(BLelems));

	if( rc < 0 ) {
		return -1;
	}
	return 0;
}


/*
*| Name:
*|	m0blgetels - retrieve Brouwer-Lyddane elements for
*|		     this instance
*|
*| Interface:
*|	subroutine
*|	m0blgetels(integer handle, double precision epoch,
*|	  double precision blelems(6) )
*|
*| Input:
*|	handle	- current navigation instance
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	epoch	- Epoch time, Julian Date and fraction
*|	blelems	- Array containing semi-major axis (km), eccentricity,
*|		  inclination (radians), right ascension of
*|		  ascending node (radians), argument of periapsis
*|		  (radians), and mean anomaly (radians)
*|
*| Return values:
*|	 0	- success
*|	-1	- this element set does not exist
*|
*| Remarks:
*|	    'Handle' must have been previously used to store
*|	elements via a call to m0blset_elements(). When compiled
*|	with M0DEBUG defined, this assumption is validated with
*|	an assertion.
*|
*| Categories: 
*|	navigation  
*/

void
m0blgetels_(Fint *F_handle, Fdouble *epoch, Fdouble *blelems)
{
	int	handle;		/* identifier for current	*
				 * instance ('int' form)	*/
	int	rc;		/* return code			*/
	BLelems	ble;		/* Current Brouwer elements	*/


	/* Get the requested orbital element item out of the	*
	 * collection ... */

	handle = *F_handle;

	rc = M0NDget(hNDbl_data, handle, sizeof(BLelems), &ble);

	M0ASSERT(rc == 0);

	/* and extract the goodies for return as individual 	*
	 * arguments */

	*epoch     = ble.epoch;
	blelems[0] = ble.smaxis;
	blelems[1] = ble.ecc;
	blelems[2] = ble.incl;
	blelems[3] = ble.asnod;
	blelems[4] = ble.argper;
	blelems[5] = ble.manom;

	return;
}

#ifdef M0DEBUG

extern void
M0BLmemchk(void) {
	M0NDmemchk(hNDbl_data);
}
#endif

