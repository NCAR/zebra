

/**** McIDAS Revision History *** */
/* 1 DI.C 27-Feb-96,13:14:04,`ROBERTM' initial checkin of DMSP nav           */
/* 2 DI.C 22-Mar-96,8:04:08,`ROBERTM' recognize flipped DMSP sensor types    */
/* 3 DI.C 17-Apr-96,14:48:14,`USER' Released                                 */
/* 4 DI.C 27-Sep-96,16:30:00,`ROBERTM' Include mcidasp.h (7018)              */
/* 5 DI.C 11-Oct-96,10:31:50,`ROBERTM' free Cheb coefficients when re-       */
/*      initializing (7020)                                                  */
/* 6 DI.C 22-Oct-96,19:41:08,`USER' Released                                 */
/**** McIDAS Revision History *** */


/*      di.c
 *
 * "DATA INTERFACE" module. This module is the link between
 * the specifics of a particular satellite and sensor and
 * generic navigation. It is hoped that all routines which
 * will have to be modified to build a particular navigation
 * package are all contained here.
 *
 *      This module is "re-entrant," i.e. you can have multiple
 * instances of it, with different data, active at once. The instance
 * is selected by an argument 'handle' to many of the routines.
 * The value of 'handle' is determined by the top-level routines
 * M0gpnini(), M0gpnfwd(), M0gpninv(), and M0gpnopt() via a call to
 * M0nchdl(). THESE ROUTINES ALL ASSUME A VALID HANDLE. If you call
 * a routine with a wrong handle, results are undefined. This assumption
 * is validated with an assertion when this module is compiled with
 * M0DEBUG defined.
 *
 *      The purpose of this module is to encapsulate all non-generic
 * (sensor-specific) aspects of the navigation problem.  Some of these
 * such as time between scans or elements, the attitude of a
 * particular sensor's scanning mechanism relative to the spacecraft
 * chassis, etc. are generally fixed. They may have to be hard-coded
 * in here, or perhaps placed in a 'server' routine using a "current
 * sensor" static variable, in the manner of planet.c. Or some of
 * them may actually be in the codicil (although it makes sense
 * to me to use the codicil only for things that change from one
 * data set to the next.
 *      Other quantities change regularly. Such things as start
 * and end time of the scene, number of scans, orbital elements,
 * etc. almost certainly will come in through the codicil (or will
 * be plucked out of system navigation). It is M0DIinit's responsibility
 * to assemble a complete set of navigation parameters from the
 * codicil, system nav, and (in the future) a local sensor
 * characteristics server.
 *      At present 128 word (Fint) navigation codicil is used to
 * pass in variable parameters. It passes from M0gpnini to M0DIinit.
 * In the future this may change but it won't matter much.
 * The information, however it is stored, is still presented to the
 * generic navigation system via the routines in this module.
 */

/* Each 'datum' collected by a satellite has a 'field of view
 * descriptor'. The scan is the count of the data collection cycle
 * (one rotation/oscillation of the 'scanner') from some reference
 * time. The line is an index to the particular detector or channel
 * and the element is the sample index within the scanner cycle. All
 * but MODIS (that I know of) have only one line per scan.
 *
 * The field of view descriptor contains TRUE image coordinates,
 * i.e. those associated with the actual scan geometry.
 * Inputs to the navigation (NVXsae_) may be in the form of
 * flipped image coordinates; these are converted to true coordinates
 * by M0DIfvmake() and back to flipped by M0DIfvextract.
 *
 * FIELD OF VIEW DESCRIPTORS should therefore NEVER contain
 * flipped image coordinates !!!!!
 */

/* "FLIPPED" DATA implementation notes.
 * Many polar-orbiter areas have their elements reversed
 * along a scan, or their scans reversed from top to 
 * bottom in the area, or both, to make them geograph-
 * ically consistent, i.e. NW in the upper left when
 * displayed. The normal relationship of area to image
 * coordinates is preserved so the image coordinate
 * produced from an area location changes. These 'flipped
 * image coordinates' are here converted to true image
 * coordinates; the inverse transform is accomplished in
 * M0DIfv_extract.

 * The transform is based on the following constraints:
 * 1) The flipped and true image coordinates navigable for
 *    a given area have the same domain, i.e. both have
 *    a minimum value of nvblk.firstscan and are valid
 *    for nvblk.nscans image scans; the last scan for which
 *    either true or flipped image coordinates are valid
 *    is nvblk.firstscan + nvblk.nscans - 1. Similar is true
 *    for element, except the start there is always 1 so
 *    element coordinates domain is 1 through nvblk.nelems.
 * 2) The last area line (nalin-1) in the true area maps
 *    to the same image coordinate as the first area line
 *    (zero) in the flipped area.
 *
 * These constraints lead to the following
 *
 *    L' = Lmin + Lmax - L				(1)
 *
 * Where L' is flipped coordinate, Lmin and Lmax are the 
 * defined range of true coordinate (recall from 1 that
 * Lmin = L'min and Lmax = L'max by convention).
 * Lmax is alternatively given by
 *
 *    Lmax = Lmin + nL -1				(2)
 *
 * Where nL is the number of lines for which navigation is
 * defined.
 *
 * The second constraint can be expressed as:
 *
 *     L'0 = Lmin+Lmax - (L0+ml*(nal-1))		(3)
 * 
 * Where L'0 is the flipped image line of the first area
 * line in the flipped area, Lmin and Lmax are as above,
 * L0 is the true image line of the first area line in the
 * true area, ml is the line magnification of the area
 * (integer 1 or more), nal is the number of area lines.
 *
 * Substituting for Lmax and combining terms yields
 *
 *     L'0 = 2*Lmin + nL -L0 - ml*(nal-1) - 1		(4)
 *
 * which provides a means for computing L'0 from 
 * quantities available in the area directory and
 * navigation codicil.
 *
 * Since navigation is operating only on image coordinates,
 * all that needs to be done here is apply (1). The
 * remainder (2-4) are just provided so you will understand
 * the flipping algorithm.
 */



/* Changes and status:
 * 96.02.05 rtm Begin making re-entrant. Move all typedef 
 *		documentation here from header file.
 * 95.08.30 rtm	Clarify comments regarding POES/TIRO flipping in
 *		m0DIXinit().
 * 95.08.24 rtm Provide for re-initialization of Chebyshev polynomials
 *		on repeated calls to m0DIXinit(). This required 
 *		introduction of a new m0MAT type for a resizeable 2-d
 *		matrix and addition of a file-scope variable to trigger
 *		the reset.
 * 95.08.02 rtm Make prototypes for norm_time() and denorm_time()
 *		consistent with definitions. Make casts to (void **)
 *		explicit in fNewMemory() calls. Above changes keep
 *		SGI compiler happy.
 * 95.08.01 rtm	Remove extraneous (commented-out) calls to obsolete
 *		m0DIXposSat() and DIXinit() functions. Make use of
 *		SGP navigation model conditional on #ifdef SGP_ON. 
 * 95.06.16 rtm Added SSM/T and SSM/T2 support.
 * 95.05.08 rtm Added support for POEX codicil; will try to
 *		pick up orbital elements from 'tle.new'
 * 95.05.04 rtm Removed unit test. Added m0DIXinit that can handle
 *		POES/POEB codicils.
 * 95.03.14 rtm	Added handlers for image/area flipping and
 *		documentation of same to DIXfvdMake and DIXfvdExtract
 * 95.03.13 rtm Change sensor types to match codicil documentation
 *		(SSMI -> MI, etc. )
 * 95.03.09 rtm Make OLS scanner amplitude negative (reverse
 *		the scan direction).
 * 95.03.07 rtm	Add oscillating scanner logic to DIXanglesFvd and
 *		DIXfvdAngles
 * 95.03.06 rtm	Begin adding handlers for OLS, SSM/I, and SSM/T
 *		codicils as per format given to Jeff Ramin.
 * 95.02.24 rtm Further increase 'padding' of start and end time
 *              of scene in DIXtimeRange.
 * 95.02.24 rtm Added new DIXinit routine that can really
 *              unpack codicils.
 * 95.02.22 rtm planet module calls prefixed with PLNX. Nav log
 *              calls prefixed with NLX.
 * 95.02.20 rtm Rename module as dix.c. Precede all routines
 *              with DIX
 * 95.02.03 rtm Modify DItimeRange to 'pad' the start and end
 *              time of the scene.
 * 95.01.30 rtm Add DIposInView (for in_open_locus)
 * 95.01.26 rtm Add implementation for DIanglesPtg
 * 95.01.17 rtm SGP positioning tested. Chassis roll and yaw tested
 *              for 180 degree rotations. Scanner pitch tested
 *              for +/-90 degree rotations (point axis down or 
 *              up). Scanner roll tested for 35 degree rotation
 *              (canted scan line).
 */

/***********************************
 * INCLUDES
 **********************************/

#include <float.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "mcidasp.h"
#include "m0frame.h"
#include "m0gpnav.h"

#ifdef SGP_ON
#include "sgp.h"
#include "sgpget.h"
#endif	/* SGP_ON */

/***********************************
 * DEFINES
 **********************************/

#define NVAR 12		/* number of variables to approximate
			 * with Chebyshev polynomials		*/
#define CALL_THRESH 2	/* Attempt Chebyshev fit after this
			 * many calls to M0pt_sat()		*/

/*********************************************
 * TYPEDEFS mainly for navigation data storage
 ********************************************/

/* SCANNER DESCRIPTION struct */

typedef struct {

	/* description of oscillating scanner (i.e. OLS)        */

	double  th_max;         /* oscillation amplitude        */
	double  th_bias;        /* oscillation frequency, Hz    */
	double	phase_incr;	/* increment of oscillator
				 * phase per element		*/
	double	phase_bias;	/* phase value at element 1	*/

	/* description of rotating scanner                      */

	double  start;          /* scanner angle at start       */
	double  incr;           /* radians per element          */

} scan_parms;


/* BROUWER ELEMENTS. I was unsure of the angular units at the
 * time I commented this. */

typedef struct {
	M0DT   epoch;		/* epoch time			*/
	double  semimajor;	/* semimajor axis, km		*/
	double  ecc;		/* eccentricity			*/
	double  incl;		/* inclination 			*/
	double  ra_asc;		/* right asc. of ascending node	*/
	double  arg_pf;		/* argument of perifocus	*/
	double  mean_anom;	/* mean anomaly			*/
} brouwer;


/* TWO-LINE ELEMENTS (for use with SGP model) */

typedef struct {
	M0DT   epoch;		/* epoch time			*/
	double  dmdt;           /* tendency of mean motion	*/
	double  d2mdt2;		/* acceleration of mean motion	*/
	Fint    d2mdt2_exp;	/* exponent of above		*/
	double  bstar;		/* drag term			*/
	Fint    bstar_exp;	/* drag term exponent		*/
	double  incl;		/* inclination, radians		*/
	double  ecc;		/* eccentricity			*/
	double  ra_asc;		/* right asc. ascending node	*/
	double  arg_pf;		/* arg. of perifocus, radians	*/
	double  mean_anom;	/* mean anomaly			*/
	double  mean_mot;	/* mean motion, rev/day		*/
} twoline;


/* CHEBYSHEV POLYNOMIAL APPROXIMATIONS for NVAR quantities	*/

typedef struct {
	M0flag	ready;		/* ready for use ?		*/
	M0MT	*coef;		/* Chebyshev coefficients	*/
	size_t	ncalls;		/* number of position calls	*/
#ifdef M0DEBUG
	size_t	count;		/* number of Chebyshev calls	*/
	double	fmean[NVAR];	/* mean value accumulator	*/
	double	fmn[NVAR];	/* minimum value accumulator	*/
	double	fmx[NVAR];	/* maximum value accumulator	*/
	double	fstdv[NVAR];	/* standard dev. accumulator	*/
#endif	/* ifdef M0DEBUG */
}
chebdata;


/* ATTITUDE describes the orientation of the
 * satellite chassis relative to the orbit-coordinate system,
 * and to describe the orientation of the scanner relative
 * to the chassis. */

typedef struct {
	double  yaw;    /* rotation about z axis        */
	double  pitch;  /* rotation about y axis        */
	double  roll;   /* rotation about x axis        */
} att;



/* The 'navigation block' is a compound struct including
 * instances of the above. It contains all of the data
 * needed to describe an instance of generic polar nav. */


typedef struct {
	enum		{ DMSP, POES }
			  sat;		/* satellite type	*/
	enum		{ AVHRR, HIRS,
			  MSU, ERBE,
			  OLS, SSMI,
			  SSMT, SSMT2 }
			  sensor;	/* sensor type		*/

	Fint		firstscan;	/* first image line	*/
	Fint		nscans;		/* number of image lines*/
	M0flag		flipScans;	/* reverse line order ?	*/
	M0flag		flipElems;	/* reverse elem order ?	*/
	M0DT		DTfirst_scan;	/* time of first scan	*/
	M0DT		DTlast_scan;	/* time of last scan	*/
	Fint		firstline;	/* first detector (for
					 * MODIS)		*/
	Fint            nlines;		/* number of detectors	*/
	Fint		firstelem;	/* first element	*/
	Fint            nelems;		/* number of elements	*/
	M0TM            TMscan;		/* time between scans	*/
	M0TM            TMelem;		/* time between elems	*/

	enum            { NADIR,
			  HORIZON }
			  levmode;	/* attitude control type*/

	enum            { SGP, BROUWER }
			  orbmodel;	/* orbit model to use	*/

	brouwer		ble;		/* Brouwer elements	*/
	twoline		tle;		/* two-line elements	*/
	chebdata	cheb;		/* Chebyshev polynomial
					 * parameters		*/
	enum            { ROT, OSC }
			  scan_type;	/* scanner type		*/

	att             attChasNom;	/* nominal chassis att	*/
	att             attChasInst;	/* instantaneous chassis*
					 * attitude		*/
	att             attScan;	/* scanner attitude	*/
	double          phi0;		/* angle from scanner	*
					 * axis to scanner locus*/
	double          phi_incr;	/* increment of above	*
					 * multi-detector scans	*/
	scan_parms      scan;		/* scanner parameters	*/
} navblock;

/*********************************
 * SUPPORT FOR MULTIPLE INSTANCES 
 ********************************/

static M0ND
*hDIdata = NULL;

/**********************************
 * DEFINITIONS of private functions
 *********************************/

static double
norm_time(M0TM dt, M0TM TMscene, M0TM tpad)
{
	/* Map time range [-tpad,TMscene+tpad] to [-1,+1].
	 * The normalized time corresponding to the elapsed
	 * time from 0 is returned; the elapsed time dt does
	 * not have to be in the range [-tpad,TMscene+tpad]
	 * for the routine to return a valid result. */
	
	M0ASSERT(TMscene!=0.);
	return(-1. + 2.*(dt+tpad)/(TMscene+2.*tpad));
}

static M0TM
denorm_time(double tnorm, M0TM TMscene, M0TM tpad)
{
	/* Map normalized time [-1,+1] to physical time
	 * [-tpad,TMscene+tpad]. The physical time since the interval
	 * start is returned; the normalized time tnorm does not have
	 * to be in the range [-1,1]. tpad is a file static constant */

	return( (2.*tpad+TMscene)*(tnorm+1.)/2.-tpad);
}

static void
pctovec(M0PT *pPTpos, M0CR *pCRin, double *avec)
{
	/* Move the contents of a point (M0PT type) and
	 * coordinate system (M0CR type) to a vector
	 * of 12 double (for Chebyshev polynomial support) */

	memcpy(&avec[0], pPTpos->c,   3*sizeof(double));
	memcpy(&avec[3], pCRin->i.c, 3*sizeof(double));
	memcpy(&avec[6], pCRin->j.c, 3*sizeof(double));
	memcpy(&avec[9], pCRin->k.c, 3*sizeof(double));

	return;
}

static void
vectopc(double *avec, M0PT *pPTout, M0CR *pCRout)
{
	/* Move an array of 12 double to a point (M0PT type)
	 * and coordinate system (M0CR type). (for Chebyshev
	 * polynomial support) */

	memcpy(pPTout->c,   &avec[0], 3*sizeof(double)); 
	memcpy(pCRout->i.c, &avec[3], 3*sizeof(double)); 
	memcpy(pCRout->j.c, &avec[6], 3*sizeof(double)); 
	memcpy(pCRout->k.c, &avec[9], 3*sizeof(double)); 

	/* Types are all assumed to be celestial Cartesian */

	pPTout->type   = CC;
	pCRout->origin = *pPTout;
	pCRout->i.type = CC;
	pCRout->j.type = CC;
	pCRout->k.type = CC;

	return;
}


/**********************************
 * DEFINITIONS of public functions
 *********************************/



/*
*| Name:
*|	M0DIinit - Initialize navigation codicil data interface
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0DIinit(int handle, Fint * codicil)
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|	codicil - navigation codicil array
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	 0	- success
*|	-1	- could not create re-entrant data collection
*|	-2	- unknown primary (codicil) type
*|	-3	- unknown secondary (DMSP sensor) type
*|	-4	- unknown orbit model type
*|	-5	- could not initialize orbit model
*|	-6	- could not save navigation block
*|	-7	- could not save this instance of re-entrant data
*|	-8      - could not re-use previous nav instance
*|
*| Remarks:
*|	    This routine should be called once before any other
*|	routines with the M0DI prefix and again when the
*|	navigation parameters need to be changed.
*|
*| Categories: 
*|	navigation  
*/


int
M0DIinit(int handle, Fint *codicil)
{
    char	codtype[sizeof(Fint)+1];	/* codicil 
						 * (primary)
						 * type		*/
    char	senstype[sizeof(Fint)+1];	/* secondary	*
						 * type		*/

    Fint	day;
    Fint	dayn;		/* 1 January == 1		*/
    Fint	Fint_handle;	/* handle for FORTRAN calls	*/
    Fint	Fint_miss;	/* missing data code		*/
    Fint	Fint_rc;	/* FORTRAN function return code	*/
    Fint	hour;
    Fint	micro;		/* microsecond			*/
    Fint	minute;
    Fint	month;
    Fint	second;
    Fint	ss;		/* satellite/sensor number	*/
    Fint	year;
    Fint	yyddd; 

    int		rc;		/* function return code		*/

    M0DT	DTepoch;	/* orbit element epoch time	*/
    M0TM	TMscene;	/* duration of data (time)	*/

    navblock	nvblk;		/* navigation data for this	*
				 * instance			*/

#ifdef SGP_ON
    double	sgp_epoch;	/* SGP model epoch time		*/
#else
    double	mean_mot;	/* SGP model mean motion	*/
#endif

    /* set missing data value */

    memset ( &Fint_miss, MCMISSING, sizeof(Fint_miss) );


#ifdef M0DEBUG 

    /* in debug version, pre-fill navigation struct with garbage;
     * use of elements not explicitly initialized by M0DIinit()
     * will quickly show themselves! The exception is nvblk.cheb.coefs;
     * this must be NULL so we do not think it has already been
     * initialized */

    memset ( (void *)&nvblk, (int)M0GARBAGE, sizeof(nvblk) );

#endif	/* ifdef M0DEBUG */

    /* On the first ever call to this routine, it is necessary to
     * initialize the collection object */

    if( hDIdata == NULL ) {
        rc = M0NDnew(sizeof(nvblk), &hDIdata);
        if( rc < 0 ) {
            return -1;
        }
    }
    else {
        /* If this is a re-initialization, there will be
	 * a previous instance. Grab it and throw away
	 * the Chebyshev polynomial coefficients, if any */

        rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk );
        if( rc < 0 ) {
            return -8;
        }
        if( nvblk.cheb.coef != NULL ) {
            M0MTdel( &(nvblk.cheb.coef) );
        }
    }
    nvblk.cheb.coef = (M0MT *)NULL;

    /* Determine codicil type. It is found in the first word
     * of the codicil */

    memcpy ( codtype, codicil, sizeof(Fint) );
    codtype[sizeof(Fint)] = '\0';
    M0NLMSG (handle, "M0DIinit() codicil type " );
    M0NLMSG (handle, codtype );
    M0NLMSG (handle, "\n" );


    /* Unpack codicil according to its particular structure.
     * Each codicil type has its own block in the following
     * lengthy if-elseif-endif control structure */


    /************************************************************
     *                                                          *
     * DMSP codicil decoder starts here                         *
     *                                                          *
     ************************************************************/

    if      ( strcmp(codtype,"DMSP") == 0 ) {

        /* The input is a DMSP codicil, used for
         * all DMSP sensors (including at present OLS, SSM/I,
         * SSM/T, and SSM/T2. */
	

        /* Fill all sensor-independent words in the
	 * navigation struct first. */

	memcpy ( senstype, &codicil[44], sizeof(Fint) );
	senstype[sizeof(Fint)] = '\0';

	nvblk.sat		= DMSP;
#ifdef SGP_ON
	nvblk.orbmodel		= SGP;
#else
	nvblk.orbmodel		= BROUWER;
#endif

	/* Byte-flipping in all but the first word of a nav	*
	 * codicil is ambiguous. So examine the sensor type	*
	 * word in both orderings to determine the actual sensor*
	 * type. */

        if     ( (strstr(senstype, "OLS") != NULL ) ||
		 (strstr(senstype, "SLO") != NULL ) )

	    nvblk.sensor = OLS;

	else if( (strstr(senstype, "MI" ) != NULL ) ||
		 (strstr(senstype, "IM" ) != NULL ) )

	    nvblk.sensor = SSMI;

	else if( (strstr(senstype, "MT" ) != NULL ) ||
		 (strstr(senstype, "TM" ) != NULL ) )

	    nvblk.sensor = SSMT;

        else if( (strstr(senstype, "MT2" ) != NULL ) ||
		 (strstr(senstype, "2TM" ) != NULL ) )

            nvblk.sensor = SSMT2;

	else {
	    M0ASSERT(M0FALSE);	/* unknown DMSP sensor type */
	    return -3;
	}


	nvblk.firstscan		= codicil[46];
	nvblk.nscans		= codicil[50];
	nvblk.flipScans		= codicil[48];
	nvblk.flipElems		= codicil[49];
	nvblk.firstline		= 1;
	nvblk.nlines		= 1;
	nvblk.firstelem		= 1;
	nvblk.nelems		= codicil[51];
	nvblk.levmode		= HORIZON;


	/* this section sets the orbital elements. */

	year  = codicil[4] / 1000;
	year += (year>=M0FIRST_YEAR) ? 1900 : 2000;

	dayn			= codicil[4] % 1000;
	DTepoch.julday = M0jd_make(year,1,1) + dayn - 1;
	DTepoch.seconds= codicil[5] * 1.e-9 * SEC_PER_DAY;

#ifdef SGP_ON

	nvblk.tle.epoch		= DTepoch;
	nvblk.tle.dmdt		= codicil[ 6] * 1.e-9;
	nvblk.tle.d2mdt2	= codicil[ 7] * 1.e-5;
	nvblk.tle.d2mdt2_exp	= codicil[ 8];
	nvblk.tle.bstar		= codicil[ 9] * 1.e-5;
	nvblk.tle.bstar_exp	= codicil[10];

	nvblk.tle.incl		= codicil[11] * 1.e-6;
	nvblk.tle.ecc		= codicil[13] * 1.e-7;
	nvblk.tle.ra_asc	= codicil[12] * 1.e-6;
	nvblk.tle.arg_pf	= codicil[14] * 1.e-6;
	nvblk.tle.mean_anom	= codicil[15] * 1.e-6;
	nvblk.tle.mean_mot	= codicil[16] * 1.e-7;

#else	/* No SGP, use Brouwer model */

	nvblk.ble.epoch		= DTepoch;
	nvblk.ble.incl		= codicil[11] * 1.e-6;
	nvblk.ble.ecc		= codicil[13] * 1.e-7;
	nvblk.ble.ra_asc	= codicil[12] * 1.e-6;
	nvblk.ble.arg_pf	= codicil[14] * 1.e-6;
	nvblk.ble.mean_anom	= codicil[15] * 1.e-6;

	mean_mot		= codicil[16] * 1.e-7;

	/* semi-major axis must be computed from mean motion
	 * available in two-line elements */

	nvblk.ble.semimajor
	  = m0mmtoa_( &mean_mot, &nvblk.ble.ecc, &nvblk.ble.incl);
#ifdef M0DEBUG
	mean_mot = m0atomm_( &(nvblk.ble.semimajor), &nvblk.ble.ecc,
	  &nvblk.ble.incl );
	M0ASSERT( fabs(mean_mot-codicil[16]*1.e-7) < 1.e-6);
#endif	/* ifdef M0DEBUG */

#endif /* SGP */

	/* This section sets nominal and instantaneous attitude
	 * of the chassis (satellite body) in space. Evening 
	 * ascenders (codicil[45] of 0) fly 'backwards;' i.e.
	 * DMSP satellites fly with the sun to their right */

	nvblk.attChasNom.yaw	= 0.;
	nvblk.attChasNom.pitch	= 0.;
	nvblk.attChasNom.roll	= codicil[45]==0 ? 180.*M0D2R : 0.;

	nvblk.attChasInst.yaw	= 0.;
	nvblk.attChasInst.pitch	= 0.;
	nvblk.attChasInst.roll	= 0.;

	/* Now fill sensor-specific words in the
	 * navigation struct. First compute some intermediate
	 * results needed for timing. Because timing for OLS
	 * is strange, the timing entries DTfirst_scan and DTlast_scan
	 * have to be done specifically for each sensor */

	yyddd	= codicil[1] % 100000;
	year	= yyddd / 1000;
	year   += (year>=M0FIRST_YEAR) ? 1900 : 2000;
	dayn	= yyddd % 1000;

	switch(nvblk.sensor) {

	    case OLS:

		nvblk.scan_type = OSC;

	        nvblk.TMscan	= codicil[52] * 1.e-6;

		/* Sampling rate is actually 102.4 Hz (ref
		 * IS-YD-821B) but motion correction optics
		 * make scan line perpendicular to satellite
		 * motion vector. The same effect can be approximated
		 * by assuming zero time per element and advancing
		 * the start time of the first scan (and implicitly
		 * all scans thereafter) by one-half of the time
		 * needed to scan a line. This constant is half
		 * a line (3661 elements) times 1/102.4e3 seconds
		 * per element, or 3.575e-2 seconds */
		 
	        nvblk.TMelem	= 0.;
	        nvblk.DTfirst_scan.julday
	          = M0jd_make(year,1,1) + dayn - 1; 
	        nvblk.DTfirst_scan.seconds
	          = codicil[47] * 1.e-3;
		nvblk.DTfirst_scan
		  = M0DTinc ( nvblk.DTfirst_scan, 3.575e-2 );
	        TMscene	= nvblk.nscans * nvblk.TMscan;
	        nvblk.DTlast_scan =
	          M0DTinc ( nvblk.DTfirst_scan, TMscene );

		nvblk.attScan.yaw	= 0.;
		nvblk.attScan.pitch	= 0.;
		nvblk.attScan.roll	= 0.;
		nvblk.phi0		= 90.*M0D2R;
		nvblk.phi_incr		= 0.;

		/* From IS-YD-821B (9 March 1990) p. 14a, the scan
		 * angle 'th' of an element 'elem' is given by
		 *
		 * th = th_max*cos(M*(elem-1)/nelem+ph_bias)+N*K
                 * 
		 * where
		 *
		 *   th_max	= 1.00967 radians (57.85 degrees)
		 *   nelem	= 7322.179 (nominal total
		 *                          sample periods)
		 *   M          = 2.66874 radians
		 *   ph_bias    = 0.23665 radians
		 *   N          = signed value of scanner offset
		 *                in units of value K, from subsync
		 *                frame of data stream (see paragraph
		 *                4.1.1.6.2).
		 *   K          = 0.00099 radians
		 *
		 * so we can replace M/nelem with 
		 *
		 *   ph_incr    = 3.644735e-4
		 *
		 * and use the simplified expression 
		 *
		 * th = th_max*cos(ph_incr*(elem-1)+ph_bias)+th_bias
		 * 
		 * The ingestor team could not find N coming out
		 * of the stream so it is set to zero. In the
		 * Vandenberg acceptance version it was erroneously
		 * set to codicil[51]*1.e-6*M0D2R which worked out
		 * to about 1.3e-4 radians, or about half an
		 * average pixel.
		 */

		nvblk.scan.th_max	= -1.00967;
		nvblk.scan.th_bias	=  0.;
		nvblk.scan.phase_incr	=  3.644735e-4;
		nvblk.scan.phase_bias	=  0.23665;
		break;

	    case SSMI:			/* nvblk.sensor	*/

		nvblk.scan_type = ROT;

		/* The value of TMscan is nominally 1.88 seconds */

	        nvblk.TMscan	= codicil[52] * 1.e-6;

		/* This value comes from navigation code from
		 * NGDC (National Geophysical Data Center) */
		 
	        nvblk.TMelem	= 0.00422;
	        nvblk.DTfirst_scan.julday
	          = M0jd_make(year,1,1) + dayn - 1; 
	        nvblk.DTfirst_scan.seconds
	          = codicil[47] * 1.e-3;
	        TMscene	= nvblk.nscans * nvblk.TMscan;
	        nvblk.DTlast_scan =
	          M0DTinc ( nvblk.DTfirst_scan, TMscene );

		nvblk.attScan.yaw	= 0.;
		nvblk.attScan.pitch	= 90.*M0D2R;
		nvblk.attScan.roll	= 0.;
		nvblk.phi0		= 45.*M0D2R;

		/* NGDC navigation code contains corrections
		 * to the scanner angle from the axis. The quantity
		 * ss-79 corresponds to the DMSP satellite number */

		ss = codicil[1] / 100000;

		switch(ss-79) {
		    case  8:
			nvblk.phi0 +=  0.25*M0D2R;
			break;
		    case 10:
			nvblk.phi0 += -0.37*M0D2R;
			break;
		    case 11:
			nvblk.phi0 +=  0.02*M0D2R;
			break;
	            default:	/* no corrections available for
				 * other spacecraft */
			break;
		}

		nvblk.phi_incr		= 0.;

		/* The SSM/I scanner mechanism looks 'behind' the
		 * spacecraft chassis. Scan.start is the angle from
		 * the X axis (which points 'behind' the space-
		 * craft given the 90 degree pitch of the SSM/I) to
		 * the first element (scene station) in the scan.
		 * The scanner then rotates counter-clockwise,
		 * about the downward pointing z axis, or 
		 * or negative in a mathematical sense */

		nvblk.scan.start	= -50.2*M0D2R;
		nvblk.scan.incr		=   0.8*M0D2R;

		break;

	    case SSMT:			/* nvblk.sensor	*/

		nvblk.scan_type = ROT;

		/* The value of TMscan is nominally 32 seconds */

	        nvblk.TMscan	= codicil[52] * 1.e-6;

		/* This value comes from "Draft DMSP Technical
		 * Specifications section 10.1 */
		 
	        nvblk.TMelem	= 3.0;
	        nvblk.DTfirst_scan.julday
	          = M0jd_make(year,1,1) + dayn - 1; 
	        nvblk.DTfirst_scan.seconds
	          = codicil[47] * 1.e-3;
	        TMscene	= nvblk.nscans * nvblk.TMscan;
	        nvblk.DTlast_scan =
	          M0DTinc ( nvblk.DTfirst_scan, TMscene );

		nvblk.attScan.yaw	= 0.;
		nvblk.attScan.pitch	= 0.;

		/* Scan is left to right relative to chassis */

		nvblk.attScan.roll	= 180.*M0D2R;
		nvblk.phi0		=  90.*M0D2R;
		nvblk.phi_incr		= 0.;

		/* Scan.start is the angle from
		 * the X axis (which points towards the surface)
		 * to the first element (scene station) in the scan.
		 * The scanner then rotates counter-clockwise,
		 * about the z axis (parallel to ground, oriented
                 * towards rear of spacecraft chassis or in the
                 * direction of flight for an evening ascender. */

		nvblk.scan.start	= -36.0*M0D2R;
		nvblk.scan.incr		=  12.0*M0D2R;

		break;

	    case SSMT2:			/* nvblk.sensor	*/

		nvblk.scan_type = ROT;

		/* The value of TMscan is nominally 8 seconds */

	        nvblk.TMscan	= codicil[52] * 1.e-6;

		/* This value comes from "Draft DMSP Technical
		 * Specifications Figure 6. The transmission rate
                 * is 27 words per second and there are 5 words
                 * per scene station. This works out to a data rate
                 * 16x that of SSM/T */
		 
	        nvblk.TMelem	= 0.19;
	        nvblk.DTfirst_scan.julday
	          = M0jd_make(year,1,1) + dayn - 1; 
	        nvblk.DTfirst_scan.seconds
	          = codicil[47] * 1.e-3;
	        TMscene	= nvblk.nscans * nvblk.TMscan;
	        nvblk.DTlast_scan =
	          M0DTinc ( nvblk.DTfirst_scan, TMscene );

		nvblk.attScan.yaw	= 0.;
		nvblk.attScan.pitch	= 0.;

		/* Scan is left to right relative to chassis */

		nvblk.attScan.roll	= 180.*M0D2R;
		nvblk.phi0		=  90.*M0D2R;
		nvblk.phi_incr		=   0.;

		/* Scan.start is the angle from
		 * the X axis (which points towards the surface)
		 * to the first element (scene station) in the scan.
		 * The scanner then rotates counter-clockwise,
		 * about the z axis (parallel to ground, oriented
                 * towards rear of spacecraft chassis or in the
                 * direction of flight for an evening ascender. */

		nvblk.scan.start	= -40.5*M0D2R;
		nvblk.scan.incr		=   3.0*M0D2R;

		break;

	    default:

		M0ASSERT(M0FALSE);	/* Unknown DMSP sensor! */
		return -3;
	}



    }

    /************************************************************
     *                                                          *
     * POES and POEB (TIRO) codicil decoder starts here         *
     *                                                          *
     * POEB - use Brouwer/Lyddane orbit model                   *
     *								*
     * POES - use NORAD SGP or SGP4 orbit prediction (requires  *
     *	      conversion of input orbital elements)             *
     * POEX - use NORAG SGP or SGP4 orbit prediction; attempt   *
     * 	      to pick up orbital elements from file 'tle.new'   *
     *                                                          *
     * The POES, POEB, and POEX codicils are identical apart    *
     * from the type; both contain Brouwer elements from the    *
     * TBUS message. The semimajor axis must be converted to a  *
     * mean motion for use with the SGP/SGP4 models from NORAD  *	
     *                                                          *
     ************************************************************/


    else if ( strcmp(codtype,"POES") == 0 || 
	      strcmp(codtype,"POEB") == 0 ||
	      strcmp(codtype,"POEX") == 0  ) {

        /* The input is a POES/POEB/POEX (a.k.a. TIROS or
         * NOAA series polar-orbiter) codicil, used
         * for all POES sensors (AVHRR, HIRS, MSU, ERBE,
	 * etc. */   

        /* Fill all sensor-independent words in the
	 * navigation struct first. */

	ss = codicil[1] / 100000;

	nvblk.sat		= POES;

	/* Select orbit model type to use based on codicil
	 * type. */
        if      ( strcmp(codtype,"POEB" ) == 0 )
	    nvblk.orbmodel = BROUWER;

#ifdef SGP_ON
	else if ( strcmp(codtype,"POES" ) == 0 )
	    nvblk.orbmodel = SGP;
	else if ( strcmp(codtype,"POEX" ) == 0 )
	    nvblk.orbmodel = SGP;
#endif	/* ifdef SGP_ON */
	else {
	    M0ASSERT(M0FALSE);	/* unknown codicil type */
	    return -2;
	}


	/* There is no 'sensor type' word in a POES/TIRO 
	 * codicil; the sensor type is deduced from the 
	 * number of elements (samples) per line */

	nvblk.nelems		= codicil[12];

        if      ( nvblk.nelems == 2048 ) 
	    nvblk.sensor = AVHRR;
	else if ( nvblk.nelems == 56 )
	    nvblk.sensor = HIRS;
	else if ( nvblk.nelems == 11 )
	    nvblk.sensor = MSU;
	else if ( nvblk.nelems == 62 )
	    nvblk.sensor = ERBE;
	else {
	    M0ASSERT(M0FALSE);	/* unknown POES/TIRO sensor type */
	    return -2;
	}

	nvblk.firstscan		= codicil[46];
	nvblk.nscans		= codicil[50];

	/* Area flipping support is problematic for POES/TIRO.
	 * For AVHRR, which scans right-to-left, flipping is
	 * applied to both scans and elements, or to neither.
	 * Therefore only one flip flag is contained in the
	 * codicil. This will not suffice for HIRS and MSU because,
	 * like OLS, they may be flipped in either of two ways,
	 * by line or by element, or not at all. The present
	 * implementation picks up the flip flag out of
	 * word 49 BUT THIS IS ONLY VALID FOR AVHRR. THE FLIP
	 * FLAG IS UNDEFINED FOR OTHER SENSORS!!! A nonzero
	 * flip flag for any other sensor type will fire an
	 * assertion if compiled with M0DEBUG defined. Otherwise
	 * it is ignored. 'imgflip.pgm' contains commented-out
	 * code to store the element flip flag in word 54 (zero-based),
	 * but this implies a codicil change. When the codicil
	 * change is made, both imgflip.pgm and the code below
	 * will have to be changed */

        M0ASSERT ( codicil[49] == 0 || nvblk.sensor == AVHRR );  

	nvblk.flipScans		= codicil[49];
	nvblk.flipElems		= codicil[49];

	/* Suggested, and untried, change to codicil for HIRS and MSU
	 * flipping support, consistent with mothballed code in
	 * 'imgflip.pgm'.
         * nvblk.flipElems	= codicil[54];
	 */

	nvblk.firstline		= 1;
	nvblk.nlines		= 1;
	nvblk.firstelem		= 1;
	nvblk.levmode		= HORIZON;



	year			=  codicil[4 ] / 10000;
	year += (year>=M0FIRST_YEAR) ? 1900 : 2000;
	month			= (codicil[4 ] % 10000) / 100;
	day			= (codicil[4 ] % 100  );
	hour			=  codicil[5 ] / 10000;
	minute			= (codicil[5 ] % 10000) / 100;
	second			= (codicil[5 ] % 100  );
	micro			=  codicil[14] * 1000;

	if ( nvblk.orbmodel == BROUWER ) {

	    /* nvblk.orbmodel == BROUWER */

	    nvblk.ble.epoch	= M0DTmake ( year, month, day,
	      hour, minute, second, micro );
	    nvblk.ble.semimajor = codicil[ 6] * 1.e-2;
	    nvblk.ble.ecc	= codicil[ 7] * 1.e-6;
	    nvblk.ble.incl	= codicil[ 8] * 1.e-3;
	    nvblk.ble.ra_asc	= codicil[11] * 1.e-3;
	    nvblk.ble.arg_pf	= codicil[10] * 1.e-3;
	    nvblk.ble.mean_anom = codicil[ 9] * 1.e-3;
	}

#ifdef SGP_ON
	else if ( nvblk.orbmodel == SGP ) {

	    if ( strcmp(codtype,"POES") == 0 ) {
	    
		double smaxis;

	        nvblk.tle.epoch	= M0DTmake ( year, month, day,
	           hour, minute, second, micro );

	        nvblk.tle.dmdt		= 0.;
	        nvblk.tle.d2mdt2	= 0.;
	        nvblk.tle.d2mdt2_exp	= 0;
	        nvblk.tle.bstar		= 0.;
	        nvblk.tle.bstar_exp	= 0;
	        nvblk.tle.incl		= codicil[ 8] * 1.e-3;
	        nvblk.tle.ecc		= codicil[ 7] * 1.e-6;
	        nvblk.tle.ra_asc	= codicil[11] * 1.e-3;
	        nvblk.tle.arg_pf	= codicil[10] * 1.e-3;
	        nvblk.tle.mean_anom	= codicil[ 9] * 1.e-3;

	        /* This function call converts a Brouwer mean semi-
	         * major axis to a mean motion. */

	        smaxis = codicil[6]*1.e-2;
	        nvblk.tle.mean_mot	=
	          m0atomm_ ( &smaxis, &nvblk.tle.ecc,
	          &nvblk.tle.incl );
            }
	    
	    /* Special section to read two-line elements *****
	     * from file 'tle.new' **************************/
	    
	    else if ( strcmp(codtype,"POEX") == 0 ) {

		Fint	sgpget_status;
		double	dfrac, yyddd;

		sgpget_status = sgpget (
		  &ss, &sgp_epoch,   &nvblk.tle.dmdt,
		  &nvblk.tle.d2mdt2, &nvblk.tle.d2mdt2_exp,
		  &nvblk.tle.bstar,  &nvblk.tle.bstar_exp,
		  &nvblk.tle.incl,   &nvblk.tle.ra_asc,
		  &nvblk.tle.ecc,    &nvblk.tle.arg_pf,
		  &nvblk.tle.mean_anom, &nvblk.tle.mean_mot );

		if ( sgpget_status != SGPGET_SUCCESS ) {
		    return -5;
		}

	        /* Epoch time sgp_epoch is suitable for initializ-
	         * ing SGP model. 'M0DT' form for nav block must
		 * be computed; it is used to compute time since
		 * epoch needed for orbital prediction */
				
		 dfrac       = modf ( sgp_epoch, &yyddd );
		 year        = yyddd/1000.;
		 dayn        = yyddd-year*1000;
	         year += (year>=M0FIRST_YEAR) ? 1900 : 2000;
				
		 nvblk.tle.epoch.julday =
		   M0jd_make(year,1,1) + dayn - 1;
		 nvblk.tle.epoch.seconds = 
		   dfrac * SEC_PER_DAY;
	    }
	}
#endif	/* ifdef SGP_ON	 */

	else {
	    M0ASSERT(M0FALSE);	/* orbit model type corrupted */
	    return -4;
	}

	/* This section sets nominal and instantaneous attitude
	 * of the chassis (satellite body) in space. */

	nvblk.attChasNom.yaw	= 0.;
	nvblk.attChasNom.pitch	= 0.;
	nvblk.attChasNom.roll	= 0.;

	nvblk.attChasInst.yaw	= 0.;
	nvblk.attChasInst.pitch	= 0.;
	nvblk.attChasInst.roll	= 0.;



	/* Now fill sensor-specific words in the
	 * navigation struct. */

	switch(nvblk.sensor) {

	    case AVHRR:				/* nvblk.sensor	*/

		nvblk.scan_type = ROT;
	        nvblk.TMscan	= codicil[52] * 1.e-6;

		/* Value in NVTIRO (routine that makes codicils)
		 * is incorrect. This value is computed from 
		 * timing information on page C-16 of 
		 *   Planet, W. G. 1988: "Data Extraction and
		 *     calibration of TIROS-N/NOAA Radiometers.
		 *     NOAA Tech. Memo. NESS 107 Rev. 1. */

	        nvblk.TMelem		= 2.82679e-5;

		nvblk.attScan.yaw	= 0.;
		nvblk.attScan.pitch	= 0.;
		nvblk.attScan.roll	= 0.;	/* right to left */
		nvblk.phi0		= 90.*M0D2R;
		nvblk.phi_incr		= 0.;

		nvblk.scan.start	= -55.4*M0D2R;
		nvblk.scan.incr		= fabs(codicil[13]*M0D2R*1.e-6);


		break;

	    case HIRS:				/* nvblk.sensor	*/

		nvblk.scan_type = ROT;
	        nvblk.TMscan	= codicil[52] * 1.e-6;
	        nvblk.TMelem	= codicil[53] * 1.e-8;

		nvblk.attScan.yaw	= 0.;
		nvblk.attScan.pitch	= 0.;

		/* scan is left to right */
		nvblk.attScan.roll	= 180.*M0D2R;
		nvblk.phi0		= 90.*M0D2R;
		nvblk.phi_incr		= 0.;

		nvblk.scan.start	= -49.5*M0D2R;
		nvblk.scan.incr		= fabs(codicil[13]*M0D2R*1.e-6);

		break;

	    case MSU:				/* nvblk.sensor	*/

		nvblk.scan_type = ROT;
	        nvblk.TMscan	= codicil[52] * 1.e-6;
	        nvblk.TMelem	= codicil[53] * 1.e-8;

		nvblk.attScan.yaw	= 0.;
		nvblk.attScan.pitch	= 0.;
		/* scan is left to right */
		nvblk.attScan.roll	= 180.*M0D2R;
		nvblk.phi0		= 90.*M0D2R;
		nvblk.phi_incr		= 0.;

		nvblk.scan.start	= -47.37*M0D2R;
		nvblk.scan.incr		= fabs(codicil[13]*M0D2R*1.e-6);
		break;

	    case ERBE:				/* nvblk.sensor	*/

		nvblk.scan_type = ROT;
	        nvblk.TMscan	= codicil[52] * 1.e-6;

		/* value in NVTIRO (makes codicils) was incorrect
		 * for a long time and may not be valid for 
		 * old areas. The correct sampling rate is 30 Hz */

	        nvblk.TMelem	= 1./30.;

		nvblk.attScan.yaw	= 0.;
		nvblk.attScan.pitch	= 0.;

		/* The ERBE scanner roll is given as the angle between
		 * the orbit plane (forward) and the start of the
		 * scan. Right to left perpendicular to line of flight
		 * in such a convention is -90 degrees; for generic
		 * nav this corresponds to a scan axis roll of 0. */
		
		if ( codicil[54] != 0 && codicil[54] != Fint_miss )
		    nvblk.attScan.roll	= (-90.-codicil[54]*1.e-3)
		      *M0D2R;
		else
		    nvblk.attScan.roll  = 0.;

		nvblk.phi0		= 90.*M0D2R;
		nvblk.phi_incr		= 0.;

		nvblk.scan.incr		= fabs(codicil[13]*M0D2R*1.e-6);

		if ( codicil[55] != 0 && codicil[55] != Fint_miss )
		    nvblk.scan.start = -(codicil[55]/1000.-1.)
		      *nvblk.scan.incr;
		else
		    nvblk.scan.start = -(nvblk.nelems-1)
		      *nvblk.scan.incr/2.;

		break;

	    default:
		M0ASSERT(M0FALSE);	/* Corrupted POES sensor type */
		return -2;
	}

	/* Compute start and end times */

	year	= (codicil[ 1] % 100000) / 1000;
	year   += (year>=M0FIRST_YEAR) ? 1900 : 2000;
	dayn	= (codicil[ 1] %   1000);

        nvblk.DTfirst_scan.julday
          = M0jd_make(year,1,1) + dayn - 1; 
        nvblk.DTfirst_scan.seconds
          = codicil[47] * 1.e-3;

	/* Examination of NVTIRO, which creates codicils,
	 * indicates that codicil[47] is the time of scan
	 * 1, not necessarily the time of the smallest scan
	 * number to navigate as is stated in the TIRO
	 * codicil documentation. So it is necessary to
	 * advance the time by the number of scans between
	 * scan 1 and nvblk.firstscan */

	nvblk.DTfirst_scan
	  = M0DTinc ( nvblk.DTfirst_scan,
	  (nvblk.firstscan-1)*nvblk.TMscan );
        TMscene	= nvblk.nscans * nvblk.TMscan;
        nvblk.DTlast_scan =
          M0DTinc ( nvblk.DTfirst_scan, TMscene );

    }
    else {			/* strcmp(codtype,"????")	*/
	/* Unknown codicil type */
	M0ASSERT(M0FALSE);
	return -2;	
    }


    /* Initialize appropriate orbital prediction model. 
     * It is assumed that all elements, including epoch time,
     * have been set according to the particular codicil type */

    if ( nvblk.orbmodel == BROUWER ) {

	double brouwer_epoch;

	brouwer_epoch = M0dabtim ( nvblk.ble.epoch );
	rc = M0BLsetels( handle, brouwer_epoch, nvblk.ble.semimajor,
	  nvblk.ble.ecc, nvblk.ble.incl, nvblk.ble.ra_asc,
	  nvblk.ble.arg_pf, nvblk.ble.mean_anom );
        if( rc < 0 ) {
	    return -5;
	}

    }
#ifdef SGP_ON
    else if      ( nvblk.orbmodel == SGP  ) {

        M0DTextract ( nvblk.tle.epoch, 
	  &year, &month, &day, &hour, &minute, &second,
	  &micro );

	sgp_epoch = 1000 * ( year % 100 )
	  + (double)M0DTdaynum(nvblk.tle.epoch)
	  + nvblk.tle.epoch.seconds/SEC_PER_DAY;

        sgpini_ ( &sgp_epoch, &nvblk.tle.dmdt, &nvblk.tle.d2mdt2,
	  &nvblk.tle.d2mdt2_exp, &nvblk.tle.bstar,
	  &nvblk.tle.bstar_exp, &nvblk.tle.incl, &nvblk.tle.ra_asc,
	  &nvblk.tle.ecc, &nvblk.tle.arg_pf, &nvblk.tle.mean_anom,
	  &nvblk.tle.mean_mot );
	
    }
#endif	/* SGP_ON */

    else {
	M0ASSERT(M0FALSE);	/* Corrupted orbit model type */
	return -4;
    }

    /* This will trigger Chebyshev polynomial reset in
     * M0DIpt_sat(). */

    nvblk.cheb.ready	= M0FALSE;
    nvblk.cheb.ncalls	= 0;

    /* This assertion verifies that the Chebyshev coefficient
     * array has been cleared out from any previous use. Otherwise
     * there will be a memory leak */

    M0ASSERT( nvblk.cheb.coef == (M0MT *)NULL );
#ifdef UNDEF
    if( nvblk.cheb.coef != (M0MT *)NULL  ) {
        M0MTdel( &(nvblk.cheb.coef) );
        nvblk.cheb.coef	= (M0MT *)NULL;
    }
#endif
#ifdef M0DEBUG
    nvblk.cheb.count	= 0;
#endif

    /* Save a copy of this instance of navigation data
     * for the handle provided */

    rc = M0NDadd( hDIdata, handle, &nvblk, sizeof(nvblk) );
    if( rc < 0 ) {
        return -7;
    }
    return 0;
}


/*
*| Name:
*|	M0DIorbpred - Predict satellite position and scanner attitude
*|		      with orbit model call
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	M0PT
*|	M0DIorbpred(int handle, M0DT time, M0CR *pCRscan)
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|	time	- of satellite position and velocity.
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pCRscan- scanner coordinate system
*|
*| Return values:
*|	satellite position 
*|
*| Remarks:
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|	    Results are in Cartesian celestial coordinates. The
*|	coordinate system is given in terms of orthonormal vectors
*|	in celestial space. The z axis is the axis of rotation of the
*|	scanner	mechanism, the x axis is the negative of local vertical
*|	(to Earth center for nvblk.levmode == NADIR and normal
*|	to spheroid surface for nvblk.levmode === HORIZON), and the
*|	y axis completes a right-handed system. Units are km for
*|	position components and unitless 'direction cosines' for
*|	coordinate axes.
*|
*| Categories: 
*|	navigation  
*/

M0PT
M0DIorbpred(int handle, M0DT time, M0CR * pCRscan)
{
	att     chassis;	/* chassis attitude		*/

	Fint	Fint_handle;	/* FORTRAN-compatible value	*
				 * of handle			*/

	int	rc;		/* function return code		*/

	M0CR	CRsat;		/* Orbit plane coordinates	*/
	M0CR	CRchassis;	/* Spacecraft chassis coords	*/

	M0PT	PTsat;		/* satellite position		*/

	M0VC	VCorbit_norm;	/* normal to orbit plane motion	*
				 * in right hand sense		*/
	M0VC	VCsat;		/* satellite motion     	*/
	M0VC	VCsatpos;	/* satellite position in vector	*
				 * form          		*/

	navblock	nvblk;	/* navigation data block for 	*
				 * this instance		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);


	Fint_handle = handle;

	if ( nvblk.orbmodel == BROUWER ) {

	    /* get position and velocity vectors from 
	     * Brouwer model */

	    double	dtime;	/* Julian date of request */

	    dtime = M0dabtim ( time );
	    m0blpred_ ( &Fint_handle, &dtime,
	        &PTsat.c[0], &PTsat.c[1], &PTsat.c[2],
		&VCsat.c[0], &VCsat.c[1], &VCsat.c[2] );
	}

#ifdef SGP_ON

	else if ( nvblk.orbmodel == SGP ) {

	    /* get position and velocity vectors from
	     * SGP model */
	    
	    M0TM    TMepoch;

	    TMepoch = M0DTdiff ( nvblk.tle.epoch, time )
	              /(double)SEC_PER_MINUTE;
	    dosgp_ ( &Fint_handle, &TMepoch,
	        &PTsat.c[0], &PTsat.c[1], &PTsat.c[2],
		&VCsat.c[0], &VCsat.c[1], &VCsat.c[2] );
	}
#endif	/* ifdef SGP_ON */

	/* Assign position and velocity vector types. Then compute
	 * an orbit plane normal from the velocity */

	PTsat.type	= CC;
	VCsat.type	= CC;
	VCsatpos  = M0PTvc ( PTsat );
	VCorbit_norm  = M0VCcross ( VCsatpos,
	  VCsat );

	/* now have in hand a satellite position vector ('vector'
	 * form VCsatpos ) and an orbit plane normal
	 * VCorbit_norm */

	VCorbit_norm = M0VCnorm ( VCorbit_norm );
	VCsatpos = M0VCnorm ( VCsatpos );

	/* First compute the 'i' vector of the satellite coordinate
	 * system. This either points towards the geoid center
	 * or towards the subpoint on the geiod */

	if ( nvblk.levmode == NADIR ) {

		CRsat.i = M0VCscale ( -1., VCsatpos );
	}
	else {

		/* Compute position of satellite subpoint (point
		 * on geoid from which satellite lies on vertical
		 * normal */

		CRsat.i  = M0PLdown ( handle, PTsat );
	}

	/* satellite coordinate 'k' vector is just the normal
	 * to 'i' in the orbit plane, and 'j' then follows
	 * from 'k' x 'i'. Then normalize all to be 
	 * a true unit vector basis. Some of these normalizations
	 * may be superfluous; this has not been validated */

	CRsat.k        = M0VCcross ( CRsat.i, VCorbit_norm );
	CRsat.j        = M0VCcross ( CRsat.k, CRsat.i );
	CRsat.origin   = PTsat;

	CRsat.i        = M0VCnorm ( CRsat.i );
	CRsat.j        = M0VCnorm ( CRsat.j );
	CRsat.k        = M0VCnorm ( CRsat.k );


	/* now compute chassis coordinates */

	chassis.yaw     = nvblk.attChasNom.yaw
			+ nvblk.attChasInst.yaw;
	chassis.pitch   = nvblk.attChasNom.pitch
			+ nvblk.attChasInst.pitch;
	chassis.roll    = nvblk.attChasNom.roll
			+ nvblk.attChasInst.roll;

	/* Rotations are always evaluated in the order of
	 * yaw, pitch, and roll, and are used to convert
	 * each of the three (i,j,k) satellite coordinates
	 * into a corresponding chassis coordinate. Some
	 * rotations may be applied to rotate a vector 
	 * about itself and will thus have no effect, but
	 * are included for completeness */

	CRchassis.i		= CRsat.i;
	CRchassis.j		= CRsat.j;
	CRchassis.k		= CRsat.k;
	CRchassis.origin	= CRsat.origin;

	if ( chassis.yaw != 0. ) {
		CRchassis.i = M0VCrotate ( CRsat.k,
				chassis.yaw, CRchassis.i );
		CRchassis.j = M0VCrotate ( CRsat.k,
				chassis.yaw, CRchassis.j );
		CRchassis.k = M0VCrotate ( CRsat.k,
				chassis.yaw, CRchassis.k );
	}
	if ( chassis.pitch != 0. ) {
		CRchassis.i = M0VCrotate ( CRsat.j,
				chassis.pitch, CRchassis.i );
		CRchassis.j = M0VCrotate ( CRsat.j,
				chassis.pitch, CRchassis.j );
		CRchassis.k = M0VCrotate ( CRsat.j,
				chassis.pitch, CRchassis.k );
	}
	if ( chassis.roll != 0. ) {
		CRchassis.i = M0VCrotate ( CRsat.i,
				chassis.roll, CRchassis.i );
		CRchassis.j = M0VCrotate ( CRsat.i,
				chassis.roll, CRchassis.j );
		CRchassis.k = M0VCrotate ( CRsat.i,
				chassis.roll, CRchassis.k );
	}


	/* now orient the scanner coordinates in similar fashion
	 * by applying successive rotations to the chassis
	 * coordinates */

	pCRscan->origin	= CRchassis.origin;
	pCRscan->i		= CRchassis.i;
	pCRscan->j		= CRchassis.j;
	pCRscan->k		= CRchassis.k;

	if ( nvblk.attScan.yaw != 0. ) {
		pCRscan->i = M0VCrotate ( pCRscan->k,
			nvblk.attScan.yaw, CRchassis.i );
		pCRscan->j = M0VCrotate ( pCRscan->k,
			nvblk.attScan.yaw, CRchassis.j );
		pCRscan->k = M0VCrotate ( pCRscan->k,
			nvblk.attScan.yaw, CRchassis.k );
	}
	if ( nvblk.attScan.pitch != 0. ) {
		pCRscan->i = M0VCrotate ( pCRscan->j,
			nvblk.attScan.pitch, CRchassis.i );
		pCRscan->j = M0VCrotate ( pCRscan->j,
			nvblk.attScan.pitch, CRchassis.j );
		pCRscan->k = M0VCrotate ( pCRscan->j,
			nvblk.attScan.pitch, CRchassis.k );
	}
	if ( nvblk.attScan.roll != 0. ) {
		pCRscan->i = M0VCrotate ( pCRscan->i,
			nvblk.attScan.roll, CRchassis.i );
		pCRscan->j = M0VCrotate ( pCRscan->i,
			nvblk.attScan.roll, CRchassis.j );
		pCRscan->k = M0VCrotate ( pCRscan->i,
			nvblk.attScan.roll, CRchassis.k );
	}

	return PTsat;
}



/*
*| Name:
*|	M0DIpt_sat - Fast computation of satellite position
*|		      and attitude (Chebyshev approximation)
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0DIpt_sat(int handle, M0DT time,
*|	  M0PT *pPTsat, crd *pCRscan)
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|	time	- of position and attitude
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pPTsat	- satellite position
*|	*pCRscan- scanner coordinate system
*|
*| Return values:
*|	 0	- nominal operation; call M0DIstatus() for details
*|	-1	- input time outside Chebyshev polynomial range;
*|		  solution from direct orbit model call.
*|	-2	- memory allocation failure; solution from orbit model
*|	-3	- could not save re-entrant data; direct solution used
*|
*| Remarks:
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|	    M0DIpt_sat() ALWAYS returns a valid result (within the
*|	limitations of the orbit model. The return code and status
*|	(available from M0DIstatus()) are for help in debugging
*|	applications.
*|	    On the first several calls, the results are from a direct
*|	call to the orbit model via M0DIorbpred(). On later calls,
*|	the results are from Chebyshev polynomials. The threshold
*|	number of calls is specified inline in an enumeration constant.
*|	    M0DIinit() may be called later during the same process
*|	with a new codicil, and this will re-initialize the Chebyshev
*|	polynomals.
*|	    The satellite position is given in Cartesian celestial
*|	coordinates. The coordinate system is given in terms of
*|      orthonormal vectors in celestial space. The z axis is the
*|      axis of rotation of the scanner mechanism, the x axis is
*|	the sensor orientation at theta=0 rotation, and the y axis
*|	completes the right-handed system.
*|	    The Chebyshev polynomial fit and evaluation using Clemshaw's
*|	recurrence is coded from the algorithm described in
*|	Press et al. 1989: Numerical Recipes (FORTRAN).
*|	Sections 5.4 and 5.6
*|
*| Categories: 
*|	navigation  
*/



int
M0DIpt_sat(int handle, M0DT time, M0PT *pPTsat, M0CR * pCRscan)
{
	enum {  MNCTERM=4,	/* Minumum number of Chebyshev
				 * terms			*/
		DTAVG=100	/* Average separation between
				 * Chebyshev fit points in
				 * initial series		*/
	};

	double	ad[3];		/* intermediate results of	*
				 * Chebyshev evaluations	*/
	double	*atnorm;	/* dynamic array of normal-	*
				 * ized times corresponding	*
				 * to Chebyshev zeroes;		*
				 * atnorm[terms]		*/ 
	double	chebfac;	/* Chebyshev term weighting	*
				 * factor			*/
	double	fa[NVAR];	/* Vector of Chebyshev approx-	*
				 * imation values		*/
	double	mxPosErr;	/* maximum allowable error in	*
				 * Chebyshev approximation of	*	
				 * satellite position, km	*/
	double	mxAttErr;	/* maximum allowable error in
				 * Chebyshev approximation of
				 * direction cosines of attitude*/
	double	pi;
	double	tnorm;		/* normalized time; range [-1,1]*/

	int	point;		/* Index for function evaluation
				 * point when fitting Chebyshev
				 * polynomials */
	int	rc;		/* function return code		*/
	int	var;		/* Index of current variable;
				 * first three are position
				 * components, next nine are
				 * x,y,z for each of i, j, and
				 * k unit vectors		*/

	M0CR	CRtemp;		/* temporary coord system	*/
	M0DT	DTend;		/* end time of scene (padded)	*/
	M0DT	DTepoch;	/* orbital element epoch time	*/
	M0DT	DTstart;	/* start time of scene (padded)	*/

	M0flag	ok;		/* return status (Boolean)	*/

	M0PT	PTtemp;		/* temporary position		*/

	M0TM	dt;		/* elapsed time since image
				 * start 			*/
	M0TM	tdiff;		/* time range of Chebyshev fit*/
	M0TM	TMscene;	/* duration of scene (orbit
				 * portion) to navigate 	*/
	navblock nvblk;		/* navigation data for this	*
				 * instance			*/

	size_t	p0, p1, p2;	/* indices to most, next most,
				 * and oldest intermediate
				 * result */

	size_t	cterm;		/* Number of terms of Chebyshev	
				 * approximation to use	
				 * (temporary, for clarity.
				 * Permanent value is in
				 * nvblk.cheb.coef->nrow)	*/
	size_t	mxcterm;	/* Number of Chebyshev terms
				 * to compute initially		*/
	size_t	term;		/* Chebyshev term index		*/


	M0MT	*hFit = NULL;	/* dynamic matrix of values of
				 * orbit model at fit times
				 * (NVAR per time)		*/

#ifdef M0NVLOG
	char	szBuf[BUFSIZ];	/* text buffer for messaging	*/
#endif



#ifdef M0DEBUG

	/* These variables are used for Chebyshev approximation 
	 * validation */

	double	fe[NVAR];	/* differences between direct 
				 * and Chebyshev solutions	*/

	M0CR	CRerr;		/* Deviation of Chebyshev
				 * scanner coordinates from
				 * orbit model			*/
	M0CR	CRtrue;		/* Scanner coordinates from
				 * orbit model			*/

	M0PT	PTerr;		/* Deviation of Chebyshev
				 * position from orbit model 	*/
	M0PT	PTtrue;		/* Position from orbit model	*/

	M0VC	VCerr;		/* Deviation of Chebyshev
				 * velocity from orbit model	*/


#endif	/* ifdef M0DEBUG */

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);


	/* increment number of position calls; this will 	*
	 * determine when to initialize Chebyshev polynomials	*/

	nvblk.cheb.ncalls++;


	/* Compute the "normalized time" used as an argument
	 * to the Chebyshev polynomials */

	M0DItime_range( handle, &DTstart, &DTend );
	TMscene	= M0DTdiff( DTstart, DTend );
	dt	= M0DTdiff( DTstart, time );
	tnorm	= norm_time(dt, TMscene, 0 );



	/********************************************************
	 * DECIDE MODE OF SOLUTION based on number of calls.
	 * The first CALL_THRESH-1 calls use the orbit model to
	 * obtain the solution. On call CALL_THRESH, attempt
 	 * to initalize the Chebyshev polynomials, but return a
	 * direct solution. On subsequent calls, evaluate the
	 * Chebyshev polynomials if they were successfully
	 * initialized
	 *******************************************************/

        if(  nvblk.cheb.ncalls > CALL_THRESH ) {

	    if( nvblk.cheb.ready && tnorm >= -1. && tnorm <= 1. ) {

		/***************************************************
		 * EVALUATE Chebyshev polynomial expansions provided
		 * that ALL of the following are satisfied:
		 *   1) Chebyshev polynomials have been fitted on a
		 *      prior call
		 *   2) tnorm (normalized time) is in range for
		 *      Chebyshev polynomials 
		 * For compactness, the various positions and vector
		 * components are computed as an array, fa[], and then
		 * moved to the M0PT and M0CR structs with a call to
		 * vectopc()
		 ***************************************************/

		 for( var=0; var<NVAR; var++ ) {

			/* For each variable, evaluate the Chebyshev
			 * polynomial sum using Clemshaw's recurrance.
			 * The intermediate values are stored in array
			 * ad[] and the indices (pointers) p0, p1, and
			 * p2 are used to keep track of the current,
			 * most recent, and next most recent inter-
			 * mediate value. */
			
			p0	= 0;
			p1	= 1;
			p2	= 2;
			ad[p2]	= 0.;
			ad[p1]	= 0.;

			/* This loop accumulates all terms in the
			 * series but the last */

			M0ASSERT(M0validPointer((void *)nvblk.cheb.coef,
			  sizeof(M0MT)) );
			cterm = nvblk.cheb.coef->nrow;

			for( term=cterm-1; term>0; term-- ) {

				M0ASSERT(M0MTvalid(nvblk.cheb.coef,
				  term,var));
			        M0ASSERT(p0>=0 && p0<3);
			        M0ASSERT(p1>=0 && p1<3);
			        M0ASSERT(p2>=0 && p2<3);
				ad[p0] = 2.*tnorm*ad[p1] - ad[p2]
				  + nvblk.cheb.coef->val[term][var];
				p0 = p0>0 ? --p0 : p0+2;
				p1 = p1>0 ? --p1 : p1+2;
				p2 = p2>0 ? --p2 : p2+2;
			}

			/* The formula to generate the value using the
			 * recurrence results and the zero-order
			 * coefficient is slightly different. */

			M0ASSERT(var>=0 && var<NVAR);
			M0ASSERT(p0>=0 && p0<3);
			M0ASSERT(p1>=0 && p1<3);
			M0ASSERT(p2>=0 && p2<3);
			M0ASSERT(M0MTvalid(nvblk.cheb.coef,0,var));
			fa[var] = tnorm*ad[p1]-ad[p2]
			  +0.5*nvblk.cheb.coef->val[0][var];
		}
		vectopc(fa, pPTsat, pCRscan);
		
#ifdef M0DEBUG

		/* If built with M0DEBUG defined, validate the results
		 * from the Chebyshev approximation against an actual
		 * orbit model call and accumulate statistics */
		
		PTtrue	= M0DIorbpred( handle, time,&CRtrue );
		VCerr   = M0PTdiff( PTtrue, *pPTsat );
		CRerr.i = M0VCdiff( CRtrue.i, pCRscan->i );
		CRerr.j = M0VCdiff( CRtrue.j, pCRscan->j );
		CRerr.k = M0VCdiff( CRtrue.k, pCRscan->k );
		PTerr	= M0VCpt( VCerr );
		pctovec( &PTerr, &CRerr, fe );
		
		for( var=0; var<NVAR; var++) {
			nvblk.cheb.fmean[var] += fe[var];
			nvblk.cheb.fstdv[var] += fe[var]*fe[var];
			nvblk.cheb.fmx[var]    =
			  fe[var] > nvblk.cheb.fmx[var] ?
			  fe[var] : nvblk.cheb.fmx[var];
			nvblk.cheb.fmn[var]    =
			  fe[var] < nvblk.cheb.fmn[var] ?
			  fe[var] : nvblk.cheb.fmn[var];
		}
		nvblk.cheb.count++;

#endif		/* M0DEBUG */

    		rc = M0NDadd( hDIdata, handle, &nvblk, sizeof(nvblk) );
    		if( rc < 0 ) {
		        return -3;
    		}

		return 0;

	    }

	    else {	/*  Either time is out of range or
		 	 * Chebyshev polynomial fit failed;
			 * so do a direct solution */

		*pPTsat = M0DIorbpred ( handle,  time, pCRscan );

		/* Set up return code depending upon condition;
		 * Time out of range merits a warning as it is
		 * possibly a bug in the caller. */

    		rc = M0NDadd( hDIdata, handle, &nvblk, sizeof(nvblk) );
    		if( rc < 0 ) {
		        return -3;
    		}

		if( ! nvblk.cheb.ready ) {

		    /* Chebyshev polynomials not initialized */

		    return 0;

		} else {

		    /* Chebyshev polynomials ready, so it
		     * must have been the time out of range */

		    return -1;
		}
	    }
	}
	
        else if ( nvblk.cheb.ncalls == CALL_THRESH) {

		
		/***************************************************
		 * Attempt to fit Chebyshev polynomials.
		 **************************************************/


		/* Determine how many Chebyshev terms to compute
		 * initially. The initial choice will result in an
		 * average time separation of at most DTAVG seconds
		 * between fit points */

		pi = acos(-1.);

		M0DItime_range( handle, &DTstart, &DTend);
		tdiff = M0DTdiff ( DTstart, DTend );

		mxcterm = tdiff/DTAVG + 2;
		mxcterm = mxcterm < MNCTERM ? MNCTERM : mxcterm;


		/* Allocate storage for Chebyshev coefficients.
		 * These are stored in the navigation data block for
		 * this instance. If memory allocations fail,
		 * abandon further allocation attempts and evaluate
		 * the orbit model directly.
 		 *
		 * Subsequent calls will not attemp a refit; a direct
		 * solution will be used for all. */

		if ( nvblk.cheb.coef == NULL ) {
			rc = M0MTnew( &nvblk.cheb.coef,
			  mxcterm, NVAR );
		}
		else {
			rc = M0MTresize( &nvblk.cheb.coef,
			  mxcterm, NVAR );
		}

		if ( rc < 0 ) {
			*pPTsat = M0DIorbpred( handle, time, pCRscan);
			return 0;
		}


		/* Allocate storage for times to evaluate polynomials,
		 * orbit model results at these times (NVAR variables).
		 * These will be freed when the Chebyshev coefficients
		 * are fitted */

		if( ! M0newMemory( (void **)&atnorm,
		  mxcterm*sizeof(double) ) ) {
			*pPTsat = M0DIorbpred( handle, time, pCRscan );
			return -2;
		}
		if( M0MTnew( &hFit, mxcterm, NVAR) < 0  ) {
			M0freeMemory( (void **)&atnorm);
			*pPTsat = M0DIorbpred( handle, time, pCRscan );
			return -2;
		}
		


		    
		/* Generate fit points (times) and compute satellite
		 * positions and scanner orientations at these times */

		for( point=0; point<mxcterm; point++) {
			M0ASSERT(M0validPointer(&atnorm[point],
			  sizeof(double)) );
			atnorm[point]	= cos(pi*(point+0.5)/mxcterm);
			tdiff		= denorm_time(atnorm[point],
			  TMscene, 0. );
			DTepoch		= M0DTinc(DTstart, tdiff);
			PTtemp		= M0DIorbpred(handle,
			  DTepoch, &CRtemp);
			pctovec( &PTtemp, &CRtemp, hFit->val[point] );
		}


		/* Compute the coefficients of each Chebyshev term.
		 * Initialize the terms to zero, compute the series
		 * for each variable and term, and rescale all */

		for(     term = 0; term < mxcterm; term++ ) {
		    for(  var = 0;  var < NVAR; var++ ) {
			M0ASSERT(M0MTvalid(nvblk.cheb.coef,term,var));
			nvblk.cheb.coef->val[term][var] = 0;
		    }
		}

		for(     term =0; term  < mxcterm; term++ ) {
		    for( point=0; point < mxcterm; point++) {
		        chebfac = cos(pi*term*(point+0.5)/mxcterm);
			for( var=0; var<NVAR; var++) {
			    M0ASSERT(M0MTvalid(nvblk.cheb.coef,
			      term, var));
			    M0ASSERT(M0MTvalid(hFit, point,var));
			    nvblk.cheb.coef->val[term][var]
			      += hFit->val[point][var]*chebfac;
			}
		    }
		}

		for(     term =0; term <mxcterm; term++ ) {
		    for( var=0; var<NVAR; var++ ) {
			M0ASSERT(M0MTvalid(nvblk.cheb.coef,term,var));
			nvblk.cheb.coef->val[term][var] *= 2./mxcterm;
		    }
		}


		/* Truncate the Chebyshev series based on critical
		 * values of position and attitude direction cosine */

		cterm = mxcterm;

/* This routine will compute nominal nadir resolution of the current
 * sensor BUT IT DOESN'T EXIST YET !!!!!
 *		DIXnomRes ( &lres, &eres, &altitude );
 *
 *		mxPosErr = lres < eres ? lres : eres;
 *		mxPosErr *= NAVACC;
 *		mxAttErr = mxPosErr / altitude;
 * (end of call to un-implemented function */

		mxPosErr = 0.5;		/* position good to
					 * 500 m 		*/
		mxAttErr = 5.e-5;	/* attitude good to
					 * ground pointing error 
					 * of 500 m at 60 degree
					 * zenith angle		*/

#ifdef M0NVLOG
		M0NLMSG(handle, "Chebyshev position terms\n");
		for(term=0; term<mxcterm; term++ ) {
		    sprintf(szBuf,"%4d %9.3lf %9.3lf %9.3lf\n",
		      term, nvblk.cheb.coef->val[term][0],
		      nvblk.cheb.coef->val[term][1],
		      nvblk.cheb.coef->val[term][2] );
		    M0NLMSG(handle, szBuf);
		}
		M0NLMSG(handle, "Chebyshev attitude (i) terms\n");
		for(term=0; term<mxcterm; term++ ) {
		    sprintf(szBuf,"%4d %9.6lf %9.6lf %9.6lf\n",
		      term, nvblk.cheb.coef->val[term][3],
		      nvblk.cheb.coef->val[term][4],
		      nvblk.cheb.coef->val[term][5] );
		    M0NLMSG(handle, szBuf);
		}

		M0NLMSG(handle, "Chebyshev attitude (j) terms\n");
		for(term=0; term<mxcterm; term++ ) {
		    sprintf(szBuf,"%4d %9.6lf %9.6lf %9.6lf\n",
		      term, nvblk.cheb.coef->val[term][6],
		      nvblk.cheb.coef->val[term][7],
		      nvblk.cheb.coef->val[term][8] );
		    M0NLMSG(handle, szBuf);
		}
		M0NLMSG(handle, "Chebyshev attitude (k) terms\n");
		for(term=0; term<mxcterm; term++ ) {
		    sprintf(szBuf,"%4d %9.6lf %9.6lf %9.6lf\n",
		      term, nvblk.cheb.coef->val[term][9],
		      nvblk.cheb.coef->val[term][10],
		      nvblk.cheb.coef->val[term][11] );
		    M0NLMSG(handle, szBuf);
		}
#endif	/* ifdef M0NVLOG */


		/* Truncate the series by dropping terms until
		 * ANY term contributes more than the specified
		 * accuracy */

		do {
			cterm--;

		}
		while ( nvblk.cheb.coef->val[cterm][ 0] < mxPosErr &&
			nvblk.cheb.coef->val[cterm][ 1] < mxPosErr &&
			nvblk.cheb.coef->val[cterm][ 2] < mxPosErr &&

			nvblk.cheb.coef->val[cterm][ 3] < mxAttErr &&
			nvblk.cheb.coef->val[cterm][ 4] < mxAttErr &&
			nvblk.cheb.coef->val[cterm][ 5] < mxAttErr &&

			nvblk.cheb.coef->val[cterm][ 6] < mxAttErr &&
			nvblk.cheb.coef->val[cterm][ 7] < mxAttErr &&
			nvblk.cheb.coef->val[cterm][ 8] < mxAttErr &&

			nvblk.cheb.coef->val[cterm][ 9] < mxAttErr &&
			nvblk.cheb.coef->val[cterm][10] < mxAttErr &&
			nvblk.cheb.coef->val[cterm][11] < mxAttErr );
		
		cterm++;

		/* Resize the dynamic array. This makes the truncation
		 * 'permanent.' On a (highly unlikely since it's
		 * shrinking) resize error, free other allocated
		 * memory and return a direct solution */

		if( M0MTresize( &nvblk.cheb.coef, cterm, NVAR) != 0 ) {
			M0freeMemory( (void **)&atnorm);
			M0MTdel( &nvblk.cheb.coef );
			M0MTdel( &hFit );
			*pPTsat = M0DIorbpred( handle, time, pCRscan );
			return -2;
		}
		

		/* Initialize Chebyshev polynomial quality
		 * statistics if compiled with M0DEBUG defined */

#ifdef M0DEBUG
		for( var=0; var<NVAR; var++ ) {
			nvblk.cheb.fmean[var] = 0.;
			nvblk.cheb.fstdv[var] = 0.;
			nvblk.cheb.fmx[var]   = -DBL_MAX;
			nvblk.cheb.fmn[var]   =  DBL_MAX;
		}
		nvblk.cheb.count = 0;
#endif

		/* Release memory used for intermediate results
		 * and set 'ready' flag for next time. then do a
		 * direct solution and return */

		M0freeMemory( (void **)&atnorm );
		M0MTdel(&hFit);

		nvblk.cheb.ready = M0TRUE;
		*pPTsat = M0DIorbpred( handle, time, pCRscan);

    		rc = M0NDadd( hDIdata, handle, &nvblk, sizeof(nvblk) );
    		if( rc < 0 ) {
		        return -3;
    		}

		return 0;

	}
	else {	/* nvblk.cheb.ncalls < CALL_THRESH */
		
		/* Direct computation; no Chebyshev polynomials
		 * fit yet. */

		*pPTsat = M0DIorbpred( handle, time, pCRscan);

    		rc = M0NDadd( hDIdata, handle, &nvblk, sizeof(nvblk) );
    		if( rc < 0 ) {
		        return -3;
    		}

		return 0;
	}
}


#ifdef M0DEBUG

/*
*| Name:
*|	M0DIcheb_eval - Print summary of Chebyshev polynomial
*|			performance
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0DIcheb_eval(int handle);
*|
*| Input:
*|	handle	- unique identifier of navigation instance
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
*|	    This routine is available only when compiled with M0DEBUG
*|	defined. The summary is written to 'stderr.'
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

void
M0DIcheb_eval(int handle)
{
	double		var;	/* variance			*/
	int		rc;	/* function return code		*/
	int		i;	/* variable index		*/
	navblock	nvblk;	/* Current instance of
				 * navigation data block	*/

	char	*varname[] = { "posx", "posy", "posz",
	  "ix  ", "iy  ", "iz  ", "jx  ", "jy  ", "jz  ",
	  "kx  ", "ky  ", "kz  " };

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);


	/* Now write the summary header */

	fprintf(stdout, "Chebyshev polynomial performance "
	  "statistics\n");
	fprintf(stdout, "based on %d comparisons with full "
	  "orbit model.\n", nvblk.cheb.count );

	if( nvblk.cheb.count > 0 ) {
		fprintf(stdout, " var      mean         stdv"
		  "         min          max\n");

		/* Compute the means and variances */

		for(i = 0; i < 12; i++) {
			nvblk.cheb.fmean[i] /= nvblk.cheb.count;
			var = nvblk.cheb.fstdv[i] / 
			  nvblk.cheb.count - nvblk.cheb.fmean[i] *
			  nvblk.cheb.fmean[i];
			if( var < 0. ) {
				nvblk.cheb.fstdv[i] = 0.;
				fprintf(stderr,"variable %2d "
				  "variance=%f\n", i, var);
			} else {
				nvblk.cheb.fstdv[i] = sqrt(var);
			}
		}

		/* Write the summary */

		for(i = 0; i < 12; i++) {
			fprintf(stdout, " %s %12.8lf %12.8lf %12.8lf "
			  "%12.8lf\n", varname[i],
			  nvblk.cheb.fmean[i], nvblk.cheb.fstdv[i],
			  nvblk.cheb.fmn[i], nvblk.cheb.fmx[i] );
		}
	}
	else {
		fprintf(stdout, " <no Chebyshev statistics "
		  "available>\n");
	}
	fprintf(stdout, "--------------------------------------"
	  "------------------\n"); 
	fprintf(stdout, "press <enter> when done viewing: ");
	fflush(stdin);
	fgetc(stdin);
	fflush(stdin);

	return;
}

void
M0DImemchk(int handle)
{
	navblock	nvblk;	/* single instance nav block	*/	
	int		rc;	/* return code			*/

	/* first mark the nav block as referenced	*/

	M0NDmemchk(hDIdata);	

	/* Then get the instance 'handle' and validate its	*
	 * dynamic portions */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);

	M0ASSERT(rc == 0);

	if( nvblk.cheb.coef != NULL ) {
		M0MTmemchk( nvblk.cheb.coef );
	}

	return;
}

#endif	/* #ifdef M0DEBUG */



/*
*| Name:
*|	M0DIpt_visible - Determine if point is theoretically
*|			 visible from satellite
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	M0flag
*|	M0DIpt_visible(int handle, M0PT PTtarget, M0DT DTview,
*|	  M0flag *pVisible);
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	PTtarget	- target point
*|	DTview		- time of view
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	M0TRUE		- if point is visible
*|	M0FALSE		- if satellite is below the horizon
*|
*| Remarks:
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|	    This routine will always produce a solution; the return
*|	code is diagnostic only.
*|
*| Categories: 
*|	navigation  
*/

M0flag
M0DIpt_visible(int handle, M0PT PTtarget, M0DT DTview)
{
	double  dot_prod;	/* dot product of line of sight
				 * to target and local vertical	*/

	int	orbit_status;	/* status of orbit model	*/
	int	rc;		/* function return code		*/

	navblock nvblk;		/* current instance of
				 * navigation data		*/

	M0CR     CRsat;		/* Satellite scanner coordinate	*
				 * at time DTview		*/

	M0PT     PTsat;		/* satellite position at time
				 * DTview			*/
	M0PT     PTtarget_cc;	/* Target position in celestial
				 * cartesian coordinates	*/

	M0VC     VCview;	/* Line of sight satellite to
				 * target			*/ 
	M0VC     VCDown;	/* Local vertical		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc==0);

	/* Compute the satellite position at time DTview and
	 * the target position in celestial cartesian
	 * coordinates. The assertion will fire if the time
	 * is outside the range of the Chebyshev polyniomials,
	 * probably indicating a bug in the computation of DTview */

	orbit_status	=  M0DIpt_sat( handle, DTview,
          &PTsat, &CRsat );
	M0ASSERT(orbit_status == 0);

	PTtarget_cc	= M0PLpt_convert( handle, PTtarget,
	  DTview, CC );

	/* Compute the line of sight vector from satellite to
	 * target and local vertical through the target. The
	 * dot product of the two must be positive if the
	 * satellite is above the horizon */

	VCview		= M0PTdiff( PTsat, PTtarget_cc);
	VCDown		= M0PLdown( handle, PTtarget_cc);
	dot_prod        = M0VCinner(VCview,VCDown);

	if (dot_prod >= 0.) {
		return M0TRUE;
	} else {
		return M0FALSE;
	}
}


/*
*| Name:
*|	M0DIstatus - Check status of orbit model
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0DIstatus(int handle, M0flag *pCheb_ready, int *pNcalls,
*|	  *pNterms);
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pCheb_ready	- M0TRUE if Chebyshev polynomials in use
*|			  M0FALSE otherwise
*|	*pNcalls	- number of orbit model calls since
*|			  last initialized 
*|	*pNterms	- number of terms in Chebyshev series
*|
*| Return values:
*|	 0	- Nominal
*|	-1	- Chebyshev polynomial fit failed; using
*|		  direct orbit model calls as backup
*|
*| Remarks:
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

int
M0DIstatus(int handle, M0flag *pCheb_ready, int *pNcalls, int *pNterms)
{
	int		rc;	/* function return code		*/
	navblock	nvblk;	/* nav data for this instance	*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);

	/* Pick up number of calls, Chebyshev polynomial
	 * status, and number of terms. Set return code
	 * accordingly */

	*pCheb_ready	= nvblk.cheb.ready;
	*pNcalls	= nvblk.cheb.ncalls;

	if( *pCheb_ready ) {
		*pNterms	= nvblk.cheb.coef->nrow;
	} else {
		*pNterms	= 0;
	}

	if( ! nvblk.cheb.ready &&
	      nvblk.cheb.ncalls >= CALL_THRESH ) {
		return -1;
	} else {
		return 0;
	}
}

/*
*| Name:
*|	M0DItime_range - Return the time range to use for nav
*|			 calculations
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	void
*|	M0DItime_range(int handle, M0DT *pDTstart, M0DT *pDTend )
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pDTstart	- start time
*|	*pDTend		- end time
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    The time range is padded by an amount M0TPAD (defined
*|	in m0gpnav.h) to allow for the time width of a closed
*|	locus (cone scanner such as SSM/I) so that the time
*|	range will bracket all view times (corresponding to
*|	the start and end of the orbit portion) and also the
*|	times when all viewed points are in the 'reference locus'	
*|	(in a plane perpendicular to the orbit plane.)
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

void
M0DItime_range(int handle, M0DT *pDTstart, M0DT *pDTend)
{
	int		rc;	/* function return code		*/
	navblock	nvblk;	/* current instance of
				 * navigation data		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);
 

	/* Compute the padded start and end times */

	*pDTstart  = M0DTinc ( nvblk.DTfirst_scan, -M0TPAD );
	*pDTend    = M0DTinc ( nvblk.DTlast_scan ,  M0TPAD );

	return;
}

/*
*| Name:
*|	M0DIfv_time - compute the time of a field of view
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	M0DT
*|	M0DIfv_time(int handle, M0FV fv);
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	fv		- a field of view descriptor
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	Time at which the satellite views 'fv'.
*|
*| Remarks:
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

M0DT
M0DIfv_time(int handle, M0FV fv)
{
	int		rc;	/* function return code		*/
	navblock	nvblk;	/* Current instance of
				 * navigation data		*/
	M0DT		DTview;	/* Time 'fv' is viewed		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);

	/* Compute the field of view time from the start time
	 * of the image, the time between scan starts, and the
	 * time between line starts */

	DTview = nvblk.DTfirst_scan;
	DTview = M0DTinc( DTview,
	  nvblk.TMscan*(fv.scan-(double)nvblk.firstscan) +
	  nvblk.TMelem*(fv.elem-1.) );

	return DTview;
}

		
/*
*| Name:
*|	M0DIfv_angles - compute sensor pointing angles for
*|			a field of view
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	void
*|	M0DIfv_angles(int handle, M0FV fv,
*|	  double *pPhi, double *pTheta);
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	fv		- a field of view descriptor
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pPhi	- angle from scanner axis to view vector
*|	*pTheta	- rotation of view vector about scanner <i> axis
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

void
M0DIfv_angles(int handle, M0FV fv, double *pPhi, double *pTheta)
{
	int		rc;	/* function return code		*/
	navblock	nvblk;	/* Current instance of
				 * navigation data		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);
 
	/* Compute angle from scanner axis <k> to view vector.
	 * Only for banked detectors (MODIS) would this number
	 * vary; otherwise it is a constant for each sensor	*/

	*pPhi = nvblk.phi0 + nvblk.phi_incr * (fv.line-1.);

	/* Compute scanner rotation angle.  Determine whether scanner
	 * is oscillatory or rotating.  OLS is the only oscillating
	 * scanner known to me as of Dec 1994; other geometries may
	 * have to be treated differently */

	if ( nvblk.scan_type == ROT ) {
	
		*pTheta = nvblk.scan.start
		       + nvblk.scan.incr * (fv.elem-1.);
	}
	else if ( nvblk.scan_type == OSC ) {

	    *pTheta = nvblk.scan.th_max * cos ( 
		     nvblk.scan.phase_incr * (fv.elem-1.)
		   + nvblk.scan.phase_bias )
		   + nvblk.scan.th_bias;
	}
	else {
		M0ASSERT(M0FALSE);  /* un-implemented scanner type */
	}
	return;
}


/*
*| Name:
*|	M0DIangles_fv - compute field of view descriptor
*|			from pointing vector angles
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	M0flag
*|	M0DIangles_fv(int handle, M0DT DTview, double phi,
*|	  double theta, M0FV *pFV);
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|	DTview	- time of view
*|	phi	- angle from scanner axis to view vector
*|	theta	- rotation of view vector about scanner <i> axis
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pFV	- a field of view descriptor
*|
*| Return values:
*|	M0TRUE	- success
*|	M0FALSE	- sensor never views those angles
*|
*| Remarks:
*|
*| Categories: 
*|	navigation  
*/

M0flag
M0DIangles_fv(int handle, M0DT DTfv, double phi, double theta,
  M0FV* pFV )
{
	double		scan;	/* scan (image line)		*/
	double		line;	/* detector number in scan	*/
	double		elem;	/* element (image element)	*/
	double		th_norm;/* (theta-th_bias)/th_max, 
				 * intermediate result in
				 * oscillating scanner
				 * element conversion */

	int     	mnScan;	/* min value of scan for sensor	*/
	int     	mxScan;	/* max value of scan for sensor	*/
	int     	mnLine;	/* min value of line for sensor	*/
	int     	mxLine;	/* max value of line for sensor	*/
	int     	mnElem;	/* min value of elem for sensor	*/
	int     	mxElem;	/* max value of elem for sensor	*/

	int		rc;	/* function return code		*/

	M0TM		TMoff;	/* elapsed time from orbit	*
				 * start			*/
	M0TM		TMelem;	/* time increment from 		*
				 * scan start to element	*/

	navblock	nvblk;	/* Current instance of
				 * navigation data		*/



	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0 );


	/* First compute the line and element numbers from
	 * the angles phi and theta. Knowing the line and
	 * element numbers are important to knowing how
	 * to compute the scan number from the time */
	
	line = 1.;

	if ( nvblk.phi_incr != 0. ) 
	    line += (phi-nvblk.phi0)/nvblk.phi_incr;

	if ( nvblk.scan_type == ROT ) {
		elem = (theta-nvblk.scan.start) 
		  / nvblk.scan.incr + 1.;
	}
	else if ( nvblk.scan_type == OSC ) {
	    
	    th_norm = (theta-nvblk.scan.th_bias)/nvblk.scan.th_max;
	    if ( fabs(th_norm) > 1. )
		return M0FALSE;
	    elem = (acos(th_norm)-nvblk.scan.phase_bias)
		 / nvblk.scan.phase_incr + 1.;
	}
	else {
	    M0ASSERT(M0FALSE);	/* unknown/unspecified scan_type */
	}

	/* Now compute the scan number from the time at which
	 * the target point lay in the 'scanner locus.'
	 * This is done by computing the elapsed time
	 * since the scene start and further reducing
	 * this interval by the the elapsed time from the
	 * start of a scan line to the computed element.
	 * This yields the start of a 'fictitious' scan
	 * cycle that passes exactly over the target.
	 * Actual scan cycles start at discrete intervals
	 * but the fractional 'scan number' of the 
	 * fictitious one can be computed by simple
	 * interpolation */

	TMoff	= M0DTdiff ( nvblk.DTfirst_scan, DTfv );
	TMelem	= nvblk.TMelem * (elem-1.);

	scan	= (TMoff-TMelem) / nvblk.TMscan +
	  (double)nvblk.firstscan;


	/* Test to be sure that scan, line, and element are
	 * within range. Return 'failure' if not */

	M0DIlimits(handle, &mnScan, &mxScan, &mnLine, 
	  &mxLine, &mnElem, &mxElem );

	if ( scan < mnScan-.5 || scan > mxScan+.5 ||
	     line < mnLine-.5 || line > mxLine+.5 ||
	     elem < mnElem-.5 || elem > mxElem+.5    ) 
	     
		return M0FALSE;

	else {

		pFV->scan = scan;
		pFV->line = line;
		pFV->elem = elem;

		return M0TRUE;
	}
}


/*
*| Name:
*|	M0DIvc_ptg - compute pointing vector from view angles
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	M0VC
*|	M0DIvc_ptg(M0CR CRscan, double phi, double theta);
*|
*| Input:
*|	CRscan	- current scanner coordinate system
*|	phi	- angle from scanner axis to view vector
*|	theta	- rotation of view vector about scanner <i> axis
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	pointing vector
*|
*| Remarks:
*|	    None.
*|
*| Categories: 
*|	navigation  
*/

M0VC
M0DIvc_ptg(M0CR CRscan, double phi, double theta)
{
	M0VC     VCptg;			/* computed pointing	* 
					 * vector		*/
	M0VC     VCptg_scan_start;	/* pointing vector at	*
					 * start of scan	*/

	/* First rotate the k vector about j to get the
	 * reference pointing vector. Note that for phi=90
	 * degrees this will be coordinate system <i>. */

	VCptg_scan_start =
	  M0VCrotate ( CRscan.j, phi, CRscan.k );

	/* Then rotate the reference pointing vector (theta=0)
	 * around scanner k axis by theta to get the actual
	 * pointing vector */

	VCptg =
	  M0VCrotate ( CRscan.k, theta, VCptg_scan_start );
	
	return VCptg;
}



/*
*| Name:
*|	M0DIangles_ptg - compute pointing angles from a view vector
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	M0VC
*|	M0DIangles_ptg(M0CR CRscan, M0VC VCptg,
*|	  double *pPhi, double *pTheta);
*|
*| Input:
*|	CRscan	- current scanner coordinate system
*|	VCptg	- pointing vector
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pPhi	- angle from scanner axis to view vector
*|	*pTheta	- rotation of view vector about scanner <i> axis
*|
*| Return values:
*|	pointing vector
*|
*| Remarks:
*|	    None.
*|
*| Categories: 
*|	navigation  
*/

void
M0DIangles_ptg(M0CR CRscan, M0VC VCptg, double *pPhi,
  double *pTheta)
{
	double	sin_th;		/* sine of theta		*/

	M0VC	VCproj;		/* normalized pointing vector
				 * projection in (i,j) plane	*/

	/* Compute 'phi' directly as angle between pointing
	 * vector and scanner axis <k> */

	*pPhi   = M0VCangle ( CRscan.k, VCptg );

	/* 'theta' is harder; it is the angle between the 
	 * projection of VCptg into scanner <i><j> plane
	 * and <i>. Do the projection first, then use a
	 * dot product to get the sine of theta */

	VCproj = M0VCnorm ( M0VCproject ( CRscan.k, VCptg ) );
	sin_th  = M0VCinner ( M0VCcross(CRscan.i,VCproj), CRscan.k);
	*pTheta = asin(sin_th);

	return;
}

/*
*| Name:
*|	M0DIfv_make - construct field of view descriptor from
*|		      image coordinates
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0DIfv_make(int handle, float scan, float line, float elem,
*|	  *pFV);
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|	scan	- scan number (image line)
*|	line	- detector number within scan
*|	elem	- element number (image element)
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pFV	- field of view descriptor
*|
*| Return values:
*|	M0TRUE	- success
*|	M0FALSE	- scan, line, or elem outside sensor limits
*|
*| Remarks:
*|	    The floating point scan, line, and elem are converted
*|	to elements of 'M0FV' by truncation. This follows McIDAS
*|	convention.
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|	
*|	    None.
*|
*| Categories: 
*|	navigation  
*/

M0flag
M0DIfv_make(int handle, float scan, float line, float elem,
  M0FV* pFV )
{
	int     	mnScan;	/* min value of scan for sensor	*/
	int     	mxScan;	/* max value of scan for sensor	*/
	int     	mnLine;	/* min value of line for sensor	*/
	int     	mxLine;	/* max value of line for sensor	*/
	int     	mnElem;	/* min value of elem for sensor	*/
	int     	mxElem;	/* max value of elem for sensor	*/
	int		rc;	/* function return code		*/

	navblock	nvblk;	/* Current instance of
				 * navigation data		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);
 

	/* Get scanner limits and check inputs against them */

	M0DIlimits(handle, &mnScan, &mxScan, &mnLine, &mxLine,
	  &mnElem, &mxElem );


	if ( scan < mnScan || scan > mxScan ||
	     line < mnLine || line > mxLine ||
	     elem < mnElem || elem > mxElem ) {

		return M0FALSE;
	}


	/* input image coordinates are within legal limits.
	 * Now handle flipping and construct a field of view
	 * descriptor */
	
	if(nvblk.flipElems) {
		pFV->elem = (double)(nvblk.firstelem
		  + nvblk.nelems - 1 - elem);
	}
	else {
		pFV->elem = (double)elem;
	}

        if(nvblk.flipScans) {
	    
		/* multiple lines are a hook for MODIS ScanCube
		 * format. It has not been determined how to
		* handle flipping (or much else, for that matter)
		* for this format. So trap flipped multi-line
		* scans with an assertion */

		M0ASSERT(nvblk.nlines==1);

		pFV->scan = (double)(nvblk.firstscan
		  + nvblk.nscans - 1 - scan); 
	}
	else {
		pFV->scan = (double)scan;
	}

	pFV->line = (double)line;

	return M0TRUE;
}


/*
*| Name:
*|	M0DIfv_extract - extract image coordinates from field of
*|		         view descriptor
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	void
*|	M0DIfv_extract(int handle, M0FV fv,
*|	  float *pScan, float *pLine, float *pElem);
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|	fv	- field of view descriptor
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pScan	- scan number (image line)
*|	*pLine	- detector number within scan
*|	*pElem	- element number (image element)
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    Field of view descriptors (type M0FV) are guaranteed
*|	valid (scan, line, elem in range) if produced using 
*|	routines in this module (M0DI series).
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|	
*|	    None.
*|
*| Categories: 
*|	navigation  
*/
void
M0DIfv_extract(int handle, M0FV fv,
  float *pScan, float *pLine, float *pElem)

/* See the beginning of this module for an exhaustive discussion
 * of image/area flipping */
{
	int		rc;	/* function return code		*/
	navblock	nvblk;	/* Current instance of
				 * navigation data		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);
 
	/* extract the components, accounting for the possibility
	 * of flipped data */

	if(nvblk.flipScans) {
	    *pScan = (float)(nvblk.firstscan+nvblk.nscans-1-fv.scan);
	}
	else {
	    *pScan = (float)fv.scan;
	}

	if(nvblk.flipElems) {
	    *pElem = (float)(nvblk.firstelem+nvblk.nelems-1-fv.elem);
	}
	else {
	    *pElem = (float)fv.elem;
	}

	*pLine  = (float)fv.line;

	return;
}


/*
*| Name:
*|	M0DIlimits - return image coordinate limits for sensor
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	void
*|	M0DIlimits(int handle,
*|	  int *pFirst_scan, int *pLast_scan,
*|	  int *pFirst_line, int *pLast_line,
*|	  int *pFirst_elem, int *pLast_elem);
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pFirst_scan	- first valid scan (image line)
*|	*pLast_scan	- last  valid scan (image line)
*|	*pFirst_line	- first valid detector line
*|	*pLast_line	- last  valid detector line
*|	*pFirst_elem	- first valid image element
*|	*pLast_elem	- last  valid image element
*|
*| Return values:
*|	none
*|
*| Remarks:
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|	
*| Categories: 
*|	navigation  
*/
void
M0DIlimits(int handle,
  int *pFirst_scan, int *pLast_scan,
  int *pFirst_line, int *pLast_line,
  int *pFirst_elem, int *pLast_elem)
{
	int		rc;	/* function return code		*/
	navblock	nvblk;	/* Current instance of
				 * navigation data		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);
 
	*pFirst_scan     = nvblk.firstscan;
	*pLast_scan      = nvblk.firstscan + nvblk.nscans - 1;

	*pFirst_line     = nvblk.firstline;
	*pLast_line      = nvblk.firstline + nvblk.nlines - 1;

	*pFirst_elem     = nvblk.firstelem;
	*pLast_elem      = nvblk.firstelem + nvblk.nelems - 1;

	return;
}


/*
*| Name:
*|	M0DIperiod - return orbital period of current satellite
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	double	
*|	M0DIperiod(int handle)
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	orbital period (seconds)
*|
*| Remarks:
*|	    Routine M0DIinit() must be called first to initialize the
*|	module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

double
M0DIperiod( int handle )
{
	int		rc;	/* function return code		*/
	navblock	nvblk;	/* Current instance of
				 * navigation data		*/
	double		period;	/* orbital period		*/

	/* Load current (handle) instance of navigation data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hDIdata, handle, sizeof(nvblk), &nvblk);
	M0ASSERT(rc == 0);
 
	if ( nvblk.orbmodel == BROUWER ) {

		/* for Brouwer model, compute orbital period from
		 * semimajor axis, eccentricity, and inclination */

		period = (double)SEC_PER_DAY
		  / m0atomm_ ( &nvblk.ble.semimajor,
		  &nvblk.ble.ecc, &nvblk.ble.incl );
	}

#ifdef SGP_ON

	else if ( nvblk.orbmodel == SGP ) {

		/* For SGP model, period comes directly out
		 * of two-line elements (with a unit change) */

		period = (double)SEC_PER_DAY / nvblk.tle.mean_mot;
	}
#endif
	else {
	    M0ASSERT(M0FALSE);	/* corrupted orbit model name */
	}
	return period;
}
