/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 GPNAV.C 27-Feb-96,13:14:42,`ROBERTM' initial checkin of DMSP nav        */
/* 2 GPNAV.C 17-Apr-96,14:48:40,`USER' Released                              */
/* 3 GPNAV.C 27-Sep-96,16:30:04,`ROBERTM' Include mcidasp.h (7020)           */
/* 4 GPNAV.C 11-Oct-96,10:31:52,`ROBERTM' free inverse nav solutions (7020)  */
/* 5 GPNAV.C 22-Oct-96,19:41:24,`USER' Released                              */
/**** McIDAS Revision History *** */

/* The navigation logging (text and graphics) are controlled
 * by compile time definition of M0NVLOG and M0LCGRF.
 *
 * At present it is assumed that the CALLER has already
 * called M0NLOPEN() to open the navigation log. Thus
 * logging only really makes sense in conjunction with the
 * unit test gpntest.c and not with McIDAS navigation.
 *
 * To allow multiple inverse solutions from nvxeas_, define
 * M0_MULTI_INVERSE in 'm0gpnav.h'. It may eventually be
 * desireable to implement an 'gpnopt()' feature to allow
 * the user to select which solution they want...first,
 * second...last, etc.
 */

/***********************************
 * INCLUDES
 **********************************/

#include <math.h>
#include <string.h>
#include "mcidas.h"
#include "mcidasp.h"

#include "m0frame.h"
#include "m0gpnav.h"

/* Changes:
 * 96.02.11 rtm	Convert to re-entrant C navigation. Rename
 *		gpnav.c
 * 95.11.15 rtm	Begin upgrade to core conventions
 * 95.06.26 rtm	Renamed 'nvxgnav.c' for use with new 'makgnav'
 * 		script on 'whirly'.
 * 95.05.04 rtm Replace call to DIXinit with m0DIXinit
 * 95.03.14 rtm Add 'XYZ ' to NVXini_ option 2. It sets
 *		CT (Cartesian terrestrial) navigation.
 *		Preset navigation I/O type to LLT.
 * 95.03.12 rtm Add satellite radius from earth center
 *		and celestial coordinate position to
 *		SPOS option in NVXopt_.
 * 95.03.11 rtm	Add options 'SPOS' (compute satellite
 *		subpoint) and 'ORBT' (compute orbital
 *		period) to NVXopt_.
 * 95.03.08 rtm	NVXini_ calls PLNXinit with a variable
 *		argument rather than a string constant.
 *		PLNXinit operations on string constant
 *		were not working when compiled using
 *		gcc.
 * 95.02.27 rtm Put option first and codicil second in
 *		nvxini call. Change nvxini 'option'
 *		handling. If 1,	initialize. If 2, set
 *		current coordinate type.  'LL  '
 *		invokes type LLT
 * 95.02.22 rtm Change all 'locus' module calls to
 *		LOCXwhatever. Prefix planet module calls with
 *		PLNX. Prefix nav calls with NVX. Prefix log
 *		calls with NLX.
 * 95.02.20 rtm Change all 'di' module function calls
 *		from DIwhatever to DIXwhatever.
 * 95.02.07 rtm Add conditional compilation of
 *		NVXeas_ under control of MULTIPLE_INVERSES
 *		to determine whether inverse navigation
 *		will solve for all solutions or just
 *		the first.
 * 95.02.03 rtm Placed all stand-alone testing code
 *		in own module 'nvxtst.c'
 * 95.02.02 rtm Added 'scene coverage' routine to plot lat
 *		lon mesh where visible in orbit swath. Tie
 *		log output to log level
 * 95.01.31 rtm Added forward/inverse nav transform pair
 *        	test.
 * 95.01.25 rtm	Change log started. Moved computation of
 *		pointing vector from angles out of NVXsae_
 *		to DIvecPtg. It's useful in plotting loci, too
 */

/******************************************************************
 * TYPEDEF for retaining I/O coordinate type for multiple instances
 *****************************************************************/

typedef struct {
	M0CT	coord;		/* earth coordinate form	*/
	M0flag	log_open;	/* nav logging file is open 	*/
}
GPNinfo;

/*********************************
 * SUPPORT FOR MULTIPLE INSTANCES 
 ********************************/

static M0ND
*hGPNdata = NULL;


/*************************************************
 * DEFINITIONS of re-entrant navigation operations
 ************************************************/

/*
*$ Name:
*$	mcgpnini - initialize generic polar navigation
*$
*$ Interface:
*$	integer function
*$	mcgpnini(integer handle, integer option, integer navparms)
*$
*$ Input:
*$	handle	- unique identifier of navigation instance
*$	option	- 1 to initialize navigation module
*$		  2 to set Earth coordinate form
*$	navparms- navigation parameter array for option=1
*$		  lit('LL  ') or lit('XYZ ') for option=2
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	none
*$
*$ Return values:
*$	  0	- navigation module initialized
*$	 -1	- could not initialize re-entrant data collection
*$	 -2	- could not initialize planet module
*$	 -3	- could not initialize inverse nav engine (locus)
*$	 -4	- navigation module could not be initialized
*$		  with the parameters given (option=1)
*$	 -5	- unrecognized Earth coordinate form (option=2)
*$	 -6	- could not save re-entrant data
*$	-10	- unrecognized 'option'
*$
*$ Remarks:
*$	'Navparms' must have a dimension of at least 128 as
*$	defined by the caller. The contents of the navigation
*$	parameter array are satellite and sensor-specific.
*$
*$ Categories: 
*$	navigation  
*/


/* "secret" return codes are -1000 or less -- they have to
 * do with returns from nav logging and such and do not occur
 * in the ship version:
 *	-1000	- could not open nav log file */

/*  QUESTIONS -- 1) how to handle 'lit()' operation in C?
 *		 2) what about byte-flipping of characters
 *		    stored in 'Fint'
 */

Fint
mcgpnini_(Fint *F_handle, Fint *F_option, Fint *F_nav_block)
{
	/* Local variables		*/

#ifdef M0NVLOG
	char		szLogmsg[BUFSIZ];	/* log message	*/
#endif
	char		szNavtype
			[sizeof(Fint)+1];	/* nav codicil	*
						 * type		*/

	GPNinfo		gpn_state;		/* nav module	*
						 * state for	*
						 * this instance*/

	int		handle;			/* 'int' form	*
						 * of nav 	*
						 * intstance	*/
	int		i;			/* loop index	*/
	int		rc;			/* return code	*/

	M0flag		ok;			/* Boolean	*
						 * return code	*/

	/* Initialized variables */

	char	szVersion[] = M0_GPN_VERSION;	/* Navigation	*
						 * version (for	*
						 * log only	*/
	char	earth[]	    = "earth";		/* current	*
						 * planet	*/
	char	szLogname[] = "gpnlog";		/* nav log file	*
						 * name	template*/

	/* On first call, initialize the re-entrant data 
	 * collection */

	if( hGPNdata == NULL ) {

		rc = M0NDnew( sizeof(GPNinfo), &hGPNdata);
		if( rc < 0 ) {
			return -1;
		}
	}

	/* Get current state for this version, if any. */

	handle = *F_handle;

	rc = M0NDget( hGPNdata, handle, sizeof(GPNinfo), &gpn_state);
	if( rc < 0 ) {

		/* No previous instance for this handle; set coordinate
		 * to Lat-Lon Terrestrial (geodetic) and note that
		 * no nav log is open */

		gpn_state.coord		= LLT;
		gpn_state.log_open	= M0FALSE;
	}

	switch ( *F_option ) {

		case 1:

			/* Set planet to 'earth.' Then attempt to
			 * initialize the navigation */

#ifdef M0NVLOG
			if( ! gpn_state.log_open ) {
		    		rc = M0NLopen( handle, szLogname,
				  szVersion);
				if( rc < 0 ) {
					return -1001;
				}
				gpn_state.log_open = M0TRUE;
			}
#endif

		    	rc = M0PLinit( handle, earth ); 
			if( rc < 0 ) {
				return -2;
			}
			rc = M0locus_init( handle );
			if( rc < 0 ) {
				return -3;
			}
		    	rc = M0DIinit( handle, F_nav_block );
		    	if ( rc < 0  ) {
		        	return -4;
		    	}
#ifdef M0LOCGRF 
			/* Switch locus plotting OFF */

			M0locus_plot_clr( handle );
#endif
			break;

		case 2:

			/* set interface I/O coordinates. At
			 * present only terrestrial coordinates 
			 * are supported. gpninv_() logic requires
			 * conversion to a terrestrial type
			 * before time is known, so celestial
			 * systems cannot be done */

			memcpy ( szNavtype, &F_nav_block[0],
			  sizeof(Fint) );
			szNavtype[sizeof(Fint)] = '\0';
			if ( strcmp(szNavtype,"LL  ") == 0 )
				gpn_state.coord = LLT;
			else if ( strcmp(szNavtype,"XYZ ") == 0 )
				gpn_state.coord = CT;
			else {
				return -5;
			}
			break;

		default:

			/* Return error status for		*
			 *  unrecognized coordinate choice	*/

			return -10;
	}

	/* save or update the state for this instance		*/

	rc = M0NDadd( hGPNdata, handle, &gpn_state,
	  sizeof(gpn_state) );
	if( rc < 0 ) {
		return -6;
	}
	return 0;
}

/*
*$ Name:
*$	mcgpnfwd - generic polar forward (image->earth) navigation
*$
*$ Interface:
*$	integer function
*$	mcpgnfwd(integer handle, real line, real elem, real dtct,
*$	  real e1, real e2, real e3)
*$
*$ Input:
*$	handle		- unique identifier of navigation instance
*$	line		- image line (scan) coordinate
*$	elem		- image element coordinate
*$	dtct		- (presently unused)
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	e1		- first Earth coordinate
*$	e2		- second Earth coordinate
*$	e3		- third Earth coordinate
*$
*$ Return values:
*$	 0		- success
*$	-1		- line or element out of range for sensor
*$	-2		- point not on Earth surface
*$
*$ Remarks:
*$	    This routine assumes that 'handle' refers to an instance
*$	already initialized with a call to mcgpnini(). Behavior is
*$	undefined otherwise.
*$	    The values of e1, e2, and e3 depend upon the I/O option
*$	specified by calling mcgpnini() with option = 2. For LLT,
*$	the values are geodetic latitude, geodetic longitude, and
*$	height above the geoid (presently always 0). Angles are
*$	in degrees, West positive as per McIDAS convention. For CT,
*$	the values are the X, Y, and Z coordinates in a Cartesian
*$	terrestrial system.
*$
*$ Categories: 
*$	navigation  
*/

Fint
mcgpnfwd_(Fint *F_handle,
  Freal *F_line, Freal *F_elem, Freal *F_dtct,
  Freal *F_e1,   Freal *F_e2,   Freal *F_e3)
{
	double	phi;		/* angle from scanner k-axis
				 * to pointing vector		*/
	double	theta;		/* angle from scanner i-axis of
				 * pointing vector projected
				 * into i-j plane		*/
	double	x, y, z;	/* Cartesian coordinates	*/

	float	lat;		/* latitude			*/
	float	lon;		/* longitude			*/

	float	dtct;		/* sensor detector		*/
	float	elem;		/* sensor scan element		*/
	float	line;		/* sensor scan line		*/

	GPNinfo	gpn_state;	/* state of module for current	*
				 * instance			*/
	int	handle;		/* current nav instance		*/
	int	rc;		/* function return code		*/

	M0CR	CRscan;		/* scanner coordinate system	*/

	M0DT	DTfv;		/* time of field of view	*/

	M0flag	ok;		/* function return code		*/

	M0FV	FV;		/* field of view descriptor 	*/

	M0PT	PTfv;		/* position on geoid (target)	*/
	M0PT	PTsat;		/* satellite position		*/

	M0VC	VCptg;		/* pointing vector 		*/


	/* test stuff */

	M0flag	cheb_ready;
	int	ncalls;




/* The following are used for supplemental graphics in
 * M0LOCGRF mode */

#if defined ( M0LOCGRF )
	double	crd_len =  500.;
	M0PT	PTsat_display;
	M0VC	VCdisplay;
	M0CT 	display = CT;
#endif
#if defined ( M0NVLOG )
	char szMsg[BUFSIZ];
#endif
	handle = *F_handle;
	line	= *F_line;
	elem	= *F_elem;
	dtct	= *F_dtct;

	/* Load module state for current (handle) instance.	*
	 * Assert on failure; it means that the current instance*
	 * was never initialized with mcgpnini().		*/

	rc = M0NDget( hGPNdata, handle, sizeof(gpn_state),
	  &gpn_state);
	M0ASSERT(rc==0);

	/* Create a field of view descriptor and compute a
 	 * time for the requested image coordinate (scan,elem) */

	ok = M0DIfv_make( handle, line, dtct, elem, &FV ); 
	if ( !ok )  {
		return -1;	/* invalid input coord */
	}

	DTfv = M0DIfv_time( handle, FV );


	/* compute the satellite position, scanner
	 * coordinates, and pointing vector view angles */

	rc = M0DIpt_sat( handle, DTfv, &PTsat, &CRscan );
	M0ASSERT(rc==0);

	M0DIfv_angles( handle, FV, &phi, &theta );

#ifdef M0LOCGRF
	sprintf ( szMsg, "PTsat = %lf %lf %lf (CC)\n", PTsat.c[0],
	  PTsat.c[1], PTsat.c[2] );
	M0NLmessage( handle, szMsg );
	PTsat_display = M0PLpt_convert( handle, PTsat, DTfv, display );
	VCdisplay     = M0PLvc_convert( handle, CRscan.i, DTfv,
	  display );
	M0NLvc( handle, 1, M0VCscale(crd_len,VCdisplay),
	  PTsat_display );
	VCdisplay     = M0PLvc_convert( handle, CRscan.j, DTfv,
	  display );
	M0NLvc( handle, 2, M0VCscale(crd_len,VCdisplay),
	  PTsat_display );
	VCdisplay     = M0PLvc_convert ( handle, CRscan.k, DTfv,
	  display );
	M0NLvc( handle, 3, M0VCscale(crd_len,VCdisplay),
	  PTsat_display );
#endif


	
	/* Apply vector rotations to determine pointing vector
	 * using scanner coordinates */

	VCptg = M0DIvc_ptg( CRscan, phi, theta );

	/* now compute position of target point where a ray from
	 * satellite position, directed along the scanner
	 * pointing vector intersects the geoid */

	ok = M0PLintersect ( handle, PTsat, VCptg, &PTfv );
	if ( !ok ) {
		return -2;	/* point is not on geoid
				 * surface */
	}
	
	/* convert position into output form according to current
	 * navigation status. For lat/lon coordinates, this
	 * will require conversion from radians to degrees
	 * and accommodation of McIDAS longitude sign 
	 * convention (West positive) */

	M0PLpt_extract ( handle, PTfv, DTfv, gpn_state.coord,
	  &x, &y, &z );

#if defined ( M0LOCGRF )
	VCdisplay = M0PTdiff ( PTsat_display,
	M0PLpt_convert ( handle, PTfv, DTfv, display ) );
	M0NLvc( handle, 5, VCdisplay, PTsat_display );
#endif

	if ( gpn_state.coord == LLT || 
	     gpn_state.coord == ADT    ) {
		lon	= -x*M0R2D;	/* note sign change ! */
		lat	=  y*M0R2D;
		*F_e1	= lat;
		*F_e2	= lon;
		*F_e3	= z;
	}
	else {
		*F_e1	= x;
		*F_e2	= y;
		*F_e3	= z;
	}
#if defined ( M0NVLOG )
	sprintf ( szMsg, "gpnfwd_(): t=%14.6lf %6.1f %6.1f"
	  " th %6.1lf nav %7.2f %7.2f (w>0)\n",
	DTfv.seconds,*F_line,*F_elem,theta*M0R2D, *F_e1, *F_e2);
	M0NLmessage( handle, szMsg );
#endif

	return 0;
}



/*
*$ Name:
*$	mcgpninv - generic polar inverse (Earth->image) navigation
*$
*$ Interface:
*$	integer function
*$	mcpgninv(integer handle, real e1, real e2, real e3,
*$	  real line, real elem, real detector)
*$
*$ Input:
*$	handle		- unique identifier of navigation instance
*$	e1		- first Earth coordinate
*$	e2		- second Earth coordinate
*$	e3		- third Earth coordinate
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	line		- image line (scan) coordinate
*$	elem		- image element coordinate
*$	detector	- (presently unused)
*$
*$ Return values:
*$	 0		- success
*$	-1		- could not create re-entrant data collection
*$	-2		- could not save locus state
*$	-3		- could not save intermediate solution
*$	-4		- point not on image
*$
*$ Remarks:
*$	    This routine assumes that 'handle' refers to an instance
*$	already initialized with a call to mcgpnini(). Behavior is
*$	undefined otherwise.
*$	    The values of e1, e2, and e3 depend upon the I/O option
*$	specified by calling mcgpnini() with option = 2. For LLT,
*$	the values are geodetic latitude, geodetic longitude, and
*$	height above the geoid (presently always 0). For CT, the
*$	values are the X, Y, and Z coordinates in a Cartesian
*$	terrestrial system.
*$
*$ Categories: 
*$	navigation  
*/

Fint
mcgpninv_(Fint *F_handle,
  Freal *F_e1,   Freal *F_e2,   Freal *F_e3,
  Freal *F_line, Freal *F_elem, Freal *F_dtct)
{

#if defined ( M0NVLOG )
	char	szBuf[BUFSIZ];
#endif
	double	theta;
	double	phi, dummy_phi;
	double 	x, y, z;	/* Cartesian coordinates	*
				 * of point			*/

	float	dtct;		/* detector number		*/
	float	elem;		/* image element		*/
	float	lat;		/* latitude			*/
	float	line;		/* image line			*/
	float	lon;		/* longitude			*/

	GPNinfo	gpn_state;	/* state of current GPN instance*/

	int	handle;		/* current nav instance		*/
	int	mnLine, mxLine;	/* image line range		*/
	int	mnDtct, mxDtct;	/* detector number range	*/
	int	mnElem, mxElem;	/* image element range		*/
	int	nFV;		/* count of solutions		*/
	int	rc;		/* Function return code		*/


	M0ND*	hNDfv_list =	/* pointer to stack of 		*/
		(M0ND *)NULL;	/* field of view descriptors for*
				 * solutions to inverse nav */
	M0FV	FV;		/* solution field of view	*/

	M0flag	ok;		/* return code	(Boolean)	*/

	M0CR	CRscan;		/* scanner coordinates	*/

	M0DT	DTend;		/* latest time of navigable	*
				 * data (padded for closed	*
				 * locus)			*/
	M0DT	DTlocus;	/* time of view of Earth point	*/
	M0DT	DTstart;	/* earliest time of navigable	*
				 * data, padded for closed	*
				 * locus			*/
	M0DT	DTstart_search;	/* time to begin search for	*
				 * Earth point			*/

	M0PT	PTinput;	/* input (earth) position	*/
	M0PT	PTsat;		/* satellite position		*/
	M0PT	PTtarget;	/* target point in terrestrial
				 * right ascension/declination
				 * (geocentric latitude) */
	M0PT	PTtarg_CC;	/* target position in celestial
				 * coordinates (computed after
				 * satellite position of possible
				 * view is known */

	M0VC	VCview;		/* vector from satellite to
				 * target when target in scan
				 * locus */ 

	/* Initialization. Get 'int' form of handle and
	 * access this instance */

	handle	= *F_handle;

	/* Load module state for current (handle) instance.	*
	 * Assert on failure; it means that the current instance*
	 * was never initialized with mcgpnini().		*/

	rc = M0NDget( hGPNdata, handle, sizeof(gpn_state),
	  &gpn_state);
	M0ASSERT(rc==0);
	

	/* If angular location, convert to radians and
	 * convert longitude to east positive		*/

#if defined ( M0NVLOG )
	sprintf ( szBuf, "mcgpninv_() raw input %f %f %f\n",
	  *F_e1, *F_e2, *F_e3 );
	M0NLmessage( handle, szBuf );
#endif

	if ( gpn_state.coord == LLT ||
	     gpn_state.coord == ADT    ) {
		lat	=   *F_e1  * M0D2R;
		lon	= -(*F_e2) * M0D2R;
		x	= lon;
		y	= lat;
		z	=  *F_e3;
	}
	else {
		x	= *F_e1;
		y	= *F_e2;
		z	= *F_e3;
	}

	  

	/* convert coordinates to terrestrial Right Ascension/
	 * Declination system. This will allow for most efficient
	 * conversion to celestial (CC) system which has to be 
	 * done many times because Earth is rotating beneath
	 * the moving satellite */

	/* Since input modes are all Terrestrial systems,
	 * and the output is too, the exact time here does
	 * not matter since there is no rotation to correct
	 * for moving vernal equinox. Use the start time of
	 * the navigation domain */

	M0DItime_range( handle, &DTstart, &DTend );
	PTinput	= M0PTmake ( x, y, z, gpn_state.coord );
	PTtarget	= M0PLpt_convert( handle, PTinput,
	   DTstart, ADT );


	/* Determine the number of lines per
	 * scan (latter valid only for banked-detector systems
	 * like MODIS. Allocate enough space to hold a field
	 * of view descriptor for each scan line */

	M0DIlimits( handle, &mnLine, &mxLine, &mnDtct, &mxDtct,
	  &mnElem, &mxElem );

	/* allocate space for a list of field of view descriptors
	 * for which the sensor views the target point */

	rc = M0NDnew( sizeof(M0FV), &hNDfv_list );
	if( rc < 0 ) {
		return -1;
	}
	nFV = 0;

		

	/* For each possible line, determine whether or not
	 * the target point ever lies in the scan locus during
	 * the period of interest. If it does, compute the
	 * pointing vector associated with it and generate a
	 * field of view descriptor and add to the list
	 *
	 * DIXtimeRange "Inflates" the valid time range slightly
	 * to account for truncation error. This will allow
	 * NVXsae_/mcgpninv_() transform pair to work even for the
	 * pixel at the very beginning and very end of the scan */

	for ( dtct = 1; dtct <= mnDtct; dtct++ ) {

		M0flag	in_locus;	/* M0TRUE for solution	*/

		/* (it only executes once for everybody but
		 * MODIS). It controls the shape of the
		 * scanner locus and so goes in the 
		 * outside loop. */

		/* get the scan locus angle for this line
		 * and set the start time for position
		 * searches */ 

		ok = M0DIfv_make( handle, mnLine, dtct, mnElem,
		  &FV );
		M0ASSERT ( ok );	/* error in specification
					 * of limits (M0DIlimits)
					 * or in M0DIfv_make */

		M0DIfv_angles( handle, FV, &phi, &theta );

		DTstart_search	= DTstart;

		/* initialize the inverse navigation "locus"	*
		 * solver.					*/

		M0locus_init(handle);

		/* there may be a way to constrain the search
		 * for later lines than 0, given that bow-tying
		 * overlap is limited to +/- 1-2 scans only */
		/* Another possibility for a constrained search
		 * might be after multiple calls to nav -- if
		 * current target is very close to a previous
		 * one, a guess time might be helpful
		 */

#if defined ( M0_MULTI_INVERSE )

		while( M0TRUE ) {

			rc = M0in_locus( handle, DTstart, DTend,
			  phi, PTtarget, &in_locus, &DTlocus);

			if( rc < 0 ) {
				M0NDdel( &hNDfv_list );
				return -2;
			}
			if( ! in_locus ) {
				break;
			}
#else
		rc = M0in_locus( handle, DTstart, DTend,
		  phi, PTtarget, &in_locus, &DTlocus);
		if( in_locus )  {

#endif
			
			/* At 'DTlocus' the target point is
			 * potentially viewable because it lies
			 * in the scan locus. Determine whether it
			 * is really viewable by:
			 *	1) determine celestial coordinate
			 *	   of target (now possible because
			 *	   time of possible view DTlocus
			 *	   is known).
			 *	2) compute view vector (sat-to-target)
			 *	3) compute pointing angles associated
			 *	   with this view.
			 *	4) determine a legal field of view
			 *	   descriptor and add to list if
			 *	   possible
			 */

			rc = M0DIpt_sat( handle, DTlocus,
			  &PTsat, &CRscan );
			PTtarg_CC = M0PLpt_convert( handle, PTtarget,
			  DTlocus, CC );
			VCview   = M0PTdiff ( PTsat, PTtarg_CC );

			M0DIangles_ptg( CRscan, VCview, &dummy_phi,
			  &theta );

			/* View angles are now known. From these,
			 * line and element can be computed.
			 * The locus time can be used to compute
			 * a scan number */

			ok = M0DIangles_fv( handle, DTlocus,
			  phi, theta, &FV );
			if( ok ) {
				nFV++;
				rc = M0NDadd( hNDfv_list, nFV,
				  &FV, sizeof(FV) );
				if( rc < 0 ) {
					M0NDdel( &hNDfv_list );
					return -3;
				}
			}
#if defined ( M0NVLOG )
			sprintf ( szBuf, "mcgpninv_() solution %d: "
			  "s=%lf e=%lf\n", nFV, FV.scan,
			  FV.elem );
			M0NLmessage( handle, szBuf );
#endif
		}
	}

	/* Handle multiple, one, or no solutions */


	if ( nFV == 0 ) {
		M0NDdel( &hNDfv_list );
		return -4;
	}
	else {
		if ( nFV >  1 ) {

			/* for now, just report the first one.
			 * It might be cool to let the user
			 * specify, via NVXopt_, what solution
			 * to fetch here */

			M0NDget( hNDfv_list, 1, sizeof(FV), &FV );
		}
		else {
			M0NDget( hNDfv_list, 1, sizeof(FV), &FV );
		}
		M0DIfv_extract( handle, FV, &line, &dtct, &elem );
		M0NDdel( &hNDfv_list );
		*F_line = line;
		*F_dtct = dtct;
		*F_elem = elem;
		return 0;
	 }
}

/*
*$ Name:
*$	mcgpnopt - generic polar navigation special services
*$
*$ Interface:
*$	integer function
*$	mcpgnopt(integer handle, integer option, real xin, real xout)
*$
*$ Input:
*$	handle		- unique identifier of navigation instance
*$	option		- special service request (see 'Remarks')
*$	xin		- input data vector; contents depend on
*$			  service (see 'Remarks')
*$
*$ Input and Output:
*$	none
*$
*$ Output:
*$	xout		- output data vector; contents depend on
*$			  service (see 'Remarks')
*$
*$ Return values:
*$	 0		- success
*$	-1		- instance not initialized (call mcgpini()
*$			  first)
*$
*$ Remarks:
*$	    This routine assumes that 'handle' refers to an instance
*$	already initialized with a call to mcgpnini(). Behavior is
*$	undefined otherwise.
*$	    The following services are available:
*$ option	input			output
*$ ------	-----			------
*$ 'SPOS'	date (yyddd)		geodetic latitdude (degrees)
*$		time (hours)		geodetic longitude (w>0)
*$					radius from Earth center (km)
*$					x (km)    Cartesian celestial
*$					y (km)	  position (X axis
*$					z (km)	  through vernal
*$						  equinox)
*$
*$ 'ORBT'	(none)			orbital period (seconds)
*$					at epoch (valid time of
*$					orbital elements)
*$
*$ Categories: 
*$	navigation  
*/

Fint
mcgpnopt_(Fint *F_handle, Fint *F_option, Freal *F_xin, Freal *F_xout)
{
	char	szOption
		[sizeof(Fint)+1];	/* string form		*
				 	 * of 'option' 		*/
	GPNinfo	gpn_state;		/* state of module for 	*
					 * current instance	*/	
	int	handle;			/* current nav instance	*/
	int	rc;			/* function return code	*/


	handle = *F_handle;

	/* Load module state for current (handle) instance.	*/

	M0ASSERT( hGPNdata != NULL );
	rc = M0NDget( hGPNdata, handle, sizeof(gpn_state),
	  &gpn_state);
	if( rc < 0 ) {
		return -1;
	}


	/* Capture 'option' in string form			*/

	memcpy( szOption, (void *)F_option, sizeof(Fint));
	szOption[sizeof(Fint)] = '\0';


	if ( strcmp(szOption,"SPOS") == 0 ) {

		/*************************************************
		 * Special service 'SPOS':
		 * 
		 * Compute satellite position at given time	
		 ************************************************/

		double	hours;		/* time (hours) since	*
					 * since 00 UTC of	*	
					 * request day 		*/
		double	lat, lon;	/* geodetic		*/
		double	hgt;		/* height above geoid	*/
		double	decl;		/* declination		*/
		double	rasc;		/* right ascension	*/
		double	radius;		/* from planet center	*/
		double	x, y, z;	/* Cartesian position	*/

		int	yyddd;
		int	year;
		int	daynum;		/* day number (1 Jan=1)	*/

		M0CR	CRsat;		/* scanner coordinates	*/

		M0DT	DTnewyear;	/* date on 00 UTC 1 Jan	*
					 * of request year	*/
		M0DT	DTsat;		/* time of request	*/	

		M0TM	TMdaynum;	/* time since 00 UTC 1 	*
					 * Jan of request year	*/

		M0PT	PTsat;		/* satellite position */
		/* Convert input date in yyddd form and time	*
		 * in fractional hours to high-precision date	*/

		yyddd		= F_xin[0];
		year		= yyddd / 1000;
		daynum		= yyddd - year*1000;
		year		= year > M0FIRST_YEAR ?
		  year+1900 : year+2000;

		DTnewyear	= M0DTmake( year, 1, 1, 0, 0, 0, 0);
		hours		= F_xin[1];
		TMdaynum	= M0TMmake( (double)(daynum-1),
		  hours, 0., 0., 0. );

		DTsat		= M0DTinc( DTnewyear, TMdaynum);


		/* Predict satellite position; M0DIorbpred()	*
		 * makes a direct orbit model call		*/
	
		PTsat = M0DIorbpred( handle, DTsat, &CRsat);


		/* Extract position components in the various	*
		 * forms needed (LLT to get geodetic lat and	*
		 * lon, ADC to get radius, and CC to get	*
		 * Cartesian celestial coordinates.		*/

		M0PLpt_extract( handle, PTsat, DTsat, CC,
		  &x, &y, &z );
		M0PLpt_extract( handle, PTsat, DTsat, ADC,
		  &rasc, &decl, &radius );
		M0PLpt_extract( handle, PTsat, DTsat, LLT,
		  &lon, &lat, &hgt );


		/* Transcribe into output vector, including	*
		 * necessary unit (radian->degree) and sign	*
		 * (West positive) changes			*/

		F_xout[0]	= lat/M0D2R;
	    	F_xout[1]	=-lon/M0D2R;
		F_xout[2]	= radius;
		F_xout[3]	= x;
		F_xout[4]	= y;
		F_xout[5]	= z;

		return 0;
	}
	else if ( strcmp(szOption,"ORBT") == 0 ) {

		/*************************************************
		 * Special service 'ORBT':
		 * 
		 * Compute orbital period at epoch
		 ************************************************/

		F_xout[0] = M0DIperiod( handle );
		
		return 0;

	}
	else {

		return -2;
        }
}

#ifdef M0DEBUG

void
M0gpn_memchk(void)
{
	M0NDmemchk(hGPNdata);
	return;
}

#endif

