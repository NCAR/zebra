/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 LOCUS.C 27-Feb-96,13:14:58,`ROBERTM' initial checkin of DMSP nav        */
/* 2 LOCUS.C 17-Apr-96,14:49:10,`USER' Released                              */
/* 3 LOCUS.C 27-Sep-96,16:30:12,`ROBERTM' Include mcidasp.h                  */
/* 4 LOCUS.C 11-Oct-96,10:31:52,`ROBERTM' clarify pointer syntax (7020)      */
/* 5 LOCUS.C 22-Oct-96,19:41:40,`USER' Released                              */
/**** McIDAS Revision History *** */


/* file locus.c	*/


/* Functions for evaluating scanner locus and reference locus
 * metrics as a function of time and finding their roots.
 * This is the 'guts' of inverse navigation.
 * 
 * This module is "re-entrant," i.e. you can have multiple
 * instances of it, with different data, active at once. The instance
 * is selected by an argument 'handle' to many of the routines.
 * The value of 'handle' is determined by the top-level routines
 * M0gpnini(), M0gpnfwd(), M0gpninv(), and M0gpnopt() via a call to
 * M0nchdl(). THESE ROUTINES ALL ASSUME A VALID HANDLE. If you call
 * a routine with a wrong handle, results are undefined. This assumption
 * is validated with an assertion when this module is compiled with
 * M0DEBUG defined.
 *
 * Definitions:
 * SCANNER LOCUS: The surface in space that a sensor could
 * potentially see, given the satellite location and orientation
 * and the scanner axis and angle from that axis ('phi'). It is
 * either a plane normal to the scanner axis (phi=90 degrees)
 * or a cone (symmetric about the scanner axis). Note the use
 * of the word "potentially;" it is important. At any given time,
 * the sensor is actually seeing only one point, and the 
 * sensor pointing vector is contained in the locus. At a later
 * time, the sensor is viewing somewhere else (its rotation
 * about the scanner axis, theta, may have changed) AND THE
 * LOCUS HAS ALSO CHANGED BECAUSE THE SATELLITE HAS MOVED TOO!
 * The "locus" as used here IS NOT the surface in space
 * traced out in time by the pointing vector!
 * OPEN LOCUS: A scanner locus whose intersection with the
 * planet surface does not divide that surface into two bounded
 * regions, i.e. the intersection goes to the horizon. A scanner
 * locus that is a plane is always open.
 * CLOSED LOCUS: A scanner locus whose intersection with the
 * planet surface divides it into two bounded regions is closed,
 * i.e. the intersection is a (approximately) circle or an ellipse. 
 * Conical scan loci are closed provided that 1) the scanner
 * axis is looking generally 'down' and the width phi of the cone
 * is small enough (smaller than angle from nadir to horizon, 
 * anyway.
 * REFERENCE LOCUS: For a closed locus, there exists another
 * locus, called the 'reference locus', that is defined by the
 * satellite position and a normal to the instantaneous ground
 * track of the satellite such that the distance between the
 * intersections of this normal and the closed locus is a maximum.
 * For a conical scanner pointing at nadir (SSM/I and the only
 * one supported at present) the reference locus is a plane
 * containing the scanner axis and the orbit plane vector.
 * LOCUS METRIC: For any satellite position, scanner coordinate
 * orientation, and target point, the difference between the
 * cosine of the angle between the satellite-to-target vector
 * and the scanner axis, and the cosine of 'phi' (defining the
 * scanner locus) is the 'locus metric.' It is zero for those
 * target points, and only those target points, that are contained
 * in the scanner locus. The first step of inverse navigation
 * is to determine those times at which the locus metric is zero,
 * i.e. when the sensor could possibly 'see' the target. This
 * is done by solving for roots in the locus metric as a function
 * of time. 
 *
 * Graphics system:
 *
 * When compiled with M0LOCGRF defined, routines 'M0locus' and
 * 'M0ref_locus' can produce graphical output to the navigation
 * log file. Routines M0locus_plot_set() and M0locus_plot_clr()
 * are used to turn this feature on and off. The Brent's
 * root-finder uses this feature to produce output of selected
 * loci during its iterative solution. */


/* Change log:
 * 95.07.24 rtm	Removed & from LOCXfunction names passed as
 *		arguments to functions; AIX was issuing warning
 *		1506-106 on four of these.
 * 95.02.22 rtm Rename all static data and functions with
 *		an embedded 'X' for simulated dynamic linking.
 *		Prefix planet module calls with PLNX. Prefix
 *		nav log calls with NLX.
 * 95.02.20 rtm Rename all 'di' module calls as DIXwhatever.
 * 95.02.07 rtm Add a 'guess' input to find_root. The guess
 *		is maintained as the previous open locus
 *		solution between calls in in_open_locus and
 * 		is computed in in_open_locus prior to
 *		find_root call.
 * 95.01.27 rtm Change log instituted. Modified LOCXin_open
 * 		to keep track of all intervals containing
 *		possible solutions */

/* TUNING IDEAS:
 * 	1) provide a quick way for M0in_open to identify,
 *	   and reject, those intervals in which the target
 *	   point is in the scanner locus but on the other side
 * 	   of the earth
 */


/***********************************
 * INCLUDES
 **********************************/

#include <float.h>
#include <math.h>
#include <string.h>
#include "mcidasp.h"
#include "m0frame.h"
#include "m0gpnav.h"

/***********************************
 * DEFINES
 **********************************/

/* 'ref_locus' assumes that the scanner for which the
 * reference locus metric is being computed points to
 * local vertical (straight down). When built with _DEBUG
 * defined, this assumption is explicitly validated by
 * making sure that the scanner 'k' axis is within
 * CRIT_NADIR_ANGLE of local vertical (it's 1/100 degree). */

#define CRIT_NADIR_ANGLE 0.000174533   

/*maximum fraction of an orbital period in which to subdivide
 * a scene when attempting to bracket roots in the locus metric.
 * If too large, a time interval might contain two roots; both
 * would be missed. If too small, the locus function has to be
 * evaluated many times, slowing down the navigation. The present
 * value is chosen to be 'safe' though has not been  optimized */

#define ORBIT_FRAC 0.25	


/**********************************************************
 * TYPEDEF for retaining locus state for multiple instances
 *********************************************************/

typedef struct {
	int	nRoot_int;
	int	mxRoot_int;
	int	iRoot_int;

	M0DT	*aDTroot_int;
	M0DT	DTstart;
	M0DT 	DTend;	
	M0DT	DTguess;

	M0PT	PTtarget;

	M0TM	tm_closed;	/* half-width, in time, of
				 * a closed locus		*/
	M0TM	TMstep;

#ifdef M0LOCGRF
	int	color;		/* color of scanner locus plot	*/
	M0flag	graphics;	/* set if graphics are on	*/
#endif

}
locus_data;

/*********************************
 * SUPPORT FOR MULTIPLE INSTANCES 
 ********************************/

static M0ND
*hLocus_data;


/********************************
 * DEFINITIONS of locus functions
 *******************************/

/*
*| Name:
*|	M0locus_init -  Initializes current instance of navigation
*|			locus solver
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0locus_init(int handle)
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
*|	 0	- success
*|	-1	- could not initialize re-entrant data
*|	-2	- could not add new instance to re-entrant data
*|
*| Remarks:
*|	    This routine should be called once before any other
*|	routines with the M0locus_ prefix and again when the
*|	navigation parameters are changed by M0DIinit().
*|
*| Categories: 
*|	navigation  
*/

int
M0locus_init(int handle)
{
	int		rc;		/* function return code	*/
	locus_data	locdat;		/* instance of graphics	*
					 * attributes		*/

	/* On first call it is necessary to initialize the
	 * collection object */

	if( hLocus_data == NULL ) {
		rc = M0NDnew(sizeof(locus_data), &hLocus_data);
		if( rc < 0 ) {
			return -1;
		}
	}

	/* First attempt to get a previous instance		*/

	rc = M0NDget( hLocus_data, handle, sizeof(locus_data),
	  &locdat);
	if( rc == 0 ) {

		/* Previous instance exists. Free dynamic
		 * portions of it */

		if( locdat.aDTroot_int != NULL ) {
			M0freeMemory( (void **)&(locdat.aDTroot_int));
		}
	}

	/* now construct the new instance and save a copy
	 * of it. Zeroing out selected components will
	 * trigger a re-initialization in M0open_locus() */

	locdat.nRoot_int	= 0;		
	locdat.mxRoot_int	= 0;
	locdat.iRoot_int	= 0;
	locdat.aDTroot_int	= NULL;
	locdat.tm_closed	= 0.;
	locdat.TMstep		= 0.;

#ifdef M0LOCGRF
	locdat.color	= 0;
	locdat.graphics	= M0FALSE;
#endif

	/* Save a copy of this instance of locus data for the
	 * handle provided */

	rc = M0NDadd( hLocus_data, handle, &locdat, sizeof(locdat) );
	if( rc < 0 ) {
		return -2;
	}
	return 0;
}


/*
*| Name:
*|	M0locus - evaluate locus metric at a target and time
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	double
*|	M0locus(int handle, M0DT DTlocus, double cosphi, M0PT PTtarget)
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	DTlocus		- time to evaluate locus
*|	cosphi		- cosine of angle between scanner axis
*|			  and locus (0. for plane)
*|	PTtarget	- position of target (terrestrial coordinates)
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	locus metric	- 0 if point is in locus,
*|			  otherwise a number between -1 and +1
*|			  whose meaning depends upon the locus
*|			  shape and its intersection with the
*|			  ground (closed or open; handled at
*|			  higher level).
*|
*| Remarks:
*|	    Routine M0locus_init() must be called first to initialize
*|	the module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

double
M0locus(int handle, M0DT DTlocus, double cosphi, M0PT PTtarget)

/* IMPLEMENTATION NOTES 
 *
 * The problem is best done in celestial coordinates.
 * Satellite positions and scanner coordinate axis are native
 * in celestial and converting to terrestrial would involve four
 * conversions. Converting target position to celestial only involves
 * modifying the X and Y coordinates (effectively a change in right
 * ascension due to the rotation of 0 longitude terrestrial
 * relative to the vernal equinox (0 longitude celestial) */

/* PLOTTING NOTE: Plotting of points defining the locus is
 * done in M0DEBUG mode provided that flag locdat.graphics is set.
 * This can be arranged by calling M0locus_plot_set(), which also
 * allows you to set the colors to be used. */

{
	double		metric;		/* locus metric 	*/
	int		orbit_stat;	/* orbit model status	*/
	M0CR		CRscan;		/* scanner coordinate 	*
					 * system; it rotates 	*
					 * in time due to the	*
					 * satellite motion	*/

	M0PT		PTsat;		/* satellite position 	*
					 * at time DTlocus, 	*
					 * celestial coordinates*/
	M0PT		PTtargCC;	/* target position,	*
					 * celestial 		*/

	M0VC		VCview;		/* view vector from	*
					 * satellite to target	*/	
	M0VC		VCview0;	/* unit vector of above	*/

#ifdef M0LOCGRF

	/* These special local variables are required for
	 * graphical output */

	int		rc;		/* function return code	*/
	locus_data	locdat;		/* This instance of	*
					 * locus data		*/	
	double		phi;		/* angle from scanner 	*
					 * axis to pointing 	*
					 * vector		*/
	double		theta;		/* rotation of pointing *
					 * vector about scanner *
					 * axis			*/
	double		theta_inc;	/* increment between 	*
					 * pointing vectors used*
					 * to plot locus	*/

	int	cTheta_inc=36;	/* number of theta increments	*/
	int	i;		

	M0flag	on_sfc;		/* point on surface ?		*/

	M0PT	PTsatCT;	/* satellite position,
				 * terrestrial coordinates	*/
	M0PT	PTsfc;		/* surface point, terrestrial	*
				 * coordinates			*/
	M0PT	PTsfcCT;	/* surface point, terrestrial	*
				 * coordinates			*/
	M0PT	PTtargCT;	/* target point, terrestrial	*
				 * coordinates			*/

	M0VC	VCptg;		/* pointing vector		*/
	M0VC	VCptgCT;	/* pointing vector, terrestrial	*
				 * coordinates			*/
#endif

	orbit_stat = M0DIpt_sat( handle, DTlocus, &PTsat, &CRscan );

	/* When compiled with M0DEBUG defined, validate that
	 * DTlocus was in expected range. Orbit model produces
	 * a correct solution even if it's not, though */

#ifdef M0DEBUG
	if( orbit_stat != 0 ) {
		Mcdprintf("M0locus(): M0DIpt_sat() status = %d\n",
		  orbit_stat);
	}
#endif

	PTtargCC   = M0PLpt_convert(handle, PTtarget, DTlocus, CC );
	VCview     = M0PTdiff ( PTsat, PTtargCC );
	VCview0    = M0VCnorm ( VCview );
	metric     = cosphi - M0VCinner ( CRscan.k, VCview0 );

#ifdef M0LOCGRF

	/* If 'M0locus_plot_set()' has been used to set
	 * locdat.graphics flag, plot the locus. Note that this
	 * feature is not even compiled unless M0LOCGRF is
	 * defined at compile time */

	/* Load current (handle) instance of locus data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		*/

	rc = M0NDget( hLocus_data, handle, sizeof(locdat), &locdat);
	M0ASSERT(rc==0);

        if ( locdat.graphics ) {
	
	    phi		= acos(cosphi);
	    theta_inc	= 2.*M0PI / cTheta_inc;
	    PTsatCT	= M0PLpt_convert( handle, PTsat, DTlocus, CT );
	    PTtargCT	= M0PLpt_convert( handle, PTtarget,
	      DTlocus, CT );

	    /* Trace out the entire scanner locus, and connect
	     * all planet intersection points */

	    on_sfc = M0FALSE;

	    for ( i = 0; i <= cTheta_inc; i++ ) {
		theta	= theta_inc * i;
		VCptg	= M0DIvc_ptg(CRscan, phi, theta );
		if ( M0PLintersect(handle, PTsat, VCptg, &PTsfc ) ) {
			PTsfcCT = M0PLpt_convert(handle, PTsfc,
	                  DTlocus, CT );
			if ( on_sfc ) {
			    M0NLpt(handle, locdat.color,
	                     PTsfcCT );
			}
			else {
			    on_sfc = M0TRUE;
			    M0NLpt(handle, 0, PTsfcCT);
			}
		}
		else {
			on_sfc = M0FALSE;
		}
	    }

	    /* Plot the pointing vector from satellite to
	     * target point at locus time */

	    VCptgCT = M0PTdiff ( PTsatCT, PTtargCT );
	    M0NLvc(handle, locdat.color, VCptgCT, PTsatCT );
	}
#endif	/* #ifdef M0LOCGRF */

	return metric;
}


/*
*| Name:
*|	M0ref_locus - evaluate the reference locus metric at a
*|		      target and time
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	double
*|	M0ref_locus(int handle, M0DT DTlocus, double cosphi,
*|	  M0PT PTtarget)
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	DTlocus		- time to evaluate locus
*|	cosphi		- cosine of angle between scanner axis
*|			  and locus (0. for plane)
*|	PTtarget	- position of target (terrestrial coordinates)
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	locus metric	- 0 if point is in locus,
*|			  otherwise a number between -1 and +1
*|			  whose meaning depends upon the locus
*|			  shape and its intersection with the
*|			  ground (closed or open; handled at
*|			  higher level).
*|
*|
*| Remarks:
*|	    Routine M0locus_init() must be called first to initialize
*|	the module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|	    The reference locus is a plane passing through the widest
*|	part of a closed locus' ground intersection. This routine
*|	is valid only for a locus whose axis points straight down
*|	(like SSM/I).
*|	     Argument 'cosphi' is not used, but is included to make
*|	the calling sequences the same for M0locus() and M0ref_locus()
*|	for convenience in the locus solver M0in_locus().
*|
*| Categories: 
*|	navigation  
*/

double
M0ref_locus(int handle, M0DT DTlocus, double cosphi, M0PT PTtarget)

/* The 'reference scanner' is a plane passing through the 'widest'
 * part of a closed locus intersection with the surface (normal
 * to the ground track). Theorem: if a point is ever on or inside
 * the closed locus, it will be so when it is located in the reference
 * locus. For a scanner axis (k) pointing straight down, the locus
 * is a circle and the reference locus is the KxJ plane; its axis
 * is the negative of the scanner I unit vector.
 *
 * For scanner axes that DO *NOT* point straight down, the intersection
 * of the locus and surface is an oval (or an ellipse if the surface is
 * approximated by a plane. Under these conditions, the reference locus
 * does not contain the scanner K axis (which goes through the surface
 * at the near focus of the ellipse) but rather contains the midpoint
 * of the ellipse.
 * I shudder to think of what it is like for a scanner that points 
 * down and to one side
 *
 * THE PRESENT IMPLEMENTATION IS VALID ONLY FOR A SCANNER WHOSE
 * AXIS POINTS STRAIGHT DOWN!!!!!   THIS IS OK FOR SSM/I BUT
 * MAYBE NOT FOR OTHER APPLICATIONS !!!!!
 *
 * The above assumption is automatically validated if compiled
 * with M0DEBUG defined. */

{
	/* local variables	*/

	double	metric;		/* locus metric (returned)	*/

	int	orbit_stat;	/* orbit model status		*/

	M0CR	CRscan;		/* scanner coordinate system; it
				 * rotates in time due to the
				 * satellite motion		*/

	M0PT	PTsat;		/* satellite position at time
				 * DTlocus, celestial coor-
				 * dinates 			*/
	M0PT	PTtargCC;	/* target position, celestial	*/

	M0VC	VCrs_axis;	/* reference scanner axis	*/
	M0VC	VCview;		/* view vector from satellite to 
				 * target */
	M0VC	VCview0;	/* unit vector of above	*/



#ifdef M0DEBUG
	
	/* The following variables are defined only in M0DEBUG
	 * version and are used to validate the vertical scanner
	 * axis assumption */ 

	double  nadir_angle;	/* angle between scanner and
				 * local vertical; used to
				 * determine if a legal cone
				 * scanner is in use */
	M0VC	VCdown;
#endif


#ifdef M0LOCGRF

	/* The following variables are defined only if 
	 * locus graphics support is selected at compile time
	 * by defining M0LOCGRF. */

	double	phi;		/* angle from scanner axis to
				 * pointing vector		*/
	double	theta;		/* rotation of pointing vector
				 * about scanner axis		*/
	double	theta_inc;	/* increment between pointing
				 * vectors used to plot locus	*/

	int	cTheta_inc=36;	/* number of theta increments	*/
	int	i;
	int	rc;		/* function return code		*/

	locus_data	locdat;	/* This instance of locus data	*/

	M0flag	on_sfc = M0FALSE;
	
	M0PT	PTsfc;
	M0PT	PTsatCT;
	M0PT	PTsfcCT;
	M0PT	PTtargCT;

	M0VC	VCptg;
	M0VC	VCptgCT;
#endif

	orbit_stat = M0DIpt_sat(handle, DTlocus, &PTsat, &CRscan );

	/* When compiled with M0DEBUG defined, validate that
	 * DTlocus was in expected range. Orbit model produces
	 * a correct solution even if it's not, though */

	M0ASSERT(orbit_stat != -1);


	/* verify (M0DEBUG mode) the scanner axis orientation.
	 * The reference locus evaluation being used assumes
	 * the axis of the scanner is near local vertical --
	 * (the previous test against DBL_EPSILON once failed
	 * with a nadir angle of 1.5e-8 !) */

#ifdef M0DEBUG
	VCdown      = M0PLdown(handle,PTsat);
	nadir_angle = M0VCangle ( CRscan.k, VCdown );
	M0ASSERT ( nadir_angle < CRIT_NADIR_ANGLE );
#endif

	/* the 'axis' of the reference scanner is the negative of
	 * the scanner i axis */

	VCrs_axis = M0VCcross( CRscan.k, CRscan.j );

	PTtargCC  = M0PLpt_convert( handle, PTtarget, DTlocus, CC );
	VCview    = M0PTdiff ( PTtargCC, PTsat );
	VCview0   = M0VCnorm ( VCview );


	/* the reference locus is a plane, so cosphi = 0. */

	metric = - M0VCinner ( VCrs_axis, VCview0 );

	/* GRAPHICAL OUTPUT. The following code provides
	 * graphical output of the reference locus if
	 * compiled with M0LOCGRF defined and executed with
	 * locdat.graphics set by a call to 'M0locus_plot_set()'. */

#ifdef M0LOCGRF

	/* Load current (handle) instance of locus data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		*/

	rc = M0NDget( hLocus_data, handle, sizeof(locdat), &locdat);
	M0ASSERT(rc==0);

        if ( locdat.graphics ) {

	    /* nature of graphical output is identical to 
	     * that of 'M0locus' */
	
	    phi		= acos(0.);
	    theta_inc	= 2.*M0PI / cTheta_inc;
	    PTsatCT	= M0PLpt_convert(handle, PTsat, DTlocus, CT );
	    PTtargCT	= M0PLpt_convert(handle, PTtarget,
	     DTlocus, CT );

	    for ( i = 0; i <= cTheta_inc; i++ ) {
		theta	= theta_inc * i;
		VCptg	= M0VCrotate( VCrs_axis, theta, CRscan.k);
		if ( M0PLintersect(handle, PTsat, VCptg, &PTsfc) ) {
			PTsfcCT = M0PLpt_convert(handle, PTsfc,
	                 DTlocus, CT );
			if ( on_sfc ) {
			    M0NLpt(handle, locdat.color, PTsfcCT);
			}
			else {
			    on_sfc = M0TRUE;
			    M0NLpt(handle, 0, PTsfcCT);
			}
		}
		else {
	            on_sfc = M0FALSE;
		}
	    }
	    VCptgCT = M0PTdiff ( PTsatCT, PTtargCT );
	    M0NLvc(handle, locdat.color, VCptgCT, PTsatCT );
	}
#endif	/* #idfef M0LOCGRF */


	return metric;
}


/*
*| Name:
*|	M0in_locus - find earliest time at which a point is in
*|		     the scanner locus
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0in_locus(int handle, M0DT DTstart, M0DT DTend, double phi,
*|	  M0PT PTtarget, M0flag *pIn_locus, M0DT *pDTlocus)
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	DTstart		- start of period
*|	DTend		- end of period
*|	phi		- angle between scanner axis and locus 
*|	PTtarget	- position of target
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pIn_locus	- M0TRUE or M0FALSE 
*|	*pDTlocus	- time when PTtarget is in locus if
*|			  *pIn_locus is M0TRUE
*|
*| Return values:
*|	 0	- success
*|	-1	- memory allocation failure in M0in_open()
*|
*| Remarks:
*|	    Routine M0locus_init() must be called first to initialize
*|	the module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|	    Accuracy may suffer for highly eccentric orbits, especially
*|	for closed loci.
*|
*| Categories: 
*|	navigation  
*/


int
M0in_locus(int handle, M0DT DTstart, M0DT DTend, double phi,
  M0PT PTtarget, M0flag *pIn_locus, M0DT *pDTlocus)
{
	M0PT	PTsat;		/* satellite position		*
				 * (celestial) 			*/
	int	rc;		/* function return code		*/
	M0DT	DTavg;		/* average time of search	*
				 * region			*/
	M0DT	DTref_locus;	/* time position is in		*
				 * reference locus		*/
	M0DT	DTclosed_start;	/* Closed locus search start	*/
	M0DT	DTclosed_end;	/* Closed locus search end	*/
	M0TM	TMclosed;	/* time from when a point on the
				 * middle of a scan is in the
				 * reference locus to when it is
				 * in the closed locus		*/
	M0CR	CRscan;		/* scanner coordinates		*/
	double	horizon_nadir;	/* nadir angle of horizon from
				 * satellite			*/
	double	cosphi;		/* cosine of angle from scanner
				 * axis to locus		*/

	enum loc_type { OPEN, INDEFINITE, CLOSED  };

	enum loc_type locus_type = INDEFINITE;

	/********************************************************
	 * determine if scanner locus is 'open' or 'closed'.
	 *
	 * An open locus is one whose intersection with the ground
	 * goes to the horizon, i.e. it does not divide the
	 * surface of the visible earth (from the satellite)
	 * into an 'inside' and 'outside'.
	 *******************************************************/
	 

	/* compute nadir angle of horizon at average time. Assume
	 * that it does not change signficantly over the rest of
	 * the time range (invalid for highly eccentric orbits).
	 * The assertion validates the time DTavg. */

	DTavg	= M0DTavg(DTstart, DTend);

	rc = M0DIpt_sat(handle, DTavg, &PTsat, &CRscan );
	M0ASSERT(rc == 0);

	horizon_nadir = M0PLhorizon( handle, PTsat );


	/* locus is definitely open if the locus angle phi is
	 * greater than the local nadir angle to the horizon. Even
	 * if the scanner were pointing straight down, the cone
	 * so defined would miss the earth entirely. If tilted off
	 * vertical, it would intersect the earth only in an open
	 * curve. */

	if ( phi > horizon_nadir ) {
		locus_type = OPEN;
	}
	else {

		/* embark on test for closed locus. This is more
		 * complicated, but locus is definitely closed
		 * if the GREATEST nadir angle attained by any pointing
		 * vector in the locus is still below the horizon.
		 * The greatest nadir angle of any pointing vector is
		 * the sum of the nadir angle of the scanner axis
		 * plus the locus angle */

		if( M0VCangle( M0PLdown(handle, PTsat), CRscan.k)
		  +phi < horizon_nadir ) {
			locus_type = CLOSED;
		}
	}
	
	M0ASSERT(locus_type != INDEFINITE);

	cosphi = cos(phi);

	if( locus_type == OPEN ) {

		/* open locus leads to a quick and direct
 		 * solution */

		rc = M0in_open( handle, M0locus, DTstart,
		  DTend, cosphi, PTtarget, pIn_locus, pDTlocus );

		return rc;
	}
	else {	/* locus_type == CLOSED */

		/* The closed locus problem involves a pre-solution
		 * using a 'reference locus' for the time when the
		 * target point is most likely to be 'inside' the
		 * locus and search outward in time from there.
		 * The 'reference locus' is the plane containing
		 * the scanner j and k axes, i.e. JxK with an angle
		 * phi of 0. Its metric is evaluated by function 
		 * M0ref_locus.
		 *
		 * The closed locus looks 'ahead' or 'behind' the
		 * satellite. If ahead, a point is actually viewed
		 * at an earlier time than that in which it is in
		 * the reference locus. If behind, the point is
		 * viewed later. This interval, called TMclosed,
		 * is that quantity which, when added to the
		 * reference locus time DTref_locus, yields the
		 * 'other' limit on the time when the target is
		 * in the closed locus. It is a maximum for a
		 * point in the middle of the scan. This value is
		 * computed by M0tm_locus().
		 */
		
		rc = M0tm_locus(handle, cosphi, &TMclosed);
		if( rc < 0 ) {
			return rc;
		}

		/* For positive TMclosed, the scanner is looking
		 * behind the satellite as it flies. The time
		 * the scanner actually views a point is later
		 * than the time that the point is in the reference
		 * locus. Conversely, the time a point in the
		 * first scan line is in the reference locus
		 * is actually earlier than the start time of
		 * the scene. For TMclosed<0 (looking ahead),
		 * the reference locus time of points in the
		 * last scan is later than the end of the scene.
		 * The range of times searched for the reference
		 * locus time must therefore be expanded.
		 * This expansion is alraeady taken care of in
		 * the computation of DTstart and DTend		 */


		while ( M0TRUE ) {

		    rc = M0in_open(handle, M0ref_locus, DTstart,
	              DTend, cosphi, PTtarget, pIn_locus,
	              &DTref_locus);

		    if( rc < 0 ) {
	                return rc;
	            }

		    if( ! *pIn_locus ) {

                        /* target point is never even in reference
			 * locus, either because of time range or
	                 * error. Return now */

	                return 0;
	            }

		    /* The time when the point is in the reference
		     * locus has been found. The point is in the
		     * actual (closed) locus, if at all, in the
		     * interval [DTref_locus,DTref_locus+TMclosed]
		     * (note that the start and end of the interval
		     * may have to be reordered */
#ifdef M0LOCGRF
		    M0NLmessage(handle, "time in reference locus "
		     "found\n" );
#endif
	 	
		    if ( TMclosed > 0. ) {
			DTclosed_start = DTref_locus;
			DTclosed_end   = M0DTinc( DTref_locus,
			  TMclosed );
		    }
		    else {
			DTclosed_start = M0DTinc( DTref_locus,
			  TMclosed );
			DTclosed_end   = DTref_locus;
		    }

		    /* All actual data fall in interval		*
		     * [DTstart,DTend] as well, so the interval	*
		     * of the potential closed locus solution	*
		     * can be further reduced			*/

		    if( M0DTearlier( DTclosed_start,DTstart) ) {
			DTclosed_start	= DTstart;  
		    }

		    if( M0DTearlier( DTend, DTclosed_end ) ) {
			DTclosed_end	= DTend;
		    }


		    rc = M0in_closed( handle, M0locus, DTclosed_start,
	              DTclosed_end, cosphi, PTtarget, pIn_locus,
	              pDTlocus );

	            if( rc < 0 ) {
	                return rc;
	            }

		    /* if a closed locus solution is found,
	             * return now. Otherwise re-execute the
	             * while() loop and get a new reference
                     * locus solution (if any left) and try again */

	            if( *pIn_locus ) {
			return 0;
		    }

		}	/* while(M0TRUE) closed locus solution */

	}		/* locus_type == CLOSED */
}

/*
*| Name:
*|	M0in_open - Compute time at which point is in open locus
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0in_open(int handle, double (*locfn)(int, M0DT, double, pos),
*|	  M0DT DTstart, M0DT DTend, double cosphi, M0PT PTtarget,
*|	  M0flag *pIn_open, M0DT *pDTlocus )
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	locfn		- pointer to locus metric function
*|	DTstart		- start of period
*|	DTend		- end of period
*|	cosphi		- cosine of angle between scanner
*|			  axis and locus 
*|	PTtarget	- position of target
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pIn_open	- M0TRUE or M0FALSE 
*|	*pDTlocus	- time when PTtarget is in locus if
*|			  *pIn_open is M0TRUE
*|
*| Return values:
*|	 0		- nominal
*|	-1		- memory allocation failure when making
*|			  list of possible solution times
*|	-2		- memory allocation failure when updating
*|			  re-entrant locus data
*|
*| Remarks:
*|	    Routine M0locus_init() must be called first to initialize
*|	the module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|	    This routine allocates memory as part of the locus data
*|	structure. On subsequent uses it may resize it but it is
*|	not expected to grow significantly.
*|
*| Categories: 
*|	navigation  
*/

int
M0in_open(int handle, double (* locfn)(int, M0DT, double, M0PT),
  M0DT DTstart, M0DT DTend, double cosphi, M0PT PTtarget,
  M0flag *pIn_open, M0DT *pDTlocus)

/* On the first call for a given set of PTtarget, DTstart, and
 * DTend, M0in_open scans the interval [DTstart,DTend] and
 * locates all sub-intervals containing a root of 'locfn'. It
 * then invokes M0locus_root to determine the actual time *pDTlocus
 * of the root in the first interval.
 *
 * Each subsequent call solves for the root in each succeeding
 * interval until all intervals with roots have been processed.
 * M0in_open then returns 1.
 *
 * The value DTguess is set to the solution for the present locus
 * and target and retained between calls. It is used as the
 * input guess to the root finder as long as it is in the 
 * bracketing interval; otherwise it is replaced by the average
 * of the bracketing interval bounds.
 *
 * Algorithm: 
 * 	Search forward in time using a time step not to exceed
 * ORBIT_FRAC of an orbital period until the locus metric changes
 * sign. This means that the time has been bracketed. Then use
 * M0locus_root to isolate it. */
{

#ifdef M0NVLOG
	char		szBuf[BUFSIZ];	/* text message buffer	*/
#endif
	double		start_metric;	/* locus metric at start*
					 * of interval		*/
	double		end_metric;	/* locus metric at end	*
					 * of interval		*/
	int		istep;		/* step index		*/
	int		nstep;		/* number of steps for
					 * search		*/
	int		rc;		/* function return code	*/

	locus_data	locdat;		/* instance of locus	*
					 * state		*/

	M0DT		DTbrkt_start;	/* start of interval
					 * bracketing a root	*/
	M0DT 		DTbrkt_end;	/* end of interval
					 * bracketing a root	*/
	M0flag		mem_ok;		/* memory allocation	*
					 * status		*/
	M0TM		TMdiff;		/*			*/
	M0TM		TMperiod;	/*			*/


	/* Load current (handle) instance of locus data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		*/

	rc = M0NDget( hLocus_data, handle, sizeof(locdat), &locdat);
	M0ASSERT(rc==0);


	/* Re-initialization of times needs to be done if the
	 * number of unprocessed root intervals remaining is
	 * zero, or if the time interval [DTstart,DTend] has
	 * changed, or if the target position PTtarget has
	 * changed */

	if ( locdat.nRoot_int == 0                     ||
	  ! M0PTidentical( PTtarget, locdat.PTtarget ) ||
	  ! M0DTidentical( DTstart,  locdat.DTstart  ) ||
	  ! M0DTidentical( DTend,    locdat.DTend    ) )  {
 
	    locdat.nRoot_int	= 0;
	    locdat.PTtarget	= PTtarget;
	    locdat.DTstart	= DTstart;
	    locdat.DTend	= DTend;
	    locdat.DTguess	= M0DTavg(DTstart, DTend);

  	    TMperiod = M0DIperiod(handle)*(double)ORBIT_FRAC;
	    TMdiff   =  M0DTdiff ( DTstart, DTend );

	    /* If the time from sector start to end is
	     * less than the maximum search period, use
	     * it. If not use the largest fraction
	     * (1/2, 1/3, ...) of the sector time
	     * increment that is less than the maximum
	     * search period */
	
	    nstep = (int)ceil ( TMdiff/TMperiod );
	    locdat.TMstep = TMdiff / (M0TM)nstep;


	    /* Initialize 'for' loop with starting time of
	     * first interval and locus metric value there */

	    DTbrkt_start	 = DTstart;
	    start_metric = (*locfn)(handle, DTbrkt_start,
	      cosphi, PTtarget); 

	    for ( istep   = 0; istep < nstep; istep++ ) {

		DTbrkt_end  = M0DTinc( DTbrkt_start, locdat.TMstep );
		end_metric = (*locfn)(handle, DTbrkt_end,
	          cosphi, PTtarget);


		/* Root is bracketed if metric changes sign
		 * across the interval. Valid solutions
		 * are also on the same side of the earth
		 * as the satellite (at either start
		 * or end of segment) */

		
	        if ( start_metric * end_metric < 0. ) {

		    /* Interval beginning at DTbrkt_start
		     * contains a root. Save it!  You may
		     * first have to allocate additional
		     * space on the array of start times */

		    locdat.nRoot_int++;
		    if( locdat.nRoot_int  > locdat.mxRoot_int ) {
			locdat.mxRoot_int = locdat.nRoot_int;

			if ( locdat.mxRoot_int == 1 ) {
			    mem_ok =
	                     M0newMemory((void **)
	                      (&(locdat.aDTroot_int)),
			      sizeof(M0DT) );
			}
			else {
			    mem_ok = M0resizeMemory(
			      (void **)(&(locdat.aDTroot_int)),
			      locdat.mxRoot_int*sizeof(M0DT) );
			}
			if( ! mem_ok ) {
	                    return -1;
	                }
		    }

		    M0ASSERT(M0validPointer(
		      (void *)(&locdat.aDTroot_int[locdat.nRoot_int-1]),
		      sizeof(M0DT) ) );
		    locdat.aDTroot_int[locdat.nRoot_int-1]
		      = DTbrkt_start;
		}

		/* End of this interval becomes start of next one */

		start_metric = end_metric;
		DTbrkt_start = DTbrkt_end;
	    }

	    /* All possible time intervals in [DTstart,DTend]
	     * have been searched for roots. Set the pointer to
	     * the first instance and initialization is done! */

	    locdat.iRoot_int = 0;
	}

	/* Initialization is now complete, or was done on an
	 * earlier call. Search forward through the list
	 * of root intervals (until locdat.iRoot_int ==
	 * locdat.nRoot_int) and return 0 if a root is found and the
	 * point is indeed visible from the satellite.
	 * If the point is not visible, try the next root
	 * interval "until supplies are exhausted"
	 *
	 * Note that the loop index locdat.iRoot_int is stored
	 * in re-entrant locus data, and that it is set to zero
	 * during the initial call, so the 'while' loop will
	 * always 'pick up where it left off' rather than
	 * restarting each time.  */

        while ( locdat.iRoot_int < locdat.nRoot_int ) {

	    /* Find the root in the next interval in the list. */
	
	    DTbrkt_start = locdat.aDTroot_int[locdat.iRoot_int];
	    DTbrkt_end	= M0DTinc ( DTbrkt_start, locdat.TMstep );
	    if ( ! M0DTearlier( DTbrkt_start, locdat.DTguess ) ||
		 ! M0DTearlier( locdat.DTguess, DTbrkt_end ) ) {
#ifdef M0NVLOG
                sprintf( szBuf, "M0in_open override locdat.DTguess "
		  "%lf\n", locdat.DTguess.seconds );
		M0NLmessage( handle, szBuf );
#endif
	        locdat.DTguess = M0DTavg( DTbrkt_start, DTbrkt_end );
	    }

	    *pDTlocus = M0locus_root( handle, locfn, DTbrkt_start,
	       DTbrkt_end, locdat.DTguess, cosphi, PTtarget);
		
	    /* Check to see if target point is visible from
	     * satellite. If it is, we have a solution. Update
	     * the locus data for this instance and return. */

	    locdat.iRoot_int++;

	    if ( M0DIpt_visible( handle, PTtarget, *pDTlocus ) )  {
		locdat.DTguess = *pDTlocus;
 
		*pIn_open = M0TRUE;

	        rc = M0NDadd( hLocus_data, handle, &locdat,
	          sizeof(locdat) );
	        if( rc < 0 ) {
	            return -2;
	        }
		return 0;

	    }
#ifdef M0LOCGRF
	    M0NLmessage( handle, "Solution on other side of planet\n");
#endif
	}

	*pIn_open = M0FALSE;

	/* All root intervals have been processed. Zero out the
	 * counter of possible remaining solutions and update
	 * the locus data. */

	locdat.nRoot_int = 0;
	rc = M0NDadd( hLocus_data, handle, &locdat, sizeof(locdat) );
        if( rc < 0 ) {
		return -2;
        }

#ifdef M0DEBUG

	/* accidental use of *pDTlocus by the caller
	 * will now result in a loud explosion */

	memset ( (void *)pDTlocus, M0GARBAGE, sizeof(M0DT) );
#endif
	return 0;
}


/*
*| Name:
*|	M0in_closed - Compute time at which point is in closed locus
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0in_closed(int handle, int (*locfn)(int, M0DT,
*|	  double, M0PT), M0DT DTclosed_start, M0DT DTclosed_end,
*|	  double cosphi, M0PT PTtarget, M0flag *pIn_closed,
*|	  M0DT* pDTlocus)
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	locfn		- pointer to locus metric function
*|	DTclosed_start	- start of period
*|	DTclosed_end	- end of period
*|	cosphi		- cosine of angle between scanner
*|			  axis and locus 
*|	PTtarget	- position of target
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pIn_closed	- M0TRUE or M0FALSE 
*|	*pDTlocus	- time when PTtarget is in locus if
*|			  *pIn_closed is M0TRUE
*|
*| Return values:
*|	 0		- nominal operation
*|
*| Remarks:
*|	    Routine M0locus_init() must be called first to initialize
*|	the module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

int
M0in_closed(int handle, double (* locfn)(int, M0DT, double, M0PT),
  M0DT DTclosed_start, M0DT DTclosed_end, double cosphi,
  M0PT PTtarget, M0flag *pIn_closed, M0DT *pDTlocus)
{
	double	start_metric;	/* locus metric at interval	*
				 * start			*/
	double	end_metric;	/* locus metric at interval end	*/

	M0DT	DTguess;	/* guess solution for time 	*
				 * point is in closed locus	*/

	/* Evaluate metric at beginning and end. If there
	 * is no sign change, there is no root in the
	 * interval and the point is never in the closed
	 * locus (too far off to one side, probably) */

	start_metric = (*locfn)(handle, DTclosed_start,
	  cosphi, PTtarget);
	end_metric   = (*locfn)(handle, DTclosed_end,
	  cosphi, PTtarget);

	if ( start_metric*end_metric > 0. ) {
		*pIn_closed = M0FALSE;
		return 0;
	}

	/* A root exists. Isolate it with 'M0locus_root'. Use
	 * as a guess the average of the start and end times */

	DTguess   = M0DTavg( DTclosed_start, DTclosed_end);
	*pDTlocus = M0locus_root( handle, locfn, DTclosed_start,
	  DTclosed_end, DTguess, cosphi, PTtarget);

	return 0;
}


/*
*| Name:
*|	M0tm_locus - Determine time half-width of a closed locus
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0tm_locus(int handle, double cosphi, M0TM *pTMlocus );
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	cosphi		- cosine of angle between scanner
*|			  axis and locus 
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	*pTMlocus	- maximum time (seconds) between
*|			  a point's being in the closed locus
*|			  and the reference locus.
*|
*| Return values:
*|	 0		- success
*|	-1		- could not update locus data
*|
*| Remarks:
*|	    Routine M0locus_init() must be called first to initialize
*|	the module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

int
M0tm_locus(int handle, double cosphi, M0TM *pTMlocus )

/* For a closed locus, determine the maximum time difference
 * between a target's being in the reference locus and
 * its being in the actual (closed) locus.
 *
 * This quantity is calculated on an initial call and then
 * stored in and recovered from a member of the re-entrant
 * data object 'locdat' for this instance
 *
 * THIS ALGORITHM ASSUMES *RELATIVELY* CIRCULAR ORBITS, SUCH
 * THAT THE COMPUTED TIME INTERVAL DOESN'T VARY MUCH.
 * THIS ASSUMPTION IS NOT VERIFIED */
{
	int		rc;		/* function return code	*/

	locus_data	locdat;		/* instance of locus	*
					 * state		*/


	/* Load current (handle) instance of locus data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		*/

	rc = M0NDget( hLocus_data, handle, sizeof(locdat), &locdat);
	M0ASSERT(rc==0);


	if ( locdat.tm_closed == 0. ) {

#ifdef M0NVLOG
		char	szBuf[BUFSIZ];	/* buffer for text	*
					 * log message		*/
#endif
		M0CR	CRscan;		/* scanner coordinates	*/

		M0DT	DTavg;		/* average data time	*/
		M0DT	DTstart;	/* start of data time	*
					 * range		*/
		M0DT	DTend;		/* end of data time	*
					 * range		*/
		M0DT	DTref;		/* time when point is in*
					 * reference locus	*/
		M0DT	DTref_start;
		M0DT	DTref_end;
		M0flag	in_open;	/* open locus solution?	*/

		M0flag	ok;
		M0PT	PTsat;		/* satellite position	*/
		M0PT	PTfov;		/* field of view
					 * position		*/
		M0TM	TMstep;

		M0VC	VCptg; 		/* pointing vector	*/

		/* If the locus time width is not initialized, we
		 * must compute it. Begin by computing the average
		 * time of the data, locate the satellite, and
		 * look at the center of a closed locus scan
		 * (theta = 0) and get a surface point */

		M0DItime_range( handle, &DTstart, &DTend);
		DTavg = M0DTavg( DTstart, DTend );

		rc = M0DIpt_sat( handle, DTavg, &PTsat, &CRscan );
		M0ASSERT(rc==0);
		VCptg = M0DIvc_ptg( CRscan, acos(cosphi), 0. );
		ok = M0PLintersect( handle, PTsat, VCptg, &PTfov );

		M0ASSERT( ok );	/* Closed locus misses Earth? */

		/* Now determine the time 'DTref' when the point
		 * above is in the reference locus and compute
	 	 * the time difference.				*/

		DTref_start	= M0DTinc( DTavg, -M0TPAD );
		DTref_end	= M0DTinc( DTavg,  M0TPAD );

		rc = M0in_open( handle, M0ref_locus,
		  DTref_start, DTref_end, cosphi, PTfov, &in_open,
		  &DTref );
	
		/* The above should never fail, because if the
		 * point is in a closed locus it must have a
		 * nearby reference locus solution. Validate
		 * this assumption now */

		M0ASSERT( rc == 0 );
		M0ASSERT( in_open );

		/* M0in_open changed locus data; get the
		 * latest and update it */

		rc = M0NDget( hLocus_data, handle, sizeof(locdat),
		  &locdat);
		locdat.tm_closed = M0DTdiff ( DTref, DTavg );
		*pTMlocus	 = locdat.tm_closed;

		rc = M0NDadd( hLocus_data, handle, &locdat,
		  sizeof(locdat) );
		if( rc < 0 ) {
			return -1;
		}
#ifdef M0NVLOG
		sprintf ( szBuf, "M0tm_locus value initialized, "
		  "locdat.tm_closed = %lf\n", locdat.tm_closed );
		M0NLmessage( handle, szBuf );
#endif
	}

	*pTMlocus = locdat.tm_closed;
	return 0;
}

/*
*| Name:
*|	M0locus_root - Solve for time point is in locus
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	M0DT
*|	M0locus_root(int handle,
*|	  double (*locfn)( int, M0DT, double, M0PT ),
*|	  M0DT DTstart, M0DT DTend, M0DT DTguess, double cosphi,
*|	  M0PT PTtarget, M0DT *pDTlocus )
*|
*| Input:
*|	handle		- unique identifier of navigation instance
*|	locfn		- locus function
*|	DTstart		- start of interval bracketing solution
*|	DTend		- end of interval bracketing solution
*|	DTguess		- first guess
*|	cosphi		- cosine of angle between scanner
*|			  axis and locus 
*|	PTtarget	- target point
*|
*| Input and Output:
*|	none
*|
*| Output:
*|	none
*|
*| Return values:
*|	time when PTtarget is in locus
*|
*| Remarks:
*|	    This routine uses a specialized Brent's algorithm to find
*|	the time at which the value of locfn() is 0. The
*|	Brent's solver was patterned after 'zeroin.c' from Netlib.
*|	The author is Oleg Keselyov. The algorithm is from 
*| 
*|	G. Forsythe, M. Malcolm, C. Moler, Computer methods for
*|	   mathematical computations. M., Mir, 1980, p. 180 of
*|	   the Russian edition (that's where the authors of
*|	   'zeroin.c' from which this routine is derived found it).
*|
*|	    The caller is responsible to insure that DTstart and
*|	DTend indeed bracket a root, and that DTguess is in the
*|	interval. Given that, Brent's algorithm is the 'doomsday
*|	machine' of root-finders. Others may be faster on occasion,
*|	but Brent's always WORKS. The assumption that [DTstart,DTend]
*|	actually contains a root is validated with an assertion when
*|	compiled with M0DEBUG defined.
*|
*| Categories: 
*|	navigation  
*/

M0DT
M0locus_root( int handle, double (*locfn)( int, M0DT, double, M0PT ),
  M0DT DTstart, M0DT DTend, M0DT DTguess, double cosphi,
  M0PT PTtarget)
{
	double	loc_end;	/* locus metric at DTend	*/
	double	loc_start;	/* locus metric at DTstart	*/
	double	loc_best;	/* locus metric at DTbest	*/
	double	loc_brac;	/* locus metric at DTbrac	*/
	double	loc_prev;	/* locus metric at DTprev	*/

	M0DT	DTbest;		/* last and best estimate	*/
	M0DT	DTbrac;		/* bracketing approximation	*
			 	 * (always 'on the other side	*
				 * of the root' from DTbest	*/
	M0DT	DTlocus;	/* solution			*/
	M0DT	DTprev;		/* most recent approximation 	*/ 

#ifdef M0LOCGRF
	int	locus_color = 1;
#endif

#ifdef M0NVLOG
	char	szBuf[BUFSIZ];
#endif

	/* initialize with the user-specified bracket
	 * and guess information. 'Guess' is assumed to
	 * be the best estimate, and it must be between
	 * 'start' and 'end' which is the bracketing
	 * estimate of the root */

#ifdef M0LOCGRF
	M0locus_plot_set(handle, locus_color);
#endif
	loc_best	= (*locfn)( handle, DTguess, cosphi, PTtarget );
#ifdef M0LOCGRF
	M0locus_plot_clr(handle);
#endif
	loc_start	= (*locfn)( handle, DTstart, cosphi, PTtarget );
	loc_end		= (*locfn)( handle, DTend,   cosphi, PTtarget );

	/* Fails if interval does not actually have a root in it
	 * (Actually, an odd number). */

	M0ASSERT( loc_start * loc_end < 0. );

	if ( loc_start * loc_best < 0. ) {
		loc_brac	= loc_start;
		loc_prev	= loc_end;
		DTbrac		= DTstart; 
		DTprev		= DTend;
	}
	else {
		loc_brac	= loc_end;
		loc_prev	= loc_start;
		DTbrac		= DTend;
		DTprev		= DTstart;
	}
	DTbest = DTguess;


	/* iterative solver -- continue until loc_best is identically
	 * zero or until the bracketing range between DTbest and
	 * DTbrac is less than the 'tolerance'
	 */

	while ( M0TRUE ) {

		M0TM	prev_step;	/* time from current (best)
					 * to previous approx. 	*/
		M0TM	tol_act = 0.01;	/* 1/100 second is about
					 * 75 m			*/
		double	p;		/* numerator of		*
					 * increment		*/
		double	q;		/* denominator of	*
					 * increment		*/
		M0TM	new_step;	/* increment at this
					 * iteration		*/

		prev_step = M0DTdiff ( DTprev, DTbest );

		if ( fabs (loc_brac) < fabs (loc_best) ) {

			/* if 'best' is an inferior estimate to
			 * 'bracket', rearrange. Move best to previous,
			 * bracket to best, and previous to bracket
			 * if original (prev,best,brac) was (a,b,c)
			 * it is now (b,c,b) */
			
			DTprev	= DTbest;
			DTbest	= DTbrac;
			DTbrac	= DTprev;

			loc_prev= loc_best;
			loc_best= loc_brac;
			loc_brac= loc_prev;
		}

		/* because the operations are always performed on the 
		 * 'M0TM' part of a 'M0DT', the numerical portion
		 * of the tolerance is applied to that portion only,
		 * not to the whole 'M0DT' */


#ifdef M0LOCGRF
		sprintf ( szBuf, "prev, best, brac %7.1lf %7.1lf "
		  "%7.1lf %6.3lf %6.3lf %6.3lf\n", DTprev.seconds,
		  DTbest.seconds, DTbrac.seconds, loc_prev,
		  loc_best, loc_brac );
		M0NLmessage( handle, szBuf );
#endif

		new_step = M0DTdiff( DTbest, DTbrac )/2.;


		/* A solution has been found if the interval between
		 * the current best estimate and another estimate
		 * known to bracket the root is less than the 
		 * estimated numerical accuracy */

		if( fabs(new_step) <= tol_act ||
		   loc_best == (M0TM)0 ) { 

		    DTlocus = DTbest;
#ifdef M0LOCGRF
		    M0locus_plot_clr( handle );
#endif
		    return DTlocus;
		}

		/* if the previous step was nonzero and in the
		 * right direction, attempt an interpolation. */

		if ( fabs(prev_step) >= tol_act
		  && fabs(loc_prev) > fabs(loc_best) ) {

			double	t1;
			double	t2;
			double	brac_width;

			brac_width = M0DTdiff ( DTbest, DTbrac );

			/* Determine type of interpolation. If there
			 * are only two points, do a linear inter-
			 * polation. Otherwise attempt the
			 * quadratic inverse interpolation */

			if ( fabs( M0DTdiff ( DTbrac, DTprev ))
			  < 1./(double)MICROSEC_PER_SEC ) {

				t1 = loc_best / loc_prev;
				p  = brac_width * t1;
				q  = 1.0 - t1;
			}
			else {
				q  = loc_prev / loc_brac;
				t1 = loc_best / loc_brac;
				t2 = loc_best / loc_prev;
				p  = t2 * (
				     brac_width * q * ( q-t1 ) -
				     (t1-1.0) * M0DTdiff ( DTprev,
				       DTbest ) );
				q  = (q-1.0) * (t1-1.0) * (t2-1.0);
			}

			/* -p, not p, was calculated by the
			 * above operations. Make p positive and
			 * assign minus sign, if any, to q */

			if ( p > (double)0 )
				q = -q;
			else
				p = -p;

			/* Tentative estimate is b+p/q. If this
			 * is within interval (best,brac) and
			 * is not more than half the (best,brac)
			 * interval, use it. Otherwise do a bisection,
			 * which is certain to reduce the (best,brac)
			 * interval by half */

			if ( p < ( 0.75*brac_width*q 
			  - fabs ( tol_act*q ) / 2 ) &&
			     p < fabs ( prev_step*q / 2 ) ) {
				new_step = p/q;
			}

			if ( fabs(new_step) < tol_act ) {
				if ( new_step > (double)0 )
					new_step =  tol_act;
				else
					new_step = -tol_act;
			}
		    }

		    /* Save old 'best' approximation as
		     * previous, and evaluate the function
		     * at the new best approximation. Then
		     * make sure that 'best' and 'bracket'
		     * have opposite signs, replacing 'bracket'
		     * with 'previous' (old best) if you have to
		     */

		    DTprev   = DTbest;
		    loc_prev = loc_best;

		    DTbest   = M0DTinc ( DTbest, new_step );
#ifdef M0LOCGRF
		    /* change the locus color, keeping it in
		     * the range 1-7 */

		    locus_color = ++locus_color % 8;
		    if ( locus_color == 0 ) 
			locus_color++;
		    M0locus_plot_set(handle, locus_color);
#endif
		    loc_best = (*locfn)( handle, DTbest, cosphi,
		      PTtarget);

		    if ( ( loc_best > 0 && loc_brac > 0 )
		      || ( loc_best < 0 && loc_brac < 0 ) ) {

			DTbrac	= DTprev;
			loc_brac= loc_prev;
		    }
	}		/* end of 'forever' loop */
}



/************************************************************
 * DEFINITIONS of conditionally compiled functions to support
 * locus debugging graphics 
 ************************************************************/

#ifdef M0LOCGRF

/*
*| Name:
*|	M0locus_plot_set - turn locus graphics on and set color
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	int
*|	M0locus_plot_set(int handle, int locus_color)
*|
*| Input:
*|	handle	- unique identifier of navigation instance
*|	color	- color level (McIDAS) of locus plot
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
*|	    Routine M0locus_init() must be called first to initialize
*|	the module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

void
M0locus_plot_set(int handle, int locus_color)
{
	int		rc;		/* function return code	*/
	locus_data	locdat;		/* instance of locus	*
					 * state		*/

	/* Load current (handle) instance of locus data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hLocus_data, handle, sizeof(locdat), &locdat);
	M0ASSERT(rc==0);

	locdat.graphics	= M0TRUE;
	locdat.color	= locus_color;

	/* this operation involves a malloc() and can conceivably
	 * fail. But since locus graphics are a debugging aid, I
	 * decided to trap it with an ASSERT rather than return
	 * a status. */

	rc = M0NDadd( hLocus_data, handle, &locdat, sizeof(locdat) );
	M0ASSERT(rc==0);

	return;	
}

/*
*| Name:
*|	M0locus_plot_clr - turn locus graphics off
*|
*| Interface:
*|	#include "m0gpnav.h"
*|
*|	void
*|	M0locus_plot_clr(int handle)
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
*|	    Routine M0locus_init() must be called first to initialize
*|	the module before this routine can be used. "Handle" is assumed
*|	to refer to an already-initialized instance; if not the results
*|	are undefined. When compiled with M0DEBUG defined this
*|	assumption is tested with an assertion.
*|
*| Categories: 
*|	navigation  
*/

void
M0locus_plot_clr( int handle )
{
	int		rc;		/* function return code	*/
	locus_data	locdat;		/* instance of locus	*
					 * state		*/

	/* Load current (handle) instance of locus data.
	 * Assert on failure; it means that higher-level
	 * routines failed to initialize the instance		 */

	rc = M0NDget( hLocus_data, handle, sizeof(locdat), &locdat);
	M0ASSERT(rc==0);

	locdat.graphics	= M0FALSE;

	/* this operation involves a malloc() and can conceivably
	 * fail. But since locus graphics are a debugging aid, I
	 * decided to trap it with an ASSERT rather than return
	 * a status. */

	rc = M0NDadd( hLocus_data, handle, &locdat, sizeof(locdat) );
	M0ASSERT(rc==0);
	return;
}

#endif

#ifdef M0DEBUG

void
M0locus_memchk(int handle)
{
	int		rc;		/* function return code	*/
	locus_data	locdat;		/* instance of locus	*
					 * state		*/

	/* note the locus data collection, then get this 	*
	 * instance and validate the array of times		*/

	M0NDmemchk( hLocus_data );

	rc = M0NDget( hLocus_data, handle, sizeof(locdat), &locdat);
	M0ASSERT(rc == 0);
	
	if( locdat.aDTroot_int != NULL ) {
		M0noteMemRef(locdat.aDTroot_int);
	}
	return;
}
#endif
