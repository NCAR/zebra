/* THIS IS SSEC PROPRIETARY SOFTWARE - ITS USE IS RESTRICTED. */

/**** McIDAS Revision History *** */
/* 1 M0GPNAV.H 27-Feb-96,13:16:48,`ROBERTM' initial checkin of DMSP nav      */
/* 2 M0GPNAV.H 17-Apr-96,14:50:04,`USER' Released                            */
/**** McIDAS Revision History *** */

/* Change log
 * 96.01.24 rtm	Add 'handle' arguments to nav log and planet routines 
 * 96.01.22 rtm	Rename "nvlog" macros and prototypes for 
 *		re-entrant naming and calling conventions. Change
 *		names only; arglists have to be changed yet.
 * 96.01.22 rtm	Introduce typedef and prototypes for re-entrant
 *		navigation (cnav.c)
 * 96.01.22 rtm	Rename m0gpnav.h
 * 95.11.15 rtm	Add macros M0NLXPT() and M0NLXVC()
 * 95.06.02 rtm Make changes suggested by BethA
 * 95.04.24 rtm	Add McIDAS library prefix to routine names
 * 95.02.28 rtm	Move 'vector.c' members vecConvert,
 *		posConvert, posExtract, adr_to_llh, and
 *		llh_to_adr to 'planet.c'. These members
 *		all require a current planet be set
 *		and therefore require name-mangling for
 *		simulated dynamic linking
 * 95.01.11 Change log added
 */

#ifndef M0GPNAV_H 
#define M0GPNAV_H

/**********************************
 * INCLUDES
 *********************************/

#include <stddef.h>
#include "m0frame.h"


/**********************************
 * DEFINES -- CONSTANTS
 *********************************/


#define M0FIRST_YEAR 72 	/* Earliest year is 1972, two	*
				 * digit years 0-71 expand to	*
				 * 20xx				*/


#define	M0TPAD 150.		/* Time 'padding' to apply to
				 * image time range to obtain
				 * valid data time range (sec).
				 * Padding is needed to allow	
				 * reference locus solutions
				 * for cone scanners		*/

#define M0_GPN_VERSION "re-entrant: 96.02.12"



/**********************************
 * DEFINES -- MACROS
 *********************************/

#ifdef M0NVLOG

#define M0NLOPEN(H,F,T) M0NLopen((H),(F),(T))
#define M0NLMSG(H,M) M0NLmessage((H),(M))
#define M0NLPOINT(H,C,X,Y,Z) M0NLpoint((C),(X),(Y),(Z))
#define M0NLPT(H,C,P) M0NLpt((H),(C),(P))
#define M0NLVC(H,C,V) M0NLvc((H),(C),(V))
#define M0NLCLOSE(H) M0NLclose(H)

#else	/* M0NVLOG not defined */

#define M0NLOPEN(H,F,T) M0TRUE		/* simulate success */
#define M0NLMSG(H,M) NULL
#define M0NLPOINT(H,C,X,Y,Z) NULL
#define M0NLPT(H,C,P) NULL
#define M0NLVC(H,C,V) NULL
#define M0NLCLOSE(H) NULL

#endif	/* M0NVLOG */

/**********************************
 * TYPEDEFS  
 *********************************/

/* M0ND is an opaque pointer to a collection of navigation
 * data objects. Implementation details are in cnav.c */

typedef struct M0ND_
M0ND;


/* M0FV is a "field of view descriptor" that abstracts
 * image coordinates (line, element). Here image line
 * is 'scan' and image element is 'elem'. 'line' anticipates
 * MODIS in which banked detectors will yield multiple image
 * lines per scan. Use access routines in the M0DI series
 * to construct and manipulate these and lots of troubles
 * (including image flipping) will be taken care of for you. */

typedef struct {
        double scan;
        double line;
        double elem;
}
M0FV;


/**********************************
 * PROTOTYPES
 *********************************/


/*******************************************************************
 *               GROUP: Re-entrant Generic Polar Navigation API
 *
 * mcgpnini_()	- initialize GPN
 *
 * mcgpnfwd_()	- GPN forward navigation (image -> Earth)
 *
 * mcgpninv_()	- GPN inverse navigation (Earth -> image)
 *
 * mcgpnopt_()	- GPN special services
 *
 ******************************************************************/

extern Fint
mcgpnini_(Fint *F_handle, Fint *F_option, Fint *F_navparms);

extern Fint
mcgpnfwd_(Fint *F_handle,
  Freal *F_line, Freal *F_elem, Freal *F_detector,
  Freal *F_e1,   Freal *F_e2,   Freal *F_e3);

extern Fint
mcgpninv_(Fint *F_handle,
  Freal *F_e1,   Freal *F_e2,   Freal *F_e3,
  Freal *F_line, Freal *F_elem, Freal *F_detector);

extern Fint
mcgpnopt_(Fint *F_option, Fint *F_handle, Freal *F_in, Freal *F_out);

#ifdef M0DEBUG
extern void
M0gpn_memchk(void);
#endif

/*******************************************************************
 *               GROUP: Re-entrant Brouwer-Lyddane model orbital
 *			element stacker
 *
 * m0blset_elements()	- store Brouwer elements for this instance
 *
 * m0blget_elements()	- retrieve Brouwer elements for this instance
 *
 ******************************************************************/

extern void
m0blgetels_(Fint *F_handle, Fdouble *epoch, Fdouble *blelems);

extern int
M0BLsetels(int handle, double epoch, double smaxis, double ecc,
  double incl, double asnod, double argper, double manom);

#ifdef M0DEBUG

extern void
M0BLmemchk(void);

#endif

/*******************************************************************
 *               GROUP: Navigation data interface
 *
 * M0DIangles_fv()	- generate a field of view descriptor from
 *			  pointing angles
 *
 * M0DIangles_ptg()	- generate pointing angles from view vector
 *
 * M0DIfv_angles()	- generate pointing angles from field of view
 * 			  descriptor
 *
 * M0DIfv_extract()	- components from field of view descriptor
 * 
 * M0DIfv_make()	 - make a field of view descriptor from
 *			  components
 * 
 * M0DIfv_time()	- compute the time of a field of view
 * 
 * 
 * M0DIinit()		- initialize data module with codicil
 *
 * M0DIlimits()		- compute limits of line and element
 * 
 * M0DIorbpred()	- predict satellite position and 
 * 			  orientation (orbit model call)
 *
 * M0DIperiod()		- compute orbital period
 *
 * M0DIpt_sat()		- fast prediction of satellite position
 *			- and scanner orientation (Chebyshev approx).
 *
 * M0DIpt_visible()	- is satellite above horizon ?
 *
 * M0DIstatus()		- what is the status of the orbit model ?
 *
 * M0DItime_range()	- return earliest and latest navigable
 * 			  times
 *
 * M0DIvc_ptg()		- compute pointing vector 
 * 
 ********************************************************************/

extern M0flag
M0DIangles_fv(int handle, M0DT DTfv, double phi, double theta,
  M0FV *pFV);

extern void
M0DIangles_ptg(M0CR crScan, M0VC vcPtg, double *pPhi,
  double *pTheta);

extern void
M0DIfv_angles(int handle, M0FV fv, double *pPhi, double *pTheta);

extern void
M0DIfv_extract(int handle, M0FV fv, float *pScan, float *pLine,
  float *pElem );

extern M0flag
M0DIfv_make(int handle, float scan, float line, float elem, M0FV *pFV);

extern M0DT
M0DIfv_time(int handle, M0FV fv);

extern int
M0DIinit(int handle, Fint *codicil);

extern void
M0DIlimits(int handle, int *pFirst_scan, int *pLast_scan,
  int *pFirst_line, int *pLast_line, int *pFirst_elem,
  int *pLast_elem);

extern M0PT
M0DIorbpred(int handle, M0DT time, M0CR *pCRscan);

extern double
M0DIperiod(int handle);

extern int
M0DIpt_sat(int handle, M0DT time, M0PT *pPTsat, M0CR *pCRscan);

extern M0flag
M0DIpt_visible(int handle, M0PT PTtarget, M0DT DTview);

extern int
M0DIstatus(int handle, M0flag *pCheb_ready, int *pNcalls, int *pNterms);

extern void
M0DItime_range(int handle, M0DT *start, M0DT *end);

extern M0VC
M0DIvc_ptg(M0CR crScan, double phi, double theta);

#ifdef M0DEBUG

/* The following functions are available only when compiled with
 * M0DEBUG defined. */ 

extern void
M0DIcheb_eval(int handle);

extern void
M0DImemchk(int handle);

#endif	/* #ifdef M0DEBUG */

/*******************************************************************
 *               GROUP: Scanner locus solutions (inverse nav)
 *
 * M0in_closed()	- solve closed locus
 *
 * M0in_locus()		- general locus solver
 *
 * M0in_open()		- solve open locus
 *
 * M0locus()		- evaluate locus metric
 *
 * M0locus_init()	- initialize locus solver for this instance
 *
 * M0locus_root()	- root-finder for locus solutions
 *
 * M0ref_locus()	- evaluate reference locus metric
 *
 * M0tm_locus()		- compute half-width (time) of closed locus
 *
 *	the following are only available when locus.c and the
 *	calling	module are compiled with M0LOCGRF defined
 *
 * M0locus_plot_set()	- turn on locus graphics 
 *
 * M0locus_plot_clr()	- turn off locus graphics
 ******************************************************************/

extern int
M0in_closed(int handle, double (* locfn)(int, M0DT, double, M0PT),
  M0DT DTclosed_start, M0DT DTclosed_end, double cosphi,
  M0PT PTtarget, M0flag *pIn_closed, M0DT *pDTlocus);

extern int
M0in_locus(int handle, M0DT DTstart, M0DT DTend, double phi,
  M0PT PTtarget, M0flag *pIn_locus, M0DT *pDTlocus);

extern int
M0in_open(int handle, double (* locfn)(int, M0DT, double, M0PT),
  M0DT DTstart, M0DT DTend, double cosphi, M0PT PTtarget,
  M0flag *pIn_open, M0DT *pDTlocus);

extern double
M0locus(int handle, M0DT DTlocus, double cosphi, M0PT PTtarget);

extern int
M0locus_init(int handle);

extern M0DT
M0locus_root(int handle, double (*locfn)( int, M0DT, double, M0PT ),
  M0DT DTstart, M0DT DTend, M0DT DTguess, double cosphi,
  M0PT PTtarget);

extern double
M0ref_locus(int handle, M0DT DTlocus, double cosphi, M0PT PTtarget);

extern int
M0tm_locus(int handle, double cosphi, M0TM *pTMlocus );

#ifdef M0LOCGRF

extern void
M0locus_plot_set(int handle, int locus_color);

extern void
M0locus_plot_clr(int handle);

#endif 	/* #ifdef M0LOCGRF */

#ifdef M0DEBUG
extern void
M0locus_memchk(int handle);
#endif

/*******************************************************************
 *               GROUP: Re-entrant C navigation support
 *
 * m0nchdl()		- get the navigation instance (for this
 *			  application) associated.
 *
 * M0NCmemchk()		- Validate memory allocations for
 *			  navigation handles.
 *
 * M0NDadd()		- add a given instance of a navigation
 *			  data object to the collection (cnav.c).
 *
 * M0NDget()		- get a copy of a given instance of a
 *			  navigation data object from the 
 *			  collection (cnav.c).
 *
 * M0NDmemchk()		- validate memory allocations for nav
 *			  data object lists.
 *
 ******************************************************************/

extern Fint 
m0nchdl_(Fint *handle);

extern void
M0NCmemchk(void);

extern int
M0NDnew(size_t size, M0ND **phND);

extern int
M0NDadd(M0ND *hND, int handle, void *pObject, size_t size);

extern int
M0NDget(M0ND *hND, int handle, size_t size, void *pObject);

extern void
M0NDdel(M0ND **hND);

extern void
M0NDmemchk(M0ND *hND);



/*******************************************************************
 *            GROUP: calculations with spheroidal planets
 *
 *
 * M0PLinit()		- initialize planet module
 *
 * M0PLsize()		- fetch planet dimensions
 *
 * M0PLvecDown		- compute local vertical
 *
 * MOPLhorizon		- compute angle to horizon
 *
 * M0PLintersect()	- compute intersection of vector w/surface
 *
 * M0PLverneq()		- compute longitude of vernal equinox (Earth)
 *
 * M0PLvc_convert()	- change coordinates of a vector
 *
 * M0PLpt_convert()	- change coordinates of a position
 *
 * M0PLptExtract()	- extract components from a position
 *
 * M0PLadr_llh()	- convert geocentric to geodetic coordinates
 *
 * M0PLllh_adr()	- convert geodetic to geocentric coordinates
 * 
 * M0PLplot()		- produce plot log output (for development)
 *
 ******************************************************************/

extern int
M0PLinit(int handle, char *pchName);

extern void
M0PLsize(int handle, double *pRe, double *pRp, double *pEcc,
  double *pFlat );

extern M0flag
M0PLintersect(int handle, M0PT PTsat, M0VC VCptg, M0PT *pPTsfc );

extern M0VC
M0PLdown(int handle, M0PT PTsat );
	
extern double
M0PLhorizon(int handle, M0PT PTsat );

extern double
M0PLverneq(int handle, M0DT DTepoch );

extern M0VC
M0PLvc_convert(int handle, M0VC VCold, M0DT DT, M0CT new_crd );

extern M0PT
M0PLpt_convert(int handle, M0PT PTinput, M0DT DT, M0CT new_crd );

extern void
M0PLpt_extract(int handle, M0PT PTinput, M0DT DT, M0CT output_crd,
    double *pc1, double *pc2, double *pc3 );

extern void
M0PLadr_llh(int handle, double adr[3], double llh[3] );
	
extern void
M0PLllh_adr(int handle, double llh[3], double adr[3] );

extern void
M0PLplot(int handle, M0DT DTepoch, M0CT type, double mnLat,
    double mxLat, double incLat, double mnLon, double mxLon,
    double incLon, double res, int color, int special );

#ifdef M0DEBUG

extern void
M0PLmemchk(int handle);

#endif

/*******************************************************************
 *            GROUP: navigation debugging log
 *
 *
 * M0NLopen()		- open navigation log file
 *
 * M0NLmessage()	- write text message to navigation log file
 *
 * M0NLpoint()		- write point (3 coordinates) to nav log file
 *
 * M0NLclose()		- close navigation log file
 *
 * M0NLpt()		- write position (M0PT type) to nav log file
 *
 * M0NLvc()		- write vector (M0VC type) to nav log file
 *
 ******************************************************************/

extern int
M0NLopen(int handle, char *szFilename, char *szTitle);

extern int
M0NLmessage(int handle, char *szMessage);

extern int
M0NLpoint(int handle, int color, int X, int Y, int Z);

extern int
M0NLclose(int handle);

extern int
M0NLpt(int handle, int color, M0PT PT);

extern int	
M0NLvc(int handle, int color, M0VC VC, M0PT PTorigin);

#ifdef M0DEBUG
extern void
M0NLmemchk(void);
#endif


/*******************************************************************
 *            GROUP: Brouwer-Lyddane orbit model
 *
 *
 * m0atomm_()		- convert semi-major axis to mean motion
 *
 * m0blset_()		- initialize Brouwer-Lyddane model
 * 
 * m0mmtoa_()		- convert mean motion to semi-major axis
 * 
 ******************************************************************/

extern double
m0atomm_(double * sma, double * ecc, double * incl);

extern Fint
m0blset_(Fint *handle, Fdouble *blepoch, Fdouble *smaxis, Fdouble *ecc,
  Fdouble *incl, Fdouble *asnod, Fdouble *argper, Fdouble *manom);

extern void
m0blpred_(Fint *handle, Fdouble *dtime,
  Fdouble *posx, Fdouble *posy, Fdouble *posz,
  Fdouble *velx, Fdouble *vely, Fdouble *velz);

extern double
m0mmtoa_(double * mm, double * ecc, double * incl);

#endif		/* #ifndef GPNAV_H	*/
