/**** McIDAS Revision History *** */
/* 1 M0FRAME.H 27-Feb-96,13:16:34,`ROBERTM' initial checkin of DMSP nav      */
/* 2 M0FRAME.H 17-Apr-96,14:49:56,`USER' Released                            */
/* 3 M0FRAME.H 27-Sep-96,16:30:08,`ROBERTM' Remove M0ASSERT(); add new M0DT  */
/*      routines (7018)                                                      */
/* 4 M0FRAME.H 22-Oct-96,19:41:50,`USER' Released                            */
/**** McIDAS Revision History *** */


/* m0frame.h
 *
 * This 'application framework' contains enhnaced debugging
 * support, including 'safe' access to the C library memory
 * management and buffered file I/O subsystems. 
 *
 * It is originally intended for use with 'generic navigation'
 * and CAMA */

/* Update Log **************************************************
96.02.05 rtm Moved from \mcidas\working\vpfview -- includes
	     M0strdup(), true_lon(), and lon_range().
95.10.12 rtm Combine all algorithmic (no static data) routine
	     interfaces into a single header file
95.06.05 rtm Add McIDAS routines fsalloc() and stralloc() to
	     safe memory management. Make pointer comparison
	     and memory block log routines local to m0frame.c
95.06.02 rtm Move to m0frame.h and begin upgrade to McIDAS core
		standards
95.03.08 rtm Create separate version for generic nav. Remove all
	     16-bit stuff.
94.11.17 rtm 'lint' produces no unexplained remarks
94.11.16 rtm Removed #elifs for compatibility with HP-UX
94.11.05 rtm Added safe file system routines
94.05.21 rtm Documentation added
****************************************************************/

/* Change log (jdate.h):
 * 95.10.11 rtm	Changed to core standards. Rename 'tinc' as
 *		M0TM and 'jdate' as M0DT. (Use TM and DT
 *		as Hungarian tags in variables and functions).
 * 95.01.11 rtm	Change log added
 */

/* Update Log (mat2) *******************************************
95.10.12 rtm 	Modified to conform to McIDAS core standards
95.08.23 rtm	Created.
****************************************************************/

/* Change log -- vector.h
 * 95.10.12 rtm	Modify to conform with core McIDAS style
 * 95.07.21 rtm	Add routine posFromVector() and vecDiff()
 *		needed by Chebyshev polynomial debugging
 * 95.01.11 rtm Routine posOffset added (planetIntersect
 *		needed such)
 * 95.01.11 rtm	Change log added
 */



#ifndef M0FRAME_H
#define M0FRAME_H

/***********************************************
 * INCLUDES
 **********************************************/

#include "mcidas.h"


/***********************************************
 * TYPEDEFS
 **********************************************/

/* These (except M0flag) all have their own routines that
 * go with them; these routines generally have the 
 * type name as a prefix; i.e. the vector routines
 * are called things like M0VCvecop().
 *
 * All the typedefs are grouped here as per the style
 * guide. The function prototypes are found later,
 * grouped by the abstract data type they serve.
 */

/*
 * M0flag	- Boolean variable
 */

typedef enum { M0FALSE=0, M0TRUE=1 }
M0flag;

/* Variables of type 'M0flag' are designed for convenient use
 * as the results of logical expressions. They also provide
 * additional clarity where a function returns TRUE or FALSE.
 * I talked about this with David Sanderson -- he recommended
 * the typical UNIX convention of a return code of 0 meaning
 * success. I tried coding with this and found that since a
 * 0 also tests 'false' in if() that you had to make a lot
 * of explicit tests against 0, etc. that the clarity of
 * the Boolean return was lost. So I reverted to using the
 * 'flag' type, protecting it using an M0 prefix
 * and introducing 'true' and 'false' using an enum rather
 * than #define.
 *
 * Here is an example of what it does for code clarity:
 *
 * M0flag ok;
 *
 * ok = ItWorked(...);
 * if ( ok ) {
 *	yes();
 * else
 *	no();
 * }
 */


/* High-precision time and date */

typedef double
M0TM;		/* High-precision time (seconds)	*/


typedef struct {
	int	julday;		/* Julian day (astronomical)	*/
	M0TM	seconds;	/* seconds since 00 UTC		*/
}
M0DT;



/* M0MT		- dynamic matrix (2-d array) */

typedef struct {
	double	**val;		/* data block			*/
	int	nrow;		/* number of rows (logical)	*/
	int	ncol;		/* number of columns (logical)	*/
	int	mxrow;		/* number of rows (physical)	*/
	int	mxcol;		/* number of columns (physical)	*/
}
M0MT;

/* I started using ints for dimension rather than size_t because
 * an error in computing an index that resulted in a negative
 * number with ints is easier to catch than one that 'wraps'
 * around to a huge unsigned number. */

 
/*
 * M0CT		- position and vector coordinate system types
 */

typedef enum { LLT, CT, ADT, ADC, CC, NIL }
M0CT;


/*
 * M0PT		- position
 */

typedef struct {
	M0CT type;	/* coordinate type	*/
	double c[3];	/* components		*/
}
M0PT;


/*
 * M0VC		- vector
 */
 
typedef struct {
	M0CT type;	/* coordinate type	*/
	double	c[3];	/* components		*/
}
M0VC;



/*
 * M0CT		- coordinate system
 */

typedef struct {
	M0PT	origin;
	M0VC	i;
	M0VC	j;
	M0VC	k;
} M0CR;

/*
 * M0ST		- univariate statistics accumulation
 */

typedef struct {
	int	n;		/* count	*/
	double	mn;		/* minimum	*/
	double	mx;		/* maximum	*/
	double	avg;		/* average	*/
	double	std;		/* std dev	*/
} M0ST;





/***********************************************
 * #DEFINES -- SYMBOLIC CONSTANTS
 **********************************************/

/*
 * M0GARBAGE	- the 'garbage' byte
 */

#define M0GARBAGE 0xCC

/* USAGE:
 * the 'garbage' byte is used for initializing newly allocated
 * memory and 'shredding' to-be-freed memory to prevent accidental
 * and undetected use by a buggy caller of dynamic memory routines
 * M0newMemory(), M0resizeMemory(), and M0freeMemory() */

/*
 * Time conversion factors
 */

#define MICROSEC_PER_SEC 1000000L
#define SEC_PER_MINUTE 60L
#define SEC_PER_HOUR 3600L
#define SEC_PER_DAY 86400L
#define MAX_TM 2147483647L	/* 2**31 -1 */
				/* What I would really like to
				 * do is tie this to the size
				 * of an 'int' */

/*
 * Trigonometric constants
 */

#define M0D2R    0.017453292519943
#define M0R2D   57.295779513082
#define M0PI     3.141592653589793
#define M0TWOPI  6.283185307179586

#define M0DEG2KM 111.12

/***********************************************
 * #DEFINES -- MACROS -- GENERAL INFORMATION
 **********************************************/

/* GENERAL NOTE:
 * The macros are used to swap debugging code in and out. When
 * this header file is included in a file being compiled with
 * M0DEBUG defined, the debugging versions of many functions
 * (memory management and file I/O in particular) will be called
 * instead of their C library equivalents*/

/* QUESTION FOR MUG: HOW/WHERE TO PUT DOC BLOCKS? They should
 * go with the macros, not the functions. Do they go here? */

/*******************************************************************
 *            GROUP: Debugging macros
 *
 * Macro	    default call	if M0DEBUG defined	
 *					when caller is compiled
 *
 * M0FOPEN()	    - fopen()		- logs file name and ptr
 *
 * M0FCLOSE()	    - fclose()		- validates ptr first
 *
 * M0FLIST()	    - nothing		- lists open files
 *
 ******************************************************************/

#ifdef M0DEBUG

/***********************************************
 * #DEFINES -- MACROS -- DEBUG VERSIONS
 **********************************************/


/*
 * M0FOPEN	- safe 'fopen'
 * M0FCLOSE	- safe 'fclose'
 * M0FLIST	- list open files
 */

#define M0FOPEN(n,m) M0safeOpen(n,m)
#define M0FCLOSE(f) M0safeClose(f)
#define M0FLIST M0listOpenFiles()

#else


/***********************************************
 * #DEFINES -- MACROS -- RELEASE VERSIONS
 **********************************************/

#define M0FOPEN(n,m) fopen(n,m)
#define M0FCLOSE(f) fclose(f)
#define M0FLIST NULL

#endif


/***********************************************
 * #PROTOTYPES
 **********************************************/

M0flag
yes ( const char * const prompt );

/*******************************************************************
 *            GROUP: Debugging functions
 *
 * M0assert	- prints file and line number and aborts if
 *		  assert condition is false
 *
 * M0safeOpen	- tests for already opened file by that name,
 *		  calls fopen() to open file, logs file name and
 *		  pointer for future reference
 *
 * M0safeClose	- tests for opened file with given pointer,
 *		  closes file and removes pointer from list
 *
 * M0flist	- lists all files previously opened with M0safeOpen.
 *
 ******************************************************************/

#ifdef M0DEBUG

extern void
M0assert(char *srcfile, size_t linenum);

extern FILE *
M0safeOpen(char *filename, char *mode );

extern int
M0safeClose(FILE *fileptr);

extern void
M0flist(void);		

extern void
M0noteOpenFiles(void);

#endif


/*******************************************************************
 *            GROUP: Safe memory manager
 *
 *		    default behavior	if M0DEBUG defined	
 *					when m0frame.c is compiled
 *
 * M0newMemory()    - malloc()		- logs block size and ptr
 *
 * M0resizeMemory() - realloc()		- logs change to block
 *
 * M0freeMemory()   - free()		- 'shreds' block, unlogs
 *
 * M0strdup()	    - as strdup()	- logs block size and ptr
 *
 * M0fsMemory()	    - fsalloc (McIDAS)	- as M0newMemory
 *
 * M0strMemory()    - stralloc (McIDAS)	- as M0newMemory
 *
 *
 * M0dumpMemInfo()  - (not available)	- list all current blocks
 *
 * M0clearMemRefs()   (not available)	- clear all 'referenced'
 *					  flags in blocks
 * M0noteMemRefs()    (not available)	- mark block as 'referenced'
 *
 * M0checkMemRefs()   (not available)	- validate that all blocks
 *					  are referenced
 *
 * M0validPointer()   (not available)	- validates pointer
 *
 ******************************************************************/


/*
 * Covers for malloc(), realloc(), free(), and strdup() are
 * always available */

extern M0flag
M0newMemory(void **phBlock, size_t blocksize);

extern M0flag
M0resizeMemory(void **phBlock, size_t blocksize);

extern void
M0freeMemory(void **phBlock);

extern char *
M0strdup(const char *sz);


/*
 * covers for fsalloc() and stralloc() are available in mcidas
 */

#ifdef _MCIDAS_H

/* These memory allocation routines are supported
 * in McIDAS only. Include their prototypes only
 * if "mcidas.h" was previously included. The
 * argument lists and return values are chosen for
 * consistency with 'stralloc()' and 'fsalloc()'. */

extern char *
M0fsMemory(const char *string, FsLen length);

extern char *
M0strMemory(char *s, ...);

#endif


#ifdef M0DEBUG

/*
 * Service routines for safe memory system
 */

extern void
M0dumpMemInfo(FILE *pDumpFile);

extern void
M0clearMemRefs(void);

extern void
M0noteMemRef(void *ptr);

extern void
M0checkMemRefs(void);

extern M0flag
M0validPointer(void *ptr, size_t size);


#endif


/*******************************************************************
 *            GROUP: High precision date and time calculations
 *
 * M0cmddt()		- fetch a high-precision date from command
 *			  line arguments
 *
 * M0dabtim()		- convert high-precision date to
 *			  'Julian Date' for Brouwer-Lyddane orbit
 *			  prediction model
 *
 * M0DTavg()		- average high-precision dates
 *
 * M0DTcurrent()	- return current high-precision date from
 *			  system clock
 *
 * M0DTdaynum() 	- Get day number (1 Jan=1) from
 *			  high-precision date
 *
 * M0DTdiff()		- compute time increment between
 *			  high-precision dates
 *
 * M0DTearlier()	- compare high-precision dates
 *
 * M0DTextract()	- get components from high-precision date
 *
 * M0DTfromcydhms()	- construct a high-precision date from
 *			  day (ccyyddd form) and time (hhmmss form)
 *
 * M0DTget_absday()	- return absolute (Julian) day component
 *                        from a high-precision date
 *
 * M0DTget_seconds()	- return seconds since midnight from
 *			  a high-precision date
 *
 * M0DTidentical()	- are high-precision dates identical?
 *
 * M0DTinc()		- add high-precision time increment to a
 *			  high-precision date
 *
 * M0DTmake()		- make high-precision date from components
 *
 * M0DTrange()		- return limits of dates that McIDAS can
 *			  accept -- in high-precision date form
 * 
 * M0DTset_absday()	- reset absolute (Julian) day component
 *                        in a high-precision date
 *
 * M0DTset_seconds()	- reset seconds since midnight in
 *			  a high-precision date
 *
 * M0DTstr()		- allocate and write a null-terminated
 *			  string representation of a high-precision
 *			  date
 *
 * M0DTtocydhms()	- compute the day (ccyyddd) and time
 *			  (hhmmss) from a high-precison date
 *
 * M0jd_extract()	- break Julian Day into components
 *
 * M0jd_make()		- construct Julian Day from components
 *
 * M0TMextract()	- get components from high-precision time
 *
 * M0TMmake()		- make high-precision time from components
 *
 ******************************************************************/

extern int
M0cmddt(int position, const char *printmsg,
  M0DT DTdef, M0DT DTmn, M0DT DTmx, M0DT *pDTvalue);

extern double
M0dabtim(M0DT epoch);

extern M0DT
M0DTavg(M0DT DTa, M0DT DTb);
	
extern M0DT
M0DTcurrent(void);

extern int
M0DTdaynum(M0DT date);

extern M0TM
M0DTdiff(M0DT DTstart, M0DT DTend);

extern M0flag
M0DTearlier(M0DT DTearly, M0DT DTlate);

extern void
M0DTextract(M0DT date, int *year, int *month, int *day, int *hour,
  int *minute, int *second, int *microsecond);

extern void
M0DTfromcydhms(int cyd, int hms, int micro, M0DT *pDT);

extern int
M0DTget_absday(M0DT DT);

extern M0TM
M0DTget_seconds(M0DT DT);

extern M0flag
M0DTidentical(M0DT DTa, M0DT DTb);

extern M0DT
M0DTinc(M0DT DTstart, M0TM TMinc);

extern M0DT
M0DTmake(int year, int month, int day, int hour,
  int minute, int second, int microsecond);

extern void
M0DTrange(M0DT *pDTmn, M0DT *pDTmx);

extern void
M0DTset_absday(int absday, M0DT *pDT);

extern void
M0DTset_seconds(M0TM seconds, M0DT *pDT);

extern int
M0DTstr(M0DT DT, char **string);

extern void
M0DTtocydhms(M0DT DT, int *cyd, int *hms, int *micro);

void
M0jd_extract(int julday, int *pYear, int *pMonth, int *pDay);

int
M0jd_make(int year, int month, int day);

extern M0TM
M0TMmake(double day, double hour, double minute,
  double second, double microsecond);

extern void
M0TMextract(M0TM time_interval, int *day, int *hour, int *minute,
  int *second, int *microsecond);


/*******************************************************************
 *            GROUP: Dynamic 2-d matrix
 *
 *		    default behavior	if M0DEBUG defined	
 *					when caller is compiled
 *
 * M0MTnew()	- create 2-d matrix
 *		  object
 *
 * M0MTresize()	- resize 2-d matrix
 *		  object
 *
 * M0MTdel()	- destroy 2-d matrix
 *		  object
 *
 * M0MTvalid()	- (none)		validate memory allocations
 *					for 2-d matrix object
 *
 ******************************************************************/



extern int
M0MTnew(M0MT **phMT, int nrow, int ncol);

extern int
M0MTresize(M0MT **phMT, int nrow, int ncol);

extern void
M0MTdel(M0MT **phMT);

#ifdef M0DEBUG

extern M0flag
M0MTvalid(M0MT *hMT, int row, int col);

extern void
M0MTmemchk(M0MT *hMT);

#endif

/*******************************************************************
 *            GROUP: Univariate statistics accumulation and display
 *
 * M0STclear()	- clear statistics register for a quantity
 *
 * M0STupdate()	- add a value to statistics register
 *
 * M0STshow()	- generate and display statistics
 *
 ******************************************************************/

void
M0STclear(M0ST *pStats);

void
M0STupdate(M0ST *pStats, double value);

void
M0STshow(M0ST *pStats, char *szLabel, FILE *stream);




/*******************************************************************
 *            GROUP: Miscellaneous earth coordinate manipulation
 *
 * true_lon()		- force a longitude into -180<lon<=180
 *
 * lon_range()		- Force a longitude difference into
 * 			  0<=dlon<360
 *
 ******************************************************************/

extern double
true_lon(double lon);

extern double
lon_range(double dlon);



/*******************************************************************
 *            GROUP: Position and vector operations
 *
 *
 * M0PTdiff()		- compute difference vector between two
 *			  positions
 * 
 * M0PTidentical()	- are two positions the same?
 *
 * M0PTmake()		- create a position from components
 * 
 * M0PToffset()		- displace a position by a given vector
 *
 * M0PTvc()		- convert position to position vector 
 *
 * M0VCangle()		- compute angle between two vectors
 *
 * M0VCcross()		- compute cross product of two vectors
 *
 * M0VCdiff()		- compute difference vector between
 *			  two vectors
 * 
 * M0VCinner()		- compute inner product of two vectors
 *
 * M0VCmag()		- compute vector magnitude (length)
 *
 * M0VCnorm()		- normalize vector (compute unit vector)
 *
 * M0VCproject()	- compute projection of vector into a plane
 *
 * M0VCpt()		- convert position vector to position
 *
 * M0VCrotate() 	- rotate one vector about another
 * 
 * M0VCscale()		- multiply vector by scalar
 *
 * M0VCvalid_type()	- is vector's coordinate type valid?
 *			  (debug only)
 *
 * M0zrot()		- rotate vector about z axis
 * 
 *
 * (the following are used by planet.c only)
 *
 *
 * M0adr_xyz()		- convert right-ascension/declination to
 *			  Cartesian  (planet only)
 *
 * M0PTget()		- read position from stream (planet only)
 * 
 * M0PTput()		- write position to stream (planet only)
 *
 * M0PTrange()		- force position components into
 *			  standard ranges (planet only)
 *
 * M0PTvalid_type()	- is position's coordinate type valid?
 *			  (debug only)
 *
 * M0VCget()		- read vector from stream (debug)
 *
 * M0VCput()		- write vector to stream (debug)
 *
 * M0VCzero()		- is vector zero? (planet)
 *
 * M0xyz_adr()		- convert Cartesian to right-ascension/
 *			  declination coordinates (planet)
 *
 * (the following are FORTRAN routines called from C)
 *
 * m0vcpro_()		- compute projection of a vector into
 *			  a plane 
 *
 * m0vcrot_()		- compute rotation of a vector about an
 *			  arbitrary axis
 *
 ******************************************************************/





 /* FUNCTION PROTOTYPES - positions (PT)		*/


extern M0VC
M0PTdiff(M0PT PTa, M0PT	PTb);

extern M0flag
M0PTidentical(M0PT PTa, M0PT PTb);

extern M0PT
M0PTmake(double c1, double c2, double c3, M0CT type);

extern M0PT
M0PToffset(M0PT	PT, M0VC VC);

extern M0VC
M0PTvc(M0PT PT);


 /* FUNCTION PROTOTYPES - vectors (VC)		*/


extern double
M0VCangle(M0VC VCa, M0VC VCb);

extern M0VC
M0VCcross(M0VC VCa, M0VC VCb);

extern M0VC
M0VCdiff(M0VC VCa, M0VC	VCb);

extern double
M0VCinner(M0VC VCa, M0VC VCb);

extern double
M0VCmag(M0VC VC);

extern M0VC
M0VCnorm(M0VC VCold);

extern M0VC
M0VCproject(M0VC VCnorm, M0VC VCa);

extern M0PT
M0VCpt(M0VC VC);

extern M0VC
M0VCrotate(M0VC	VCaxis, double angle, M0VC VCa );

extern M0VC
M0VCscale(double scale, M0VC VC);

extern M0flag
M0PTequal(M0PT	PTa, M0PT PTb);

extern void
M0zrot(double a[3], double gamma, double b[3]);

/* FUNCTION PROTOTYPES -- FORTRAN ROUTINES */

extern void
m0vcpro_(double *, double *, double *);

extern void
m0vcrot_(double *, double *, double *, double *);



/* Routines used in 'planet.c' module only	*/

extern void
M0xyz_adr(double xyz[3], double adr[3]);

extern void
M0adr_xyz(double adr[3], double xyz[3]);

extern M0VC
M0VCget(char *);

extern void
M0VCput(char *, M0VC);

extern M0PT
M0PTget(char *);

extern void
M0PTput(char *, M0PT);

extern void
M0PTrange(M0PT *pPT);

extern M0flag
M0PTvalid_type(M0CT type);

extern M0flag
M0VCzero(M0VC VCtest);

extern M0flag
M0VCvalid_type(M0CT type);


#endif
