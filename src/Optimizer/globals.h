/*
 * Global definitions
 * $Id: globals.h,v 1.1 1991-06-16 17:12:09 burghart Exp $
 */

# include <math.h>
# include <ui_param.h>		/* For bool definition */

/*
 * Trig stuff and convenience functions
 */
# define PI	3.141592654
# define DEG_TO_RAD(x)	((x)*0.017453292)
# define RAD_TO_DEG(x)	((x)*57.29577951)

# define COSD(x)	(cos (DEG_TO_RAD (x)))
# define SIND(x)	(sin (DEG_TO_RAD (x)))
# define TAND(x)	(tan (DEG_TO_RAD (x)))
# define ATAND(x)	(RAD_TO_DEG (atan (x)))
# define MIN(x,y)	((x) < (y) ? (x) : (y))
# define MAX(x,y)	((x) > (y) ? (x) : (y))

/*
 * Dummy time value for generating the fastest possible scan
 */
# define TIME_ASAP	-999.0

/*
 * Configuration name
 */
extern char	ConfigName[];

/*
 * User-specified desired spatial resolution and minimum beam separation
 */
extern float	Hres, Vres;
extern float	Hsep_min, Vsep_min;

/*
 * Volume description
 */
extern float	Vol_bot, Vol_top;

/*
 * Volume scan time to use (= TIME_ASAP if it isn't fixed)
 */
extern float	Vol_time;

/*
 * Are we connected to the message handler?
 * Is the data store available?
 */
extern bool	Msg;
extern bool	Ds;

/*
 * Step to use for generating alternate resolutions
 */
# define RES_STEP	0.02
