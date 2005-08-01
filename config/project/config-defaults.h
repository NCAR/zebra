/* ================================================================
 * If you want to change the setting of anything in this file, override it
 * in config.h.  This file is not meant to be changed.  If a definition 
 * in config.h causes a conflict with a definition in this file, then 
 * surround the definition in this file with ifndef.
 *
 * This file is ONLY meant to be include by config-defaults.h, and hence
 * is not surrounded by its own ifndefs.
 * ================================================================ */


/*========================================================================
 * Distribution Options
 *------------------------------------------------------------------------
 * This section of definitions is meant for those who are creating a
 * customized Zebra distribution.  If you are only installing Zebra at your
 * local site, then you are finished configuring Zebra.  Save this file and
 * continue with the installation instructions.  Otherwise, read on.
 *
 * Zebra's Imakefiles contain several built-in features meant for creating
 * and customizing Zebra distribution trees.  In particular, here are some
 * important targets to be familiar with:
 *
 * distfiles::		Echoes to the terminal the path names of all files
 *			which are meant to be distributed in a directory
 *			and all of the subdirectories.  By setting the
 *			DISTCURDIR variable on the command line, all of the
 *			file names printed will be prefixed with the value
 *			of the variable.  See the tarfile: target in the
 *			top-level Imakefile for an example of how this can
 *			be used.
 *
 * distmakefiles::	Makes Makefiles for ALL of the distribution 
 *			directories, not just those which have been "turned
 *			on" to be built.  This is necessary before performing
 *			other dist targets in order to create the Makefile
 *			which the dist targets will operate on.
 *
 * distclean::		Cleans in ALL directories in the distribution.  This
 *			is useful when programs have been compiled
 *			explicitly from within a directory which has been
 *			disabled in the regular distribution tree.  From the
 *			top-level, this target also removes the ./share
 *			directory of header file links created by make depend.
 *
 * It used to be that several symbols existed for defining parts of the tree
 * which should not be distributed.  This is no longer necessary as the build
 * configuration just ignores any directories which are not present.
 * Therefore, if you want to create a distribution which does not contain
 * the ingest directory, Optimizer, data utilities, map utilities, or xhelp, 
 * just remove those directories from your tree.  Those directories and their
 * files will not be included in the distfiles output, and no builds will
 * recurse into those directories.
 *
 * Questions about any of this can go to granger@ncar.ucar.edu
 * Also see the chapter on Imake in the Zebra Developer's Guide.
 */

# ifdef MAKING_MAKEFILE
/*
 * If you're using Mosaic exclusively and do not want to build or install
 * the xhelp utility, define BuildXHelp to NO.  It defaults to NO if
 * UseMosaic is YES and BuildXHelp is not defined.  If you want the default
 * to be Mosaic, but you still want to compile xhelp capability, define
 * BuildXHelp to YES and UseMosaic to YES.  If BuildXHelp is NO, then zebra
 * will always try to use Mosaic no matter what (since xhelp won't be there).
 *
 * If you have Mosaic and will not be using older project configurations
 * such as the StormFEST CD's (which have project-specific info under xhelp),
 * then you may as well define this to NO.
 */
# define BuildXHelp NO

#endif /* MAKING_MAKEFILE */

#ifndef MOSAIC_COMMAND
# define MOSAIC_COMMAND "netscape"
#endif

/*========================================================================
 * Selecting Graphics Process Plot Types
 *------------------------------------------------------------------------
 * The following definitions allow you to configure in or out various
 * pieces of the Zebra graphics process.  There are very few cases in which
 * it is desirable to change any of the following. (HENCE the reason they
 * were moved here to simplify config.h.)
 *------------------------------------------------------------------------*/
/*
 * --- The various plot types ---
 */
# define C_PT_CAP	YES		/* Constant altitude plots	*/
# define C_PT_SKEWT	YES		/* Skew T plots			*/
# define C_PT_XSECT	YES		/* Cross section plots		*/
# define C_PT_TSERIES	YES		/* Time series plots		*/
# define C_PT_XYGRAPH	YES		/* XY-Graph plots		*/
# define C_PT_XYWIND	YES		/* XY-Graph wind plots		*/
# define C_PT_HISTOGRAM	YES		/* Histograms			*/
# define C_PT_THETAPLOT	YES		/* Theta plots			*/

/*
 * --- CAP subplots ---
 *
 * Define these YES if you want them.  If C_PT_CAP above is not YES, then
 * ALL of these definitions will be ignored and none of these plots will
 * be built.  Again, it is rare that anyone would want or need to define
 * any of these to NO.
 *
 * The one possible exception is POLAR, which enables the "polar"
 * representation and contours of polar data.  This is probably not
 * useful to the bulk of people out there, but there is no harm in
 * including it either.  We could make it conditional on
 * HasSweepFiles, except currently that is only defined when
 * MAKING_MAKEFILE and so is not available here.  Polar plots
 * currently only work from sweepfile data, so you will need to have
 * enabled sweepfile access as well.
 */
# if C_PT_CAP
#	define C_CAP_OVERLAY	YES	/* Overlays			*/
#	define C_CAP_VECTOR	YES	/* Vector plots			*/
# 	define C_CAP_LIGHTNING	YES	/* Lightning location		*/
#	define C_CAP_RASTER	YES	/* Raster plots			*/
#	define C_CAP_TRACKS	YES	/* Track plots (e.g. aircraft)	*/
# 	define C_CAP_POLAR	YES	/* Plots of polar data		*/
# endif
/*------------------------------------------------------------------------*/

/*========================================================================
 * Customization Options
 *------------------------------------------------------------------------
 * Several parameter defaults used throughout the Zebra distribution can be
 * modified by setting them in config.h rather than changing them in all of the
 * include files.  The intention is that certain projects can keep their
 * settings in this file and turn them on by defining a single symbol, like
 * ARM_PROJECT for the ARM project.  All of the symbols defined here have
 * the prefix 'CFG_'.  If a symbol is not defined, then the include files
 * use their own defaults.
 */

/* #define ARM_PROJECT		*/
/* #define NEXUS_PROJECT	*/

/*------------------------------------------------------------------------
 * //////////////////// Settings for the ARM project /////////////////////
 *------------------------------------------------------------------------*/
#ifdef ARM_PROJECT

/* 
 * The default altitude units for datachunks and for netCDF files whose alt
 * field does not have a units attribute (or has a blank one).  If the
 * altitude units of a datachunk are not explicitly overridden, this will
 * be the units which are stored in the "units" attribute of the "alt"
 * field in netCDF files.
 */
#define CFG_ALTITUDE_UNITS	AU_mMSL

#define DistributeIngest 	NO
#define DistributeOptimizer 	NO

#endif /* ARM_PROJECT */
/*------------------------------------------------------------------------*/


/*------------------------------------------------------------------------
 * //////////////////////// Settings for NEXUS ///////////////////////////
 *------------------------------------------------------------------------*/
#ifdef NEXUS_PROJECT

#define DistributeIngest 	NO
#define DistributeOptimizer 	NO
#define DistributeDataUtil 	NO
#define DistributeRealtimeDataStoreTools YES

#endif /* NEXUS_PROJECT */
/*------------------------------------------------------------------------*/


/*------------------------------------------------------------------------*/
/* ------------------ Defaults for compile-time constants ----------------*/
/*------------------------------------------------------------------------*/
/* The definitions below will not usually need to be changed, but they are
 * provided here so that they can be conveniently found and changed in
 * single file.  DO NOT EDIT THESE UNLESS YOU KNOW WHAT YOU ARE DOING!  If
 * any of these definitions are unreasonable or downright incorrect, you
 * may not be able to compile or run Zebra.
 **************************************************************************/

/* #define CFG_WARN_FIELD_NAMES *//* Warn about CDL-illegal field names */

/*
 * If defined, the 'alt' field will not be given a 'units' attribute in
 * netCDF files.
 */
/* #define CFG_NC_NO_ALT_UNITS */

/*
 * Various string length limits
 */
#ifndef CFG_PLATNAME_LEN
#define CFG_PLATNAME_LEN 80	/* Len of platform names		     */
#endif
#ifndef CFG_DIMNAME_LEN
#define CFG_DIMNAME_LEN	32	/* Longest allowable dimension name	     */
#endif
#ifndef CFG_PDPARAM_LEN
#define CFG_PDPARAM_LEN	40	/* Length of parameter names	*/
#endif
#ifndef CFG_ASCTIME_LEN
#define CFG_ASCTIME_LEN	40	/* Length needed for encoded times*/
#endif
#ifndef CFG_PLATCLASS_LEN
#define CFG_PLATCLASS_LEN 80	/* Len of platform class names		     */
#endif
#ifndef CFG_FILENAME_LEN
#define CFG_FILENAME_LEN 32	/* Len of general file names (not full paths)*/
#endif
#ifndef CFG_DATAFILE_LEN
#define CFG_DATAFILE_LEN 50	/* Len of data file names (not full paths)   */
#endif
#ifndef CFG_FILEPATH_LEN
#define CFG_FILEPATH_LEN 256	/* Len of full path filenames		     */
#endif
#ifndef CFG_PDNAME_LEN
#define CFG_PDNAME_LEN	40	/* Len of name of a plot description table   */
#endif
#ifndef CFG_MSGNAME_LEN
#define CFG_MSGNAME_LEN	64	/* Max length of a message client name	     */
#endif
#ifndef CFG_MSGEVENT_LEN
#define CFG_MSGEVENT_LEN 200	/* Length of extended logger event messages  */
#endif
#ifndef CFG_FIELD_NAME_LEN
#define CFG_FIELD_NAME_LEN 40	/* Length of field names		     */
#endif
#ifndef CFG_FIELD_LONG_LEN
#define CFG_FIELD_LONG_LEN 256	/* Length of field description (long name)   */
#endif
#ifndef CFG_FIELD_UNITS_LEN
#define CFG_FIELD_UNITS_LEN 40	/* Length of field units string		     */
#endif
#ifndef CFG_SEARCHPATH_LEN
#define CFG_SEARCHPATH_LEN 512	/* Length of exec search paths and such	     */
#endif

/*
 * Maximum number of fields (field IDs) in the fields table
 */
#ifndef CFG_FIELD_MAX_ID
#define CFG_FIELD_MAX_ID	512
#endif

/*
 * Maximum number of platforms (including subplatforms)
 */
#ifndef CFG_MAX_PLATFORMS
#define CFG_MAX_PLATFORMS 2048
#endif

/*
 * DataChunk symbols
 */
#ifndef CFG_DC_MAXFIELD
#define CFG_DC_MAXFIELD	255	/* Maximum allowable fields in one datachunk */
#endif
#ifndef CFG_DC_MF_HASH_SIZE
#define CFG_DC_MF_HASH_SIZE 256	/* Least power of 2 > (not =) CFG_DC_MAXFIELD*/
#endif
#ifndef CFG_DC_MAXDIMN
#define CFG_DC_MAXDIMN	30	/* Max number of dimensions for NSpace chunk,*/
                                /* should be less than netCDF limit	     */
#endif
#ifndef CFG_DC_DEFAULT_BADVAL
#define CFG_DC_DEFAULT_BADVAL -99999.	/* Default floating poing bad value  */
		/* WARNING: Make sure this value contains a decimal point!   */
#endif

/*
 * Plot desctiption limits
 */
#ifndef CFG_PD_MAXCOMP
#define CFG_PD_MAXCOMP	50	/* Maximum # components expected in a PD     */
#endif
#ifndef CFG_PD_RAWTEMP
#define CFG_PD_RAWTEMP	40960	/* Raw temp buffer space for raw PD's	     */
#endif

/*
 * Message manager limits
 */
#ifndef CFG_MSG_MAXBCAST
#define CFG_MSG_MAXBCAST 1500	   /* Maximum length of a broadcast message  */
#endif
#ifndef CFG_MSG_DEFAULT_PORT
#define CFG_MSG_DEFAULT_PORT 1500  /* Default tcp port			     */
#endif
#ifndef CFG_MSG_SOCKET_NAME
#define CFG_MSG_SOCKET_NAME "/tmp/fcc.socket"
                         /* UNIX domain socket name in file system namespace */
#endif

/*
 * Display manager limits
 */
#ifndef CFG_DM_CODELEN
#define CFG_DM_CODELEN	20	/* Length of a DM event code	*/
#endif
#ifndef CFG_DM_MAXADATA
#define CFG_DM_MAXADATA 128	/* Length of DM action data	*/
#endif

/*
 * Graphics process limits
 */
#ifndef CFG_GP_MAX_CONTOURS
#define CFG_GP_MAX_CONTOURS 50	/* Contour levels allowed without aborting */
#endif

/*
 * Other parameters
 */
#ifndef CFG_ALTITUDE_UNITS
#define CFG_ALTITUDE_UNITS 	AU_kmMSL /* Default units for "alt" field */
#endif
/*------------------------------------------------------------------------*/



/*------------------------------------------------------------------------*/
/*
 * These are always NO -- do not change them!
 */
# if !C_PT_CAP
#	define C_CAP_OVERLAY	NO	/* Overlays			*/
#	define C_CAP_VECTOR	NO	/* Vector plots			*/
# 	define C_CAP_LIGHTNING	NO	/* Lightning location		*/
#	define C_CAP_RASTER	NO	/* Raster plots			*/
#	define C_CAP_TRACKS	NO	/* Track plots			*/
# 	define C_CAP_POLAR	NO	/* Plots of polar data		*/
# endif

/*
 * Some parameters for dealing with buffer lengths.
 */
# define MAX_PLAT_LEN	160	/* Maximum platform len -- remember lists */
# define MAX_PARAM_LEN	40	/* Length of parameter names		*/
# define TIME_LEN	40	/* Length needed for encoded times	*/

/* --- Some obsolete configuration symbols put here to be forgotten --- */
/* --- and to simplify the installation process                     --- */

/*
 * The PD monitor utility is an emacs program which provides interactive
 * run-time editing, checking, and monitoring of graphics processes' plot
 * descriptions.  If you have emacs (not version 19), and wish to compile
 * the emacs lisp code for the PD monitor, define HaveEmacs to YES and make
 * sure EmacsPath points to the emacs program.  The default is NO.  Even if
 * the elisp code is not compiled and installed, the elisp source files
 * will still be installed, so these can be used as is or byte-compiled
 * later.  If you have Emacs 19, the elisp code cannot be batch compiled,
 * so set HaveEmacs to NO. The Emacs 19 lisp file will still be installed.
 */
# define HaveEmacs NO
# define EmacsPath emacs

