
XCOMM ================================================================
XCOMM $Id: config-ingest.h,v 1.4 2003-01-28 22:52:24 burghart Exp $
XCOMM Definitions from config-ingest.h.
XCOMM ================================================================

/*
 * This file configures settings specific to the ingest and data
 * utility source tree, such as which ingest modules to build,
 * paths to special libraries they need, and FORTRAN compiler and
 * libraries.
 */
# ifdef MAKING_MAKEFILE

/*
 * Here you can select which individual ingest modules to build.  
 */
#ifndef BuildIngestScheduler
# define	BuildIngestScheduler	NO	/* Sun only	 	*/
#endif
#ifndef BuildClassIngest
# define	BuildClassIngest	NO
#endif
#ifndef BuildRadarIngest
# define	BuildRadarIngest	NO	/* Sun only, see PCAP below*/
#endif
#ifndef BuildSLGrabber
# define	BuildSLGrabber		NO	/* Sun only		*/
#endif
#ifndef BuildNowradIngest
# define	BuildNowradIngest	NO
#endif
#ifndef BuildProfsIngest
# define	BuildProfsIngest	NO
#endif
#ifndef BuildSatelliteIngest
# define	BuildSatelliteIngest	NO
#endif
#ifndef BuildP3Composite
# define	BuildP3Composite	NO
#endif
#ifndef BuildTOGASlowTape
# define	BuildTOGASlowTape	NO
#endif
#ifndef BuildSlowTapeIngest
# define	BuildSlowTapeIngest	NO
#endif
#ifndef BuildTAOIngest
# define 	BuildTAOIngest		NO
#endif
#ifndef Build
# define	Build_GMS_ISCCP		NO
#endif
#ifndef Build
# define	Build_TRMM_SSMI		NO
#endif
#ifndef Build
# define	Build_TRMM_Rain		NO	/* Sun only		*/
#endif
#ifndef Build
# define	Build_TRMM_Sonde	NO
#endif
#ifndef Build
# define	Build_FEST_P3_C130	NO
#endif
#ifndef BuildPrecipIngest
# define	BuildPrecipIngest	NO
#endif
#ifndef BuildDAPIngest
# define	BuildDAPIngest		NO
#endif
#ifndef BuildRAPDataServerIngest
# define	BuildRAPDataServerIngest NO
#endif
#ifndef BuildWetnetIngest
# define	BuildWetnetIngest	NO
#endif

/*
 * The PAM ingest programs (pam_ingest and daypam_ingest) require libraries
 * which are compiled within the RDSS source tree.  Do not define this to YES
 * unless your RDSS source tree still contains the compiled mda, pamutil, and
 * unp libraries (i.e., it all built successfully and has not been cleaned).
 */
#ifndef BuildPamIngest
# define	BuildPamIngest		NO   /* Requires RDSS compiled tree */
#endif

/*
 * The data file utilities are the program for handling various file 
 * and converting files to Zebra's netCDF conventions.  Some examples
 * are gprotocdf and mudtocdf, for GENPRO and MUDRAS, respectively.
 * Unless you know you will need them, you might as well wait and only
 * compile the programs you need when you need them.
 */
#ifndef BuildDataUtilities
# define	BuildDataUtilities		NO
#endif

/* The following 2 libraries will probably need to be explicitly
 * defined here, but ONLY if you need them for ingest modules.
 *
 * The SUDS library is only needed for the PAM and CLASS ingest modules.
 *
 *	SudsLibrary	$(RDSSDIR)/suds/libsuds.a
 *
 * The 'PAM configuration library', libunp.a, is only needed by
 * the PAM ingest module.
 *
 *	PamCfgLibrary	-lunp
 *
 * If you don't need to ingest any PAM or CLASS data (which will be the
 * case 99% of the time), you can safely ignore the definition of the
 * two symbols above.
 */

/*
 * This one works if the RDSS source tree is in /rdss and has not been cleaned
 */
/* # define PamCfgLibrary /rdss/pam/cfg/access/libunp.a */

/*
 * This one is for RDSS libraries installed into /usr/local/lib
 */
/* # define PamCfgLibrary /usr/local/lib/libunp.a */

/*
 * If you have a nonstandard Fortran compiler (i.e. your "make" won't figure
 * it out automatically) set it here.  (This example for the GNU compiler).
 * If you are compiling under Linux, the appropriate FORTRAN compiler and
 * libraries are given defaults further down in this file, and you don't
 * need to uncomment this definition.
 */
/* # define FortranCompiler g77 */

/*
 * FORTRAN libraries tend to be very system-specific.  Define this
 * parameter to be the command-line options necessary to link with
 * the FORTRAN libraries on your system.  If not using g77 or Suns,
 * the defaults below are probably fine.  Otherwise, uncomment the
 * g77 example definition below, or choose the correct definition
 * from among the examples for Sun architectures.
 *
 * Fortran libraries are no longer needed to build the graphics process.
 * They are only necessary if building the data utilities or certain
 * ingestors, such as the satellite GOES and GVAR ingestors.
 *
 * Similar to FortranCompiler, FortranLibraries has a default for
 * Linux defined below.
 */

/* G77 -- may also have to give -L if not linking with gcc */
/* # define FortranLibraries -lf2c */

#ifndef FortranLibraries
#ifdef SunArchitecture
/*
 * Change the FortranLibraries definition below if you are not using the
 * SC4.2 version of the Fortran compiler.
 */
#ifndef FortranLibraries
# define FortranLibraries -L/opt/SUNWspro/SC4.2/lib -lF77 -lV77 -lM77 -lsunmath
#endif

#else
#ifdef HPArchitecture /* HP-UX */

# define FortranLibraries -lU77 -lf -lisamstub

#else
#ifdef SGIArchitecture /* IRIX 5.x */

# define FortranLibraries -lF77 -lI77 -U77 -lisam

#else
#ifdef LinuxArchitecture

# define FortranCompiler g77
# define FortranLibraries `g77 --print-file-name=libg2c.a`

#else 
#ifdef OSF1Architecture
# define FortranCompiler f77
# define FortranLibraries -lfor -lFutil -lUfor -lots
 
#else
#ifdef AIXArchitecture

# define FortranCompiler xlf
# define FortranLibraries -lxlf -lxlf90
 
#endif /* AIX */
#endif /* OSF */
#endif /* Linux */
#endif /* SGI */
#endif /* !HP */
#endif /* !Sun */
#endif /* !FortranLibraries */

# endif /* MAKING_MAKEFILE */
