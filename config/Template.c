/*
 * This is the master rdss configuration file, intended to be included by cpp
 * during the Makefile generation step.  You should not have to edit this
 * file.
 */

# define Yes 1
# define No  0

/*
 * Include the system-specific config file.
 */
# ifdef sun
# include "sun.h"
# endif

# ifdef titan
# include "titan.h"
# endif

/*
 * Now put in site-specific configuration information.
 */
# include "site-def.h"

/* --------------------------------------------------------------------
 * From here are supplied default parameters for things not overridden
 * in the system-specific file.
 */

/*
 * C compiler options.
 */
# ifndef UseGcc		/* Do we use the GNU C compiler?		*/
# define UseGcc No
# endif

# ifndef CCompiler	/* Decide which compiler here			*/
# if UseGcc
# define CCompiler gcc
# else
# define CCompiler cc
# endif
# endif /* CCompiler */


/*
 * Where the local libraries are kept.  
 */

# ifndef RDSSLibraries
# define RDSSLibraries /usr/local/lib
# endif

/*
 * Where the local executables are kept when installed.
 */
# ifndef RDSSBin
# define RDSSBin /usr/local/bin
# endif

/*
 * Do we have MIT X11 Release 4 or newer?  
 */
# ifndef UseXWindows
# define UseXWindows Yes
# endif	

/*
 * Default to using OldOpenWin, version 2.0
 */
# ifndef OldOpenWin
# define OldOpenWin Yes
# endif

/*
 * Do we have X11 Release 3 or older?
 */
# ifndef Old_X11
# define Old_X11 No
# endif

/*
 * Do we have netcdf?
 */
# ifndef UseNetCDF
# define UseNetCDF No
# endif

/* 
 * Do we have Makedepend?
 */
# ifndef HaveMakeDepend
# define HaveMakeDepend No
# endif

/*
 * Do we want to use Sunview?
 */
# ifndef UseSunview
# define UseSunview No
# endif

/*
 * Various system libraries. ****************************
 */
# ifndef OpenWinHome
# define OpenWinHome /usr/openwin
# endif

# ifndef XToolkitLibs		/* What else for Xt appls	*/
#  if (UseXWindows || UseOpenWin)
#    ifdef OldOpenWin
#       define XToolkitLibs
#    else
#       define XToolkitLibs -lXaw -lXmu -lXt -lXext 
#    endif /* OldOpenWin */
#  else
#    define XToolkitLibs          /* Nothing */
#  endif
# endif /* ndef XToolkitLibs */

# ifndef FortranLibs		/* Fortran libs when linking in C	*/
# define FortranLibs -lF77
# endif

# ifndef MathLib		/* Default math library */
# define MathLib -lm
# endif

/*
 * End of site-def defaults.  Following are derived Makefile macros:
 * --------------------------------------------------------------------
 */

# if (UseOpenWin && !UseXWindows)  
# define XLibrary -lX11
# define XLibraries $(OPENWINHOME)/lib
# define XInclude -I$(OPENWINHOME)/include
# endif         /* Use OpenWindows paths only if X11 is not available. */


# ifndef XLibrary		/* Set to nothing - defaults elsewhere */
# define XLibrary 
# endif

# ifndef XInclude
# define XInclude               /* Set to nothing -defaults elsewhere */
# endif

# ifndef XLibraries
# define XLibraries             /* Set to nothing -defaults elsewhere */
# endif


# if (UseOpenWin || UseXWindows)        /* XLibrary paths */
# define XLibraryPath -L$(XLIBRARYPATH) 
# else
# define XLibraryPath           /* Nothing */
# endif


/*
 * Figure out RGB color database.
 *
 */
# if (UseOpenWin)
# define ColorDB \"$(OPENWINHOME)/lib/rgb.txt\"
# elif (UseXWindows)
# define ColorDB \"XLibraries/X11/rgb.txt\"
# else
# define ColorDB \"$(ROOT)/graphics/rgb.txt\"
# endif

/*
 * Figure out the compiler flags.
 */
# ifndef CompileDebug
# define CompileDebug No
# endif

# ifndef OtherCFlags
# define OtherCFlags	/* Nothing */
# endif

# ifndef CompileOptimize
# define CompileOptimize No
# endif

# ifndef StdOptCFlag
# define StdOptCFlag -O
# endif

# if UseNetCDF
# define NetCDFFlag -DNETCDF
# define NetCDFLib -L$(NETCDF_DESTDIR)/lib -lnetcdf
# define NetCDFInclude -I$(NETCDF_DESTDIR)/include
# else
# define NetCDFFlag     /* Nothing */
# define NetCDFLib
# define NetCDFInclude
# endif

# if UseSunview
# define SunviewFlag -lsuntool -lsunwindow -lpixrect
# else
# define SunviewFlag   /* Nothing */
# endif

# ifndef XWindowsFlag
#    if ((UseXWindows || UseOpenWin) && !OldOpenWin)
#       define XWindowsFlag -DXSUPPORT
#    else
#       define XWindowsFlag   /* Nothing */
#    endif
# endif

# if UseGcc
# if CompileOptimize 
# define OptCFlag StdOptCFlag
# endif /* CompileOptimize */
# else
# if CompileOptimize && ! CompileDebug
# define OptCFlag StdOptCFlag
# endif
# endif /* UseGcc */

# ifndef OptCFlag
# define OptCFlag
# endif

# ifndef OtherFFlags		/* Other FORTRAN flags */
# define OtherFFlags	/* Nothing */
# endif

# if CompileDebug
# define StdCFlags -I$(INCLUDEDIR) -g OptCFlag OtherCFlags
# else
# define StdCFlags -I$(INCLUDEDIR) OptCFlag OtherCFlags
# endif

# if CompileDebug
# define StdFFlags -I$(INCLUDEDIR) -g OtherFFlags 
# else
# define StdFFlags -I$(INCLUDEDIR) OtherFFlags
# endif


/* ----------------------------------------------------------------------
 * Here goes the actual template info which goes into each Makefile.  You
 * shouldn't have to edit this.
 */

/**/# DO NOT EDIT -- EDIT Makefile.cpp INSTEAD!

CC = CCompiler
ROOT = RDSSRoot
INCLUDEDIR = RDSSInclude
RADARINCLUDE = RADARInclude
CFLAGS = StdCFlags $(LOCALCFLAGS)
FFLAGS = StdFFlags $(LOCALFFLAGS)
MFARCH = MakefileArch
RDSSBIN = RDSSBin
RDSSLIBRARIES = RDSSLibraries
XLIBRARYPATH = XLibraries
XLIBRARIES = XLibraryPath
XINCLUDE = XInclude
OPENWINHOME = OpenWinHome
NETCDFFLAG = NetCDFFlag
NETCDF_DESTDIR = NetCDF_DESTDIR
NETCDFLIB = NetCDFLib
NETCDFINCLUDE = NetCDFInclude
SUNVIEWFLAG = SunviewFlag
XWINDOWSFLAG = XWindowsFlag
COLORDB = ColorDB
DEFDBDIR = DefDBDir
DEFDBTYPE = DefDBType
DEFDBNAME = DefDBName

/*
 * Pull in the Makefile itself here.
 */
# include "Makefile.cpp"


/*
 * Standard rules that go at the end.
 */

Makefile:	mf

mf:
	mv Makefile Makefile~
	cc -I. -I$(TOP)/config -E $(TOP)/config/Template.c | grep -v '^# [0-9]' | cat -s >> Makefile
	make depend

testmf:
	@mftest $(MFARCH)
	


