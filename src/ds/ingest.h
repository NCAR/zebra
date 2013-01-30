/*
 * ingest.h --- Public protoypes and macros for ingest modules
 *
 * $Id: ingest.h,v 1.16 1998-10-28 21:21:14 corbet Exp $
 *
 * ingest.h --- A common ingest interface and support routines for 
 *		Zeb ingest modules
 *
 * In general, it defines the IngestLog() function which allows its
 * messages to be directed to stdout and/or to the EventLogger.
 * stderr messages can be screened with the command line option -log.
 * Other command line options allow disabling the DataStore and message
 * sending for testing and running in 'dryrun' mode.  Calls to
 * ds_Store and ds_LookupPlatform are macro-defined in ingest.h
 * to point to ingest.c's internal implementations.  These implementations
 * flag ds_Store calls and failures, or ignores the calls when
 * DryRun mode is in effect.  See IngestUsage() in ingest.c for the full list
 * of ingest options, or use the -help option with any program that uses
 * IngestParseOptions().
 *
 * An application using the ingest package should do the following:
 *
 * #include "ingest.h"
 *
 * Parse the ingest cmd-line options with IngestParseOptions().  Call
 * IngestInitialize(), which intializes the message and DataStore unless
 * these have been disabled by the application or command-line options.
 * Call IngestUsage() from the application's usage() function to show the
 * user what the ingest options are.  Alternatively, if the application has
 * no usage() function, just pass IngestUsage() to IngestParseOptions()
 * as the usage function.  Lastly, use IngestLog() rather than
 * msg_ELog() to log messages with the EventLogger.  The application's
 * usage() function should take one argument, the name of the
 * program (usually passed as argv[0]).
 *
 * IngestInitialize() also installs a default message handler which
 * handles MH_SHUTDOWN calls.  This handler can be overridden by
 * defining your own protocol handlers with the message library functions.
 */

# ifndef _ingest_h_
# define _ingest_h_

# include <stdio.h>
# include <ctype.h>

# include <defs.h>
# include <message.h>
# include <timer.h>
# include "DataStore.h"

# ifndef streq
# define streq(a,b) (strcmp(a,b) == 0)
# endif

# if __cplusplus
extern "C"
{
# endif

/* ---------------------------------------------------------------------
 * Ingest Public Prototypes:						*/

/* extern void	IngestLog (); */
#define IngestLog msg_ELog
extern void	IngestParseOptions FP((int *argc, char *argv[], 
					void (*usage)(/* char *prog */)));
extern void 	IngestInitialize FP((char *module_name));
extern void	IngestUsage();

extern zbool _Ingest_ds_Store FP((DataChunk *dc, int newfile, 
				dsDetail *details, int ndetail));
#ifdef notdef
extern zbool _Ingest_ds_StoreBlocks FP((DataChunk *dc, int newfile, 
				       dsDetail *details, int ndetail));
#endif
extern PlatformId _Ingest_ds_LookupPlatform FP((char *name));

/*
 * A useful function for removing recognized options from argv[]
 */
extern void IngestRemoveOptions FP((int *argc, char *argv[], int i, int n));


/*
 * During testing and DryRun mode, the ds functions will be 
 * omitted by the Ingest cover functions _Ingest_ds*
 */
#define ds_Store(a,b,c,d) _Ingest_ds_Store(a,b,c,d)
#define ds_LookupPlatform(a) _Ingest_ds_LookupPlatform(a)
#define ds_StoreBlocks(a,b,c,d) _Ingest_ds_Store(a,b,c,d)
/* #define ds_DeleteData(a,b) _Ingest_ds_DeleteData(a,b) */

/* -------------------------------------------------------------------- */

/*
 * Define a NEW event logger flag for messages that will only be written
 * while a program is under development, i.e. being run in DryRun mode.  If
 * a message is passed to IngestLog with this flag only, the message will
 * be written to stderr according to IngestLogFlags just like any other
 * event message.  However, messages with only EF_DEVELOP set will not be
 * sent to the ELogger regardless of the value of NoEventLogger.  The goal
 * of this flag is to allow voluminous output when debugging and running
 * stand-alone, i.e.  during development, but restrict such debugging
 * output when messages are actually being sent to the EventLogger, such as
 * during normal operations.  This reduces clutter in the EventLogger.
 */
#ifndef EF_DEVELOP
#define EF_DEVELOP (0x400)	/* moved to message.h */
#endif

/*
 * Exported flags so that ingest modules can test their debug state
 */
extern int   IngestLogFlags;
extern short NoDataStore;
extern short NoMessageHandler;
extern short NoEventLogger;
extern short DryRun;
extern short DumpDataChunks;
extern short ShowIngestName;

/* -----------------------------------------------------------------
 * These macros allow ingest modules to set their default debug
 * state within the program, such as during development, so that the
 * command line does not have to be crowded with options.
 *
 * Do not use these anymore.  Use the macros prefixed with Ingest
 * instead, defined further on.
 */
#define SetNoMessageHandler() {\
		NoDataStore = 1; \
		NoMessageHandler = 1; \
		NoEventLogger = 1; \
		}

#define SetNoEventLogger() {\
		NoEventLogger = 1; \
		}

#define SetNoDataStore() {\
		NoDataStore = 1;  \
		}

#define SetDryRun() {\
		DryRun = 1; \
		SetNoMessageHandler(); \
		}

#define IngestShowName() {\
		ShowIngestName = 1; \
		}

#define IngestSetNoMessageHandler()	SetNoMessageHandler()
#define IngestSetNoEventLogger()	SetNoEventLogger()
#define IngestSetNoDataStore()		SetNoDataStore()
#define IngestSetDryRun()		SetDryRun()

# if __cplusplus
} // end of extern "C"
# endif


# endif /* _ingest_h_ */
